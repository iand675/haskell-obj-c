{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type-safe Objective-C message sending.
--
-- This module wraps the low-level @libffi@-based 'ObjC.Runtime.MsgSend' API
-- with a type-safe interface driven by the phantom type parameters on
-- 'Selector'.  Each element in the selector's type-level argument list
-- becomes a curried function parameter, and the return type is
-- unmarshalled automatically.
--
-- @
-- -- Old style (manual libffi plumbing):
-- sendMsg obj sel (retPtr retVoid) [argPtr rawToken, argPtr rawSecret]
--
-- -- New style (type-safe, curried):
-- sendMessage obj sel token secret
-- @
--
-- Uses the @varargs@ library ('VarArgs.sequenceArgs') to lift monadic
-- CPS brackets into variadic function argument positions, keeping all
-- pointer-backed values alive until the FFI call completes.
module ObjC.Runtime.Message
  ( -- * Type-safe message sending
    sendMessage
  , sendOwnedMessage
  , sendClassMessage
  , sendOwnedClassMessage

    -- * Marshalling type classes
  , ObjCArgument(..)
  , ObjCReturn(..)
  , MsgSendVariant(..)

    -- * Argument gathering (exported for extensibility)
  , GatherObjCArgs(..)
  ) where

import Data.Int (Int8)
import Data.Kind (Type)
import Foreign.C.Types
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.LibFFI
import Foreign.Ptr (Ptr, FunPtr, castPtr)
import VarArgs (type (:->:), VarArgs, sequenceArgs)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (objcMsgSendPtr, objcMsgSendFpretPtr, objcMsgSendStretPtr)

-- ---------------------------------------------------------------------------
-- MsgSend variant selection
-- ---------------------------------------------------------------------------

-- | Which @objc_msgSend@ variant to use for a given return type.
--
-- On ARM64 all three are identical, but on x86_64 floating-point and
-- struct returns require dedicated trampolines.
data MsgSendVariant
  = MsgSendNormal  -- ^ Standard @objc_msgSend@
  | MsgSendFpret   -- ^ @objc_msgSend_fpret@ (x86_64 float\/double returns)
  | MsgSendStret   -- ^ @objc_msgSend_stret@ (x86_64 large-struct returns)

msgSendFunPtr :: MsgSendVariant -> FunPtr a
msgSendFunPtr MsgSendNormal = objcMsgSendPtr
msgSendFunPtr MsgSendFpret  = objcMsgSendFpretPtr
msgSendFunPtr MsgSendStret  = objcMsgSendStretPtr

-- ---------------------------------------------------------------------------
-- ObjCArgument — marshalling Haskell values to libffi Args
-- ---------------------------------------------------------------------------

-- | Types that can be marshalled as Objective-C message arguments.
--
-- The CPS style ensures that pointer-backed values (e.g., 'Id') remain
-- alive for the duration of the FFI call via nested 'withForeignPtr'
-- brackets.
--
-- To add support for a custom type, provide an instance:
--
-- @
-- instance ObjCArgument MyStruct where
--   withObjCArg s k = 'Foreign.LibFFI.Base.withStorableArg' s k
-- @
class ObjCArgument a where
  -- | Marshal a value to a libffi 'Arg', keeping it alive in the
  -- continuation @k@.
  withObjCArg :: a -> (Arg -> IO r) -> IO r

-- | Managed object pointer.  Uses 'withForeignPtr' to keep the object
-- alive until the FFI call completes.
instance ObjCArgument (Id a) where
  withObjCArg (Id fp) k =
    withForeignPtr fp $ \ptr ->
      k (argPtr (castPtr ptr :: Ptr ()))

-- | Const-qualified argument — delegates to the underlying type.
instance ObjCArgument a => ObjCArgument (Const a) where
  withObjCArg (Const x) k = withObjCArg x k

-- | Volatile-qualified argument — delegates to the underlying type.
instance ObjCArgument a => ObjCArgument (Volatile a) where
  withObjCArg (Volatile x) k = withObjCArg x k

-- | Raw pointer argument.
instance ObjCArgument (Ptr a) where
  withObjCArg ptr k = k (argPtr ptr)

-- | Unmanaged raw object pointer.
instance ObjCArgument RawId where
  withObjCArg (RawId ptr) k = k (argPtr ptr)

-- | Class pointer argument (for methods that take a @Class@ parameter).
instance ObjCArgument Class where
  withObjCArg (Class ptr) k = k (argPtr ptr)

-- | Selector argument (e.g., for @performSelector:@).
instance ObjCArgument (Selector args ret) where
  withObjCArg (Selector ptr) k = k (argPtr ptr)

-- Floating-point --------------------------------------------------------

instance ObjCArgument CDouble where
  withObjCArg x k = k (argCDouble x)

instance ObjCArgument CFloat where
  withObjCArg x k = k (argCFloat x)

-- Integer types ---------------------------------------------------------

instance ObjCArgument CLong where
  withObjCArg x k = k (argCLong x)

instance ObjCArgument CULong where
  withObjCArg x k = k (argCULong x)

instance ObjCArgument CInt where
  withObjCArg x k = k (argCInt x)

instance ObjCArgument CUInt where
  withObjCArg x k = k (argCUInt x)

instance ObjCArgument CShort where
  withObjCArg x k = k (argCInt (fromIntegral x))

instance ObjCArgument CUShort where
  withObjCArg x k = k (argCUInt (fromIntegral x))

instance ObjCArgument CSChar where
  withObjCArg x k = k (argCChar (fromIntegral x))

instance ObjCArgument CChar where
  withObjCArg x k = k (argCChar x)

instance ObjCArgument CUChar where
  withObjCArg x k = k (argCUChar x)

-- Boolean types ---------------------------------------------------------

-- | Haskell 'Bool' encoded as @BOOL@ (unsigned char: 0 or 1).
instance ObjCArgument Bool where
  withObjCArg b k = k (argCUChar (if b then 1 else 0))

-- | Objective-C @BOOL@ (backed by @CBool@ / unsigned char).
instance ObjCArgument ObjCBool where
  withObjCArg (ObjCBool (CBool b)) k = k (argCUChar (CUChar b))

-- ---------------------------------------------------------------------------
-- ObjCReturn — unmarshalling libffi return values
-- ---------------------------------------------------------------------------

-- | Types that can be returned from Objective-C messages.
--
-- 'RawReturn' is the intermediate type that @libffi@ produces, which is
-- then converted to the final Haskell type via 'fromRetained' or
-- 'fromOwned'.
--
-- * 'fromRetained' — for most methods: retains the returned object
--   (wraps a +0 / autoreleased reference).
-- * 'fromOwned' — for @init@\/@copy@\/@new@\/@mutableCopy@ methods:
--   takes ownership of a +1 reference without an extra retain.
--
-- For non-object types both methods are identical.
class ObjCReturn a where
  -- | The raw type returned by @libffi@.
  type RawReturn a

  -- | The @libffi@ return type descriptor.
  objcRetType :: RetType (RawReturn a)

  -- | Which @objc_msgSend@ variant to use.
  msgSendVariant :: MsgSendVariant

  -- | Convert from the raw return value with __retained__ semantics.
  fromRetained :: RawReturn a -> IO a

  -- | Convert from the raw return value with __owned__ semantics.
  fromOwned :: RawReturn a -> IO a

-- Void ------------------------------------------------------------------

instance ObjCReturn () where
  type RawReturn () = ()
  objcRetType = retVoid
  msgSendVariant = MsgSendNormal
  fromRetained = pure
  fromOwned = pure

-- Managed object pointers -----------------------------------------------

-- | Default: 'fromRetained' calls 'retainedObject', 'fromOwned' calls
-- 'ownedObject'.
instance ObjCReturn (Id a) where
  type RawReturn (Id a) = Ptr ()
  objcRetType = retPtr retVoid
  msgSendVariant = MsgSendNormal
  fromRetained = retainedObject . castPtr
  fromOwned = ownedObject . castPtr

-- Raw pointers ----------------------------------------------------------

instance ObjCReturn (Ptr a) where
  type RawReturn (Ptr a) = Ptr ()
  objcRetType = retPtr retVoid
  msgSendVariant = MsgSendNormal
  fromRetained = pure . castPtr
  fromOwned = pure . castPtr

-- Floating-point (use fpret variant on x86_64) --------------------------

instance ObjCReturn CDouble where
  type RawReturn CDouble = CDouble
  objcRetType = retCDouble
  msgSendVariant = MsgSendFpret
  fromRetained = pure
  fromOwned = pure

instance ObjCReturn CFloat where
  type RawReturn CFloat = CFloat
  objcRetType = retCFloat
  msgSendVariant = MsgSendFpret
  fromRetained = pure
  fromOwned = pure

-- Integer types ---------------------------------------------------------

instance ObjCReturn CLong where
  type RawReturn CLong = CLong
  objcRetType = retCLong
  msgSendVariant = MsgSendNormal
  fromRetained = pure
  fromOwned = pure

instance ObjCReturn CULong where
  type RawReturn CULong = CULong
  objcRetType = retCULong
  msgSendVariant = MsgSendNormal
  fromRetained = pure
  fromOwned = pure

instance ObjCReturn CInt where
  type RawReturn CInt = CInt
  objcRetType = retCInt
  msgSendVariant = MsgSendNormal
  fromRetained = pure
  fromOwned = pure

instance ObjCReturn CUInt where
  type RawReturn CUInt = CUInt
  objcRetType = retCUInt
  msgSendVariant = MsgSendNormal
  fromRetained = pure
  fromOwned = pure

-- | @short@ — promoted through @CInt@ since libffi lacks a direct @short@ type.
instance ObjCReturn CShort where
  type RawReturn CShort = CInt
  objcRetType = retCInt
  msgSendVariant = MsgSendNormal
  fromRetained = pure . fromIntegral
  fromOwned = pure . fromIntegral

-- | @unsigned short@ — promoted through @CUInt@.
instance ObjCReturn CUShort where
  type RawReturn CUShort = CUInt
  objcRetType = retCUInt
  msgSendVariant = MsgSendNormal
  fromRetained = pure . fromIntegral
  fromOwned = pure . fromIntegral

-- | @signed char@ — promoted through @Int8@.
instance ObjCReturn CSChar where
  type RawReturn CSChar = Int8
  objcRetType = retInt8
  msgSendVariant = MsgSendNormal
  fromRetained = pure . fromIntegral
  fromOwned = pure . fromIntegral

instance ObjCReturn CChar where
  type RawReturn CChar = CChar
  objcRetType = retCChar
  msgSendVariant = MsgSendNormal
  fromRetained = pure
  fromOwned = pure

instance ObjCReturn CUChar where
  type RawReturn CUChar = CUChar
  objcRetType = retCUChar
  msgSendVariant = MsgSendNormal
  fromRetained = pure
  fromOwned = pure

-- Boolean types ---------------------------------------------------------

-- | Returns 'True' when the raw @Int8@ is non-zero.
instance ObjCReturn Bool where
  type RawReturn Bool = Int8
  objcRetType = retInt8
  msgSendVariant = MsgSendNormal
  fromRetained x = pure (x /= 0)
  fromOwned    x = pure (x /= 0)

-- | Decoded from the raw @Int8@ that @libffi@ returns for @BOOL@.
instance ObjCReturn ObjCBool where
  type RawReturn ObjCBool = Int8
  objcRetType = retInt8
  msgSendVariant = MsgSendNormal
  fromRetained x = pure (ObjCBool (CBool (fromIntegral x)))
  fromOwned    x = pure (ObjCBool (CBool (fromIntegral x)))

-- | Raw @Int8@ passthrough (useful as a building block).
instance ObjCReturn Int8 where
  type RawReturn Int8 = Int8
  objcRetType = retInt8
  msgSendVariant = MsgSendNormal
  fromRetained = pure
  fromOwned = pure

-- ObjC runtime types ----------------------------------------------------

-- | Class pointer return (e.g., @[obj class]@).
instance ObjCReturn Class where
  type RawReturn Class = Ptr ()
  objcRetType = retPtr retVoid
  msgSendVariant = MsgSendNormal
  fromRetained = pure . Class . castPtr
  fromOwned = pure . Class . castPtr

-- | Raw (unmanaged) object return.
instance ObjCReturn RawId where
  type RawReturn RawId = Ptr ()
  objcRetType = retPtr retVoid
  msgSendVariant = MsgSendNormal
  fromRetained = pure . RawId . castPtr
  fromOwned = pure . RawId . castPtr

-- | Selector return (e.g., for introspection methods).
instance ObjCReturn (Selector args ret) where
  type RawReturn (Selector args ret) = Ptr ()
  objcRetType = retPtr retVoid
  msgSendVariant = MsgSendNormal
  fromRetained = pure . Selector . castPtr
  fromOwned = pure . Selector . castPtr

-- Qualifier wrappers ----------------------------------------------------

-- | Const-qualified return — delegates to underlying type, wraps result.
instance ObjCReturn a => ObjCReturn (Const a) where
  type RawReturn (Const a) = RawReturn a
  objcRetType = objcRetType @a
  msgSendVariant = msgSendVariant @a
  fromRetained raw = Const <$> fromRetained @a raw
  fromOwned raw = Const <$> fromOwned @a raw

-- | Volatile-qualified return — delegates to underlying type, wraps result.
instance ObjCReturn a => ObjCReturn (Volatile a) where
  type RawReturn (Volatile a) = RawReturn a
  objcRetType = objcRetType @a
  msgSendVariant = msgSendVariant @a
  fromRetained raw = Volatile <$> fromRetained @a raw
  fromOwned raw = Volatile <$> fromOwned @a raw

-- ---------------------------------------------------------------------------
-- GatherObjCArgs — CPS-based argument gathering
-- ---------------------------------------------------------------------------

-- | Inductively gathers 'ObjCArgument' values into a @libffi@ 'Arg' list,
-- nesting CPS brackets so that every pointer-backed value remains alive
-- when 'Foreign.LibFFI.callFFI' executes.
--
-- Uses 'sequenceArgs' from @varargs@ to lift the monadic CPS result back
-- into a variadic function at each inductive step.
class GatherObjCArgs (args :: [Type]) where
  gatherObjCArgs
    :: ([Arg] -> [Arg])     -- ^ Difference-list accumulator
    -> ([Arg] -> IO r)      -- ^ Continuation receiving the final arg list
    -> args :->: IO r

instance GatherObjCArgs '[] where
  gatherObjCArgs accum k = k (accum [])

instance (ObjCArgument arg, GatherObjCArgs rest, VarArgs rest)
      => GatherObjCArgs (arg ': rest) where
  gatherObjCArgs :: forall r. ([Arg] -> [Arg]) -> ([Arg] -> IO r) -> (arg ': rest) :->: IO r
  gatherObjCArgs accum k = \a ->
    sequenceArgs @rest @r @IO $
      withObjCArg a $ \ffiArg ->
        pure (gatherObjCArgs @rest (accum . (ffiArg :)) k)

-- ---------------------------------------------------------------------------
-- Type-safe message sending
-- ---------------------------------------------------------------------------

-- | Send a message to an Objective-C object with type-safe argument
-- unfolding.
--
-- The 'Selector' phantom types drive the function signature: each type
-- in the selector's argument list becomes a curried function parameter.
--
-- Uses __retained__ semantics for object return values (suitable for
-- property getters, factory methods returning autoreleased objects, and
-- most other methods).  For @init@\/@copy@\/@new@\/@mutableCopy@ methods,
-- use 'sendOwnedMessage' instead.
--
-- @
-- let sel = mkSelector \"length\" :: Selector '[] CULong
-- len <- sendMessage nsString sel
--
-- let sel2 = mkSelector \"addProgressRangeFromStart:toEnd:\"
--         :: Selector '[CDouble, CDouble] ()
-- sendMessage activity sel2 0.0 0.5
-- @
sendMessage
  :: forall args ret recv.
     (IsObjCObject recv, GatherObjCArgs args, ObjCReturn ret)
  => recv -> Selector args ret -> args :->: IO ret
sendMessage recv (Selector sel) =
  gatherObjCArgs @args id $ \argList ->
    withObjCPtr recv $ \ptr -> do
      raw <- callFFI
        (msgSendFunPtr (msgSendVariant @ret))
        (objcRetType @ret)
        (argPtr ptr : argPtr sel : argList)
      fromRetained @ret raw

-- | Like 'sendMessage', but uses __owned__ semantics for the return value.
--
-- Use for methods whose names start with @init@, @copy@, @new@, or
-- @mutableCopy@ — these return a +1 reference that the caller owns.
--
-- @
-- let sel = mkSelector \"initWithString:\"
--        :: Selector '[Id NSString] (Id NSMutableString)
-- result <- sendOwnedMessage allocated sel str
-- @
sendOwnedMessage
  :: forall args ret recv.
     (IsObjCObject recv, GatherObjCArgs args, ObjCReturn ret)
  => recv -> Selector args ret -> args :->: IO ret
sendOwnedMessage recv (Selector sel) =
  gatherObjCArgs @args id $ \argList ->
    withObjCPtr recv $ \ptr -> do
      raw <- callFFI
        (msgSendFunPtr (msgSendVariant @ret))
        (objcRetType @ret)
        (argPtr ptr : argPtr sel : argList)
      fromOwned @ret raw

-- | Send a message to an Objective-C class (class method).
--
-- Uses retained semantics for object return values.
--
-- @
-- let sel = mkSelector \"string\" :: Selector '[] (Id NSString)
-- s <- sendClassMessage nsStringClass sel
-- @
sendClassMessage
  :: forall args ret.
     (GatherObjCArgs args, ObjCReturn ret)
  => Class -> Selector args ret -> args :->: IO ret
sendClassMessage (Class cls) (Selector sel) =
  gatherObjCArgs @args id $ \argList -> do
    raw <- callFFI
      (msgSendFunPtr (msgSendVariant @ret))
      (objcRetType @ret)
      (argPtr cls : argPtr sel : argList)
    fromRetained @ret raw

-- | Like 'sendClassMessage', but uses owned semantics.
--
-- Use for class methods like @+new@, @+alloc@.
sendOwnedClassMessage
  :: forall args ret.
     (GatherObjCArgs args, ObjCReturn ret)
  => Class -> Selector args ret -> args :->: IO ret
sendOwnedClassMessage (Class cls) (Selector sel) =
  gatherObjCArgs @args id $ \argList -> do
    raw <- callFFI
      (msgSendFunPtr (msgSendVariant @ret))
      (objcRetType @ret)
      (argPtr cls : argPtr sel : argList)
    fromOwned @ret raw
