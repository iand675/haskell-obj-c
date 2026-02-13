{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Core types for the Objective-C runtime.
--
-- These are opaque pointer wrappers corresponding to the types defined in
-- @\<objc\/runtime.h\>@ and @\<objc\/objc.h\>@.
--
-- The two main object types are:
--
-- * 'RawId' — a raw, unmanaged @id@ pointer for low-level FFI plumbing.
-- * @'Id' a@ — a managed, phantom-typed 'ForeignPtr' whose finalizer
--   calls @-[obj release]@ when the Haskell GC collects it.
module ObjC.Runtime.Types
  ( -- * Object types
    ObjCObject
  , RawId(..)
  , nilRawId

    -- * Managed, phantom-typed object pointers
  , Id(..)
  , IsObjCObject(..)
  , unsafeCastId

    -- * Object creation helpers
  , ownedObject
  , retainedObject
  , unmanagedObject
  , nilId
  , retainAutorelease

    -- * Helpers for generated IsObjCObject instances
  , idToRawId
  , withIdObjCPtr

    -- * Class types
  , ObjCClass
  , Class(..)
  , nilClass

    -- * Selector types
  , ObjCSel
  , Selector(..)
  , Sel
  , nilSelector

    -- * Method types
  , ObjCMethod
  , Method(..)

    -- * Instance variable types
  , ObjCIvar
  , Ivar(..)

    -- * Category types
  , ObjCCategory
  , Category(..)

    -- * Property types
  , ObjCPropertyOpaque
  , ObjCProperty(..)

    -- * Protocol types
  , ObjCProtocol
  , Protocol(..)

    -- * Implementation pointer
  , IMP(..)

    -- * Boolean type
  , ObjCBool(..)
  , objcTrue
  , objcFalse
  , fromObjCBool
  , toObjCBool

    -- * Method description
  , ObjCMethodDescription(..)

    -- * Property attribute
  , ObjCPropertyAttribute(..)

    -- * Association policy
  , ObjCAssociationPolicy(..)
  , pattern OBJC_ASSOCIATION_ASSIGN
  , pattern OBJC_ASSOCIATION_RETAIN_NONATOMIC
  , pattern OBJC_ASSOCIATION_COPY_NONATOMIC
  , pattern OBJC_ASSOCIATION_RETAIN
  , pattern OBJC_ASSOCIATION_COPY

    -- * Super struct
  , ObjCSuper(..)

    -- * C type qualifier annotations
  , Const(..)
  , Volatile(..)

    -- * Bitmask helpers
  , hasFlag
  ) where

import Data.Bits (Bits, (.&.))
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.String (IsString(..))
import Foreign.Ptr (Ptr, FunPtr, nullPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, newForeignPtr_, withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Storable (Storable(..))
import Foreign.C.Types (CBool(..), CUIntPtr(..))
import Foreign.C.String (CString, withCString)
import System.IO.Unsafe (unsafePerformIO)

-- ---------------------------------------------------------------------------
-- Phantom types for opaque pointers
-- ---------------------------------------------------------------------------

-- | Phantom type for @id@ (Objective-C object pointer).
data ObjCObject

-- | Phantom type for @Class@.
data ObjCClass

-- | Phantom type for @SEL@.
data ObjCSel

-- | Phantom type for @Method@.
data ObjCMethod

-- | Phantom type for @Ivar@.
data ObjCIvar

-- | Phantom type for @Category@.
data ObjCCategory

-- | Phantom type for @objc_property_t@.
data ObjCPropertyOpaque

-- | Phantom type for @Protocol@.
data ObjCProtocol

-- ---------------------------------------------------------------------------
-- RawId — raw, unmanaged object pointer for low-level FFI
-- ---------------------------------------------------------------------------

-- | A raw Objective-C object pointer (@id@).
--
-- This is the unmanaged, untyped pointer used internally by 'sendRawMsg',
-- 'ObjCSuper', and the C FFI. Most user code should use @'Id' a@ instead.
newtype RawId = RawId { unRawId :: Ptr ObjCObject }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Storable)

-- | The nil object (raw).
nilRawId :: RawId
nilRawId = RawId nullPtr

-- ---------------------------------------------------------------------------
-- Id a — managed, phantom-typed ForeignPtr
-- ---------------------------------------------------------------------------

-- | A managed, phantom-typed Objective-C object pointer.
--
-- Internally wraps a 'ForeignPtr' whose finalizer sends @-[obj release]@
-- when the Haskell GC collects it.
--
-- The phantom type @a@ distinguishes different ObjC class types at the
-- Haskell level (e.g., @Id NSString@ vs @Id NSArray@).
--
-- The phantom type has a __nominal__ role, which means GHC's @coerce@
-- (from "Data.Coerce") __cannot__ convert between @Id a@ and @Id b@.
type role Id nominal
newtype Id a = Id { idForeignPtr :: ForeignPtr ObjCObject }

instance Eq (Id a) where
  Id a == Id b = unsafeForeignPtrToPtr a == unsafeForeignPtrToPtr b

instance Ord (Id a) where
  compare (Id a) (Id b) = compare (unsafeForeignPtrToPtr a) (unsafeForeignPtrToPtr b)

instance Show (Id a) where
  showsPrec d (Id fp) = showParen (d > 10) $
    showString "Id " . showsPrec 11 (unsafeForeignPtrToPtr fp)

-- | Zero-cost conversion between 'Id' types.
--
-- Used by generated type class instances (e.g., 'IsNSString') to implement
-- upcast methods. This is safe when used within generated code that correctly
-- encodes the ObjC class hierarchy; calling it directly bypasses type safety.
unsafeCastId :: Id a -> Id b
unsafeCastId (Id fp) = Id fp

-- ---------------------------------------------------------------------------
-- IsObjCObject typeclass
-- ---------------------------------------------------------------------------

-- | Base type class for all Objective-C objects.
--
-- Provides raw pointer extraction, safe scoped access, and static class
-- lookup. Generated bindings provide instances for each ObjC class.
--
-- The superclass chain for generated type classes is:
--
-- @
-- IsObjCObject            -- toRawId, withObjCPtr, staticClass
--   |-- IsNSObject         -- toNSObject
--        |-- IsNSString    -- toNSString
--             |-- ...
-- @
class IsObjCObject a where
  -- | Extract the raw pointer. The caller must ensure @a@ stays alive
  -- for the duration of use (prefer 'withObjCPtr' instead).
  toRawId :: a -> RawId

  -- | Safely extract the underlying pointer for the duration of a callback.
  -- Keeps the managed object alive until @f@ returns.
  withObjCPtr :: a -> (Ptr ObjCObject -> IO b) -> IO b

  -- | Get the Objective-C 'Class' corresponding to this Haskell type.
  staticClass :: Proxy a -> IO Class

-- | 'RawId' is trivially an 'IsObjCObject'. No finalizer, no management.
instance IsObjCObject RawId where
  toRawId = id
  withObjCPtr (RawId ptr) f = f ptr
  staticClass _ = error "staticClass: RawId has no static class"

-- | Lift 'IsObjCObject' through 'Const'.
-- This allows @withObjCPtr@ on @Const (Id Foo)@ parameters generated from
-- @const@-qualified ObjC method arguments.
instance IsObjCObject a => IsObjCObject (Const a) where
  toRawId (Const x) = toRawId x
  withObjCPtr (Const x) f = withObjCPtr x f
  staticClass _ = error "staticClass: Const wrapper has no single static class"

-- | Extract the raw pointer from an 'Id'.  Unsafe: the pointer is only valid
-- as long as the 'ForeignPtr' is alive.  Intended for generated
-- 'IsObjCObject' instances.
idToRawId :: Id a -> RawId
idToRawId (Id fp) = RawId (unsafeForeignPtrToPtr fp)

-- | Bracket-style access to the raw pointer inside an 'Id'.
-- Keeps the 'ForeignPtr' alive for the duration of @f@.  Intended for
-- generated 'IsObjCObject' instances.
withIdObjCPtr :: Id a -> (Ptr ObjCObject -> IO b) -> IO b
withIdObjCPtr (Id fp) f = withForeignPtr fp f

-- ---------------------------------------------------------------------------
-- FFI for retain / release
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "hs_objc_retain"
  c_objc_retain :: Ptr ObjCObject -> IO (Ptr ObjCObject)

foreign import ccall unsafe "hs_objc_autorelease"
  c_objc_autorelease :: Ptr ObjCObject -> IO (Ptr ObjCObject)

-- | Function pointer to @hs_objc_release@, suitable for use as a
-- 'ForeignPtr' finalizer.
foreign import ccall "&hs_objc_release"
  p_objc_release :: FunPtr (Ptr ObjCObject -> IO ())

-- ---------------------------------------------------------------------------
-- Object creation helpers
-- ---------------------------------------------------------------------------

-- | Wrap a @+1@ (owned) return value. Attaches a release finalizer.
--
-- Use for results of @init@, @copy@, @new@, @mutableCopy@ methods.
ownedObject :: Ptr ObjCObject -> IO (Id a)
ownedObject ptr = Id <$> newForeignPtr p_objc_release ptr

-- | Wrap a @+0@ (borrowed/autoreleased) return value. Retains it first,
-- then attaches a release finalizer.
--
-- Use for results of most Objective-C methods (getters, factory methods
-- that don't start with init/copy/new/mutableCopy).
retainedObject :: Ptr ObjCObject -> IO (Id a)
retainedObject ptr = do
  _ <- c_objc_retain ptr
  Id <$> newForeignPtr p_objc_release ptr

-- | Wrap a pointer with no finalizer. Used for freshly @alloc@'d objects
-- that have not yet been @init@'d — the @init@ method will produce a
-- properly managed 'Id'.
unmanagedObject :: Ptr ObjCObject -> IO (Id a)
unmanagedObject ptr = Id <$> newForeignPtr_ ptr

-- | The nil object as a managed 'Id'.
--
-- Wraps 'nullPtr' in a 'ForeignPtr' with no finalizer.  Safe because
-- Objective-C's @release(nil)@ is a no-op, and 'withForeignPtr' on it
-- yields 'nullPtr'.
--
-- Useful for passing @nil@ to typed API functions:
--
-- @
-- TV.setHeaderView tableView nilId   -- removes the header view
-- @
nilId :: Id a
nilId = Id (unsafePerformIO (newForeignPtr_ nullPtr))
{-# NOINLINE nilId #-}

-- | Retain and autorelease an 'Id', returning the raw pointer.
--
-- Follows the standard Objective-C @+0@ return convention: the object's
-- retain count is bumped and then placed in the current autorelease pool.
-- This is the correct way to hand a Haskell-managed 'Id' back to
-- Objective-C as a @+0@ return value (e.g., from delegate callbacks).
--
-- @
-- returnId :: Id a -> IO RawId
-- returnId obj = retainAutorelease obj
-- @
retainAutorelease :: Id a -> IO RawId
retainAutorelease (Id fp) = withForeignPtr fp $ \ptr -> do
  _ <- c_objc_retain ptr
  _ <- c_objc_autorelease ptr
  pure (RawId ptr)

-- ---------------------------------------------------------------------------
-- Class, Selector, Method, Ivar, etc. — unchanged
-- ---------------------------------------------------------------------------

-- | An Objective-C class pointer (@Class@).
newtype Class = Class { unClass :: Ptr ObjCClass }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Storable)

-- | Nil class.
nilClass :: Class
nilClass = Class nullPtr

-- | An Objective-C selector (@SEL@), optionally carrying phantom type
-- information about the method's parameter types and return type.
--
-- The phantom parameters exist for documentation and future type-safe
-- dispatch; they do not affect runtime representation.
type role Selector phantom phantom
newtype Selector (args :: [Type]) (ret :: Type) = Selector { unSelector :: Ptr ObjCSel }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Storable)

-- | Untyped selector alias.  Use this in contexts where the selector's
-- method signature is unknown or irrelevant (FFI, runtime introspection).
type Sel = Selector '[] ()

-- | Nil selector.
nilSelector :: Sel
nilSelector = Selector nullPtr

foreign import ccall unsafe "sel_registerName"
  c_sel_registerName :: CString -> IO (Ptr ObjCSel)

-- | @Selector@ can be created from a string literal via @OverloadedStrings@.
--
-- @sel_registerName@ is idempotent and thread-safe, so 'unsafePerformIO'
-- is safe here.
instance IsString (Selector args ret) where
  fromString s = Selector (unsafePerformIO (withCString s c_sel_registerName))
  {-# NOINLINE fromString #-}

-- | An Objective-C method (@Method@).
newtype Method = Method { unMethod :: Ptr ObjCMethod }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Storable)

-- | An Objective-C instance variable (@Ivar@).
newtype Ivar = Ivar { unIvar :: Ptr ObjCIvar }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Storable)

-- | An Objective-C category (@Category@).
newtype Category = Category { unCategory :: Ptr ObjCCategory }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Storable)

-- | An Objective-C declared property (@objc_property_t@).
newtype ObjCProperty = ObjCProperty { unObjCProperty :: Ptr ObjCPropertyOpaque }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Storable)

-- | An Objective-C protocol (@Protocol *@).
newtype Protocol = Protocol { unProtocol :: Ptr ObjCProtocol }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Storable)

-- | An Objective-C method implementation pointer (@IMP@).
newtype IMP = IMP { unIMP :: FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)) }
  -- Note: IMP's type still uses the raw ObjCSel phantom type, not Selector.
  deriving stock (Eq, Ord, Show)
  deriving newtype (Storable)

-- ---------------------------------------------------------------------------
-- BOOL
-- ---------------------------------------------------------------------------

-- | Objective-C @BOOL@ type. On modern Apple platforms this is @_Bool@ (CBool).
newtype ObjCBool = ObjCBool { unObjCBool :: CBool }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Storable)

objcTrue :: ObjCBool
objcTrue = ObjCBool 1

objcFalse :: ObjCBool
objcFalse = ObjCBool 0

fromObjCBool :: ObjCBool -> Bool
fromObjCBool (ObjCBool b) = b /= 0

toObjCBool :: Bool -> ObjCBool
toObjCBool True  = objcTrue
toObjCBool False = objcFalse

-- ---------------------------------------------------------------------------
-- Structs
-- ---------------------------------------------------------------------------

-- | @struct objc_method_description@ — describes a method's selector and type encoding.
data ObjCMethodDescription = ObjCMethodDescription
  { methodDescName  :: !Sel
  , methodDescTypes :: !CString
  } deriving (Eq, Show)

instance Storable ObjCMethodDescription where
  sizeOf _ = sizeOf (undefined :: Ptr ()) * 2
  alignment _ = alignment (undefined :: Ptr ())
  peek p = do
    sel   <- peekByteOff p 0
    types <- peekByteOff p (sizeOf (undefined :: Ptr ()))
    pure (ObjCMethodDescription (Selector sel) types)
  poke p (ObjCMethodDescription (Selector sel) types) = do
    pokeByteOff p 0 sel
    pokeByteOff p (sizeOf (undefined :: Ptr ())) types

-- | @objc_property_attribute_t@ — a property attribute name/value pair.
data ObjCPropertyAttribute = ObjCPropertyAttribute
  { propertyAttrName  :: !CString
  , propertyAttrValue :: !CString
  } deriving (Eq, Show)

instance Storable ObjCPropertyAttribute where
  sizeOf _ = sizeOf (undefined :: Ptr ()) * 2
  alignment _ = alignment (undefined :: Ptr ())
  peek p = do
    name  <- peekByteOff p 0
    value <- peekByteOff p (sizeOf (undefined :: Ptr ()))
    pure (ObjCPropertyAttribute name value)
  poke p (ObjCPropertyAttribute name value) = do
    pokeByteOff p 0 name
    pokeByteOff p (sizeOf (undefined :: Ptr ())) value

-- ---------------------------------------------------------------------------
-- Association policy
-- ---------------------------------------------------------------------------

-- | Policy for @objc_setAssociatedObject@.
newtype ObjCAssociationPolicy = ObjCAssociationPolicy CUIntPtr
  deriving stock (Eq, Ord, Show)
  deriving newtype (Storable)

pattern OBJC_ASSOCIATION_ASSIGN :: ObjCAssociationPolicy
pattern OBJC_ASSOCIATION_ASSIGN = ObjCAssociationPolicy 0

pattern OBJC_ASSOCIATION_RETAIN_NONATOMIC :: ObjCAssociationPolicy
pattern OBJC_ASSOCIATION_RETAIN_NONATOMIC = ObjCAssociationPolicy 1

pattern OBJC_ASSOCIATION_COPY_NONATOMIC :: ObjCAssociationPolicy
pattern OBJC_ASSOCIATION_COPY_NONATOMIC = ObjCAssociationPolicy 3

pattern OBJC_ASSOCIATION_RETAIN :: ObjCAssociationPolicy
pattern OBJC_ASSOCIATION_RETAIN = ObjCAssociationPolicy 0x301

pattern OBJC_ASSOCIATION_COPY :: ObjCAssociationPolicy
pattern OBJC_ASSOCIATION_COPY = ObjCAssociationPolicy 0x303

-- ---------------------------------------------------------------------------
-- objc_super
-- ---------------------------------------------------------------------------

-- | @struct objc_super@ — used for @objc_msgSendSuper@.
data ObjCSuper = ObjCSuper
  { superReceiver    :: !RawId
  , superSuperclass  :: !Class
  } deriving (Eq, Show)

instance Storable ObjCSuper where
  sizeOf _ = sizeOf (undefined :: Ptr ()) * 2
  alignment _ = alignment (undefined :: Ptr ())
  peek p = do
    receiver <- peekByteOff p 0
    cls      <- peekByteOff p (sizeOf (undefined :: Ptr ()))
    pure (ObjCSuper (RawId receiver) (Class cls))
  poke p (ObjCSuper (RawId receiver) (Class cls)) = do
    pokeByteOff p 0 receiver
    pokeByteOff p (sizeOf (undefined :: Ptr ())) cls

-- ---------------------------------------------------------------------------
-- C type qualifier annotations
-- ---------------------------------------------------------------------------

-- | Type-level annotation for C @const@-qualified values.
--
-- For example, @Const (Ptr CChar)@ represents @const char *@.
-- Zero-cost newtype: use 'Const' to wrap and 'unConst' to unwrap.
newtype Const a = Const { unConst :: a }
  deriving (Eq, Ord, Show)

-- | Type-level annotation for C @volatile@-qualified values.
--
-- For example, @Volatile (Ptr CInt)@ represents @volatile int *@.
-- Zero-cost newtype: use 'Volatile' to wrap and 'unVolatile' to unwrap.
newtype Volatile a = Volatile { unVolatile :: a }
  deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- Bitmask helpers
-- ---------------------------------------------------------------------------

-- | Check whether a bitmask value contains a specific flag.
--
-- Intended for use with @ViewPatterns@:
--
-- @
-- {-\# LANGUAGE ViewPatterns \#-}
-- case mask of
--   (hasFlag NSWindowStyleMaskTitled -> True) -> ...
-- @
hasFlag :: (Bits a, Eq a) => a -> a -> Bool
hasFlag flag value = value .&. flag == flag
