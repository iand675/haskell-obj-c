{-# LANGUAGE ForeignFunctionInterface #-}

-- | Infrastructure for dynamically defined Objective-C classes with
-- Haskell-backed method implementations.
--
-- Each instance of a dynamic class carries a /vtable/ — a C array of
-- function pointers stored in an ivar called @_hs_vtable@.  The class's
-- IMP stubs (registered at class-creation time) read the vtable from
-- @self@ and dispatch through the appropriate slot.
--
-- This module provides the low-level helpers.  Most users should use
-- the Template Haskell API in "ObjC.Runtime.TH" instead.
module ObjC.Runtime.DynClass
  ( -- * Vtable ivar management
    addVtableIvar
  , readVtable
  , writeVtable

    -- * Dealloc handler
  , addDeallocHandler

    -- * Method registration helpers
  , addObjCMethod

    -- * IMP casting
  , castIMP
  ) where

import Data.Word (Word8)
import Foreign.Ptr (Ptr, FunPtr, castFunPtrToPtr, castPtrToFunPtr, freeHaskellFunPtr, nullPtr)
import Foreign.C.Types (CSize(..), CPtrdiff(..))
import Foreign.C.String (withCString)
import Foreign.Marshal.Alloc (free)
import Foreign.Storable (peekByteOff, pokeByteOff, peekElemOff)
import Foreign.LibFFI (retVoid)

import ObjC.Runtime.Types
import ObjC.Runtime.Class
  (class_addIvar, class_addMethod, class_getInstanceVariable, class_getSuperclass)
import ObjC.Runtime.Ivar (ivar_getOffset)
import ObjC.Runtime.MsgSend (sendSuperMsg)
import ObjC.Runtime.Selector (mkSelector)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | The name of the ivar that stores the vtable pointer.
vtableIvarName :: String
vtableIvarName = "_hs_vtable"

-- | Pointer size in bytes.
ptrSize :: CSize
ptrSize = fromIntegral (8 :: Int)  -- 64-bit

-- | log2(pointer alignment) for 64-bit.
ptrAlignLog2 :: Word8
ptrAlignLog2 = 3

-- ---------------------------------------------------------------------------
-- Vtable ivar
-- ---------------------------------------------------------------------------

-- | Add the @_hs_vtable@ ivar to a class being constructed (between
-- 'objc_allocateClassPair' and 'objc_registerClassPair').
--
-- The ivar is a single pointer (@^v@ type encoding) that will hold
-- the address of a heap-allocated array of 'FunPtr's.
addVtableIvar :: Class -> IO ()
addVtableIvar cls = do
  ok <- withCString vtableIvarName $ \nameC ->
    withCString "^v" $ \typeC ->
      class_addIvar cls nameC ptrSize ptrAlignLog2 typeC
  if ok
    then pure ()
    else error "addVtableIvar: failed to add _hs_vtable ivar"

-- | Read the vtable pointer from an instance's @_hs_vtable@ ivar.
--
-- The returned pointer points to a C array of @void *@ slots, each
-- holding a 'FunPtr' (cast to @Ptr ()@).
--
-- Takes a raw @Ptr ObjCObject@ (as received by an IMP) rather than
-- 'RawId' to avoid unnecessary wrapping in dispatch stubs.
readVtable :: Ptr ObjCObject -> IO (Ptr (Ptr ()))
readVtable self = do
  offset <- vtableOffset self
  peekByteOff self (fromIntegral offset)

-- | Write the vtable pointer into an instance's @_hs_vtable@ ivar.
writeVtable :: RawId -> Ptr (Ptr ()) -> IO ()
writeVtable (RawId self) vtable = do
  offset <- vtableOffset self
  pokeByteOff self (fromIntegral offset) vtable

-- | Get the byte offset of @_hs_vtable@ from the start of an object.
vtableOffset :: Ptr ObjCObject -> IO CPtrdiff
vtableOffset self = do
  -- Look up the class from self, then get the ivar descriptor.
  -- object_getClass is a simple pointer read.
  cls <- c_object_getClass (RawId self)
  ivar <- withCString vtableIvarName $ \nameC ->
    class_getInstanceVariable cls nameC
  ivar_getOffset ivar

foreign import ccall unsafe "object_getClass"
  c_object_getClass :: RawId -> IO Class

-- ---------------------------------------------------------------------------
-- Method registration
-- ---------------------------------------------------------------------------

-- | Add a method to a class under construction.
--
-- @addObjCMethod cls \"increment:\" \"v\@:\@\" funPtr@
--
-- This is a convenience wrapper around 'class_addMethod' that takes
-- Haskell 'String's for the selector and type encoding.
addObjCMethod :: Class -> String -> String -> FunPtr a -> IO ()
addObjCMethod cls selName typeEnc fp = do
  let sel = mkSelector selName
  ok <- withCString typeEnc $ \typeC ->
    class_addMethod cls sel (castIMP fp) typeC
  if ok
    then pure ()
    else error ("addObjCMethod: failed to add method " ++ selName)

-- ---------------------------------------------------------------------------
-- Dealloc handler
-- ---------------------------------------------------------------------------

foreign import ccall "wrapper"
  wrapDeallocIMP :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
                 -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

-- | Register a @dealloc@ method on a class under construction that frees
-- the vtable and its 'FunPtr' slots, then calls @[super dealloc]@.
--
-- Must be called between 'objc_allocateClassPair' and 'objc_registerClassPair',
-- after 'addVtableIvar'.
--
-- @nSlots@ is the number of instance method slots in the vtable (i.e., the
-- number of 'FunPtr's that were poked into each instance's vtable array).
addDeallocHandler :: Class -> Int -> IO ()
addDeallocHandler cls nSlots = do
  superCls <- class_getSuperclass cls
  let deallocSel = mkSelector "dealloc"
  stub <- wrapDeallocIMP $ \self _cmd -> do
    vt <- readVtable self
    if vt /= nullPtr
      then do
        -- Free each wrapped FunPtr in the vtable.
        mapM_ (\i -> do
          p <- peekElemOff vt i
          if p /= nullPtr
            then freeHaskellFunPtr (castPtrToFunPtr p :: FunPtr ())
            else pure ()
          ) [0 .. nSlots - 1]
        -- Free the vtable array itself.
        free vt
      else pure ()
    -- Call [super dealloc].
    let super_ = ObjCSuper (RawId self) superCls
    sendSuperMsg super_ deallocSel retVoid []
  addObjCMethod cls "dealloc" "v@:" stub

-- ---------------------------------------------------------------------------
-- IMP casting
-- ---------------------------------------------------------------------------

-- | Cast any 'FunPtr' to 'IMP'.  The Objective-C runtime stores all
-- method implementations as 'IMP' regardless of the actual signature;
-- the correct calling convention is ensured by the type encoding.
castIMP :: FunPtr a -> IMP
castIMP fp = IMP (castPtrToFunPtr (castFunPtrToPtr fp))
