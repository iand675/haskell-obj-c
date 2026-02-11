{-# LANGUAGE ForeignFunctionInterface #-}

-- | StablePtr-in-ivar infrastructure for Haskell-backed ObjC classes.
--
-- This module provides an alternative to the per-instance vtable approach
-- in "ObjC.Runtime.DynClass".  Instead of allocating a C array of
-- @FunPtr@s per instance, a single @StablePtr@ to an arbitrary Haskell
-- value is stored in the instance's @_hs_vtable@ ivar.
--
-- IMP stubs (registered at class-creation time) read the @StablePtr@
-- from @self@, deref it to recover the Haskell value, and dispatch
-- directly — no @foreign import ccall \"dynamic\"@ boundary crossing.
--
-- Used by "ObjC.Runtime.ActionTarget", "ObjC.Runtime.DelegateProxy",
-- and (future) codegen'd delegate/subclass modules.
module ObjC.Runtime.StableIvar
  ( -- * Ivar management
    addHsDataIvar
  , writeHsData
  , readHsData

    -- * Dealloc handler
  , addStablePtrDeallocHandler

    -- * Re-exports
  , addObjCMethod
  , castIMP
  ) where

import Data.Word (Word8)
import Foreign.Ptr (Ptr, FunPtr, nullPtr, castPtr)
import Foreign.C.Types (CSize(..), CPtrdiff(..))
import Foreign.C.String (withCString)
import Foreign.StablePtr
  (StablePtr, castStablePtrToPtr, castPtrToStablePtr, freeStablePtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import Foreign.LibFFI (retVoid)

import ObjC.Runtime.Types
import ObjC.Runtime.Class
  (class_addIvar, class_getInstanceVariable, class_getSuperclass)
import ObjC.Runtime.Ivar (ivar_getOffset)
import ObjC.Runtime.MsgSend (sendSuperMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.DynClass (addObjCMethod, castIMP)

-- ---------------------------------------------------------------------------
-- Constants (must agree with DynClass)
-- ---------------------------------------------------------------------------

-- | The ivar name.  Shared with "ObjC.Runtime.DynClass" — both modules
-- use the same pointer-sized slot.  A class should use one approach or
-- the other, not both.
ivarName :: String
ivarName = "_hs_vtable"

-- | Pointer size in bytes (64-bit).
ptrSize :: CSize
ptrSize = 8

-- | log2(pointer alignment) for 64-bit.
ptrAlignLog2 :: Word8
ptrAlignLog2 = 3

-- ---------------------------------------------------------------------------
-- Ivar management
-- ---------------------------------------------------------------------------

-- | Add the @_hs_vtable@ ivar to a class being constructed.
--
-- Must be called between 'objc_allocateClassPair' and
-- 'objc_registerClassPair'.  The ivar is a single pointer that will
-- hold a @StablePtr@ (cast to @Ptr ()@).
addHsDataIvar :: Class -> IO ()
addHsDataIvar cls = do
  ok <- withCString ivarName $ \nameC ->
    withCString "^v" $ \typeC ->
      class_addIvar cls nameC ptrSize ptrAlignLog2 typeC
  if ok
    then pure ()
    else error "addHsDataIvar: failed to add _hs_vtable ivar"

-- | Store a @StablePtr@ in an instance's ivar.
writeHsData :: RawId -> StablePtr a -> IO ()
writeHsData (RawId self) sp = do
  offset <- ivarOffset self
  pokeByteOff self (fromIntegral offset) (castStablePtrToPtr sp)

-- | Read the @StablePtr@ from an instance's ivar.
--
-- Takes a raw @Ptr ObjCObject@ (as received by an IMP stub) to avoid
-- wrapping in the hot dispatch path.
readHsData :: Ptr ObjCObject -> IO (StablePtr a)
readHsData self = do
  offset <- ivarOffset self
  p <- peekByteOff self (fromIntegral offset)
  pure (castPtrToStablePtr p)

-- ---------------------------------------------------------------------------
-- Ivar offset lookup (same logic as DynClass.vtableOffset)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "object_getClass"
  c_object_getClass :: RawId -> IO Class

ivarOffset :: Ptr ObjCObject -> IO CPtrdiff
ivarOffset self = do
  cls <- c_object_getClass (RawId self)
  ivar <- withCString ivarName $ \nameC ->
    class_getInstanceVariable cls nameC
  ivar_getOffset ivar

-- ---------------------------------------------------------------------------
-- Dealloc handler
-- ---------------------------------------------------------------------------

foreign import ccall "wrapper"
  wrapDeallocIMP :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
                 -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

-- | Register a @dealloc@ method that frees the instance's @StablePtr@
-- and calls @[super dealloc]@.
--
-- Must be called between 'objc_allocateClassPair' and
-- 'objc_registerClassPair', after 'addHsDataIvar'.
addStablePtrDeallocHandler :: Class -> IO ()
addStablePtrDeallocHandler cls = do
  superCls <- class_getSuperclass cls
  let deallocSel = mkSelector "dealloc"
  stub <- wrapDeallocIMP $ \self _cmd -> do
    sp <- readHsData self :: IO (StablePtr ())
    let p = castStablePtrToPtr sp
    if castPtr p /= nullPtr
      then freeStablePtr sp
      else pure ()
    let super_ = ObjCSuper (RawId self) superCls
    sendSuperMsg super_ deallocSel retVoid []
  addObjCMethod cls "dealloc" "v@:" stub
