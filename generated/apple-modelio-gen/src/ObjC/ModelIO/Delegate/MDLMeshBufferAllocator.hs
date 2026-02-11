{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MDLMeshBufferAllocator@.
--
-- Usage:
--
-- @
-- delegate <- newMDLMeshBufferAllocator defaultMDLMeshBufferAllocatorOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ModelIO.Delegate.MDLMeshBufferAllocator
  ( MDLMeshBufferAllocatorOverrides(..)
  , defaultMDLMeshBufferAllocatorOverrides
  , newMDLMeshBufferAllocator
  ) where

import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.C.Types
import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (withCString)
import Foreign.LibFFI (retCULong, argPtr)

import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass, class_createInstance)
import ObjC.Runtime.ClassBuilder (objc_allocateClassPair, objc_registerClassPair)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.MsgSend (sendSuperMsg)
import ObjC.Runtime.StableIvar

-- | Overrides record for @\@protocol MDLMeshBufferAllocator@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MDLMeshBufferAllocatorOverrides = MDLMeshBufferAllocatorOverrides
  { _newZone :: !(Maybe (Int -> IO RawId))
  , _newZoneForBuffersWithSize_andType :: !(Maybe (RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMDLMeshBufferAllocatorOverrides :: MDLMeshBufferAllocatorOverrides
defaultMDLMeshBufferAllocatorOverrides = MDLMeshBufferAllocatorOverrides
  { _newZone = Nothing
  , _newZoneForBuffersWithSize_andType = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_Q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mdlMeshBufferAllocatorDelegateClass #-}
mdlMeshBufferAllocatorDelegateClass :: Class
mdlMeshBufferAllocatorDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMDLMeshBufferAllocator" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_newZone = unSelector (mkSelector "newZone:")
      sel_newZoneForBuffersWithSize_andType = unSelector (mkSelector "newZoneForBuffersWithSize:andType:")
  -- newZone:
  stub_0 <- wrap_Q_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLMeshBufferAllocatorOverrides
    case _newZone rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (fromIntegral arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "newZone:" "@@:Q" stub_0

  -- newZoneForBuffersWithSize:andType:
  stub_1 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLMeshBufferAllocatorOverrides
    case _newZoneForBuffersWithSize_andType rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "newZoneForBuffersWithSize:andType:" "@@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLMeshBufferAllocatorOverrides
    if queriedSel == sel_newZone then pure (maybe 0 (const 1) (_newZone rec_))
    else if queriedSel == sel_newZoneForBuffersWithSize_andType then pure (maybe 0 (const 1) (_newZoneForBuffersWithSize_andType rec_))
    else do
      let super_ = ObjCSuper (RawId self) superCls
      sendSuperMsg super_ (mkSelector "respondsToSelector:") retCULong
        [argPtr (castPtr queriedSel :: Ptr ())]
  addObjCMethod cls "respondsToSelector:" "B@::" rtsStub

  addStablePtrDeallocHandler cls
  objc_registerClassPair cls
  pure cls

-- | Create a new delegate implementing this protocol.
--
-- The returned 'RawId' can be used as a delegate or data source.
newMDLMeshBufferAllocator :: MDLMeshBufferAllocatorOverrides -> IO RawId
newMDLMeshBufferAllocator overrides = do
  inst <- class_createInstance mdlMeshBufferAllocatorDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
