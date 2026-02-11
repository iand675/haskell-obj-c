{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol FSVolumeOperations@.
--
-- Usage:
--
-- @
-- delegate <- newFSVolumeOperations defaultFSVolumeOperationsOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.FSKit.Delegate.FSVolumeOperations
  ( FSVolumeOperationsOverrides(..)
  , defaultFSVolumeOperationsOverrides
  , newFSVolumeOperations
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

-- | Overrides record for @\@protocol FSVolumeOperations@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data FSVolumeOperationsOverrides = FSVolumeOperationsOverrides
  { _supportedVolumeCapabilities :: !(Maybe (IO RawId))
  , _volumeStatistics :: !(Maybe (IO RawId))
  , _enableOpenUnlinkEmulation :: !(Maybe (IO Bool))
  , _setEnableOpenUnlinkEmulation :: !(Maybe (Bool -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultFSVolumeOperationsOverrides :: FSVolumeOperationsOverrides
defaultFSVolumeOperationsOverrides = FSVolumeOperationsOverrides
  { _supportedVolumeCapabilities = Nothing
  , _volumeStatistics = Nothing
  , _enableOpenUnlinkEmulation = Nothing
  , _setEnableOpenUnlinkEmulation = Nothing
  }

foreign import ccall "wrapper"
  wrap_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE fsVolumeOperationsDelegateClass #-}
fsVolumeOperationsDelegateClass :: Class
fsVolumeOperationsDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsFSVolumeOperations" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_supportedVolumeCapabilities = unSelector (mkSelector "supportedVolumeCapabilities")
      sel_volumeStatistics = unSelector (mkSelector "volumeStatistics")
      sel_enableOpenUnlinkEmulation = unSelector (mkSelector "enableOpenUnlinkEmulation")
      sel_setEnableOpenUnlinkEmulation = unSelector (mkSelector "setEnableOpenUnlinkEmulation:")
  -- supportedVolumeCapabilities
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumeOperationsOverrides
    case _supportedVolumeCapabilities rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "supportedVolumeCapabilities" "@@:" stub_0

  -- volumeStatistics
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumeOperationsOverrides
    case _volumeStatistics rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "volumeStatistics" "@@:" stub_1

  -- enableOpenUnlinkEmulation
  stub_2 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumeOperationsOverrides
    case _enableOpenUnlinkEmulation rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "enableOpenUnlinkEmulation" "B@:" stub_2

  -- setEnableOpenUnlinkEmulation:
  stub_3 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumeOperationsOverrides
    case _setEnableOpenUnlinkEmulation rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setEnableOpenUnlinkEmulation:" "v@:B" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumeOperationsOverrides
    if queriedSel == sel_supportedVolumeCapabilities then pure (maybe 0 (const 1) (_supportedVolumeCapabilities rec_))
    else if queriedSel == sel_volumeStatistics then pure (maybe 0 (const 1) (_volumeStatistics rec_))
    else if queriedSel == sel_enableOpenUnlinkEmulation then pure (maybe 0 (const 1) (_enableOpenUnlinkEmulation rec_))
    else if queriedSel == sel_setEnableOpenUnlinkEmulation then pure (maybe 0 (const 1) (_setEnableOpenUnlinkEmulation rec_))
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
newFSVolumeOperations :: FSVolumeOperationsOverrides -> IO RawId
newFSVolumeOperations overrides = do
  inst <- class_createInstance fsVolumeOperationsDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
