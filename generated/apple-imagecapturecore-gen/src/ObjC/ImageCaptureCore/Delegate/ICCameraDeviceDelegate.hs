{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ICCameraDeviceDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newICCameraDeviceDelegate defaultICCameraDeviceDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ImageCaptureCore.Delegate.ICCameraDeviceDelegate
  ( ICCameraDeviceDelegateOverrides(..)
  , defaultICCameraDeviceDelegateOverrides
  , newICCameraDeviceDelegate
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

-- | Overrides record for @\@protocol ICCameraDeviceDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ICCameraDeviceDelegateOverrides = ICCameraDeviceDelegateOverrides
  { _cameraDevice_didAddItems :: !(Maybe (RawId -> RawId -> IO ()))
  , _cameraDevice_didRemoveItems :: !(Maybe (RawId -> RawId -> IO ()))
  , _cameraDevice_didReceiveMetadata_forItem_error :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _cameraDevice_didRenameItems :: !(Maybe (RawId -> RawId -> IO ()))
  , _cameraDeviceDidChangeCapability :: !(Maybe (RawId -> IO ()))
  , _cameraDevice_didReceivePTPEvent :: !(Maybe (RawId -> RawId -> IO ()))
  , _deviceDidBecomeReadyWithCompleteContentCatalog :: !(Maybe (RawId -> IO ()))
  , _cameraDeviceDidRemoveAccessRestriction :: !(Maybe (RawId -> IO ()))
  , _cameraDeviceDidEnableAccessRestriction :: !(Maybe (RawId -> IO ()))
  , _cameraDevice_shouldGetThumbnailOfItem :: !(Maybe (RawId -> RawId -> IO Bool))
  , _cameraDevice_shouldGetMetadataOfItem :: !(Maybe (RawId -> RawId -> IO Bool))
  , _cameraDevice_didCompleteDeleteFilesWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _cameraDevice_didAddItem :: !(Maybe (RawId -> RawId -> IO ()))
  , _cameraDevice_didRemoveItem :: !(Maybe (RawId -> RawId -> IO ()))
  , _cameraDevice_didReceiveThumbnailForItem :: !(Maybe (RawId -> RawId -> IO ()))
  , _cameraDevice_didReceiveMetadataForItem :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultICCameraDeviceDelegateOverrides :: ICCameraDeviceDelegateOverrides
defaultICCameraDeviceDelegateOverrides = ICCameraDeviceDelegateOverrides
  { _cameraDevice_didAddItems = Nothing
  , _cameraDevice_didRemoveItems = Nothing
  , _cameraDevice_didReceiveMetadata_forItem_error = Nothing
  , _cameraDevice_didRenameItems = Nothing
  , _cameraDeviceDidChangeCapability = Nothing
  , _cameraDevice_didReceivePTPEvent = Nothing
  , _deviceDidBecomeReadyWithCompleteContentCatalog = Nothing
  , _cameraDeviceDidRemoveAccessRestriction = Nothing
  , _cameraDeviceDidEnableAccessRestriction = Nothing
  , _cameraDevice_shouldGetThumbnailOfItem = Nothing
  , _cameraDevice_shouldGetMetadataOfItem = Nothing
  , _cameraDevice_didCompleteDeleteFilesWithError = Nothing
  , _cameraDevice_didAddItem = Nothing
  , _cameraDevice_didRemoveItem = Nothing
  , _cameraDevice_didReceiveThumbnailForItem = Nothing
  , _cameraDevice_didReceiveMetadataForItem = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE icCameraDeviceDelegateDelegateClass #-}
icCameraDeviceDelegateDelegateClass :: Class
icCameraDeviceDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsICCameraDeviceDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_cameraDevice_didAddItems = unSelector (mkSelector "cameraDevice:didAddItems:")
      sel_cameraDevice_didRemoveItems = unSelector (mkSelector "cameraDevice:didRemoveItems:")
      sel_cameraDevice_didReceiveMetadata_forItem_error = unSelector (mkSelector "cameraDevice:didReceiveMetadata:forItem:error:")
      sel_cameraDevice_didRenameItems = unSelector (mkSelector "cameraDevice:didRenameItems:")
      sel_cameraDeviceDidChangeCapability = unSelector (mkSelector "cameraDeviceDidChangeCapability:")
      sel_cameraDevice_didReceivePTPEvent = unSelector (mkSelector "cameraDevice:didReceivePTPEvent:")
      sel_deviceDidBecomeReadyWithCompleteContentCatalog = unSelector (mkSelector "deviceDidBecomeReadyWithCompleteContentCatalog:")
      sel_cameraDeviceDidRemoveAccessRestriction = unSelector (mkSelector "cameraDeviceDidRemoveAccessRestriction:")
      sel_cameraDeviceDidEnableAccessRestriction = unSelector (mkSelector "cameraDeviceDidEnableAccessRestriction:")
      sel_cameraDevice_shouldGetThumbnailOfItem = unSelector (mkSelector "cameraDevice:shouldGetThumbnailOfItem:")
      sel_cameraDevice_shouldGetMetadataOfItem = unSelector (mkSelector "cameraDevice:shouldGetMetadataOfItem:")
      sel_cameraDevice_didCompleteDeleteFilesWithError = unSelector (mkSelector "cameraDevice:didCompleteDeleteFilesWithError:")
      sel_cameraDevice_didAddItem = unSelector (mkSelector "cameraDevice:didAddItem:")
      sel_cameraDevice_didRemoveItem = unSelector (mkSelector "cameraDevice:didRemoveItem:")
      sel_cameraDevice_didReceiveThumbnailForItem = unSelector (mkSelector "cameraDevice:didReceiveThumbnailForItem:")
      sel_cameraDevice_didReceiveMetadataForItem = unSelector (mkSelector "cameraDevice:didReceiveMetadataForItem:")
  -- cameraDevice:didAddItems:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICCameraDeviceDelegateOverrides
    case _cameraDevice_didAddItems rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "cameraDevice:didAddItems:" "v@:@@" stub_0

  -- cameraDevice:didRemoveItems:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICCameraDeviceDelegateOverrides
    case _cameraDevice_didRemoveItems rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "cameraDevice:didRemoveItems:" "v@:@@" stub_1

  -- cameraDevice:didReceiveMetadata:forItem:error:
  stub_2 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICCameraDeviceDelegateOverrides
    case _cameraDevice_didReceiveMetadata_forItem_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "cameraDevice:didReceiveMetadata:forItem:error:" "v@:@@@@" stub_2

  -- cameraDevice:didRenameItems:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICCameraDeviceDelegateOverrides
    case _cameraDevice_didRenameItems rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "cameraDevice:didRenameItems:" "v@:@@" stub_3

  -- cameraDeviceDidChangeCapability:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICCameraDeviceDelegateOverrides
    case _cameraDeviceDidChangeCapability rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "cameraDeviceDidChangeCapability:" "v@:@" stub_4

  -- cameraDevice:didReceivePTPEvent:
  stub_5 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICCameraDeviceDelegateOverrides
    case _cameraDevice_didReceivePTPEvent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "cameraDevice:didReceivePTPEvent:" "v@:@@" stub_5

  -- deviceDidBecomeReadyWithCompleteContentCatalog:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICCameraDeviceDelegateOverrides
    case _deviceDidBecomeReadyWithCompleteContentCatalog rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deviceDidBecomeReadyWithCompleteContentCatalog:" "v@:@" stub_6

  -- cameraDeviceDidRemoveAccessRestriction:
  stub_7 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICCameraDeviceDelegateOverrides
    case _cameraDeviceDidRemoveAccessRestriction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "cameraDeviceDidRemoveAccessRestriction:" "v@:@" stub_7

  -- cameraDeviceDidEnableAccessRestriction:
  stub_8 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICCameraDeviceDelegateOverrides
    case _cameraDeviceDidEnableAccessRestriction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "cameraDeviceDidEnableAccessRestriction:" "v@:@" stub_8

  -- cameraDevice:shouldGetThumbnailOfItem:
  stub_9 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICCameraDeviceDelegateOverrides
    case _cameraDevice_shouldGetThumbnailOfItem rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "cameraDevice:shouldGetThumbnailOfItem:" "B@:@@" stub_9

  -- cameraDevice:shouldGetMetadataOfItem:
  stub_10 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICCameraDeviceDelegateOverrides
    case _cameraDevice_shouldGetMetadataOfItem rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "cameraDevice:shouldGetMetadataOfItem:" "B@:@@" stub_10

  -- cameraDevice:didCompleteDeleteFilesWithError:
  stub_11 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICCameraDeviceDelegateOverrides
    case _cameraDevice_didCompleteDeleteFilesWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "cameraDevice:didCompleteDeleteFilesWithError:" "v@:@@" stub_11

  -- cameraDevice:didAddItem:
  stub_12 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICCameraDeviceDelegateOverrides
    case _cameraDevice_didAddItem rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "cameraDevice:didAddItem:" "v@:@@" stub_12

  -- cameraDevice:didRemoveItem:
  stub_13 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICCameraDeviceDelegateOverrides
    case _cameraDevice_didRemoveItem rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "cameraDevice:didRemoveItem:" "v@:@@" stub_13

  -- cameraDevice:didReceiveThumbnailForItem:
  stub_14 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICCameraDeviceDelegateOverrides
    case _cameraDevice_didReceiveThumbnailForItem rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "cameraDevice:didReceiveThumbnailForItem:" "v@:@@" stub_14

  -- cameraDevice:didReceiveMetadataForItem:
  stub_15 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICCameraDeviceDelegateOverrides
    case _cameraDevice_didReceiveMetadataForItem rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "cameraDevice:didReceiveMetadataForItem:" "v@:@@" stub_15

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ICCameraDeviceDelegateOverrides
    if queriedSel == sel_cameraDevice_didAddItems then pure (maybe 0 (const 1) (_cameraDevice_didAddItems rec_))
    else if queriedSel == sel_cameraDevice_didRemoveItems then pure (maybe 0 (const 1) (_cameraDevice_didRemoveItems rec_))
    else if queriedSel == sel_cameraDevice_didReceiveMetadata_forItem_error then pure (maybe 0 (const 1) (_cameraDevice_didReceiveMetadata_forItem_error rec_))
    else if queriedSel == sel_cameraDevice_didRenameItems then pure (maybe 0 (const 1) (_cameraDevice_didRenameItems rec_))
    else if queriedSel == sel_cameraDeviceDidChangeCapability then pure (maybe 0 (const 1) (_cameraDeviceDidChangeCapability rec_))
    else if queriedSel == sel_cameraDevice_didReceivePTPEvent then pure (maybe 0 (const 1) (_cameraDevice_didReceivePTPEvent rec_))
    else if queriedSel == sel_deviceDidBecomeReadyWithCompleteContentCatalog then pure (maybe 0 (const 1) (_deviceDidBecomeReadyWithCompleteContentCatalog rec_))
    else if queriedSel == sel_cameraDeviceDidRemoveAccessRestriction then pure (maybe 0 (const 1) (_cameraDeviceDidRemoveAccessRestriction rec_))
    else if queriedSel == sel_cameraDeviceDidEnableAccessRestriction then pure (maybe 0 (const 1) (_cameraDeviceDidEnableAccessRestriction rec_))
    else if queriedSel == sel_cameraDevice_shouldGetThumbnailOfItem then pure (maybe 0 (const 1) (_cameraDevice_shouldGetThumbnailOfItem rec_))
    else if queriedSel == sel_cameraDevice_shouldGetMetadataOfItem then pure (maybe 0 (const 1) (_cameraDevice_shouldGetMetadataOfItem rec_))
    else if queriedSel == sel_cameraDevice_didCompleteDeleteFilesWithError then pure (maybe 0 (const 1) (_cameraDevice_didCompleteDeleteFilesWithError rec_))
    else if queriedSel == sel_cameraDevice_didAddItem then pure (maybe 0 (const 1) (_cameraDevice_didAddItem rec_))
    else if queriedSel == sel_cameraDevice_didRemoveItem then pure (maybe 0 (const 1) (_cameraDevice_didRemoveItem rec_))
    else if queriedSel == sel_cameraDevice_didReceiveThumbnailForItem then pure (maybe 0 (const 1) (_cameraDevice_didReceiveThumbnailForItem rec_))
    else if queriedSel == sel_cameraDevice_didReceiveMetadataForItem then pure (maybe 0 (const 1) (_cameraDevice_didReceiveMetadataForItem rec_))
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
newICCameraDeviceDelegate :: ICCameraDeviceDelegateOverrides -> IO RawId
newICCameraDeviceDelegate overrides = do
  inst <- class_createInstance icCameraDeviceDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
