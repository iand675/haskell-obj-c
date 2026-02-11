{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTRXPCServerProtocol_MTRDeviceController@.
--
-- Usage:
--
-- @
-- delegate <- newMTRXPCServerProtocol_MTRDeviceController defaultMTRXPCServerProtocol_MTRDeviceControllerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Matter.Delegate.MTRXPCServerProtocol_MTRDeviceController
  ( MTRXPCServerProtocol_MTRDeviceControllerOverrides(..)
  , defaultMTRXPCServerProtocol_MTRDeviceControllerOverrides
  , newMTRXPCServerProtocol_MTRDeviceController
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

-- | Overrides record for @\@protocol MTRXPCServerProtocol_MTRDeviceController@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTRXPCServerProtocol_MTRDeviceControllerOverrides = MTRXPCServerProtocol_MTRDeviceControllerOverrides
  { _deviceController_deleteNodeID :: !(Maybe (RawId -> RawId -> IO ()))
  , _deviceController_registerNodeID :: !(Maybe (RawId -> RawId -> IO ()))
  , _deviceController_unregisterNodeID :: !(Maybe (RawId -> RawId -> IO ()))
  , _deviceController_updateControllerConfiguration :: !(Maybe (RawId -> RawId -> IO ()))
  , _deviceController_getNodesWithStoredDataWithReply :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTRXPCServerProtocol_MTRDeviceControllerOverrides :: MTRXPCServerProtocol_MTRDeviceControllerOverrides
defaultMTRXPCServerProtocol_MTRDeviceControllerOverrides = MTRXPCServerProtocol_MTRDeviceControllerOverrides
  { _deviceController_deleteNodeID = Nothing
  , _deviceController_registerNodeID = Nothing
  , _deviceController_unregisterNodeID = Nothing
  , _deviceController_updateControllerConfiguration = Nothing
  , _deviceController_getNodesWithStoredDataWithReply = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtrxpcServerProtocol_MTRDeviceControllerDelegateClass #-}
mtrxpcServerProtocol_MTRDeviceControllerDelegateClass :: Class
mtrxpcServerProtocol_MTRDeviceControllerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTRXPCServerProtocol_MTRDeviceController" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_deviceController_deleteNodeID = unSelector (mkSelector "deviceController:deleteNodeID:")
      sel_deviceController_registerNodeID = unSelector (mkSelector "deviceController:registerNodeID:")
      sel_deviceController_unregisterNodeID = unSelector (mkSelector "deviceController:unregisterNodeID:")
      sel_deviceController_updateControllerConfiguration = unSelector (mkSelector "deviceController:updateControllerConfiguration:")
      sel_deviceController_getNodesWithStoredDataWithReply = unSelector (mkSelector "deviceController:getNodesWithStoredDataWithReply:")
  -- deviceController:deleteNodeID:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRXPCServerProtocol_MTRDeviceControllerOverrides
    case _deviceController_deleteNodeID rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "deviceController:deleteNodeID:" "v@:@@" stub_0

  -- deviceController:registerNodeID:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRXPCServerProtocol_MTRDeviceControllerOverrides
    case _deviceController_registerNodeID rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "deviceController:registerNodeID:" "v@:@@" stub_1

  -- deviceController:unregisterNodeID:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRXPCServerProtocol_MTRDeviceControllerOverrides
    case _deviceController_unregisterNodeID rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "deviceController:unregisterNodeID:" "v@:@@" stub_2

  -- deviceController:updateControllerConfiguration:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRXPCServerProtocol_MTRDeviceControllerOverrides
    case _deviceController_updateControllerConfiguration rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "deviceController:updateControllerConfiguration:" "v@:@@" stub_3

  -- deviceController:getNodesWithStoredDataWithReply:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRXPCServerProtocol_MTRDeviceControllerOverrides
    case _deviceController_getNodesWithStoredDataWithReply rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "deviceController:getNodesWithStoredDataWithReply:" "v@:@@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRXPCServerProtocol_MTRDeviceControllerOverrides
    if queriedSel == sel_deviceController_deleteNodeID then pure (maybe 0 (const 1) (_deviceController_deleteNodeID rec_))
    else if queriedSel == sel_deviceController_registerNodeID then pure (maybe 0 (const 1) (_deviceController_registerNodeID rec_))
    else if queriedSel == sel_deviceController_unregisterNodeID then pure (maybe 0 (const 1) (_deviceController_unregisterNodeID rec_))
    else if queriedSel == sel_deviceController_updateControllerConfiguration then pure (maybe 0 (const 1) (_deviceController_updateControllerConfiguration rec_))
    else if queriedSel == sel_deviceController_getNodesWithStoredDataWithReply then pure (maybe 0 (const 1) (_deviceController_getNodesWithStoredDataWithReply rec_))
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
newMTRXPCServerProtocol_MTRDeviceController :: MTRXPCServerProtocol_MTRDeviceControllerOverrides -> IO RawId
newMTRXPCServerProtocol_MTRDeviceController overrides = do
  inst <- class_createInstance mtrxpcServerProtocol_MTRDeviceControllerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
