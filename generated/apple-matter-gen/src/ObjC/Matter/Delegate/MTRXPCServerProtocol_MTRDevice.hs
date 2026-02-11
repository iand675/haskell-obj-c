{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTRXPCServerProtocol_MTRDevice@.
--
-- Usage:
--
-- @
-- delegate <- newMTRXPCServerProtocol_MTRDevice defaultMTRXPCServerProtocol_MTRDeviceOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Matter.Delegate.MTRXPCServerProtocol_MTRDevice
  ( MTRXPCServerProtocol_MTRDeviceOverrides(..)
  , defaultMTRXPCServerProtocol_MTRDeviceOverrides
  , newMTRXPCServerProtocol_MTRDevice
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

-- | Overrides record for @\@protocol MTRXPCServerProtocol_MTRDevice@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTRXPCServerProtocol_MTRDeviceOverrides = MTRXPCServerProtocol_MTRDeviceOverrides
  { _deviceController_nodeID_readAttributeWithEndpointID_clusterID_attributeID_params_withReply :: !(Maybe (RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> IO ()))
  , _deviceController_nodeID_writeAttributeWithEndpointID_clusterID_attributeID_value_expectedValueInterval_timedWriteTimeout :: !(Maybe (RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> IO ()))
  , _deviceController_nodeID_readAttributePaths_withReply :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTRXPCServerProtocol_MTRDeviceOverrides :: MTRXPCServerProtocol_MTRDeviceOverrides
defaultMTRXPCServerProtocol_MTRDeviceOverrides = MTRXPCServerProtocol_MTRDeviceOverrides
  { _deviceController_nodeID_readAttributeWithEndpointID_clusterID_attributeID_params_withReply = Nothing
  , _deviceController_nodeID_writeAttributeWithEndpointID_clusterID_attributeID_value_expectedValueInterval_timedWriteTimeout = Nothing
  , _deviceController_nodeID_readAttributePaths_withReply = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtrxpcServerProtocol_MTRDeviceDelegateClass #-}
mtrxpcServerProtocol_MTRDeviceDelegateClass :: Class
mtrxpcServerProtocol_MTRDeviceDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTRXPCServerProtocol_MTRDevice" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_deviceController_nodeID_readAttributeWithEndpointID_clusterID_attributeID_params_withReply = unSelector (mkSelector "deviceController:nodeID:readAttributeWithEndpointID:clusterID:attributeID:params:withReply:")
      sel_deviceController_nodeID_writeAttributeWithEndpointID_clusterID_attributeID_value_expectedValueInterval_timedWriteTimeout = unSelector (mkSelector "deviceController:nodeID:writeAttributeWithEndpointID:clusterID:attributeID:value:expectedValueInterval:timedWriteTimeout:")
      sel_deviceController_nodeID_readAttributePaths_withReply = unSelector (mkSelector "deviceController:nodeID:readAttributePaths:withReply:")
  -- deviceController:nodeID:readAttributeWithEndpointID:clusterID:attributeID:params:withReply:
  stub_0 <- wrap_at_at_at_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 arg4 arg5 arg6 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRXPCServerProtocol_MTRDeviceOverrides
    case _deviceController_nodeID_readAttributeWithEndpointID_clusterID_attributeID_params_withReply rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3) (RawId arg4) (RawId arg5) (RawId arg6)
  addObjCMethod cls "deviceController:nodeID:readAttributeWithEndpointID:clusterID:attributeID:params:withReply:" "v@:@@@@@@@" stub_0

  -- deviceController:nodeID:writeAttributeWithEndpointID:clusterID:attributeID:value:expectedValueInterval:timedWriteTimeout:
  stub_1 <- wrap_at_at_at_at_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRXPCServerProtocol_MTRDeviceOverrides
    case _deviceController_nodeID_writeAttributeWithEndpointID_clusterID_attributeID_value_expectedValueInterval_timedWriteTimeout rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3) (RawId arg4) (RawId arg5) (RawId arg6) (RawId arg7)
  addObjCMethod cls "deviceController:nodeID:writeAttributeWithEndpointID:clusterID:attributeID:value:expectedValueInterval:timedWriteTimeout:" "v@:@@@@@@@@" stub_1

  -- deviceController:nodeID:readAttributePaths:withReply:
  stub_2 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRXPCServerProtocol_MTRDeviceOverrides
    case _deviceController_nodeID_readAttributePaths_withReply rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "deviceController:nodeID:readAttributePaths:withReply:" "v@:@@@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRXPCServerProtocol_MTRDeviceOverrides
    if queriedSel == sel_deviceController_nodeID_readAttributeWithEndpointID_clusterID_attributeID_params_withReply then pure (maybe 0 (const 1) (_deviceController_nodeID_readAttributeWithEndpointID_clusterID_attributeID_params_withReply rec_))
    else if queriedSel == sel_deviceController_nodeID_writeAttributeWithEndpointID_clusterID_attributeID_value_expectedValueInterval_timedWriteTimeout then pure (maybe 0 (const 1) (_deviceController_nodeID_writeAttributeWithEndpointID_clusterID_attributeID_value_expectedValueInterval_timedWriteTimeout rec_))
    else if queriedSel == sel_deviceController_nodeID_readAttributePaths_withReply then pure (maybe 0 (const 1) (_deviceController_nodeID_readAttributePaths_withReply rec_))
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
newMTRXPCServerProtocol_MTRDevice :: MTRXPCServerProtocol_MTRDeviceOverrides -> IO RawId
newMTRXPCServerProtocol_MTRDevice overrides = do
  inst <- class_createInstance mtrxpcServerProtocol_MTRDeviceDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
