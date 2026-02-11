{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTRDeviceControllerClientProtocol@.
--
-- Usage:
--
-- @
-- delegate <- newMTRDeviceControllerClientProtocol defaultMTRDeviceControllerClientProtocolOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Matter.Delegate.MTRDeviceControllerClientProtocol
  ( MTRDeviceControllerClientProtocolOverrides(..)
  , defaultMTRDeviceControllerClientProtocolOverrides
  , newMTRDeviceControllerClientProtocol
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

-- | Overrides record for @\@protocol MTRDeviceControllerClientProtocol@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTRDeviceControllerClientProtocolOverrides = MTRDeviceControllerClientProtocolOverrides
  { _handleReportWithController_nodeId_values_error :: !(Maybe (RawId -> Int -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTRDeviceControllerClientProtocolOverrides :: MTRDeviceControllerClientProtocolOverrides
defaultMTRDeviceControllerClientProtocolOverrides = MTRDeviceControllerClientProtocolOverrides
  { _handleReportWithController_nodeId_values_error = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_Q_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtrDeviceControllerClientProtocolDelegateClass #-}
mtrDeviceControllerClientProtocolDelegateClass :: Class
mtrDeviceControllerClientProtocolDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTRDeviceControllerClientProtocol" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_handleReportWithController_nodeId_values_error = unSelector (mkSelector "handleReportWithController:nodeId:values:error:")
  -- handleReportWithController:nodeId:values:error:
  stub_0 <- wrap_at_Q_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRDeviceControllerClientProtocolOverrides
    case _handleReportWithController_nodeId_values_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "handleReportWithController:nodeId:values:error:" "v@:@Q@@" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRDeviceControllerClientProtocolOverrides
    if queriedSel == sel_handleReportWithController_nodeId_values_error then pure (maybe 0 (const 1) (_handleReportWithController_nodeId_values_error rec_))
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
newMTRDeviceControllerClientProtocol :: MTRDeviceControllerClientProtocolOverrides -> IO RawId
newMTRDeviceControllerClientProtocol overrides = do
  inst <- class_createInstance mtrDeviceControllerClientProtocolDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
