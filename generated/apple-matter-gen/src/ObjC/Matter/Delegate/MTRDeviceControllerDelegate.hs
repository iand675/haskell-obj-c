{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTRDeviceControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newMTRDeviceControllerDelegate defaultMTRDeviceControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Matter.Delegate.MTRDeviceControllerDelegate
  ( MTRDeviceControllerDelegateOverrides(..)
  , defaultMTRDeviceControllerDelegateOverrides
  , newMTRDeviceControllerDelegate
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

-- | Overrides record for @\@protocol MTRDeviceControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTRDeviceControllerDelegateOverrides = MTRDeviceControllerDelegateOverrides
  { _controller_commissioningSessionEstablishmentDone :: !(Maybe (RawId -> RawId -> IO ()))
  , _controller_commissioningComplete :: !(Maybe (RawId -> RawId -> IO ()))
  , _controller_commissioningComplete_nodeID :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _controller_commissioningComplete_nodeID_metrics :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _controller_readCommissioneeInfo :: !(Maybe (RawId -> RawId -> IO ()))
  , _controller_readCommissioningInfo :: !(Maybe (RawId -> RawId -> IO ()))
  , _controller_suspendedChangedTo :: !(Maybe (RawId -> Bool -> IO ()))
  , _devicesChangedForController :: !(Maybe (RawId -> IO ()))
  , _controller_commissioneeHasReceivedNetworkCredentials :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTRDeviceControllerDelegateOverrides :: MTRDeviceControllerDelegateOverrides
defaultMTRDeviceControllerDelegateOverrides = MTRDeviceControllerDelegateOverrides
  { _controller_commissioningSessionEstablishmentDone = Nothing
  , _controller_commissioningComplete = Nothing
  , _controller_commissioningComplete_nodeID = Nothing
  , _controller_commissioningComplete_nodeID_metrics = Nothing
  , _controller_readCommissioneeInfo = Nothing
  , _controller_readCommissioningInfo = Nothing
  , _controller_suspendedChangedTo = Nothing
  , _devicesChangedForController = Nothing
  , _controller_commissioneeHasReceivedNetworkCredentials = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtrDeviceControllerDelegateDelegateClass #-}
mtrDeviceControllerDelegateDelegateClass :: Class
mtrDeviceControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTRDeviceControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_controller_commissioningSessionEstablishmentDone = unSelector (mkSelector "controller:commissioningSessionEstablishmentDone:")
      sel_controller_commissioningComplete = unSelector (mkSelector "controller:commissioningComplete:")
      sel_controller_commissioningComplete_nodeID = unSelector (mkSelector "controller:commissioningComplete:nodeID:")
      sel_controller_commissioningComplete_nodeID_metrics = unSelector (mkSelector "controller:commissioningComplete:nodeID:metrics:")
      sel_controller_readCommissioneeInfo = unSelector (mkSelector "controller:readCommissioneeInfo:")
      sel_controller_readCommissioningInfo = unSelector (mkSelector "controller:readCommissioningInfo:")
      sel_controller_suspendedChangedTo = unSelector (mkSelector "controller:suspendedChangedTo:")
      sel_devicesChangedForController = unSelector (mkSelector "devicesChangedForController:")
      sel_controller_commissioneeHasReceivedNetworkCredentials = unSelector (mkSelector "controller:commissioneeHasReceivedNetworkCredentials:")
  -- controller:commissioningSessionEstablishmentDone:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRDeviceControllerDelegateOverrides
    case _controller_commissioningSessionEstablishmentDone rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "controller:commissioningSessionEstablishmentDone:" "v@:@@" stub_0

  -- controller:commissioningComplete:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRDeviceControllerDelegateOverrides
    case _controller_commissioningComplete rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "controller:commissioningComplete:" "v@:@@" stub_1

  -- controller:commissioningComplete:nodeID:
  stub_2 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRDeviceControllerDelegateOverrides
    case _controller_commissioningComplete_nodeID rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "controller:commissioningComplete:nodeID:" "v@:@@@" stub_2

  -- controller:commissioningComplete:nodeID:metrics:
  stub_3 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRDeviceControllerDelegateOverrides
    case _controller_commissioningComplete_nodeID_metrics rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "controller:commissioningComplete:nodeID:metrics:" "v@:@@@@" stub_3

  -- controller:readCommissioneeInfo:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRDeviceControllerDelegateOverrides
    case _controller_readCommissioneeInfo rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "controller:readCommissioneeInfo:" "v@:@@" stub_4

  -- controller:readCommissioningInfo:
  stub_5 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRDeviceControllerDelegateOverrides
    case _controller_readCommissioningInfo rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "controller:readCommissioningInfo:" "v@:@@" stub_5

  -- controller:suspendedChangedTo:
  stub_6 <- wrap_at_B_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRDeviceControllerDelegateOverrides
    case _controller_suspendedChangedTo rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (arg1 /= 0)
  addObjCMethod cls "controller:suspendedChangedTo:" "v@:@B" stub_6

  -- devicesChangedForController:
  stub_7 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRDeviceControllerDelegateOverrides
    case _devicesChangedForController rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "devicesChangedForController:" "v@:@" stub_7

  -- controller:commissioneeHasReceivedNetworkCredentials:
  stub_8 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRDeviceControllerDelegateOverrides
    case _controller_commissioneeHasReceivedNetworkCredentials rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "controller:commissioneeHasReceivedNetworkCredentials:" "v@:@@" stub_8

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRDeviceControllerDelegateOverrides
    if queriedSel == sel_controller_commissioningSessionEstablishmentDone then pure (maybe 0 (const 1) (_controller_commissioningSessionEstablishmentDone rec_))
    else if queriedSel == sel_controller_commissioningComplete then pure (maybe 0 (const 1) (_controller_commissioningComplete rec_))
    else if queriedSel == sel_controller_commissioningComplete_nodeID then pure (maybe 0 (const 1) (_controller_commissioningComplete_nodeID rec_))
    else if queriedSel == sel_controller_commissioningComplete_nodeID_metrics then pure (maybe 0 (const 1) (_controller_commissioningComplete_nodeID_metrics rec_))
    else if queriedSel == sel_controller_readCommissioneeInfo then pure (maybe 0 (const 1) (_controller_readCommissioneeInfo rec_))
    else if queriedSel == sel_controller_readCommissioningInfo then pure (maybe 0 (const 1) (_controller_readCommissioningInfo rec_))
    else if queriedSel == sel_controller_suspendedChangedTo then pure (maybe 0 (const 1) (_controller_suspendedChangedTo rec_))
    else if queriedSel == sel_devicesChangedForController then pure (maybe 0 (const 1) (_devicesChangedForController rec_))
    else if queriedSel == sel_controller_commissioneeHasReceivedNetworkCredentials then pure (maybe 0 (const 1) (_controller_commissioneeHasReceivedNetworkCredentials rec_))
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
newMTRDeviceControllerDelegate :: MTRDeviceControllerDelegateOverrides -> IO RawId
newMTRDeviceControllerDelegate overrides = do
  inst <- class_createInstance mtrDeviceControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
