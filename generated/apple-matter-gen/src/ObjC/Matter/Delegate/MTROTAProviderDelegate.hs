{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTROTAProviderDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newMTROTAProviderDelegate defaultMTROTAProviderDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Matter.Delegate.MTROTAProviderDelegate
  ( MTROTAProviderDelegateOverrides(..)
  , defaultMTROTAProviderDelegateOverrides
  , newMTROTAProviderDelegate
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

-- | Overrides record for @\@protocol MTROTAProviderDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTROTAProviderDelegateOverrides = MTROTAProviderDelegateOverrides
  { _handleBDXTransferSessionEndForNodeID_controller_metrics_error :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _handleBDXTransferSessionEndForNodeID_controller_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTROTAProviderDelegateOverrides :: MTROTAProviderDelegateOverrides
defaultMTROTAProviderDelegateOverrides = MTROTAProviderDelegateOverrides
  { _handleBDXTransferSessionEndForNodeID_controller_metrics_error = Nothing
  , _handleBDXTransferSessionEndForNodeID_controller_error = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtrotaProviderDelegateDelegateClass #-}
mtrotaProviderDelegateDelegateClass :: Class
mtrotaProviderDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTROTAProviderDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_handleBDXTransferSessionEndForNodeID_controller_metrics_error = unSelector (mkSelector "handleBDXTransferSessionEndForNodeID:controller:metrics:error:")
      sel_handleBDXTransferSessionEndForNodeID_controller_error = unSelector (mkSelector "handleBDXTransferSessionEndForNodeID:controller:error:")
  -- handleBDXTransferSessionEndForNodeID:controller:metrics:error:
  stub_0 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTROTAProviderDelegateOverrides
    case _handleBDXTransferSessionEndForNodeID_controller_metrics_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "handleBDXTransferSessionEndForNodeID:controller:metrics:error:" "v@:@@@@" stub_0

  -- handleBDXTransferSessionEndForNodeID:controller:error:
  stub_1 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTROTAProviderDelegateOverrides
    case _handleBDXTransferSessionEndForNodeID_controller_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "handleBDXTransferSessionEndForNodeID:controller:error:" "v@:@@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTROTAProviderDelegateOverrides
    if queriedSel == sel_handleBDXTransferSessionEndForNodeID_controller_metrics_error then pure (maybe 0 (const 1) (_handleBDXTransferSessionEndForNodeID_controller_metrics_error rec_))
    else if queriedSel == sel_handleBDXTransferSessionEndForNodeID_controller_error then pure (maybe 0 (const 1) (_handleBDXTransferSessionEndForNodeID_controller_error rec_))
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
newMTROTAProviderDelegate :: MTROTAProviderDelegateOverrides -> IO RawId
newMTROTAProviderDelegate overrides = do
  inst <- class_createInstance mtrotaProviderDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
