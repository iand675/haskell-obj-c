{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CMWaterSubmersionManagerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newCMWaterSubmersionManagerDelegate defaultCMWaterSubmersionManagerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreMotion.Delegate.CMWaterSubmersionManagerDelegate
  ( CMWaterSubmersionManagerDelegateOverrides(..)
  , defaultCMWaterSubmersionManagerDelegateOverrides
  , newCMWaterSubmersionManagerDelegate
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

-- | Overrides record for @\@protocol CMWaterSubmersionManagerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CMWaterSubmersionManagerDelegateOverrides = CMWaterSubmersionManagerDelegateOverrides
  { _manager_didUpdateEvent :: !(Maybe (RawId -> RawId -> IO ()))
  , _manager_didUpdateMeasurement :: !(Maybe (RawId -> RawId -> IO ()))
  , _manager_didUpdateTemperature :: !(Maybe (RawId -> RawId -> IO ()))
  , _manager_errorOccurred :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultCMWaterSubmersionManagerDelegateOverrides :: CMWaterSubmersionManagerDelegateOverrides
defaultCMWaterSubmersionManagerDelegateOverrides = CMWaterSubmersionManagerDelegateOverrides
  { _manager_didUpdateEvent = Nothing
  , _manager_didUpdateMeasurement = Nothing
  , _manager_didUpdateTemperature = Nothing
  , _manager_errorOccurred = Nothing
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
{-# NOINLINE cmWaterSubmersionManagerDelegateDelegateClass #-}
cmWaterSubmersionManagerDelegateDelegateClass :: Class
cmWaterSubmersionManagerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCMWaterSubmersionManagerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_manager_didUpdateEvent = unSelector (mkSelector "manager:didUpdateEvent:")
      sel_manager_didUpdateMeasurement = unSelector (mkSelector "manager:didUpdateMeasurement:")
      sel_manager_didUpdateTemperature = unSelector (mkSelector "manager:didUpdateTemperature:")
      sel_manager_errorOccurred = unSelector (mkSelector "manager:errorOccurred:")
  -- manager:didUpdateEvent:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMWaterSubmersionManagerDelegateOverrides
    case _manager_didUpdateEvent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "manager:didUpdateEvent:" "v@:@@" stub_0

  -- manager:didUpdateMeasurement:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMWaterSubmersionManagerDelegateOverrides
    case _manager_didUpdateMeasurement rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "manager:didUpdateMeasurement:" "v@:@@" stub_1

  -- manager:didUpdateTemperature:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMWaterSubmersionManagerDelegateOverrides
    case _manager_didUpdateTemperature rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "manager:didUpdateTemperature:" "v@:@@" stub_2

  -- manager:errorOccurred:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMWaterSubmersionManagerDelegateOverrides
    case _manager_errorOccurred rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "manager:errorOccurred:" "v@:@@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CMWaterSubmersionManagerDelegateOverrides
    if queriedSel == sel_manager_didUpdateEvent then pure (maybe 0 (const 1) (_manager_didUpdateEvent rec_))
    else if queriedSel == sel_manager_didUpdateMeasurement then pure (maybe 0 (const 1) (_manager_didUpdateMeasurement rec_))
    else if queriedSel == sel_manager_didUpdateTemperature then pure (maybe 0 (const 1) (_manager_didUpdateTemperature rec_))
    else if queriedSel == sel_manager_errorOccurred then pure (maybe 0 (const 1) (_manager_errorOccurred rec_))
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
newCMWaterSubmersionManagerDelegate :: CMWaterSubmersionManagerDelegateOverrides -> IO RawId
newCMWaterSubmersionManagerDelegate overrides = do
  inst <- class_createInstance cmWaterSubmersionManagerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
