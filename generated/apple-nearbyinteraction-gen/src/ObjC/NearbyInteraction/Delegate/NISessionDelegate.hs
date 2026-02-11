{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NISessionDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNISessionDelegate defaultNISessionDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.NearbyInteraction.Delegate.NISessionDelegate
  ( NISessionDelegateOverrides(..)
  , defaultNISessionDelegateOverrides
  , newNISessionDelegate
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

-- | Overrides record for @\@protocol NISessionDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NISessionDelegateOverrides = NISessionDelegateOverrides
  { _session_didUpdateNearbyObjects :: !(Maybe (RawId -> RawId -> IO ()))
  , _sessionWasSuspended :: !(Maybe (RawId -> IO ()))
  , _sessionSuspensionEnded :: !(Maybe (RawId -> IO ()))
  , _session_didInvalidateWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _session_didGenerateShareableConfigurationData_forObject :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _session_didUpdateAlgorithmConvergence_forObject :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _session_didUpdateDLTDOAMeasurements :: !(Maybe (RawId -> RawId -> IO ()))
  , _sessionDidStartRunning :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNISessionDelegateOverrides :: NISessionDelegateOverrides
defaultNISessionDelegateOverrides = NISessionDelegateOverrides
  { _session_didUpdateNearbyObjects = Nothing
  , _sessionWasSuspended = Nothing
  , _sessionSuspensionEnded = Nothing
  , _session_didInvalidateWithError = Nothing
  , _session_didGenerateShareableConfigurationData_forObject = Nothing
  , _session_didUpdateAlgorithmConvergence_forObject = Nothing
  , _session_didUpdateDLTDOAMeasurements = Nothing
  , _sessionDidStartRunning = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE niSessionDelegateDelegateClass #-}
niSessionDelegateDelegateClass :: Class
niSessionDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNISessionDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_session_didUpdateNearbyObjects = unSelector (mkSelector "session:didUpdateNearbyObjects:")
      sel_sessionWasSuspended = unSelector (mkSelector "sessionWasSuspended:")
      sel_sessionSuspensionEnded = unSelector (mkSelector "sessionSuspensionEnded:")
      sel_session_didInvalidateWithError = unSelector (mkSelector "session:didInvalidateWithError:")
      sel_session_didGenerateShareableConfigurationData_forObject = unSelector (mkSelector "session:didGenerateShareableConfigurationData:forObject:")
      sel_session_didUpdateAlgorithmConvergence_forObject = unSelector (mkSelector "session:didUpdateAlgorithmConvergence:forObject:")
      sel_session_didUpdateDLTDOAMeasurements = unSelector (mkSelector "session:didUpdateDLTDOAMeasurements:")
      sel_sessionDidStartRunning = unSelector (mkSelector "sessionDidStartRunning:")
  -- session:didUpdateNearbyObjects:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NISessionDelegateOverrides
    case _session_didUpdateNearbyObjects rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "session:didUpdateNearbyObjects:" "v@:@@" stub_0

  -- sessionWasSuspended:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NISessionDelegateOverrides
    case _sessionWasSuspended rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "sessionWasSuspended:" "v@:@" stub_1

  -- sessionSuspensionEnded:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NISessionDelegateOverrides
    case _sessionSuspensionEnded rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "sessionSuspensionEnded:" "v@:@" stub_2

  -- session:didInvalidateWithError:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NISessionDelegateOverrides
    case _session_didInvalidateWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "session:didInvalidateWithError:" "v@:@@" stub_3

  -- session:didGenerateShareableConfigurationData:forObject:
  stub_4 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NISessionDelegateOverrides
    case _session_didGenerateShareableConfigurationData_forObject rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "session:didGenerateShareableConfigurationData:forObject:" "v@:@@@" stub_4

  -- session:didUpdateAlgorithmConvergence:forObject:
  stub_5 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NISessionDelegateOverrides
    case _session_didUpdateAlgorithmConvergence_forObject rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "session:didUpdateAlgorithmConvergence:forObject:" "v@:@@@" stub_5

  -- session:didUpdateDLTDOAMeasurements:
  stub_6 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NISessionDelegateOverrides
    case _session_didUpdateDLTDOAMeasurements rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "session:didUpdateDLTDOAMeasurements:" "v@:@@" stub_6

  -- sessionDidStartRunning:
  stub_7 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NISessionDelegateOverrides
    case _sessionDidStartRunning rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "sessionDidStartRunning:" "v@:@" stub_7

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NISessionDelegateOverrides
    if queriedSel == sel_session_didUpdateNearbyObjects then pure (maybe 0 (const 1) (_session_didUpdateNearbyObjects rec_))
    else if queriedSel == sel_sessionWasSuspended then pure (maybe 0 (const 1) (_sessionWasSuspended rec_))
    else if queriedSel == sel_sessionSuspensionEnded then pure (maybe 0 (const 1) (_sessionSuspensionEnded rec_))
    else if queriedSel == sel_session_didInvalidateWithError then pure (maybe 0 (const 1) (_session_didInvalidateWithError rec_))
    else if queriedSel == sel_session_didGenerateShareableConfigurationData_forObject then pure (maybe 0 (const 1) (_session_didGenerateShareableConfigurationData_forObject rec_))
    else if queriedSel == sel_session_didUpdateAlgorithmConvergence_forObject then pure (maybe 0 (const 1) (_session_didUpdateAlgorithmConvergence_forObject rec_))
    else if queriedSel == sel_session_didUpdateDLTDOAMeasurements then pure (maybe 0 (const 1) (_session_didUpdateDLTDOAMeasurements rec_))
    else if queriedSel == sel_sessionDidStartRunning then pure (maybe 0 (const 1) (_sessionDidStartRunning rec_))
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
newNISessionDelegate :: NISessionDelegateOverrides -> IO RawId
newNISessionDelegate overrides = do
  inst <- class_createInstance niSessionDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
