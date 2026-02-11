{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AEAssessmentSessionDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newAEAssessmentSessionDelegate defaultAEAssessmentSessionDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AutomaticAssessmentConfiguration.Delegate.AEAssessmentSessionDelegate
  ( AEAssessmentSessionDelegateOverrides(..)
  , defaultAEAssessmentSessionDelegateOverrides
  , newAEAssessmentSessionDelegate
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

-- | Overrides record for @\@protocol AEAssessmentSessionDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AEAssessmentSessionDelegateOverrides = AEAssessmentSessionDelegateOverrides
  { _assessmentSessionDidBegin :: !(Maybe (RawId -> IO ()))
  , _assessmentSession_failedToBeginWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _assessmentSession_wasInterruptedWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _assessmentSessionDidEnd :: !(Maybe (RawId -> IO ()))
  , _assessmentSessionDidUpdate :: !(Maybe (RawId -> IO ()))
  , _assessmentSession_failedToUpdateToConfiguration_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAEAssessmentSessionDelegateOverrides :: AEAssessmentSessionDelegateOverrides
defaultAEAssessmentSessionDelegateOverrides = AEAssessmentSessionDelegateOverrides
  { _assessmentSessionDidBegin = Nothing
  , _assessmentSession_failedToBeginWithError = Nothing
  , _assessmentSession_wasInterruptedWithError = Nothing
  , _assessmentSessionDidEnd = Nothing
  , _assessmentSessionDidUpdate = Nothing
  , _assessmentSession_failedToUpdateToConfiguration_error = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE aeAssessmentSessionDelegateDelegateClass #-}
aeAssessmentSessionDelegateDelegateClass :: Class
aeAssessmentSessionDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAEAssessmentSessionDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_assessmentSessionDidBegin = unSelector (mkSelector "assessmentSessionDidBegin:")
      sel_assessmentSession_failedToBeginWithError = unSelector (mkSelector "assessmentSession:failedToBeginWithError:")
      sel_assessmentSession_wasInterruptedWithError = unSelector (mkSelector "assessmentSession:wasInterruptedWithError:")
      sel_assessmentSessionDidEnd = unSelector (mkSelector "assessmentSessionDidEnd:")
      sel_assessmentSessionDidUpdate = unSelector (mkSelector "assessmentSessionDidUpdate:")
      sel_assessmentSession_failedToUpdateToConfiguration_error = unSelector (mkSelector "assessmentSession:failedToUpdateToConfiguration:error:")
  -- assessmentSessionDidBegin:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AEAssessmentSessionDelegateOverrides
    case _assessmentSessionDidBegin rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "assessmentSessionDidBegin:" "v@:@" stub_0

  -- assessmentSession:failedToBeginWithError:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AEAssessmentSessionDelegateOverrides
    case _assessmentSession_failedToBeginWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "assessmentSession:failedToBeginWithError:" "v@:@@" stub_1

  -- assessmentSession:wasInterruptedWithError:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AEAssessmentSessionDelegateOverrides
    case _assessmentSession_wasInterruptedWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "assessmentSession:wasInterruptedWithError:" "v@:@@" stub_2

  -- assessmentSessionDidEnd:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AEAssessmentSessionDelegateOverrides
    case _assessmentSessionDidEnd rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "assessmentSessionDidEnd:" "v@:@" stub_3

  -- assessmentSessionDidUpdate:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AEAssessmentSessionDelegateOverrides
    case _assessmentSessionDidUpdate rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "assessmentSessionDidUpdate:" "v@:@" stub_4

  -- assessmentSession:failedToUpdateToConfiguration:error:
  stub_5 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AEAssessmentSessionDelegateOverrides
    case _assessmentSession_failedToUpdateToConfiguration_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "assessmentSession:failedToUpdateToConfiguration:error:" "v@:@@@" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AEAssessmentSessionDelegateOverrides
    if queriedSel == sel_assessmentSessionDidBegin then pure (maybe 0 (const 1) (_assessmentSessionDidBegin rec_))
    else if queriedSel == sel_assessmentSession_failedToBeginWithError then pure (maybe 0 (const 1) (_assessmentSession_failedToBeginWithError rec_))
    else if queriedSel == sel_assessmentSession_wasInterruptedWithError then pure (maybe 0 (const 1) (_assessmentSession_wasInterruptedWithError rec_))
    else if queriedSel == sel_assessmentSessionDidEnd then pure (maybe 0 (const 1) (_assessmentSessionDidEnd rec_))
    else if queriedSel == sel_assessmentSessionDidUpdate then pure (maybe 0 (const 1) (_assessmentSessionDidUpdate rec_))
    else if queriedSel == sel_assessmentSession_failedToUpdateToConfiguration_error then pure (maybe 0 (const 1) (_assessmentSession_failedToUpdateToConfiguration_error rec_))
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
newAEAssessmentSessionDelegate :: AEAssessmentSessionDelegateOverrides -> IO RawId
newAEAssessmentSessionDelegate overrides = do
  inst <- class_createInstance aeAssessmentSessionDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
