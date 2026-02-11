{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AMWorkflowControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newAMWorkflowControllerDelegate defaultAMWorkflowControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Automator.Delegate.AMWorkflowControllerDelegate
  ( AMWorkflowControllerDelegateOverrides(..)
  , defaultAMWorkflowControllerDelegateOverrides
  , newAMWorkflowControllerDelegate
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

-- | Overrides record for @\@protocol AMWorkflowControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AMWorkflowControllerDelegateOverrides = AMWorkflowControllerDelegateOverrides
  { _workflowControllerWillRun :: !(Maybe (RawId -> IO ()))
  , _workflowControllerWillStop :: !(Maybe (RawId -> IO ()))
  , _workflowControllerDidRun :: !(Maybe (RawId -> IO ()))
  , _workflowControllerDidStop :: !(Maybe (RawId -> IO ()))
  , _workflowController_willRunAction :: !(Maybe (RawId -> RawId -> IO ()))
  , _workflowController_didRunAction :: !(Maybe (RawId -> RawId -> IO ()))
  , _workflowController_didError :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAMWorkflowControllerDelegateOverrides :: AMWorkflowControllerDelegateOverrides
defaultAMWorkflowControllerDelegateOverrides = AMWorkflowControllerDelegateOverrides
  { _workflowControllerWillRun = Nothing
  , _workflowControllerWillStop = Nothing
  , _workflowControllerDidRun = Nothing
  , _workflowControllerDidStop = Nothing
  , _workflowController_willRunAction = Nothing
  , _workflowController_didRunAction = Nothing
  , _workflowController_didError = Nothing
  }

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
{-# NOINLINE amWorkflowControllerDelegateDelegateClass #-}
amWorkflowControllerDelegateDelegateClass :: Class
amWorkflowControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAMWorkflowControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_workflowControllerWillRun = unSelector (mkSelector "workflowControllerWillRun:")
      sel_workflowControllerWillStop = unSelector (mkSelector "workflowControllerWillStop:")
      sel_workflowControllerDidRun = unSelector (mkSelector "workflowControllerDidRun:")
      sel_workflowControllerDidStop = unSelector (mkSelector "workflowControllerDidStop:")
      sel_workflowController_willRunAction = unSelector (mkSelector "workflowController:willRunAction:")
      sel_workflowController_didRunAction = unSelector (mkSelector "workflowController:didRunAction:")
      sel_workflowController_didError = unSelector (mkSelector "workflowController:didError:")
  -- workflowControllerWillRun:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AMWorkflowControllerDelegateOverrides
    case _workflowControllerWillRun rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "workflowControllerWillRun:" "v@:@" stub_0

  -- workflowControllerWillStop:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AMWorkflowControllerDelegateOverrides
    case _workflowControllerWillStop rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "workflowControllerWillStop:" "v@:@" stub_1

  -- workflowControllerDidRun:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AMWorkflowControllerDelegateOverrides
    case _workflowControllerDidRun rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "workflowControllerDidRun:" "v@:@" stub_2

  -- workflowControllerDidStop:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AMWorkflowControllerDelegateOverrides
    case _workflowControllerDidStop rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "workflowControllerDidStop:" "v@:@" stub_3

  -- workflowController:willRunAction:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AMWorkflowControllerDelegateOverrides
    case _workflowController_willRunAction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "workflowController:willRunAction:" "v@:@@" stub_4

  -- workflowController:didRunAction:
  stub_5 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AMWorkflowControllerDelegateOverrides
    case _workflowController_didRunAction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "workflowController:didRunAction:" "v@:@@" stub_5

  -- workflowController:didError:
  stub_6 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AMWorkflowControllerDelegateOverrides
    case _workflowController_didError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "workflowController:didError:" "v@:@@" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AMWorkflowControllerDelegateOverrides
    if queriedSel == sel_workflowControllerWillRun then pure (maybe 0 (const 1) (_workflowControllerWillRun rec_))
    else if queriedSel == sel_workflowControllerWillStop then pure (maybe 0 (const 1) (_workflowControllerWillStop rec_))
    else if queriedSel == sel_workflowControllerDidRun then pure (maybe 0 (const 1) (_workflowControllerDidRun rec_))
    else if queriedSel == sel_workflowControllerDidStop then pure (maybe 0 (const 1) (_workflowControllerDidStop rec_))
    else if queriedSel == sel_workflowController_willRunAction then pure (maybe 0 (const 1) (_workflowController_willRunAction rec_))
    else if queriedSel == sel_workflowController_didRunAction then pure (maybe 0 (const 1) (_workflowController_didRunAction rec_))
    else if queriedSel == sel_workflowController_didError then pure (maybe 0 (const 1) (_workflowController_didError rec_))
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
newAMWorkflowControllerDelegate :: AMWorkflowControllerDelegateOverrides -> IO RawId
newAMWorkflowControllerDelegate overrides = do
  inst <- class_createInstance amWorkflowControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
