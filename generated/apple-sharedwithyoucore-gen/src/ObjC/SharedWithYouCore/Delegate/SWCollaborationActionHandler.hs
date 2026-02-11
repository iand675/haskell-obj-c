{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SWCollaborationActionHandler@.
--
-- Usage:
--
-- @
-- delegate <- newSWCollaborationActionHandler defaultSWCollaborationActionHandlerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SharedWithYouCore.Delegate.SWCollaborationActionHandler
  ( SWCollaborationActionHandlerOverrides(..)
  , defaultSWCollaborationActionHandlerOverrides
  , newSWCollaborationActionHandler
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

-- | Overrides record for @\@protocol SWCollaborationActionHandler@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SWCollaborationActionHandlerOverrides = SWCollaborationActionHandlerOverrides
  { _collaborationCoordinator_handleStartCollaborationAction :: !(Maybe (RawId -> RawId -> IO ()))
  , _collaborationCoordinator_handleUpdateCollaborationParticipantsAction :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultSWCollaborationActionHandlerOverrides :: SWCollaborationActionHandlerOverrides
defaultSWCollaborationActionHandlerOverrides = SWCollaborationActionHandlerOverrides
  { _collaborationCoordinator_handleStartCollaborationAction = Nothing
  , _collaborationCoordinator_handleUpdateCollaborationParticipantsAction = Nothing
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
{-# NOINLINE swCollaborationActionHandlerDelegateClass #-}
swCollaborationActionHandlerDelegateClass :: Class
swCollaborationActionHandlerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSWCollaborationActionHandler" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_collaborationCoordinator_handleStartCollaborationAction = unSelector (mkSelector "collaborationCoordinator:handleStartCollaborationAction:")
      sel_collaborationCoordinator_handleUpdateCollaborationParticipantsAction = unSelector (mkSelector "collaborationCoordinator:handleUpdateCollaborationParticipantsAction:")
  -- collaborationCoordinator:handleStartCollaborationAction:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SWCollaborationActionHandlerOverrides
    case _collaborationCoordinator_handleStartCollaborationAction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "collaborationCoordinator:handleStartCollaborationAction:" "v@:@@" stub_0

  -- collaborationCoordinator:handleUpdateCollaborationParticipantsAction:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SWCollaborationActionHandlerOverrides
    case _collaborationCoordinator_handleUpdateCollaborationParticipantsAction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "collaborationCoordinator:handleUpdateCollaborationParticipantsAction:" "v@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SWCollaborationActionHandlerOverrides
    if queriedSel == sel_collaborationCoordinator_handleStartCollaborationAction then pure (maybe 0 (const 1) (_collaborationCoordinator_handleStartCollaborationAction rec_))
    else if queriedSel == sel_collaborationCoordinator_handleUpdateCollaborationParticipantsAction then pure (maybe 0 (const 1) (_collaborationCoordinator_handleUpdateCollaborationParticipantsAction rec_))
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
newSWCollaborationActionHandler :: SWCollaborationActionHandlerOverrides -> IO RawId
newSWCollaborationActionHandler overrides = do
  inst <- class_createInstance swCollaborationActionHandlerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
