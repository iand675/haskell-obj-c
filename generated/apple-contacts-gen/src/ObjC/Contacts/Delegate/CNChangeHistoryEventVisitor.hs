{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CNChangeHistoryEventVisitor@.
--
-- Usage:
--
-- @
-- delegate <- newCNChangeHistoryEventVisitor defaultCNChangeHistoryEventVisitorOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Contacts.Delegate.CNChangeHistoryEventVisitor
  ( CNChangeHistoryEventVisitorOverrides(..)
  , defaultCNChangeHistoryEventVisitorOverrides
  , newCNChangeHistoryEventVisitor
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

-- | Overrides record for @\@protocol CNChangeHistoryEventVisitor@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CNChangeHistoryEventVisitorOverrides = CNChangeHistoryEventVisitorOverrides
  { _visitDropEverythingEvent :: !(Maybe (RawId -> IO ()))
  , _visitAddContactEvent :: !(Maybe (RawId -> IO ()))
  , _visitUpdateContactEvent :: !(Maybe (RawId -> IO ()))
  , _visitDeleteContactEvent :: !(Maybe (RawId -> IO ()))
  , _visitAddGroupEvent :: !(Maybe (RawId -> IO ()))
  , _visitUpdateGroupEvent :: !(Maybe (RawId -> IO ()))
  , _visitDeleteGroupEvent :: !(Maybe (RawId -> IO ()))
  , _visitAddMemberToGroupEvent :: !(Maybe (RawId -> IO ()))
  , _visitRemoveMemberFromGroupEvent :: !(Maybe (RawId -> IO ()))
  , _visitAddSubgroupToGroupEvent :: !(Maybe (RawId -> IO ()))
  , _visitRemoveSubgroupFromGroupEvent :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultCNChangeHistoryEventVisitorOverrides :: CNChangeHistoryEventVisitorOverrides
defaultCNChangeHistoryEventVisitorOverrides = CNChangeHistoryEventVisitorOverrides
  { _visitDropEverythingEvent = Nothing
  , _visitAddContactEvent = Nothing
  , _visitUpdateContactEvent = Nothing
  , _visitDeleteContactEvent = Nothing
  , _visitAddGroupEvent = Nothing
  , _visitUpdateGroupEvent = Nothing
  , _visitDeleteGroupEvent = Nothing
  , _visitAddMemberToGroupEvent = Nothing
  , _visitRemoveMemberFromGroupEvent = Nothing
  , _visitAddSubgroupToGroupEvent = Nothing
  , _visitRemoveSubgroupFromGroupEvent = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE cnChangeHistoryEventVisitorDelegateClass #-}
cnChangeHistoryEventVisitorDelegateClass :: Class
cnChangeHistoryEventVisitorDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCNChangeHistoryEventVisitor" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_visitDropEverythingEvent = unSelector (mkSelector "visitDropEverythingEvent:")
      sel_visitAddContactEvent = unSelector (mkSelector "visitAddContactEvent:")
      sel_visitUpdateContactEvent = unSelector (mkSelector "visitUpdateContactEvent:")
      sel_visitDeleteContactEvent = unSelector (mkSelector "visitDeleteContactEvent:")
      sel_visitAddGroupEvent = unSelector (mkSelector "visitAddGroupEvent:")
      sel_visitUpdateGroupEvent = unSelector (mkSelector "visitUpdateGroupEvent:")
      sel_visitDeleteGroupEvent = unSelector (mkSelector "visitDeleteGroupEvent:")
      sel_visitAddMemberToGroupEvent = unSelector (mkSelector "visitAddMemberToGroupEvent:")
      sel_visitRemoveMemberFromGroupEvent = unSelector (mkSelector "visitRemoveMemberFromGroupEvent:")
      sel_visitAddSubgroupToGroupEvent = unSelector (mkSelector "visitAddSubgroupToGroupEvent:")
      sel_visitRemoveSubgroupFromGroupEvent = unSelector (mkSelector "visitRemoveSubgroupFromGroupEvent:")
  -- visitDropEverythingEvent:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CNChangeHistoryEventVisitorOverrides
    case _visitDropEverythingEvent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "visitDropEverythingEvent:" "v@:@" stub_0

  -- visitAddContactEvent:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CNChangeHistoryEventVisitorOverrides
    case _visitAddContactEvent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "visitAddContactEvent:" "v@:@" stub_1

  -- visitUpdateContactEvent:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CNChangeHistoryEventVisitorOverrides
    case _visitUpdateContactEvent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "visitUpdateContactEvent:" "v@:@" stub_2

  -- visitDeleteContactEvent:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CNChangeHistoryEventVisitorOverrides
    case _visitDeleteContactEvent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "visitDeleteContactEvent:" "v@:@" stub_3

  -- visitAddGroupEvent:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CNChangeHistoryEventVisitorOverrides
    case _visitAddGroupEvent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "visitAddGroupEvent:" "v@:@" stub_4

  -- visitUpdateGroupEvent:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CNChangeHistoryEventVisitorOverrides
    case _visitUpdateGroupEvent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "visitUpdateGroupEvent:" "v@:@" stub_5

  -- visitDeleteGroupEvent:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CNChangeHistoryEventVisitorOverrides
    case _visitDeleteGroupEvent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "visitDeleteGroupEvent:" "v@:@" stub_6

  -- visitAddMemberToGroupEvent:
  stub_7 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CNChangeHistoryEventVisitorOverrides
    case _visitAddMemberToGroupEvent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "visitAddMemberToGroupEvent:" "v@:@" stub_7

  -- visitRemoveMemberFromGroupEvent:
  stub_8 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CNChangeHistoryEventVisitorOverrides
    case _visitRemoveMemberFromGroupEvent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "visitRemoveMemberFromGroupEvent:" "v@:@" stub_8

  -- visitAddSubgroupToGroupEvent:
  stub_9 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CNChangeHistoryEventVisitorOverrides
    case _visitAddSubgroupToGroupEvent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "visitAddSubgroupToGroupEvent:" "v@:@" stub_9

  -- visitRemoveSubgroupFromGroupEvent:
  stub_10 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CNChangeHistoryEventVisitorOverrides
    case _visitRemoveSubgroupFromGroupEvent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "visitRemoveSubgroupFromGroupEvent:" "v@:@" stub_10

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CNChangeHistoryEventVisitorOverrides
    if queriedSel == sel_visitDropEverythingEvent then pure (maybe 0 (const 1) (_visitDropEverythingEvent rec_))
    else if queriedSel == sel_visitAddContactEvent then pure (maybe 0 (const 1) (_visitAddContactEvent rec_))
    else if queriedSel == sel_visitUpdateContactEvent then pure (maybe 0 (const 1) (_visitUpdateContactEvent rec_))
    else if queriedSel == sel_visitDeleteContactEvent then pure (maybe 0 (const 1) (_visitDeleteContactEvent rec_))
    else if queriedSel == sel_visitAddGroupEvent then pure (maybe 0 (const 1) (_visitAddGroupEvent rec_))
    else if queriedSel == sel_visitUpdateGroupEvent then pure (maybe 0 (const 1) (_visitUpdateGroupEvent rec_))
    else if queriedSel == sel_visitDeleteGroupEvent then pure (maybe 0 (const 1) (_visitDeleteGroupEvent rec_))
    else if queriedSel == sel_visitAddMemberToGroupEvent then pure (maybe 0 (const 1) (_visitAddMemberToGroupEvent rec_))
    else if queriedSel == sel_visitRemoveMemberFromGroupEvent then pure (maybe 0 (const 1) (_visitRemoveMemberFromGroupEvent rec_))
    else if queriedSel == sel_visitAddSubgroupToGroupEvent then pure (maybe 0 (const 1) (_visitAddSubgroupToGroupEvent rec_))
    else if queriedSel == sel_visitRemoveSubgroupFromGroupEvent then pure (maybe 0 (const 1) (_visitRemoveSubgroupFromGroupEvent rec_))
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
newCNChangeHistoryEventVisitor :: CNChangeHistoryEventVisitorOverrides -> IO RawId
newCNChangeHistoryEventVisitor overrides = do
  inst <- class_createInstance cnChangeHistoryEventVisitorDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
