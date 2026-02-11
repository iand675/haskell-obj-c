{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol INSearchForMessagesIntentHandling@.
--
-- Usage:
--
-- @
-- delegate <- newINSearchForMessagesIntentHandling defaultINSearchForMessagesIntentHandlingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Intents.Delegate.INSearchForMessagesIntentHandling
  ( INSearchForMessagesIntentHandlingOverrides(..)
  , defaultINSearchForMessagesIntentHandlingOverrides
  , newINSearchForMessagesIntentHandling
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

-- | Overrides record for @\@protocol INSearchForMessagesIntentHandling@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data INSearchForMessagesIntentHandlingOverrides = INSearchForMessagesIntentHandlingOverrides
  { _resolveRecipientsForSearchForMessages_withCompletion :: !(Maybe (RawId -> RawId -> IO ()))
  , _resolveSendersForSearchForMessages_withCompletion :: !(Maybe (RawId -> RawId -> IO ()))
  , _resolveGroupNamesForSearchForMessages_withCompletion :: !(Maybe (RawId -> RawId -> IO ()))
  , _resolveSpeakableGroupNamesForSearchForMessages_withCompletion :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultINSearchForMessagesIntentHandlingOverrides :: INSearchForMessagesIntentHandlingOverrides
defaultINSearchForMessagesIntentHandlingOverrides = INSearchForMessagesIntentHandlingOverrides
  { _resolveRecipientsForSearchForMessages_withCompletion = Nothing
  , _resolveSendersForSearchForMessages_withCompletion = Nothing
  , _resolveGroupNamesForSearchForMessages_withCompletion = Nothing
  , _resolveSpeakableGroupNamesForSearchForMessages_withCompletion = Nothing
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
{-# NOINLINE inSearchForMessagesIntentHandlingDelegateClass #-}
inSearchForMessagesIntentHandlingDelegateClass :: Class
inSearchForMessagesIntentHandlingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsINSearchForMessagesIntentHandling" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_resolveRecipientsForSearchForMessages_withCompletion = unSelector (mkSelector "resolveRecipientsForSearchForMessages:withCompletion:")
      sel_resolveSendersForSearchForMessages_withCompletion = unSelector (mkSelector "resolveSendersForSearchForMessages:withCompletion:")
      sel_resolveGroupNamesForSearchForMessages_withCompletion = unSelector (mkSelector "resolveGroupNamesForSearchForMessages:withCompletion:")
      sel_resolveSpeakableGroupNamesForSearchForMessages_withCompletion = unSelector (mkSelector "resolveSpeakableGroupNamesForSearchForMessages:withCompletion:")
  -- resolveRecipientsForSearchForMessages:withCompletion:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO INSearchForMessagesIntentHandlingOverrides
    case _resolveRecipientsForSearchForMessages_withCompletion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "resolveRecipientsForSearchForMessages:withCompletion:" "v@:@@" stub_0

  -- resolveSendersForSearchForMessages:withCompletion:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO INSearchForMessagesIntentHandlingOverrides
    case _resolveSendersForSearchForMessages_withCompletion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "resolveSendersForSearchForMessages:withCompletion:" "v@:@@" stub_1

  -- resolveGroupNamesForSearchForMessages:withCompletion:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO INSearchForMessagesIntentHandlingOverrides
    case _resolveGroupNamesForSearchForMessages_withCompletion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "resolveGroupNamesForSearchForMessages:withCompletion:" "v@:@@" stub_2

  -- resolveSpeakableGroupNamesForSearchForMessages:withCompletion:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO INSearchForMessagesIntentHandlingOverrides
    case _resolveSpeakableGroupNamesForSearchForMessages_withCompletion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "resolveSpeakableGroupNamesForSearchForMessages:withCompletion:" "v@:@@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO INSearchForMessagesIntentHandlingOverrides
    if queriedSel == sel_resolveRecipientsForSearchForMessages_withCompletion then pure (maybe 0 (const 1) (_resolveRecipientsForSearchForMessages_withCompletion rec_))
    else if queriedSel == sel_resolveSendersForSearchForMessages_withCompletion then pure (maybe 0 (const 1) (_resolveSendersForSearchForMessages_withCompletion rec_))
    else if queriedSel == sel_resolveGroupNamesForSearchForMessages_withCompletion then pure (maybe 0 (const 1) (_resolveGroupNamesForSearchForMessages_withCompletion rec_))
    else if queriedSel == sel_resolveSpeakableGroupNamesForSearchForMessages_withCompletion then pure (maybe 0 (const 1) (_resolveSpeakableGroupNamesForSearchForMessages_withCompletion rec_))
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
newINSearchForMessagesIntentHandling :: INSearchForMessagesIntentHandlingOverrides -> IO RawId
newINSearchForMessagesIntentHandling overrides = do
  inst <- class_createInstance inSearchForMessagesIntentHandlingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
