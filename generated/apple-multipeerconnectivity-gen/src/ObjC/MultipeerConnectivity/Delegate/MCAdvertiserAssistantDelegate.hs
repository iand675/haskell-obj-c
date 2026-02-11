{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MCAdvertiserAssistantDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newMCAdvertiserAssistantDelegate defaultMCAdvertiserAssistantDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MultipeerConnectivity.Delegate.MCAdvertiserAssistantDelegate
  ( MCAdvertiserAssistantDelegateOverrides(..)
  , defaultMCAdvertiserAssistantDelegateOverrides
  , newMCAdvertiserAssistantDelegate
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

-- | Overrides record for @\@protocol MCAdvertiserAssistantDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MCAdvertiserAssistantDelegateOverrides = MCAdvertiserAssistantDelegateOverrides
  { _advertiserAssistantWillPresentInvitation :: !(Maybe (RawId -> IO ()))
  , _advertiserAssistantDidDismissInvitation :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMCAdvertiserAssistantDelegateOverrides :: MCAdvertiserAssistantDelegateOverrides
defaultMCAdvertiserAssistantDelegateOverrides = MCAdvertiserAssistantDelegateOverrides
  { _advertiserAssistantWillPresentInvitation = Nothing
  , _advertiserAssistantDidDismissInvitation = Nothing
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
{-# NOINLINE mcAdvertiserAssistantDelegateDelegateClass #-}
mcAdvertiserAssistantDelegateDelegateClass :: Class
mcAdvertiserAssistantDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMCAdvertiserAssistantDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_advertiserAssistantWillPresentInvitation = unSelector (mkSelector "advertiserAssistantWillPresentInvitation:")
      sel_advertiserAssistantDidDismissInvitation = unSelector (mkSelector "advertiserAssistantDidDismissInvitation:")
  -- advertiserAssistantWillPresentInvitation:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MCAdvertiserAssistantDelegateOverrides
    case _advertiserAssistantWillPresentInvitation rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "advertiserAssistantWillPresentInvitation:" "v@:@" stub_0

  -- advertiserAssistantDidDismissInvitation:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MCAdvertiserAssistantDelegateOverrides
    case _advertiserAssistantDidDismissInvitation rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "advertiserAssistantDidDismissInvitation:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MCAdvertiserAssistantDelegateOverrides
    if queriedSel == sel_advertiserAssistantWillPresentInvitation then pure (maybe 0 (const 1) (_advertiserAssistantWillPresentInvitation rec_))
    else if queriedSel == sel_advertiserAssistantDidDismissInvitation then pure (maybe 0 (const 1) (_advertiserAssistantDidDismissInvitation rec_))
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
newMCAdvertiserAssistantDelegate :: MCAdvertiserAssistantDelegateOverrides -> IO RawId
newMCAdvertiserAssistantDelegate overrides = do
  inst <- class_createInstance mcAdvertiserAssistantDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
