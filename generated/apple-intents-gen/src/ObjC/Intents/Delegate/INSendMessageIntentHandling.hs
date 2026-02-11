{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol INSendMessageIntentHandling@.
--
-- Usage:
--
-- @
-- delegate <- newINSendMessageIntentHandling defaultINSendMessageIntentHandlingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Intents.Delegate.INSendMessageIntentHandling
  ( INSendMessageIntentHandlingOverrides(..)
  , defaultINSendMessageIntentHandlingOverrides
  , newINSendMessageIntentHandling
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

-- | Overrides record for @\@protocol INSendMessageIntentHandling@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data INSendMessageIntentHandlingOverrides = INSendMessageIntentHandlingOverrides
  { _resolveRecipientsForSendMessage_withCompletion :: !(Maybe (RawId -> RawId -> IO ()))
  , _resolveRecipientsForSendMessage_completion :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultINSendMessageIntentHandlingOverrides :: INSendMessageIntentHandlingOverrides
defaultINSendMessageIntentHandlingOverrides = INSendMessageIntentHandlingOverrides
  { _resolveRecipientsForSendMessage_withCompletion = Nothing
  , _resolveRecipientsForSendMessage_completion = Nothing
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
{-# NOINLINE inSendMessageIntentHandlingDelegateClass #-}
inSendMessageIntentHandlingDelegateClass :: Class
inSendMessageIntentHandlingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsINSendMessageIntentHandling" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_resolveRecipientsForSendMessage_withCompletion = unSelector (mkSelector "resolveRecipientsForSendMessage:withCompletion:")
      sel_resolveRecipientsForSendMessage_completion = unSelector (mkSelector "resolveRecipientsForSendMessage:completion:")
  -- resolveRecipientsForSendMessage:withCompletion:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO INSendMessageIntentHandlingOverrides
    case _resolveRecipientsForSendMessage_withCompletion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "resolveRecipientsForSendMessage:withCompletion:" "v@:@@" stub_0

  -- resolveRecipientsForSendMessage:completion:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO INSendMessageIntentHandlingOverrides
    case _resolveRecipientsForSendMessage_completion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "resolveRecipientsForSendMessage:completion:" "v@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO INSendMessageIntentHandlingOverrides
    if queriedSel == sel_resolveRecipientsForSendMessage_withCompletion then pure (maybe 0 (const 1) (_resolveRecipientsForSendMessage_withCompletion rec_))
    else if queriedSel == sel_resolveRecipientsForSendMessage_completion then pure (maybe 0 (const 1) (_resolveRecipientsForSendMessage_completion rec_))
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
newINSendMessageIntentHandling :: INSendMessageIntentHandlingOverrides -> IO RawId
newINSendMessageIntentHandling overrides = do
  inst <- class_createInstance inSendMessageIntentHandlingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
