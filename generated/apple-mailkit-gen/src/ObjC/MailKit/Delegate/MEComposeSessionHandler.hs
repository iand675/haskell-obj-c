{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MEComposeSessionHandler@.
--
-- Usage:
--
-- @
-- delegate <- newMEComposeSessionHandler defaultMEComposeSessionHandlerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MailKit.Delegate.MEComposeSessionHandler
  ( MEComposeSessionHandlerOverrides(..)
  , defaultMEComposeSessionHandlerOverrides
  , newMEComposeSessionHandler
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

-- | Overrides record for @\@protocol MEComposeSessionHandler@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MEComposeSessionHandlerOverrides = MEComposeSessionHandlerOverrides
  { _mailComposeSessionDidBegin :: !(Maybe (RawId -> IO ()))
  , _mailComposeSessionDidEnd :: !(Maybe (RawId -> IO ()))
  , _viewControllerForSession :: !(Maybe (RawId -> IO RawId))
  , _session_annotateAddressesWithCompletionHandler :: !(Maybe (RawId -> RawId -> IO ()))
  , _additionalHeadersForSession :: !(Maybe (RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMEComposeSessionHandlerOverrides :: MEComposeSessionHandlerOverrides
defaultMEComposeSessionHandlerOverrides = MEComposeSessionHandlerOverrides
  { _mailComposeSessionDidBegin = Nothing
  , _mailComposeSessionDidEnd = Nothing
  , _viewControllerForSession = Nothing
  , _session_annotateAddressesWithCompletionHandler = Nothing
  , _additionalHeadersForSession = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE meComposeSessionHandlerDelegateClass #-}
meComposeSessionHandlerDelegateClass :: Class
meComposeSessionHandlerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMEComposeSessionHandler" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_mailComposeSessionDidBegin = unSelector (mkSelector "mailComposeSessionDidBegin:")
      sel_mailComposeSessionDidEnd = unSelector (mkSelector "mailComposeSessionDidEnd:")
      sel_viewControllerForSession = unSelector (mkSelector "viewControllerForSession:")
      sel_session_annotateAddressesWithCompletionHandler = unSelector (mkSelector "session:annotateAddressesWithCompletionHandler:")
      sel_additionalHeadersForSession = unSelector (mkSelector "additionalHeadersForSession:")
  -- mailComposeSessionDidBegin:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEComposeSessionHandlerOverrides
    case _mailComposeSessionDidBegin rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "mailComposeSessionDidBegin:" "v@:@" stub_0

  -- mailComposeSessionDidEnd:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEComposeSessionHandlerOverrides
    case _mailComposeSessionDidEnd rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "mailComposeSessionDidEnd:" "v@:@" stub_1

  -- viewControllerForSession:
  stub_2 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEComposeSessionHandlerOverrides
    case _viewControllerForSession rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "viewControllerForSession:" "@@:@" stub_2

  -- session:annotateAddressesWithCompletionHandler:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEComposeSessionHandlerOverrides
    case _session_annotateAddressesWithCompletionHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "session:annotateAddressesWithCompletionHandler:" "v@:@@" stub_3

  -- additionalHeadersForSession:
  stub_4 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEComposeSessionHandlerOverrides
    case _additionalHeadersForSession rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "additionalHeadersForSession:" "@@:@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEComposeSessionHandlerOverrides
    if queriedSel == sel_mailComposeSessionDidBegin then pure (maybe 0 (const 1) (_mailComposeSessionDidBegin rec_))
    else if queriedSel == sel_mailComposeSessionDidEnd then pure (maybe 0 (const 1) (_mailComposeSessionDidEnd rec_))
    else if queriedSel == sel_viewControllerForSession then pure (maybe 0 (const 1) (_viewControllerForSession rec_))
    else if queriedSel == sel_session_annotateAddressesWithCompletionHandler then pure (maybe 0 (const 1) (_session_annotateAddressesWithCompletionHandler rec_))
    else if queriedSel == sel_additionalHeadersForSession then pure (maybe 0 (const 1) (_additionalHeadersForSession rec_))
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
newMEComposeSessionHandler :: MEComposeSessionHandlerOverrides -> IO RawId
newMEComposeSessionHandler overrides = do
  inst <- class_createInstance meComposeSessionHandlerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
