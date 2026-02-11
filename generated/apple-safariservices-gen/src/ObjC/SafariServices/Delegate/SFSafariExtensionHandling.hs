{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SFSafariExtensionHandling@.
--
-- Usage:
--
-- @
-- delegate <- newSFSafariExtensionHandling defaultSFSafariExtensionHandlingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SafariServices.Delegate.SFSafariExtensionHandling
  ( SFSafariExtensionHandlingOverrides(..)
  , defaultSFSafariExtensionHandlingOverrides
  , newSFSafariExtensionHandling
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

-- | Overrides record for @\@protocol SFSafariExtensionHandling@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SFSafariExtensionHandlingOverrides = SFSafariExtensionHandlingOverrides
  { _messageReceivedWithName_fromPage_userInfo :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _messageReceivedFromContainingAppWithName_userInfo :: !(Maybe (RawId -> RawId -> IO ()))
  , _toolbarItemClickedInWindow :: !(Maybe (RawId -> IO ()))
  , _contextMenuItemSelectedWithCommand_inPage_userInfo :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _popoverWillShowInWindow :: !(Maybe (RawId -> IO ()))
  , _popoverDidCloseInWindow :: !(Maybe (RawId -> IO ()))
  , _popoverViewController :: !(Maybe (IO RawId))
  , _additionalRequestHeadersForURL_completionHandler :: !(Maybe (RawId -> RawId -> IO ()))
  , _contentBlockerWithIdentifier_blockedResourcesWithURLs_onPage :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _page_willNavigateToURL :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultSFSafariExtensionHandlingOverrides :: SFSafariExtensionHandlingOverrides
defaultSFSafariExtensionHandlingOverrides = SFSafariExtensionHandlingOverrides
  { _messageReceivedWithName_fromPage_userInfo = Nothing
  , _messageReceivedFromContainingAppWithName_userInfo = Nothing
  , _toolbarItemClickedInWindow = Nothing
  , _contextMenuItemSelectedWithCommand_inPage_userInfo = Nothing
  , _popoverWillShowInWindow = Nothing
  , _popoverDidCloseInWindow = Nothing
  , _popoverViewController = Nothing
  , _additionalRequestHeadersForURL_completionHandler = Nothing
  , _contentBlockerWithIdentifier_blockedResourcesWithURLs_onPage = Nothing
  , _page_willNavigateToURL = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE sfSafariExtensionHandlingDelegateClass #-}
sfSafariExtensionHandlingDelegateClass :: Class
sfSafariExtensionHandlingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSFSafariExtensionHandling" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_messageReceivedWithName_fromPage_userInfo = unSelector (mkSelector "messageReceivedWithName:fromPage:userInfo:")
      sel_messageReceivedFromContainingAppWithName_userInfo = unSelector (mkSelector "messageReceivedFromContainingAppWithName:userInfo:")
      sel_toolbarItemClickedInWindow = unSelector (mkSelector "toolbarItemClickedInWindow:")
      sel_contextMenuItemSelectedWithCommand_inPage_userInfo = unSelector (mkSelector "contextMenuItemSelectedWithCommand:inPage:userInfo:")
      sel_popoverWillShowInWindow = unSelector (mkSelector "popoverWillShowInWindow:")
      sel_popoverDidCloseInWindow = unSelector (mkSelector "popoverDidCloseInWindow:")
      sel_popoverViewController = unSelector (mkSelector "popoverViewController")
      sel_additionalRequestHeadersForURL_completionHandler = unSelector (mkSelector "additionalRequestHeadersForURL:completionHandler:")
      sel_contentBlockerWithIdentifier_blockedResourcesWithURLs_onPage = unSelector (mkSelector "contentBlockerWithIdentifier:blockedResourcesWithURLs:onPage:")
      sel_page_willNavigateToURL = unSelector (mkSelector "page:willNavigateToURL:")
  -- messageReceivedWithName:fromPage:userInfo:
  stub_0 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SFSafariExtensionHandlingOverrides
    case _messageReceivedWithName_fromPage_userInfo rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "messageReceivedWithName:fromPage:userInfo:" "v@:@@@" stub_0

  -- messageReceivedFromContainingAppWithName:userInfo:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SFSafariExtensionHandlingOverrides
    case _messageReceivedFromContainingAppWithName_userInfo rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "messageReceivedFromContainingAppWithName:userInfo:" "v@:@@" stub_1

  -- toolbarItemClickedInWindow:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SFSafariExtensionHandlingOverrides
    case _toolbarItemClickedInWindow rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "toolbarItemClickedInWindow:" "v@:@" stub_2

  -- contextMenuItemSelectedWithCommand:inPage:userInfo:
  stub_3 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SFSafariExtensionHandlingOverrides
    case _contextMenuItemSelectedWithCommand_inPage_userInfo rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "contextMenuItemSelectedWithCommand:inPage:userInfo:" "v@:@@@" stub_3

  -- popoverWillShowInWindow:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SFSafariExtensionHandlingOverrides
    case _popoverWillShowInWindow rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "popoverWillShowInWindow:" "v@:@" stub_4

  -- popoverDidCloseInWindow:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SFSafariExtensionHandlingOverrides
    case _popoverDidCloseInWindow rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "popoverDidCloseInWindow:" "v@:@" stub_5

  -- popoverViewController
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SFSafariExtensionHandlingOverrides
    case _popoverViewController rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "popoverViewController" "@@:" stub_6

  -- additionalRequestHeadersForURL:completionHandler:
  stub_7 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SFSafariExtensionHandlingOverrides
    case _additionalRequestHeadersForURL_completionHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "additionalRequestHeadersForURL:completionHandler:" "v@:@@" stub_7

  -- contentBlockerWithIdentifier:blockedResourcesWithURLs:onPage:
  stub_8 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SFSafariExtensionHandlingOverrides
    case _contentBlockerWithIdentifier_blockedResourcesWithURLs_onPage rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "contentBlockerWithIdentifier:blockedResourcesWithURLs:onPage:" "v@:@@@" stub_8

  -- page:willNavigateToURL:
  stub_9 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SFSafariExtensionHandlingOverrides
    case _page_willNavigateToURL rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "page:willNavigateToURL:" "v@:@@" stub_9

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SFSafariExtensionHandlingOverrides
    if queriedSel == sel_messageReceivedWithName_fromPage_userInfo then pure (maybe 0 (const 1) (_messageReceivedWithName_fromPage_userInfo rec_))
    else if queriedSel == sel_messageReceivedFromContainingAppWithName_userInfo then pure (maybe 0 (const 1) (_messageReceivedFromContainingAppWithName_userInfo rec_))
    else if queriedSel == sel_toolbarItemClickedInWindow then pure (maybe 0 (const 1) (_toolbarItemClickedInWindow rec_))
    else if queriedSel == sel_contextMenuItemSelectedWithCommand_inPage_userInfo then pure (maybe 0 (const 1) (_contextMenuItemSelectedWithCommand_inPage_userInfo rec_))
    else if queriedSel == sel_popoverWillShowInWindow then pure (maybe 0 (const 1) (_popoverWillShowInWindow rec_))
    else if queriedSel == sel_popoverDidCloseInWindow then pure (maybe 0 (const 1) (_popoverDidCloseInWindow rec_))
    else if queriedSel == sel_popoverViewController then pure (maybe 0 (const 1) (_popoverViewController rec_))
    else if queriedSel == sel_additionalRequestHeadersForURL_completionHandler then pure (maybe 0 (const 1) (_additionalRequestHeadersForURL_completionHandler rec_))
    else if queriedSel == sel_contentBlockerWithIdentifier_blockedResourcesWithURLs_onPage then pure (maybe 0 (const 1) (_contentBlockerWithIdentifier_blockedResourcesWithURLs_onPage rec_))
    else if queriedSel == sel_page_willNavigateToURL then pure (maybe 0 (const 1) (_page_willNavigateToURL rec_))
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
newSFSafariExtensionHandling :: SFSafariExtensionHandlingOverrides -> IO RawId
newSFSafariExtensionHandling overrides = do
  inst <- class_createInstance sfSafariExtensionHandlingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
