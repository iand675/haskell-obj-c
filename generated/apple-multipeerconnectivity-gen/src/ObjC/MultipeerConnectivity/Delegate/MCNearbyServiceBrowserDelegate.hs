{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MCNearbyServiceBrowserDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newMCNearbyServiceBrowserDelegate defaultMCNearbyServiceBrowserDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MultipeerConnectivity.Delegate.MCNearbyServiceBrowserDelegate
  ( MCNearbyServiceBrowserDelegateOverrides(..)
  , defaultMCNearbyServiceBrowserDelegateOverrides
  , newMCNearbyServiceBrowserDelegate
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

-- | Overrides record for @\@protocol MCNearbyServiceBrowserDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MCNearbyServiceBrowserDelegateOverrides = MCNearbyServiceBrowserDelegateOverrides
  { _browser_foundPeer_withDiscoveryInfo :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _browser_lostPeer :: !(Maybe (RawId -> RawId -> IO ()))
  , _browser_didNotStartBrowsingForPeers :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMCNearbyServiceBrowserDelegateOverrides :: MCNearbyServiceBrowserDelegateOverrides
defaultMCNearbyServiceBrowserDelegateOverrides = MCNearbyServiceBrowserDelegateOverrides
  { _browser_foundPeer_withDiscoveryInfo = Nothing
  , _browser_lostPeer = Nothing
  , _browser_didNotStartBrowsingForPeers = Nothing
  }

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
{-# NOINLINE mcNearbyServiceBrowserDelegateDelegateClass #-}
mcNearbyServiceBrowserDelegateDelegateClass :: Class
mcNearbyServiceBrowserDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMCNearbyServiceBrowserDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_browser_foundPeer_withDiscoveryInfo = unSelector (mkSelector "browser:foundPeer:withDiscoveryInfo:")
      sel_browser_lostPeer = unSelector (mkSelector "browser:lostPeer:")
      sel_browser_didNotStartBrowsingForPeers = unSelector (mkSelector "browser:didNotStartBrowsingForPeers:")
  -- browser:foundPeer:withDiscoveryInfo:
  stub_0 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MCNearbyServiceBrowserDelegateOverrides
    case _browser_foundPeer_withDiscoveryInfo rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "browser:foundPeer:withDiscoveryInfo:" "v@:@@@" stub_0

  -- browser:lostPeer:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MCNearbyServiceBrowserDelegateOverrides
    case _browser_lostPeer rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "browser:lostPeer:" "v@:@@" stub_1

  -- browser:didNotStartBrowsingForPeers:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MCNearbyServiceBrowserDelegateOverrides
    case _browser_didNotStartBrowsingForPeers rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "browser:didNotStartBrowsingForPeers:" "v@:@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MCNearbyServiceBrowserDelegateOverrides
    if queriedSel == sel_browser_foundPeer_withDiscoveryInfo then pure (maybe 0 (const 1) (_browser_foundPeer_withDiscoveryInfo rec_))
    else if queriedSel == sel_browser_lostPeer then pure (maybe 0 (const 1) (_browser_lostPeer rec_))
    else if queriedSel == sel_browser_didNotStartBrowsingForPeers then pure (maybe 0 (const 1) (_browser_didNotStartBrowsingForPeers rec_))
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
newMCNearbyServiceBrowserDelegate :: MCNearbyServiceBrowserDelegateOverrides -> IO RawId
newMCNearbyServiceBrowserDelegate overrides = do
  inst <- class_createInstance mcNearbyServiceBrowserDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
