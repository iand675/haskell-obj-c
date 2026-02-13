{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MCBrowserViewController@.
module ObjC.MultipeerConnectivity.MCBrowserViewController
  ( MCBrowserViewController
  , IsMCBrowserViewController(..)
  , initWithServiceType_session
  , initWithBrowser_session
  , delegate
  , setDelegate
  , browser
  , session
  , minimumNumberOfPeers
  , setMinimumNumberOfPeers
  , maximumNumberOfPeers
  , setMaximumNumberOfPeers
  , browserSelector
  , delegateSelector
  , initWithBrowser_sessionSelector
  , initWithServiceType_sessionSelector
  , maximumNumberOfPeersSelector
  , minimumNumberOfPeersSelector
  , sessionSelector
  , setDelegateSelector
  , setMaximumNumberOfPeersSelector
  , setMinimumNumberOfPeersSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MultipeerConnectivity.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithServiceType:session:@
initWithServiceType_session :: (IsMCBrowserViewController mcBrowserViewController, IsNSString serviceType, IsMCSession session) => mcBrowserViewController -> serviceType -> session -> IO (Id MCBrowserViewController)
initWithServiceType_session mcBrowserViewController serviceType session =
  sendOwnedMessage mcBrowserViewController initWithServiceType_sessionSelector (toNSString serviceType) (toMCSession session)

-- | @- initWithBrowser:session:@
initWithBrowser_session :: (IsMCBrowserViewController mcBrowserViewController, IsMCNearbyServiceBrowser browser, IsMCSession session) => mcBrowserViewController -> browser -> session -> IO (Id MCBrowserViewController)
initWithBrowser_session mcBrowserViewController browser session =
  sendOwnedMessage mcBrowserViewController initWithBrowser_sessionSelector (toMCNearbyServiceBrowser browser) (toMCSession session)

-- | @- delegate@
delegate :: IsMCBrowserViewController mcBrowserViewController => mcBrowserViewController -> IO RawId
delegate mcBrowserViewController =
  sendMessage mcBrowserViewController delegateSelector

-- | @- setDelegate:@
setDelegate :: IsMCBrowserViewController mcBrowserViewController => mcBrowserViewController -> RawId -> IO ()
setDelegate mcBrowserViewController value =
  sendMessage mcBrowserViewController setDelegateSelector value

-- | @- browser@
browser :: IsMCBrowserViewController mcBrowserViewController => mcBrowserViewController -> IO (Id MCNearbyServiceBrowser)
browser mcBrowserViewController =
  sendMessage mcBrowserViewController browserSelector

-- | @- session@
session :: IsMCBrowserViewController mcBrowserViewController => mcBrowserViewController -> IO (Id MCSession)
session mcBrowserViewController =
  sendMessage mcBrowserViewController sessionSelector

-- | @- minimumNumberOfPeers@
minimumNumberOfPeers :: IsMCBrowserViewController mcBrowserViewController => mcBrowserViewController -> IO CULong
minimumNumberOfPeers mcBrowserViewController =
  sendMessage mcBrowserViewController minimumNumberOfPeersSelector

-- | @- setMinimumNumberOfPeers:@
setMinimumNumberOfPeers :: IsMCBrowserViewController mcBrowserViewController => mcBrowserViewController -> CULong -> IO ()
setMinimumNumberOfPeers mcBrowserViewController value =
  sendMessage mcBrowserViewController setMinimumNumberOfPeersSelector value

-- | @- maximumNumberOfPeers@
maximumNumberOfPeers :: IsMCBrowserViewController mcBrowserViewController => mcBrowserViewController -> IO CULong
maximumNumberOfPeers mcBrowserViewController =
  sendMessage mcBrowserViewController maximumNumberOfPeersSelector

-- | @- setMaximumNumberOfPeers:@
setMaximumNumberOfPeers :: IsMCBrowserViewController mcBrowserViewController => mcBrowserViewController -> CULong -> IO ()
setMaximumNumberOfPeers mcBrowserViewController value =
  sendMessage mcBrowserViewController setMaximumNumberOfPeersSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithServiceType:session:@
initWithServiceType_sessionSelector :: Selector '[Id NSString, Id MCSession] (Id MCBrowserViewController)
initWithServiceType_sessionSelector = mkSelector "initWithServiceType:session:"

-- | @Selector@ for @initWithBrowser:session:@
initWithBrowser_sessionSelector :: Selector '[Id MCNearbyServiceBrowser, Id MCSession] (Id MCBrowserViewController)
initWithBrowser_sessionSelector = mkSelector "initWithBrowser:session:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @browser@
browserSelector :: Selector '[] (Id MCNearbyServiceBrowser)
browserSelector = mkSelector "browser"

-- | @Selector@ for @session@
sessionSelector :: Selector '[] (Id MCSession)
sessionSelector = mkSelector "session"

-- | @Selector@ for @minimumNumberOfPeers@
minimumNumberOfPeersSelector :: Selector '[] CULong
minimumNumberOfPeersSelector = mkSelector "minimumNumberOfPeers"

-- | @Selector@ for @setMinimumNumberOfPeers:@
setMinimumNumberOfPeersSelector :: Selector '[CULong] ()
setMinimumNumberOfPeersSelector = mkSelector "setMinimumNumberOfPeers:"

-- | @Selector@ for @maximumNumberOfPeers@
maximumNumberOfPeersSelector :: Selector '[] CULong
maximumNumberOfPeersSelector = mkSelector "maximumNumberOfPeers"

-- | @Selector@ for @setMaximumNumberOfPeers:@
setMaximumNumberOfPeersSelector :: Selector '[CULong] ()
setMaximumNumberOfPeersSelector = mkSelector "setMaximumNumberOfPeers:"

