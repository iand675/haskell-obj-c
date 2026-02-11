{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MCBrowserViewController@.
module ObjC.MultipeerConnectivity.MCBrowserViewController
  ( MCBrowserViewController
  , IsMCBrowserViewController(..)
  , initWithServiceType_session
  , initWithBrowser_session
  , browser
  , session
  , minimumNumberOfPeers
  , setMinimumNumberOfPeers
  , maximumNumberOfPeers
  , setMaximumNumberOfPeers
  , initWithServiceType_sessionSelector
  , initWithBrowser_sessionSelector
  , browserSelector
  , sessionSelector
  , minimumNumberOfPeersSelector
  , setMinimumNumberOfPeersSelector
  , maximumNumberOfPeersSelector
  , setMaximumNumberOfPeersSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MultipeerConnectivity.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithServiceType:session:@
initWithServiceType_session :: (IsMCBrowserViewController mcBrowserViewController, IsNSString serviceType, IsMCSession session) => mcBrowserViewController -> serviceType -> session -> IO (Id MCBrowserViewController)
initWithServiceType_session mcBrowserViewController  serviceType session =
withObjCPtr serviceType $ \raw_serviceType ->
  withObjCPtr session $ \raw_session ->
      sendMsg mcBrowserViewController (mkSelector "initWithServiceType:session:") (retPtr retVoid) [argPtr (castPtr raw_serviceType :: Ptr ()), argPtr (castPtr raw_session :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithBrowser:session:@
initWithBrowser_session :: (IsMCBrowserViewController mcBrowserViewController, IsMCNearbyServiceBrowser browser, IsMCSession session) => mcBrowserViewController -> browser -> session -> IO (Id MCBrowserViewController)
initWithBrowser_session mcBrowserViewController  browser session =
withObjCPtr browser $ \raw_browser ->
  withObjCPtr session $ \raw_session ->
      sendMsg mcBrowserViewController (mkSelector "initWithBrowser:session:") (retPtr retVoid) [argPtr (castPtr raw_browser :: Ptr ()), argPtr (castPtr raw_session :: Ptr ())] >>= ownedObject . castPtr

-- | @- browser@
browser :: IsMCBrowserViewController mcBrowserViewController => mcBrowserViewController -> IO (Id MCNearbyServiceBrowser)
browser mcBrowserViewController  =
  sendMsg mcBrowserViewController (mkSelector "browser") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- session@
session :: IsMCBrowserViewController mcBrowserViewController => mcBrowserViewController -> IO (Id MCSession)
session mcBrowserViewController  =
  sendMsg mcBrowserViewController (mkSelector "session") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- minimumNumberOfPeers@
minimumNumberOfPeers :: IsMCBrowserViewController mcBrowserViewController => mcBrowserViewController -> IO CULong
minimumNumberOfPeers mcBrowserViewController  =
  sendMsg mcBrowserViewController (mkSelector "minimumNumberOfPeers") retCULong []

-- | @- setMinimumNumberOfPeers:@
setMinimumNumberOfPeers :: IsMCBrowserViewController mcBrowserViewController => mcBrowserViewController -> CULong -> IO ()
setMinimumNumberOfPeers mcBrowserViewController  value =
  sendMsg mcBrowserViewController (mkSelector "setMinimumNumberOfPeers:") retVoid [argCULong (fromIntegral value)]

-- | @- maximumNumberOfPeers@
maximumNumberOfPeers :: IsMCBrowserViewController mcBrowserViewController => mcBrowserViewController -> IO CULong
maximumNumberOfPeers mcBrowserViewController  =
  sendMsg mcBrowserViewController (mkSelector "maximumNumberOfPeers") retCULong []

-- | @- setMaximumNumberOfPeers:@
setMaximumNumberOfPeers :: IsMCBrowserViewController mcBrowserViewController => mcBrowserViewController -> CULong -> IO ()
setMaximumNumberOfPeers mcBrowserViewController  value =
  sendMsg mcBrowserViewController (mkSelector "setMaximumNumberOfPeers:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithServiceType:session:@
initWithServiceType_sessionSelector :: Selector
initWithServiceType_sessionSelector = mkSelector "initWithServiceType:session:"

-- | @Selector@ for @initWithBrowser:session:@
initWithBrowser_sessionSelector :: Selector
initWithBrowser_sessionSelector = mkSelector "initWithBrowser:session:"

-- | @Selector@ for @browser@
browserSelector :: Selector
browserSelector = mkSelector "browser"

-- | @Selector@ for @session@
sessionSelector :: Selector
sessionSelector = mkSelector "session"

-- | @Selector@ for @minimumNumberOfPeers@
minimumNumberOfPeersSelector :: Selector
minimumNumberOfPeersSelector = mkSelector "minimumNumberOfPeers"

-- | @Selector@ for @setMinimumNumberOfPeers:@
setMinimumNumberOfPeersSelector :: Selector
setMinimumNumberOfPeersSelector = mkSelector "setMinimumNumberOfPeers:"

-- | @Selector@ for @maximumNumberOfPeers@
maximumNumberOfPeersSelector :: Selector
maximumNumberOfPeersSelector = mkSelector "maximumNumberOfPeers"

-- | @Selector@ for @setMaximumNumberOfPeers:@
setMaximumNumberOfPeersSelector :: Selector
setMaximumNumberOfPeersSelector = mkSelector "setMaximumNumberOfPeers:"

