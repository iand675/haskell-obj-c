{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | RPBroadcastController
--
-- Available once a user has successfully initiated a broadcast using an RPBroadcastActivityViewController. Can be used to start, pause and stop a broadcast.
--
-- Generated bindings for @RPBroadcastController@.
module ObjC.ReplayKit.RPBroadcastController
  ( RPBroadcastController
  , IsRPBroadcastController(..)
  , startBroadcastWithHandler
  , pauseBroadcast
  , resumeBroadcast
  , finishBroadcastWithHandler
  , broadcasting
  , paused
  , broadcastURL
  , serviceInfo
  , delegate
  , setDelegate
  , broadcastExtensionBundleID
  , broadcastExtensionBundleIDSelector
  , broadcastURLSelector
  , broadcastingSelector
  , delegateSelector
  , finishBroadcastWithHandlerSelector
  , pauseBroadcastSelector
  , pausedSelector
  , resumeBroadcastSelector
  , serviceInfoSelector
  , setDelegateSelector
  , startBroadcastWithHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ReplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- startBroadcastWithHandler:@
startBroadcastWithHandler :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> Ptr () -> IO ()
startBroadcastWithHandler rpBroadcastController handler =
  sendMessage rpBroadcastController startBroadcastWithHandlerSelector handler

-- | @- pauseBroadcast@
pauseBroadcast :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> IO ()
pauseBroadcast rpBroadcastController =
  sendMessage rpBroadcastController pauseBroadcastSelector

-- | @- resumeBroadcast@
resumeBroadcast :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> IO ()
resumeBroadcast rpBroadcastController =
  sendMessage rpBroadcastController resumeBroadcastSelector

-- | @- finishBroadcastWithHandler:@
finishBroadcastWithHandler :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> Ptr () -> IO ()
finishBroadcastWithHandler rpBroadcastController handler =
  sendMessage rpBroadcastController finishBroadcastWithHandlerSelector handler

-- | @- broadcasting@
broadcasting :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> IO Bool
broadcasting rpBroadcastController =
  sendMessage rpBroadcastController broadcastingSelector

-- | @- paused@
paused :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> IO Bool
paused rpBroadcastController =
  sendMessage rpBroadcastController pausedSelector

-- | @- broadcastURL@
broadcastURL :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> IO (Id NSURL)
broadcastURL rpBroadcastController =
  sendMessage rpBroadcastController broadcastURLSelector

-- | @- serviceInfo@
serviceInfo :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> IO (Id NSDictionary)
serviceInfo rpBroadcastController =
  sendMessage rpBroadcastController serviceInfoSelector

-- | @- delegate@
delegate :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> IO RawId
delegate rpBroadcastController =
  sendMessage rpBroadcastController delegateSelector

-- | @- setDelegate:@
setDelegate :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> RawId -> IO ()
setDelegate rpBroadcastController value =
  sendMessage rpBroadcastController setDelegateSelector value

-- | @- broadcastExtensionBundleID@
broadcastExtensionBundleID :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> IO (Id NSString)
broadcastExtensionBundleID rpBroadcastController =
  sendMessage rpBroadcastController broadcastExtensionBundleIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startBroadcastWithHandler:@
startBroadcastWithHandlerSelector :: Selector '[Ptr ()] ()
startBroadcastWithHandlerSelector = mkSelector "startBroadcastWithHandler:"

-- | @Selector@ for @pauseBroadcast@
pauseBroadcastSelector :: Selector '[] ()
pauseBroadcastSelector = mkSelector "pauseBroadcast"

-- | @Selector@ for @resumeBroadcast@
resumeBroadcastSelector :: Selector '[] ()
resumeBroadcastSelector = mkSelector "resumeBroadcast"

-- | @Selector@ for @finishBroadcastWithHandler:@
finishBroadcastWithHandlerSelector :: Selector '[Ptr ()] ()
finishBroadcastWithHandlerSelector = mkSelector "finishBroadcastWithHandler:"

-- | @Selector@ for @broadcasting@
broadcastingSelector :: Selector '[] Bool
broadcastingSelector = mkSelector "broadcasting"

-- | @Selector@ for @paused@
pausedSelector :: Selector '[] Bool
pausedSelector = mkSelector "paused"

-- | @Selector@ for @broadcastURL@
broadcastURLSelector :: Selector '[] (Id NSURL)
broadcastURLSelector = mkSelector "broadcastURL"

-- | @Selector@ for @serviceInfo@
serviceInfoSelector :: Selector '[] (Id NSDictionary)
serviceInfoSelector = mkSelector "serviceInfo"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @broadcastExtensionBundleID@
broadcastExtensionBundleIDSelector :: Selector '[] (Id NSString)
broadcastExtensionBundleIDSelector = mkSelector "broadcastExtensionBundleID"

