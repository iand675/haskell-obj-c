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
  , startBroadcastWithHandlerSelector
  , pauseBroadcastSelector
  , resumeBroadcastSelector
  , finishBroadcastWithHandlerSelector
  , broadcastingSelector
  , pausedSelector
  , broadcastURLSelector
  , serviceInfoSelector
  , delegateSelector
  , setDelegateSelector
  , broadcastExtensionBundleIDSelector


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

import ObjC.ReplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- startBroadcastWithHandler:@
startBroadcastWithHandler :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> Ptr () -> IO ()
startBroadcastWithHandler rpBroadcastController  handler =
    sendMsg rpBroadcastController (mkSelector "startBroadcastWithHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | @- pauseBroadcast@
pauseBroadcast :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> IO ()
pauseBroadcast rpBroadcastController  =
    sendMsg rpBroadcastController (mkSelector "pauseBroadcast") retVoid []

-- | @- resumeBroadcast@
resumeBroadcast :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> IO ()
resumeBroadcast rpBroadcastController  =
    sendMsg rpBroadcastController (mkSelector "resumeBroadcast") retVoid []

-- | @- finishBroadcastWithHandler:@
finishBroadcastWithHandler :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> Ptr () -> IO ()
finishBroadcastWithHandler rpBroadcastController  handler =
    sendMsg rpBroadcastController (mkSelector "finishBroadcastWithHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | @- broadcasting@
broadcasting :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> IO Bool
broadcasting rpBroadcastController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg rpBroadcastController (mkSelector "broadcasting") retCULong []

-- | @- paused@
paused :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> IO Bool
paused rpBroadcastController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg rpBroadcastController (mkSelector "paused") retCULong []

-- | @- broadcastURL@
broadcastURL :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> IO (Id NSURL)
broadcastURL rpBroadcastController  =
    sendMsg rpBroadcastController (mkSelector "broadcastURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- serviceInfo@
serviceInfo :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> IO (Id NSDictionary)
serviceInfo rpBroadcastController  =
    sendMsg rpBroadcastController (mkSelector "serviceInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- delegate@
delegate :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> IO RawId
delegate rpBroadcastController  =
    fmap (RawId . castPtr) $ sendMsg rpBroadcastController (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> RawId -> IO ()
setDelegate rpBroadcastController  value =
    sendMsg rpBroadcastController (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- broadcastExtensionBundleID@
broadcastExtensionBundleID :: IsRPBroadcastController rpBroadcastController => rpBroadcastController -> IO (Id NSString)
broadcastExtensionBundleID rpBroadcastController  =
    sendMsg rpBroadcastController (mkSelector "broadcastExtensionBundleID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startBroadcastWithHandler:@
startBroadcastWithHandlerSelector :: Selector
startBroadcastWithHandlerSelector = mkSelector "startBroadcastWithHandler:"

-- | @Selector@ for @pauseBroadcast@
pauseBroadcastSelector :: Selector
pauseBroadcastSelector = mkSelector "pauseBroadcast"

-- | @Selector@ for @resumeBroadcast@
resumeBroadcastSelector :: Selector
resumeBroadcastSelector = mkSelector "resumeBroadcast"

-- | @Selector@ for @finishBroadcastWithHandler:@
finishBroadcastWithHandlerSelector :: Selector
finishBroadcastWithHandlerSelector = mkSelector "finishBroadcastWithHandler:"

-- | @Selector@ for @broadcasting@
broadcastingSelector :: Selector
broadcastingSelector = mkSelector "broadcasting"

-- | @Selector@ for @paused@
pausedSelector :: Selector
pausedSelector = mkSelector "paused"

-- | @Selector@ for @broadcastURL@
broadcastURLSelector :: Selector
broadcastURLSelector = mkSelector "broadcastURL"

-- | @Selector@ for @serviceInfo@
serviceInfoSelector :: Selector
serviceInfoSelector = mkSelector "serviceInfo"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @broadcastExtensionBundleID@
broadcastExtensionBundleIDSelector :: Selector
broadcastExtensionBundleIDSelector = mkSelector "broadcastExtensionBundleID"

