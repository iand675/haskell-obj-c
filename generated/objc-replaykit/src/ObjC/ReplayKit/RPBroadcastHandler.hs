{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | RPBroadcastProcessExtension
--
-- Base class for extensions that are responsible for handling video and audio data.
--
-- Generated bindings for @RPBroadcastHandler@.
module ObjC.ReplayKit.RPBroadcastHandler
  ( RPBroadcastHandler
  , IsRPBroadcastHandler(..)
  , updateServiceInfo
  , updateBroadcastURL
  , updateServiceInfoSelector
  , updateBroadcastURLSelector


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

-- | Call this method, supplying it with a dictionary defined by the service, to populate the serviceInfo property on RPBroadcastController. This can be used to communicate viewing stats or messages back to the broadcasting app.
--
-- @serviceInfo@ — Dictionary that can be passed back to the broadcasting app that may contain information about the ongoing broadcast.
--
-- ObjC selector: @- updateServiceInfo:@
updateServiceInfo :: (IsRPBroadcastHandler rpBroadcastHandler, IsNSDictionary serviceInfo) => rpBroadcastHandler -> serviceInfo -> IO ()
updateServiceInfo rpBroadcastHandler  serviceInfo =
withObjCPtr serviceInfo $ \raw_serviceInfo ->
    sendMsg rpBroadcastHandler (mkSelector "updateServiceInfo:") retVoid [argPtr (castPtr raw_serviceInfo :: Ptr ())]

-- | Call this method, supplying it with a URL to update the broadcastURL property on RPBroadcastController.
--
-- @broadcastURL@ — URL of the resource where broadcast can be viewed which will be passed to the broadcasting app.
--
-- ObjC selector: @- updateBroadcastURL:@
updateBroadcastURL :: (IsRPBroadcastHandler rpBroadcastHandler, IsNSURL broadcastURL) => rpBroadcastHandler -> broadcastURL -> IO ()
updateBroadcastURL rpBroadcastHandler  broadcastURL =
withObjCPtr broadcastURL $ \raw_broadcastURL ->
    sendMsg rpBroadcastHandler (mkSelector "updateBroadcastURL:") retVoid [argPtr (castPtr raw_broadcastURL :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateServiceInfo:@
updateServiceInfoSelector :: Selector
updateServiceInfoSelector = mkSelector "updateServiceInfo:"

-- | @Selector@ for @updateBroadcastURL:@
updateBroadcastURLSelector :: Selector
updateBroadcastURLSelector = mkSelector "updateBroadcastURL:"

