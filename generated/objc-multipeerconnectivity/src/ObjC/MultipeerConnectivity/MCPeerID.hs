{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MCPeerID@.
module ObjC.MultipeerConnectivity.MCPeerID
  ( MCPeerID
  , IsMCPeerID(..)
  , initWithDisplayName
  , displayName
  , initWithDisplayNameSelector
  , displayNameSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithDisplayName:@
initWithDisplayName :: (IsMCPeerID mcPeerID, IsNSString myDisplayName) => mcPeerID -> myDisplayName -> IO (Id MCPeerID)
initWithDisplayName mcPeerID  myDisplayName =
withObjCPtr myDisplayName $ \raw_myDisplayName ->
    sendMsg mcPeerID (mkSelector "initWithDisplayName:") (retPtr retVoid) [argPtr (castPtr raw_myDisplayName :: Ptr ())] >>= ownedObject . castPtr

-- | @- displayName@
displayName :: IsMCPeerID mcPeerID => mcPeerID -> IO (Id NSString)
displayName mcPeerID  =
  sendMsg mcPeerID (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDisplayName:@
initWithDisplayNameSelector :: Selector
initWithDisplayNameSelector = mkSelector "initWithDisplayName:"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

