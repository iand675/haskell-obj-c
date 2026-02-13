{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MCPeerID@.
module ObjC.MultipeerConnectivity.MCPeerID
  ( MCPeerID
  , IsMCPeerID(..)
  , initWithDisplayName
  , displayName
  , displayNameSelector
  , initWithDisplayNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MultipeerConnectivity.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDisplayName:@
initWithDisplayName :: (IsMCPeerID mcPeerID, IsNSString myDisplayName) => mcPeerID -> myDisplayName -> IO (Id MCPeerID)
initWithDisplayName mcPeerID myDisplayName =
  sendOwnedMessage mcPeerID initWithDisplayNameSelector (toNSString myDisplayName)

-- | @- displayName@
displayName :: IsMCPeerID mcPeerID => mcPeerID -> IO (Id NSString)
displayName mcPeerID =
  sendMessage mcPeerID displayNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDisplayName:@
initWithDisplayNameSelector :: Selector '[Id NSString] (Id MCPeerID)
initWithDisplayNameSelector = mkSelector "initWithDisplayName:"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

