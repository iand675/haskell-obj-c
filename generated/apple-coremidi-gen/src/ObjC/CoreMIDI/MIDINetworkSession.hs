{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MIDINetworkSession@.
module ObjC.CoreMIDI.MIDINetworkSession
  ( MIDINetworkSession
  , IsMIDINetworkSession(..)
  , defaultSession
  , contacts
  , addContact
  , removeContact
  , connections
  , addConnection
  , removeConnection
  , sourceEndpoint
  , destinationEndpoint
  , enabled
  , setEnabled
  , networkPort
  , networkName
  , localName
  , connectionPolicy
  , setConnectionPolicy
  , addConnectionSelector
  , addContactSelector
  , connectionPolicySelector
  , connectionsSelector
  , contactsSelector
  , defaultSessionSelector
  , destinationEndpointSelector
  , enabledSelector
  , localNameSelector
  , networkNameSelector
  , networkPortSelector
  , removeConnectionSelector
  , removeContactSelector
  , setConnectionPolicySelector
  , setEnabledSelector
  , sourceEndpointSelector

  -- * Enum types
  , MIDINetworkConnectionPolicy(MIDINetworkConnectionPolicy)
  , pattern MIDINetworkConnectionPolicy_NoOne
  , pattern MIDINetworkConnectionPolicy_HostsInContactList
  , pattern MIDINetworkConnectionPolicy_Anyone

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMIDI.Internal.Classes
import ObjC.CoreMIDI.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ defaultSession@
defaultSession :: IO (Id MIDINetworkSession)
defaultSession  =
  do
    cls' <- getRequiredClass "MIDINetworkSession"
    sendClassMessage cls' defaultSessionSelector

-- | @- contacts@
contacts :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> IO (Id NSSet)
contacts midiNetworkSession =
  sendMessage midiNetworkSession contactsSelector

-- | @- addContact:@
addContact :: (IsMIDINetworkSession midiNetworkSession, IsMIDINetworkHost contact) => midiNetworkSession -> contact -> IO Bool
addContact midiNetworkSession contact =
  sendMessage midiNetworkSession addContactSelector (toMIDINetworkHost contact)

-- | @- removeContact:@
removeContact :: (IsMIDINetworkSession midiNetworkSession, IsMIDINetworkHost contact) => midiNetworkSession -> contact -> IO Bool
removeContact midiNetworkSession contact =
  sendMessage midiNetworkSession removeContactSelector (toMIDINetworkHost contact)

-- | @- connections@
connections :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> IO (Id NSSet)
connections midiNetworkSession =
  sendMessage midiNetworkSession connectionsSelector

-- | @- addConnection:@
addConnection :: (IsMIDINetworkSession midiNetworkSession, IsMIDINetworkConnection connection) => midiNetworkSession -> connection -> IO Bool
addConnection midiNetworkSession connection =
  sendMessage midiNetworkSession addConnectionSelector (toMIDINetworkConnection connection)

-- | @- removeConnection:@
removeConnection :: (IsMIDINetworkSession midiNetworkSession, IsMIDINetworkConnection connection) => midiNetworkSession -> connection -> IO Bool
removeConnection midiNetworkSession connection =
  sendMessage midiNetworkSession removeConnectionSelector (toMIDINetworkConnection connection)

-- | @- sourceEndpoint@
sourceEndpoint :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> IO CUInt
sourceEndpoint midiNetworkSession =
  sendMessage midiNetworkSession sourceEndpointSelector

-- | @- destinationEndpoint@
destinationEndpoint :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> IO CUInt
destinationEndpoint midiNetworkSession =
  sendMessage midiNetworkSession destinationEndpointSelector

-- | @- enabled@
enabled :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> IO Bool
enabled midiNetworkSession =
  sendMessage midiNetworkSession enabledSelector

-- | @- setEnabled:@
setEnabled :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> Bool -> IO ()
setEnabled midiNetworkSession value =
  sendMessage midiNetworkSession setEnabledSelector value

-- | @- networkPort@
networkPort :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> IO CULong
networkPort midiNetworkSession =
  sendMessage midiNetworkSession networkPortSelector

-- | @- networkName@
networkName :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> IO (Id NSString)
networkName midiNetworkSession =
  sendMessage midiNetworkSession networkNameSelector

-- | @- localName@
localName :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> IO (Id NSString)
localName midiNetworkSession =
  sendMessage midiNetworkSession localNameSelector

-- | @- connectionPolicy@
connectionPolicy :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> IO MIDINetworkConnectionPolicy
connectionPolicy midiNetworkSession =
  sendMessage midiNetworkSession connectionPolicySelector

-- | @- setConnectionPolicy:@
setConnectionPolicy :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> MIDINetworkConnectionPolicy -> IO ()
setConnectionPolicy midiNetworkSession value =
  sendMessage midiNetworkSession setConnectionPolicySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultSession@
defaultSessionSelector :: Selector '[] (Id MIDINetworkSession)
defaultSessionSelector = mkSelector "defaultSession"

-- | @Selector@ for @contacts@
contactsSelector :: Selector '[] (Id NSSet)
contactsSelector = mkSelector "contacts"

-- | @Selector@ for @addContact:@
addContactSelector :: Selector '[Id MIDINetworkHost] Bool
addContactSelector = mkSelector "addContact:"

-- | @Selector@ for @removeContact:@
removeContactSelector :: Selector '[Id MIDINetworkHost] Bool
removeContactSelector = mkSelector "removeContact:"

-- | @Selector@ for @connections@
connectionsSelector :: Selector '[] (Id NSSet)
connectionsSelector = mkSelector "connections"

-- | @Selector@ for @addConnection:@
addConnectionSelector :: Selector '[Id MIDINetworkConnection] Bool
addConnectionSelector = mkSelector "addConnection:"

-- | @Selector@ for @removeConnection:@
removeConnectionSelector :: Selector '[Id MIDINetworkConnection] Bool
removeConnectionSelector = mkSelector "removeConnection:"

-- | @Selector@ for @sourceEndpoint@
sourceEndpointSelector :: Selector '[] CUInt
sourceEndpointSelector = mkSelector "sourceEndpoint"

-- | @Selector@ for @destinationEndpoint@
destinationEndpointSelector :: Selector '[] CUInt
destinationEndpointSelector = mkSelector "destinationEndpoint"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @networkPort@
networkPortSelector :: Selector '[] CULong
networkPortSelector = mkSelector "networkPort"

-- | @Selector@ for @networkName@
networkNameSelector :: Selector '[] (Id NSString)
networkNameSelector = mkSelector "networkName"

-- | @Selector@ for @localName@
localNameSelector :: Selector '[] (Id NSString)
localNameSelector = mkSelector "localName"

-- | @Selector@ for @connectionPolicy@
connectionPolicySelector :: Selector '[] MIDINetworkConnectionPolicy
connectionPolicySelector = mkSelector "connectionPolicy"

-- | @Selector@ for @setConnectionPolicy:@
setConnectionPolicySelector :: Selector '[MIDINetworkConnectionPolicy] ()
setConnectionPolicySelector = mkSelector "setConnectionPolicy:"

