{-# LANGUAGE PatternSynonyms #-}
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
  , defaultSessionSelector
  , contactsSelector
  , addContactSelector
  , removeContactSelector
  , connectionsSelector
  , addConnectionSelector
  , removeConnectionSelector
  , sourceEndpointSelector
  , destinationEndpointSelector
  , enabledSelector
  , setEnabledSelector
  , networkPortSelector
  , networkNameSelector
  , localNameSelector
  , connectionPolicySelector
  , setConnectionPolicySelector

  -- * Enum types
  , MIDINetworkConnectionPolicy(MIDINetworkConnectionPolicy)
  , pattern MIDINetworkConnectionPolicy_NoOne
  , pattern MIDINetworkConnectionPolicy_HostsInContactList
  , pattern MIDINetworkConnectionPolicy_Anyone

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

import ObjC.CoreMIDI.Internal.Classes
import ObjC.CoreMIDI.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ defaultSession@
defaultSession :: IO (Id MIDINetworkSession)
defaultSession  =
  do
    cls' <- getRequiredClass "MIDINetworkSession"
    sendClassMsg cls' (mkSelector "defaultSession") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- contacts@
contacts :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> IO (Id NSSet)
contacts midiNetworkSession  =
  sendMsg midiNetworkSession (mkSelector "contacts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- addContact:@
addContact :: (IsMIDINetworkSession midiNetworkSession, IsMIDINetworkHost contact) => midiNetworkSession -> contact -> IO Bool
addContact midiNetworkSession  contact =
withObjCPtr contact $ \raw_contact ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiNetworkSession (mkSelector "addContact:") retCULong [argPtr (castPtr raw_contact :: Ptr ())]

-- | @- removeContact:@
removeContact :: (IsMIDINetworkSession midiNetworkSession, IsMIDINetworkHost contact) => midiNetworkSession -> contact -> IO Bool
removeContact midiNetworkSession  contact =
withObjCPtr contact $ \raw_contact ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiNetworkSession (mkSelector "removeContact:") retCULong [argPtr (castPtr raw_contact :: Ptr ())]

-- | @- connections@
connections :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> IO (Id NSSet)
connections midiNetworkSession  =
  sendMsg midiNetworkSession (mkSelector "connections") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- addConnection:@
addConnection :: (IsMIDINetworkSession midiNetworkSession, IsMIDINetworkConnection connection) => midiNetworkSession -> connection -> IO Bool
addConnection midiNetworkSession  connection =
withObjCPtr connection $ \raw_connection ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiNetworkSession (mkSelector "addConnection:") retCULong [argPtr (castPtr raw_connection :: Ptr ())]

-- | @- removeConnection:@
removeConnection :: (IsMIDINetworkSession midiNetworkSession, IsMIDINetworkConnection connection) => midiNetworkSession -> connection -> IO Bool
removeConnection midiNetworkSession  connection =
withObjCPtr connection $ \raw_connection ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiNetworkSession (mkSelector "removeConnection:") retCULong [argPtr (castPtr raw_connection :: Ptr ())]

-- | @- sourceEndpoint@
sourceEndpoint :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> IO CUInt
sourceEndpoint midiNetworkSession  =
  sendMsg midiNetworkSession (mkSelector "sourceEndpoint") retCUInt []

-- | @- destinationEndpoint@
destinationEndpoint :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> IO CUInt
destinationEndpoint midiNetworkSession  =
  sendMsg midiNetworkSession (mkSelector "destinationEndpoint") retCUInt []

-- | @- enabled@
enabled :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> IO Bool
enabled midiNetworkSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiNetworkSession (mkSelector "enabled") retCULong []

-- | @- setEnabled:@
setEnabled :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> Bool -> IO ()
setEnabled midiNetworkSession  value =
  sendMsg midiNetworkSession (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- networkPort@
networkPort :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> IO CULong
networkPort midiNetworkSession  =
  sendMsg midiNetworkSession (mkSelector "networkPort") retCULong []

-- | @- networkName@
networkName :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> IO (Id NSString)
networkName midiNetworkSession  =
  sendMsg midiNetworkSession (mkSelector "networkName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localName@
localName :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> IO (Id NSString)
localName midiNetworkSession  =
  sendMsg midiNetworkSession (mkSelector "localName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- connectionPolicy@
connectionPolicy :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> IO MIDINetworkConnectionPolicy
connectionPolicy midiNetworkSession  =
  fmap (coerce :: CULong -> MIDINetworkConnectionPolicy) $ sendMsg midiNetworkSession (mkSelector "connectionPolicy") retCULong []

-- | @- setConnectionPolicy:@
setConnectionPolicy :: IsMIDINetworkSession midiNetworkSession => midiNetworkSession -> MIDINetworkConnectionPolicy -> IO ()
setConnectionPolicy midiNetworkSession  value =
  sendMsg midiNetworkSession (mkSelector "setConnectionPolicy:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultSession@
defaultSessionSelector :: Selector
defaultSessionSelector = mkSelector "defaultSession"

-- | @Selector@ for @contacts@
contactsSelector :: Selector
contactsSelector = mkSelector "contacts"

-- | @Selector@ for @addContact:@
addContactSelector :: Selector
addContactSelector = mkSelector "addContact:"

-- | @Selector@ for @removeContact:@
removeContactSelector :: Selector
removeContactSelector = mkSelector "removeContact:"

-- | @Selector@ for @connections@
connectionsSelector :: Selector
connectionsSelector = mkSelector "connections"

-- | @Selector@ for @addConnection:@
addConnectionSelector :: Selector
addConnectionSelector = mkSelector "addConnection:"

-- | @Selector@ for @removeConnection:@
removeConnectionSelector :: Selector
removeConnectionSelector = mkSelector "removeConnection:"

-- | @Selector@ for @sourceEndpoint@
sourceEndpointSelector :: Selector
sourceEndpointSelector = mkSelector "sourceEndpoint"

-- | @Selector@ for @destinationEndpoint@
destinationEndpointSelector :: Selector
destinationEndpointSelector = mkSelector "destinationEndpoint"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @networkPort@
networkPortSelector :: Selector
networkPortSelector = mkSelector "networkPort"

-- | @Selector@ for @networkName@
networkNameSelector :: Selector
networkNameSelector = mkSelector "networkName"

-- | @Selector@ for @localName@
localNameSelector :: Selector
localNameSelector = mkSelector "localName"

-- | @Selector@ for @connectionPolicy@
connectionPolicySelector :: Selector
connectionPolicySelector = mkSelector "connectionPolicy"

-- | @Selector@ for @setConnectionPolicy:@
setConnectionPolicySelector :: Selector
setConnectionPolicySelector = mkSelector "setConnectionPolicy:"

