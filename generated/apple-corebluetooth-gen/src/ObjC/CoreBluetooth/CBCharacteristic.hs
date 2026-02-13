{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CBCharacteristic
--
-- Represents a service's characteristic.
--
-- Generated bindings for @CBCharacteristic@.
module ObjC.CoreBluetooth.CBCharacteristic
  ( CBCharacteristic
  , IsCBCharacteristic(..)
  , service
  , properties
  , value
  , descriptors
  , isBroadcasted
  , isNotifying
  , descriptorsSelector
  , isBroadcastedSelector
  , isNotifyingSelector
  , propertiesSelector
  , serviceSelector
  , valueSelector

  -- * Enum types
  , CBCharacteristicProperties(CBCharacteristicProperties)
  , pattern CBCharacteristicPropertyBroadcast
  , pattern CBCharacteristicPropertyRead
  , pattern CBCharacteristicPropertyWriteWithoutResponse
  , pattern CBCharacteristicPropertyWrite
  , pattern CBCharacteristicPropertyNotify
  , pattern CBCharacteristicPropertyIndicate
  , pattern CBCharacteristicPropertyAuthenticatedSignedWrites
  , pattern CBCharacteristicPropertyExtendedProperties
  , pattern CBCharacteristicPropertyNotifyEncryptionRequired
  , pattern CBCharacteristicPropertyIndicateEncryptionRequired

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreBluetooth.Internal.Classes
import ObjC.CoreBluetooth.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | service
--
-- A back-pointer to the service this characteristic belongs to.
--
-- ObjC selector: @- service@
service :: IsCBCharacteristic cbCharacteristic => cbCharacteristic -> IO (Id CBService)
service cbCharacteristic =
  sendMessage cbCharacteristic serviceSelector

-- | properties
--
-- The properties of the characteristic.
--
-- ObjC selector: @- properties@
properties :: IsCBCharacteristic cbCharacteristic => cbCharacteristic -> IO CBCharacteristicProperties
properties cbCharacteristic =
  sendMessage cbCharacteristic propertiesSelector

-- | value
--
-- The value of the characteristic.
--
-- ObjC selector: @- value@
value :: IsCBCharacteristic cbCharacteristic => cbCharacteristic -> IO (Id NSData)
value cbCharacteristic =
  sendMessage cbCharacteristic valueSelector

-- | descriptors
--
-- A list of the CBDescriptors that have so far been discovered in this characteristic.
--
-- ObjC selector: @- descriptors@
descriptors :: IsCBCharacteristic cbCharacteristic => cbCharacteristic -> IO (Id NSArray)
descriptors cbCharacteristic =
  sendMessage cbCharacteristic descriptorsSelector

-- | isBroadcasted
--
-- Whether the characteristic is currently broadcasted or not.
--
-- ObjC selector: @- isBroadcasted@
isBroadcasted :: IsCBCharacteristic cbCharacteristic => cbCharacteristic -> IO Bool
isBroadcasted cbCharacteristic =
  sendMessage cbCharacteristic isBroadcastedSelector

-- | isNotifying
--
-- Whether the characteristic is currently notifying or not.
--
-- ObjC selector: @- isNotifying@
isNotifying :: IsCBCharacteristic cbCharacteristic => cbCharacteristic -> IO Bool
isNotifying cbCharacteristic =
  sendMessage cbCharacteristic isNotifyingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @service@
serviceSelector :: Selector '[] (Id CBService)
serviceSelector = mkSelector "service"

-- | @Selector@ for @properties@
propertiesSelector :: Selector '[] CBCharacteristicProperties
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSData)
valueSelector = mkSelector "value"

-- | @Selector@ for @descriptors@
descriptorsSelector :: Selector '[] (Id NSArray)
descriptorsSelector = mkSelector "descriptors"

-- | @Selector@ for @isBroadcasted@
isBroadcastedSelector :: Selector '[] Bool
isBroadcastedSelector = mkSelector "isBroadcasted"

-- | @Selector@ for @isNotifying@
isNotifyingSelector :: Selector '[] Bool
isNotifyingSelector = mkSelector "isNotifying"

