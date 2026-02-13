{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCarHeadUnit@.
module ObjC.Intents.INCarHeadUnit
  ( INCarHeadUnit
  , IsINCarHeadUnit(..)
  , init_
  , initWithBluetoothIdentifier_iAP2Identifier
  , bluetoothIdentifier
  , iAP2Identifier
  , bluetoothIdentifierSelector
  , iAP2IdentifierSelector
  , initSelector
  , initWithBluetoothIdentifier_iAP2IdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINCarHeadUnit inCarHeadUnit => inCarHeadUnit -> IO (Id INCarHeadUnit)
init_ inCarHeadUnit =
  sendOwnedMessage inCarHeadUnit initSelector

-- | @- initWithBluetoothIdentifier:iAP2Identifier:@
initWithBluetoothIdentifier_iAP2Identifier :: (IsINCarHeadUnit inCarHeadUnit, IsNSString bluetoothIdentifier, IsNSString iAP2Identifier) => inCarHeadUnit -> bluetoothIdentifier -> iAP2Identifier -> IO (Id INCarHeadUnit)
initWithBluetoothIdentifier_iAP2Identifier inCarHeadUnit bluetoothIdentifier iAP2Identifier =
  sendOwnedMessage inCarHeadUnit initWithBluetoothIdentifier_iAP2IdentifierSelector (toNSString bluetoothIdentifier) (toNSString iAP2Identifier)

-- | @- bluetoothIdentifier@
bluetoothIdentifier :: IsINCarHeadUnit inCarHeadUnit => inCarHeadUnit -> IO (Id NSString)
bluetoothIdentifier inCarHeadUnit =
  sendMessage inCarHeadUnit bluetoothIdentifierSelector

-- | @- iAP2Identifier@
iAP2Identifier :: IsINCarHeadUnit inCarHeadUnit => inCarHeadUnit -> IO (Id NSString)
iAP2Identifier inCarHeadUnit =
  sendMessage inCarHeadUnit iAP2IdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INCarHeadUnit)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithBluetoothIdentifier:iAP2Identifier:@
initWithBluetoothIdentifier_iAP2IdentifierSelector :: Selector '[Id NSString, Id NSString] (Id INCarHeadUnit)
initWithBluetoothIdentifier_iAP2IdentifierSelector = mkSelector "initWithBluetoothIdentifier:iAP2Identifier:"

-- | @Selector@ for @bluetoothIdentifier@
bluetoothIdentifierSelector :: Selector '[] (Id NSString)
bluetoothIdentifierSelector = mkSelector "bluetoothIdentifier"

-- | @Selector@ for @iAP2Identifier@
iAP2IdentifierSelector :: Selector '[] (Id NSString)
iAP2IdentifierSelector = mkSelector "iAP2Identifier"

