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
  , initSelector
  , initWithBluetoothIdentifier_iAP2IdentifierSelector
  , bluetoothIdentifierSelector
  , iAP2IdentifierSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINCarHeadUnit inCarHeadUnit => inCarHeadUnit -> IO (Id INCarHeadUnit)
init_ inCarHeadUnit  =
  sendMsg inCarHeadUnit (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithBluetoothIdentifier:iAP2Identifier:@
initWithBluetoothIdentifier_iAP2Identifier :: (IsINCarHeadUnit inCarHeadUnit, IsNSString bluetoothIdentifier, IsNSString iAP2Identifier) => inCarHeadUnit -> bluetoothIdentifier -> iAP2Identifier -> IO (Id INCarHeadUnit)
initWithBluetoothIdentifier_iAP2Identifier inCarHeadUnit  bluetoothIdentifier iAP2Identifier =
withObjCPtr bluetoothIdentifier $ \raw_bluetoothIdentifier ->
  withObjCPtr iAP2Identifier $ \raw_iAP2Identifier ->
      sendMsg inCarHeadUnit (mkSelector "initWithBluetoothIdentifier:iAP2Identifier:") (retPtr retVoid) [argPtr (castPtr raw_bluetoothIdentifier :: Ptr ()), argPtr (castPtr raw_iAP2Identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- bluetoothIdentifier@
bluetoothIdentifier :: IsINCarHeadUnit inCarHeadUnit => inCarHeadUnit -> IO (Id NSString)
bluetoothIdentifier inCarHeadUnit  =
  sendMsg inCarHeadUnit (mkSelector "bluetoothIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- iAP2Identifier@
iAP2Identifier :: IsINCarHeadUnit inCarHeadUnit => inCarHeadUnit -> IO (Id NSString)
iAP2Identifier inCarHeadUnit  =
  sendMsg inCarHeadUnit (mkSelector "iAP2Identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithBluetoothIdentifier:iAP2Identifier:@
initWithBluetoothIdentifier_iAP2IdentifierSelector :: Selector
initWithBluetoothIdentifier_iAP2IdentifierSelector = mkSelector "initWithBluetoothIdentifier:iAP2Identifier:"

-- | @Selector@ for @bluetoothIdentifier@
bluetoothIdentifierSelector :: Selector
bluetoothIdentifierSelector = mkSelector "bluetoothIdentifier"

-- | @Selector@ for @iAP2Identifier@
iAP2IdentifierSelector :: Selector
iAP2IdentifierSelector = mkSelector "iAP2Identifier"

