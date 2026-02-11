{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MIDICIDeviceInfo@.
module ObjC.CoreMIDI.MIDICIDeviceInfo
  ( MIDICIDeviceInfo
  , IsMIDICIDeviceInfo(..)
  , init_
  , initWithDestination_manufacturer_family_model_revision
  , manufacturerID
  , family_
  , modelNumber
  , revisionLevel
  , midiDestination
  , initSelector
  , initWithDestination_manufacturer_family_model_revisionSelector
  , manufacturerIDSelector
  , familySelector
  , modelNumberSelector
  , revisionLevelSelector
  , midiDestinationSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMIDICIDeviceInfo midiciDeviceInfo => midiciDeviceInfo -> IO (Id MIDICIDeviceInfo)
init_ midiciDeviceInfo  =
  sendMsg midiciDeviceInfo (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDestination:manufacturer:family:model:revision:@
initWithDestination_manufacturer_family_model_revision :: (IsMIDICIDeviceInfo midiciDeviceInfo, IsNSData manufacturer, IsNSData family_, IsNSData modelNumber, IsNSData revisionLevel) => midiciDeviceInfo -> CUInt -> manufacturer -> family_ -> modelNumber -> revisionLevel -> IO (Id MIDICIDeviceInfo)
initWithDestination_manufacturer_family_model_revision midiciDeviceInfo  midiDestination manufacturer family_ modelNumber revisionLevel =
withObjCPtr manufacturer $ \raw_manufacturer ->
  withObjCPtr family_ $ \raw_family_ ->
    withObjCPtr modelNumber $ \raw_modelNumber ->
      withObjCPtr revisionLevel $ \raw_revisionLevel ->
          sendMsg midiciDeviceInfo (mkSelector "initWithDestination:manufacturer:family:model:revision:") (retPtr retVoid) [argCUInt (fromIntegral midiDestination), argPtr (castPtr raw_manufacturer :: Ptr ()), argPtr (castPtr raw_family_ :: Ptr ()), argPtr (castPtr raw_modelNumber :: Ptr ()), argPtr (castPtr raw_revisionLevel :: Ptr ())] >>= ownedObject . castPtr

-- | @- manufacturerID@
manufacturerID :: IsMIDICIDeviceInfo midiciDeviceInfo => midiciDeviceInfo -> IO (Id NSData)
manufacturerID midiciDeviceInfo  =
  sendMsg midiciDeviceInfo (mkSelector "manufacturerID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- family@
family_ :: IsMIDICIDeviceInfo midiciDeviceInfo => midiciDeviceInfo -> IO (Id NSData)
family_ midiciDeviceInfo  =
  sendMsg midiciDeviceInfo (mkSelector "family") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- modelNumber@
modelNumber :: IsMIDICIDeviceInfo midiciDeviceInfo => midiciDeviceInfo -> IO (Id NSData)
modelNumber midiciDeviceInfo  =
  sendMsg midiciDeviceInfo (mkSelector "modelNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- revisionLevel@
revisionLevel :: IsMIDICIDeviceInfo midiciDeviceInfo => midiciDeviceInfo -> IO (Id NSData)
revisionLevel midiciDeviceInfo  =
  sendMsg midiciDeviceInfo (mkSelector "revisionLevel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- midiDestination@
midiDestination :: IsMIDICIDeviceInfo midiciDeviceInfo => midiciDeviceInfo -> IO CUInt
midiDestination midiciDeviceInfo  =
  sendMsg midiciDeviceInfo (mkSelector "midiDestination") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDestination:manufacturer:family:model:revision:@
initWithDestination_manufacturer_family_model_revisionSelector :: Selector
initWithDestination_manufacturer_family_model_revisionSelector = mkSelector "initWithDestination:manufacturer:family:model:revision:"

-- | @Selector@ for @manufacturerID@
manufacturerIDSelector :: Selector
manufacturerIDSelector = mkSelector "manufacturerID"

-- | @Selector@ for @family@
familySelector :: Selector
familySelector = mkSelector "family"

-- | @Selector@ for @modelNumber@
modelNumberSelector :: Selector
modelNumberSelector = mkSelector "modelNumber"

-- | @Selector@ for @revisionLevel@
revisionLevelSelector :: Selector
revisionLevelSelector = mkSelector "revisionLevel"

-- | @Selector@ for @midiDestination@
midiDestinationSelector :: Selector
midiDestinationSelector = mkSelector "midiDestination"

