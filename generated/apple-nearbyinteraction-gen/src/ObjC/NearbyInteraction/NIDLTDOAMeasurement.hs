{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a single measurement relative to a DL-TDOA anchor.
--
-- Generated bindings for @NIDLTDOAMeasurement@.
module ObjC.NearbyInteraction.NIDLTDOAMeasurement
  ( NIDLTDOAMeasurement
  , IsNIDLTDOAMeasurement(..)
  , init_
  , new
  , address
  , measurementType
  , transmitTime
  , receiveTime
  , signalStrength
  , carrierFrequencyOffset
  , coordinatesType
  , addressSelector
  , carrierFrequencyOffsetSelector
  , coordinatesTypeSelector
  , initSelector
  , measurementTypeSelector
  , newSelector
  , receiveTimeSelector
  , signalStrengthSelector
  , transmitTimeSelector

  -- * Enum types
  , NIDLTDOACoordinatesType(NIDLTDOACoordinatesType)
  , pattern NIDLTDOACoordinatesTypeGeodetic
  , pattern NIDLTDOACoordinatesTypeRelative
  , NIDLTDOAMeasurementType(NIDLTDOAMeasurementType)
  , pattern NIDLTDOAMeasurementTypePoll
  , pattern NIDLTDOAMeasurementTypeResponse
  , pattern NIDLTDOAMeasurementTypeFinal

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NearbyInteraction.Internal.Classes
import ObjC.NearbyInteraction.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Unavailable
--
-- ObjC selector: @- init@
init_ :: IsNIDLTDOAMeasurement nidltdoaMeasurement => nidltdoaMeasurement -> IO (Id NIDLTDOAMeasurement)
init_ nidltdoaMeasurement =
  sendOwnedMessage nidltdoaMeasurement initSelector

-- | @+ new@
new :: IO (Id NIDLTDOAMeasurement)
new  =
  do
    cls' <- getRequiredClass "NIDLTDOAMeasurement"
    sendOwnedClassMessage cls' newSelector

-- | Indicates the address of anchor of this measurement.
--
-- ObjC selector: @- address@
address :: IsNIDLTDOAMeasurement nidltdoaMeasurement => nidltdoaMeasurement -> IO CULong
address nidltdoaMeasurement =
  sendMessage nidltdoaMeasurement addressSelector

-- | Indicates the type of this measurement.
--
-- ObjC selector: @- measurementType@
measurementType :: IsNIDLTDOAMeasurement nidltdoaMeasurement => nidltdoaMeasurement -> IO NIDLTDOAMeasurementType
measurementType nidltdoaMeasurement =
  sendMessage nidltdoaMeasurement measurementTypeSelector

-- | Indicates the transmission timestamp (in seconds).
--
-- ObjC selector: @- transmitTime@
transmitTime :: IsNIDLTDOAMeasurement nidltdoaMeasurement => nidltdoaMeasurement -> IO CDouble
transmitTime nidltdoaMeasurement =
  sendMessage nidltdoaMeasurement transmitTimeSelector

-- | Indicates the reception timestamp (in seconds).
--
-- ObjC selector: @- receiveTime@
receiveTime :: IsNIDLTDOAMeasurement nidltdoaMeasurement => nidltdoaMeasurement -> IO CDouble
receiveTime nidltdoaMeasurement =
  sendMessage nidltdoaMeasurement receiveTimeSelector

-- | Indicates the signal strength in dBm.
--
-- ObjC selector: @- signalStrength@
signalStrength :: IsNIDLTDOAMeasurement nidltdoaMeasurement => nidltdoaMeasurement -> IO CDouble
signalStrength nidltdoaMeasurement =
  sendMessage nidltdoaMeasurement signalStrengthSelector

-- | Indicates the estimated carrier frequency offset (dimensionless).
--
-- ObjC selector: @- carrierFrequencyOffset@
carrierFrequencyOffset :: IsNIDLTDOAMeasurement nidltdoaMeasurement => nidltdoaMeasurement -> IO CDouble
carrierFrequencyOffset nidltdoaMeasurement =
  sendMessage nidltdoaMeasurement carrierFrequencyOffsetSelector

-- | Inidicates the type of coordinates of this anchor.
--
-- ObjC selector: @- coordinatesType@
coordinatesType :: IsNIDLTDOAMeasurement nidltdoaMeasurement => nidltdoaMeasurement -> IO NIDLTDOACoordinatesType
coordinatesType nidltdoaMeasurement =
  sendMessage nidltdoaMeasurement coordinatesTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NIDLTDOAMeasurement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NIDLTDOAMeasurement)
newSelector = mkSelector "new"

-- | @Selector@ for @address@
addressSelector :: Selector '[] CULong
addressSelector = mkSelector "address"

-- | @Selector@ for @measurementType@
measurementTypeSelector :: Selector '[] NIDLTDOAMeasurementType
measurementTypeSelector = mkSelector "measurementType"

-- | @Selector@ for @transmitTime@
transmitTimeSelector :: Selector '[] CDouble
transmitTimeSelector = mkSelector "transmitTime"

-- | @Selector@ for @receiveTime@
receiveTimeSelector :: Selector '[] CDouble
receiveTimeSelector = mkSelector "receiveTime"

-- | @Selector@ for @signalStrength@
signalStrengthSelector :: Selector '[] CDouble
signalStrengthSelector = mkSelector "signalStrength"

-- | @Selector@ for @carrierFrequencyOffset@
carrierFrequencyOffsetSelector :: Selector '[] CDouble
carrierFrequencyOffsetSelector = mkSelector "carrierFrequencyOffset"

-- | @Selector@ for @coordinatesType@
coordinatesTypeSelector :: Selector '[] NIDLTDOACoordinatesType
coordinatesTypeSelector = mkSelector "coordinatesType"

