{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRElectrocardiogramData@.
module ObjC.SensorKit.SRElectrocardiogramData
  ( SRElectrocardiogramData
  , IsSRElectrocardiogramData(..)
  , init_
  , new
  , flags
  , value
  , flagsSelector
  , initSelector
  , newSelector
  , valueSelector

  -- * Enum types
  , SRElectrocardiogramDataFlags(SRElectrocardiogramDataFlags)
  , pattern SRElectrocardiogramDataFlagsNone
  , pattern SRElectrocardiogramDataFlagsSignalInvalid
  , pattern SRElectrocardiogramDataFlagsCrownTouched

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSRElectrocardiogramData srElectrocardiogramData => srElectrocardiogramData -> IO (Id SRElectrocardiogramData)
init_ srElectrocardiogramData =
  sendOwnedMessage srElectrocardiogramData initSelector

-- | @+ new@
new :: IO (Id SRElectrocardiogramData)
new  =
  do
    cls' <- getRequiredClass "SRElectrocardiogramData"
    sendOwnedClassMessage cls' newSelector

-- | @- flags@
flags :: IsSRElectrocardiogramData srElectrocardiogramData => srElectrocardiogramData -> IO SRElectrocardiogramDataFlags
flags srElectrocardiogramData =
  sendMessage srElectrocardiogramData flagsSelector

-- | value
--
-- Value of the ECG AC data in microvolts
--
-- ObjC selector: @- value@
value :: IsSRElectrocardiogramData srElectrocardiogramData => srElectrocardiogramData -> IO (Id NSMeasurement)
value srElectrocardiogramData =
  sendMessage srElectrocardiogramData valueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SRElectrocardiogramData)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SRElectrocardiogramData)
newSelector = mkSelector "new"

-- | @Selector@ for @flags@
flagsSelector :: Selector '[] SRElectrocardiogramDataFlags
flagsSelector = mkSelector "flags"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSMeasurement)
valueSelector = mkSelector "value"

