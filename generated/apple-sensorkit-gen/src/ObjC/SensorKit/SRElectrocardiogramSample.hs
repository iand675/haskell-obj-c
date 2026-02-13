{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRElectrocardiogramSample@.
module ObjC.SensorKit.SRElectrocardiogramSample
  ( SRElectrocardiogramSample
  , IsSRElectrocardiogramSample(..)
  , init_
  , new
  , date
  , frequency
  , session
  , lead
  , data_
  , dataSelector
  , dateSelector
  , frequencySelector
  , initSelector
  , leadSelector
  , newSelector
  , sessionSelector

  -- * Enum types
  , SRElectrocardiogramLead(SRElectrocardiogramLead)
  , pattern SRElectrocardiogramLeadRightArmMinusLeftArm
  , pattern SRElectrocardiogramLeadLeftArmMinusRightArm

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
init_ :: IsSRElectrocardiogramSample srElectrocardiogramSample => srElectrocardiogramSample -> IO (Id SRElectrocardiogramSample)
init_ srElectrocardiogramSample =
  sendOwnedMessage srElectrocardiogramSample initSelector

-- | @+ new@
new :: IO (Id SRElectrocardiogramSample)
new  =
  do
    cls' <- getRequiredClass "SRElectrocardiogramSample"
    sendOwnedClassMessage cls' newSelector

-- | date
--
-- Date of the start of the batch of ECG data
--
-- ObjC selector: @- date@
date :: IsSRElectrocardiogramSample srElectrocardiogramSample => srElectrocardiogramSample -> IO (Id NSDate)
date srElectrocardiogramSample =
  sendMessage srElectrocardiogramSample dateSelector

-- | frequency
--
-- Frequency in hertz at which the ECG data was recorded
--
-- ObjC selector: @- frequency@
frequency :: IsSRElectrocardiogramSample srElectrocardiogramSample => srElectrocardiogramSample -> IO (Id NSMeasurement)
frequency srElectrocardiogramSample =
  sendMessage srElectrocardiogramSample frequencySelector

-- | session
--
-- The session to which this sample belongs
--
-- ObjC selector: @- session@
session :: IsSRElectrocardiogramSample srElectrocardiogramSample => srElectrocardiogramSample -> IO (Id SRElectrocardiogramSession)
session srElectrocardiogramSample =
  sendMessage srElectrocardiogramSample sessionSelector

-- | @- lead@
lead :: IsSRElectrocardiogramSample srElectrocardiogramSample => srElectrocardiogramSample -> IO SRElectrocardiogramLead
lead srElectrocardiogramSample =
  sendMessage srElectrocardiogramSample leadSelector

-- | @- data@
data_ :: IsSRElectrocardiogramSample srElectrocardiogramSample => srElectrocardiogramSample -> IO (Id NSArray)
data_ srElectrocardiogramSample =
  sendMessage srElectrocardiogramSample dataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SRElectrocardiogramSample)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SRElectrocardiogramSample)
newSelector = mkSelector "new"

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

-- | @Selector@ for @frequency@
frequencySelector :: Selector '[] (Id NSMeasurement)
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @session@
sessionSelector :: Selector '[] (Id SRElectrocardiogramSession)
sessionSelector = mkSelector "session"

-- | @Selector@ for @lead@
leadSelector :: Selector '[] SRElectrocardiogramLead
leadSelector = mkSelector "lead"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSArray)
dataSelector = mkSelector "data"

