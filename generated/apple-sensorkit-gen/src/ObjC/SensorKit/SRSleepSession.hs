{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRSleepSession@.
module ObjC.SensorKit.SRSleepSession
  ( SRSleepSession
  , IsSRSleepSession(..)
  , init_
  , new
  , startDate
  , duration
  , identifier
  , durationSelector
  , identifierSelector
  , initSelector
  , newSelector
  , startDateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSRSleepSession srSleepSession => srSleepSession -> IO (Id SRSleepSession)
init_ srSleepSession =
  sendOwnedMessage srSleepSession initSelector

-- | @+ new@
new :: IO (Id SRSleepSession)
new  =
  do
    cls' <- getRequiredClass "SRSleepSession"
    sendOwnedClassMessage cls' newSelector

-- | startDate
--
-- Start date of sleep session
--
-- ObjC selector: @- startDate@
startDate :: IsSRSleepSession srSleepSession => srSleepSession -> IO (Id NSDate)
startDate srSleepSession =
  sendMessage srSleepSession startDateSelector

-- | duration
--
-- Sleep session duration
--
-- Equal to 0 if endReason is SRSleepSessionEndReasonNoEndEvent
--
-- ObjC selector: @- duration@
duration :: IsSRSleepSession srSleepSession => srSleepSession -> IO CDouble
duration srSleepSession =
  sendMessage srSleepSession durationSelector

-- | identifier
--
-- Sleep session unique identifier
--
-- ObjC selector: @- identifier@
identifier :: IsSRSleepSession srSleepSession => srSleepSession -> IO (Id NSString)
identifier srSleepSession =
  sendMessage srSleepSession identifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SRSleepSession)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SRSleepSession)
newSelector = mkSelector "new"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

