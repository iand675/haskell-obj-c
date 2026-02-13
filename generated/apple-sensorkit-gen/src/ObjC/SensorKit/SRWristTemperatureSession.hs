{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRWristTemperatureSession@.
module ObjC.SensorKit.SRWristTemperatureSession
  ( SRWristTemperatureSession
  , IsSRWristTemperatureSession(..)
  , init_
  , new
  , startDate
  , duration
  , version
  , temperatures
  , durationSelector
  , initSelector
  , newSelector
  , startDateSelector
  , temperaturesSelector
  , versionSelector


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
init_ :: IsSRWristTemperatureSession srWristTemperatureSession => srWristTemperatureSession -> IO (Id SRWristTemperatureSession)
init_ srWristTemperatureSession =
  sendOwnedMessage srWristTemperatureSession initSelector

-- | @+ new@
new :: IO (Id SRWristTemperatureSession)
new  =
  do
    cls' <- getRequiredClass "SRWristTemperatureSession"
    sendOwnedClassMessage cls' newSelector

-- | startDate
--
-- Indicates when temperatures were collected.
--
-- ObjC selector: @- startDate@
startDate :: IsSRWristTemperatureSession srWristTemperatureSession => srWristTemperatureSession -> IO (Id NSDate)
startDate srWristTemperatureSession =
  sendMessage srWristTemperatureSession startDateSelector

-- | duration
--
-- Precise number of seconds temperatures were collected.
--
-- ObjC selector: @- duration@
duration :: IsSRWristTemperatureSession srWristTemperatureSession => srWristTemperatureSession -> IO CDouble
duration srWristTemperatureSession =
  sendMessage srWristTemperatureSession durationSelector

-- | version
--
-- Algorithm version.
--
-- ObjC selector: @- version@
version :: IsSRWristTemperatureSession srWristTemperatureSession => srWristTemperatureSession -> IO (Id NSString)
version srWristTemperatureSession =
  sendMessage srWristTemperatureSession versionSelector

-- | temperatures
--
-- Enumerator which could be used to view calculated wrist temperatures, along with their reading accuracy.
--
-- ObjC selector: @- temperatures@
temperatures :: IsSRWristTemperatureSession srWristTemperatureSession => srWristTemperatureSession -> IO (Id NSEnumerator)
temperatures srWristTemperatureSession =
  sendMessage srWristTemperatureSession temperaturesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SRWristTemperatureSession)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SRWristTemperatureSession)
newSelector = mkSelector "new"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @version@
versionSelector :: Selector '[] (Id NSString)
versionSelector = mkSelector "version"

-- | @Selector@ for @temperatures@
temperaturesSelector :: Selector '[] (Id NSEnumerator)
temperaturesSelector = mkSelector "temperatures"

