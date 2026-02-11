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
  , initSelector
  , newSelector
  , startDateSelector
  , durationSelector
  , versionSelector
  , temperaturesSelector


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

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSRWristTemperatureSession srWristTemperatureSession => srWristTemperatureSession -> IO (Id SRWristTemperatureSession)
init_ srWristTemperatureSession  =
    sendMsg srWristTemperatureSession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SRWristTemperatureSession)
new  =
  do
    cls' <- getRequiredClass "SRWristTemperatureSession"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | startDate
--
-- Indicates when temperatures were collected.
--
-- ObjC selector: @- startDate@
startDate :: IsSRWristTemperatureSession srWristTemperatureSession => srWristTemperatureSession -> IO (Id NSDate)
startDate srWristTemperatureSession  =
    sendMsg srWristTemperatureSession (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | duration
--
-- Precise number of seconds temperatures were collected.
--
-- ObjC selector: @- duration@
duration :: IsSRWristTemperatureSession srWristTemperatureSession => srWristTemperatureSession -> IO CDouble
duration srWristTemperatureSession  =
    sendMsg srWristTemperatureSession (mkSelector "duration") retCDouble []

-- | version
--
-- Algorithm version.
--
-- ObjC selector: @- version@
version :: IsSRWristTemperatureSession srWristTemperatureSession => srWristTemperatureSession -> IO (Id NSString)
version srWristTemperatureSession  =
    sendMsg srWristTemperatureSession (mkSelector "version") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | temperatures
--
-- Enumerator which could be used to view calculated wrist temperatures, along with their reading accuracy.
--
-- ObjC selector: @- temperatures@
temperatures :: IsSRWristTemperatureSession srWristTemperatureSession => srWristTemperatureSession -> IO (Id NSEnumerator)
temperatures srWristTemperatureSession  =
    sendMsg srWristTemperatureSession (mkSelector "temperatures") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

-- | @Selector@ for @temperatures@
temperaturesSelector :: Selector
temperaturesSelector = mkSelector "temperatures"

