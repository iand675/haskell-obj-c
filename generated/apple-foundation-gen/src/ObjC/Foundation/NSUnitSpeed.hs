{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitSpeed@.
module ObjC.Foundation.NSUnitSpeed
  ( NSUnitSpeed
  , IsNSUnitSpeed(..)
  , metersPerSecond
  , kilometersPerHour
  , milesPerHour
  , knots
  , kilometersPerHourSelector
  , knotsSelector
  , metersPerSecondSelector
  , milesPerHourSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ metersPerSecond@
metersPerSecond :: IO (Id NSUnitSpeed)
metersPerSecond  =
  do
    cls' <- getRequiredClass "NSUnitSpeed"
    sendClassMessage cls' metersPerSecondSelector

-- | @+ kilometersPerHour@
kilometersPerHour :: IO (Id NSUnitSpeed)
kilometersPerHour  =
  do
    cls' <- getRequiredClass "NSUnitSpeed"
    sendClassMessage cls' kilometersPerHourSelector

-- | @+ milesPerHour@
milesPerHour :: IO (Id NSUnitSpeed)
milesPerHour  =
  do
    cls' <- getRequiredClass "NSUnitSpeed"
    sendClassMessage cls' milesPerHourSelector

-- | @+ knots@
knots :: IO (Id NSUnitSpeed)
knots  =
  do
    cls' <- getRequiredClass "NSUnitSpeed"
    sendClassMessage cls' knotsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @metersPerSecond@
metersPerSecondSelector :: Selector '[] (Id NSUnitSpeed)
metersPerSecondSelector = mkSelector "metersPerSecond"

-- | @Selector@ for @kilometersPerHour@
kilometersPerHourSelector :: Selector '[] (Id NSUnitSpeed)
kilometersPerHourSelector = mkSelector "kilometersPerHour"

-- | @Selector@ for @milesPerHour@
milesPerHourSelector :: Selector '[] (Id NSUnitSpeed)
milesPerHourSelector = mkSelector "milesPerHour"

-- | @Selector@ for @knots@
knotsSelector :: Selector '[] (Id NSUnitSpeed)
knotsSelector = mkSelector "knots"

