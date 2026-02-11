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
  , metersPerSecondSelector
  , kilometersPerHourSelector
  , milesPerHourSelector
  , knotsSelector


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

import ObjC.Foundation.Internal.Classes

-- | @+ metersPerSecond@
metersPerSecond :: IO (Id NSUnitSpeed)
metersPerSecond  =
  do
    cls' <- getRequiredClass "NSUnitSpeed"
    sendClassMsg cls' (mkSelector "metersPerSecond") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ kilometersPerHour@
kilometersPerHour :: IO (Id NSUnitSpeed)
kilometersPerHour  =
  do
    cls' <- getRequiredClass "NSUnitSpeed"
    sendClassMsg cls' (mkSelector "kilometersPerHour") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ milesPerHour@
milesPerHour :: IO (Id NSUnitSpeed)
milesPerHour  =
  do
    cls' <- getRequiredClass "NSUnitSpeed"
    sendClassMsg cls' (mkSelector "milesPerHour") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ knots@
knots :: IO (Id NSUnitSpeed)
knots  =
  do
    cls' <- getRequiredClass "NSUnitSpeed"
    sendClassMsg cls' (mkSelector "knots") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @metersPerSecond@
metersPerSecondSelector :: Selector
metersPerSecondSelector = mkSelector "metersPerSecond"

-- | @Selector@ for @kilometersPerHour@
kilometersPerHourSelector :: Selector
kilometersPerHourSelector = mkSelector "kilometersPerHour"

-- | @Selector@ for @milesPerHour@
milesPerHourSelector :: Selector
milesPerHourSelector = mkSelector "milesPerHour"

-- | @Selector@ for @knots@
knotsSelector :: Selector
knotsSelector = mkSelector "knots"

