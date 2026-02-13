{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A representation of a collection of metrics data for an operation.
--
-- Generated bindings for @MTRMetrics@.
module ObjC.Matter.MTRMetrics
  ( MTRMetrics
  , IsMTRMetrics(..)
  , init_
  , new
  , metricDataForKey
  , uniqueIdentifier
  , allKeys
  , allKeysSelector
  , initSelector
  , metricDataForKeySelector
  , newSelector
  , uniqueIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRMetrics mtrMetrics => mtrMetrics -> IO (Id MTRMetrics)
init_ mtrMetrics =
  sendOwnedMessage mtrMetrics initSelector

-- | @+ new@
new :: IO (Id MTRMetrics)
new  =
  do
    cls' <- getRequiredClass "MTRMetrics"
    sendOwnedClassMessage cls' newSelector

-- | Returns metric data corresponding to the metric identified by its key.
--
-- @key@ — Name of the metric
--
-- Returns: An object containing the metric data, nil if key is invalid.
--
-- ObjC selector: @- metricDataForKey:@
metricDataForKey :: (IsMTRMetrics mtrMetrics, IsNSString key) => mtrMetrics -> key -> IO (Id MTRMetricData)
metricDataForKey mtrMetrics key =
  sendMessage mtrMetrics metricDataForKeySelector (toNSString key)

-- | Returns a unique identifier for the object
--
-- ObjC selector: @- uniqueIdentifier@
uniqueIdentifier :: IsMTRMetrics mtrMetrics => mtrMetrics -> IO (Id NSUUID)
uniqueIdentifier mtrMetrics =
  sendMessage mtrMetrics uniqueIdentifierSelector

-- | Returns the names of all the metrics data items collected.
--
-- ObjC selector: @- allKeys@
allKeys :: IsMTRMetrics mtrMetrics => mtrMetrics -> IO (Id NSArray)
allKeys mtrMetrics =
  sendMessage mtrMetrics allKeysSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRMetrics)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRMetrics)
newSelector = mkSelector "new"

-- | @Selector@ for @metricDataForKey:@
metricDataForKeySelector :: Selector '[Id NSString] (Id MTRMetricData)
metricDataForKeySelector = mkSelector "metricDataForKey:"

-- | @Selector@ for @uniqueIdentifier@
uniqueIdentifierSelector :: Selector '[] (Id NSUUID)
uniqueIdentifierSelector = mkSelector "uniqueIdentifier"

-- | @Selector@ for @allKeys@
allKeysSelector :: Selector '[] (Id NSArray)
allKeysSelector = mkSelector "allKeys"

