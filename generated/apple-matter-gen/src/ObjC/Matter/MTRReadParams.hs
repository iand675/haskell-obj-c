{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTRReadParams    This is used to control the behavior of attribute/event reads and subscribes.    If not provided (i.e. nil passed for the MTRReadParams argument), will be    treated as if a default-initialized object was passed in.
--
-- Generated bindings for @MTRReadParams@.
module ObjC.Matter.MTRReadParams
  ( MTRReadParams
  , IsMTRReadParams(..)
  , filterByFabric
  , setFilterByFabric
  , minEventNumber
  , setMinEventNumber
  , assumeUnknownAttributesReportable
  , setAssumeUnknownAttributesReportable
  , fabricFiltered
  , setFabricFiltered
  , assumeUnknownAttributesReportableSelector
  , fabricFilteredSelector
  , filterByFabricSelector
  , minEventNumberSelector
  , setAssumeUnknownAttributesReportableSelector
  , setFabricFilteredSelector
  , setFilterByFabricSelector
  , setMinEventNumberSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Whether the read/subscribe is fabric-filtered. The default is YES.
--
-- If YES, the read/subscribe is fabric-filtered and will only see things associated with the fabric of the reader/subscriber.
--
-- If NO, the read/subscribe is not fabric-filtered and will see all non-fabric-sensitive data for the given attribute path.
--
-- ObjC selector: @- filterByFabric@
filterByFabric :: IsMTRReadParams mtrReadParams => mtrReadParams -> IO Bool
filterByFabric mtrReadParams =
  sendMessage mtrReadParams filterByFabricSelector

-- | Whether the read/subscribe is fabric-filtered. The default is YES.
--
-- If YES, the read/subscribe is fabric-filtered and will only see things associated with the fabric of the reader/subscriber.
--
-- If NO, the read/subscribe is not fabric-filtered and will see all non-fabric-sensitive data for the given attribute path.
--
-- ObjC selector: @- setFilterByFabric:@
setFilterByFabric :: IsMTRReadParams mtrReadParams => mtrReadParams -> Bool -> IO ()
setFilterByFabric mtrReadParams value =
  sendMessage mtrReadParams setFilterByFabricSelector value

-- | Sets a filter for which events will be reported in the read/subscribe interaction.
--
-- If nil (the default value), all of the queued events will be reported from lowest to highest event number.
--
-- If not nil, queued events with an event number smaller than minEventNumber will not be reported.
--
-- ObjC selector: @- minEventNumber@
minEventNumber :: IsMTRReadParams mtrReadParams => mtrReadParams -> IO (Id NSNumber)
minEventNumber mtrReadParams =
  sendMessage mtrReadParams minEventNumberSelector

-- | Sets a filter for which events will be reported in the read/subscribe interaction.
--
-- If nil (the default value), all of the queued events will be reported from lowest to highest event number.
--
-- If not nil, queued events with an event number smaller than minEventNumber will not be reported.
--
-- ObjC selector: @- setMinEventNumber:@
setMinEventNumber :: (IsMTRReadParams mtrReadParams, IsNSNumber value) => mtrReadParams -> value -> IO ()
setMinEventNumber mtrReadParams value =
  sendMessage mtrReadParams setMinEventNumberSelector (toNSNumber value)

-- | Controls whether attributes without known schema (e.g. vendor-specific attributes) should be assumed to be reportable normally via subscriptions. The default is YES.
--
-- This setting is only relevant to some consumers of MTRReadParams.  One of those consumers is readAttributeWithEndpointID:clusterID:attributeID:params: on MTRDevice.
--
-- ObjC selector: @- assumeUnknownAttributesReportable@
assumeUnknownAttributesReportable :: IsMTRReadParams mtrReadParams => mtrReadParams -> IO Bool
assumeUnknownAttributesReportable mtrReadParams =
  sendMessage mtrReadParams assumeUnknownAttributesReportableSelector

-- | Controls whether attributes without known schema (e.g. vendor-specific attributes) should be assumed to be reportable normally via subscriptions. The default is YES.
--
-- This setting is only relevant to some consumers of MTRReadParams.  One of those consumers is readAttributeWithEndpointID:clusterID:attributeID:params: on MTRDevice.
--
-- ObjC selector: @- setAssumeUnknownAttributesReportable:@
setAssumeUnknownAttributesReportable :: IsMTRReadParams mtrReadParams => mtrReadParams -> Bool -> IO ()
setAssumeUnknownAttributesReportable mtrReadParams value =
  sendMessage mtrReadParams setAssumeUnknownAttributesReportableSelector value

-- | @- fabricFiltered@
fabricFiltered :: IsMTRReadParams mtrReadParams => mtrReadParams -> IO (Id NSNumber)
fabricFiltered mtrReadParams =
  sendMessage mtrReadParams fabricFilteredSelector

-- | @- setFabricFiltered:@
setFabricFiltered :: (IsMTRReadParams mtrReadParams, IsNSNumber value) => mtrReadParams -> value -> IO ()
setFabricFiltered mtrReadParams value =
  sendMessage mtrReadParams setFabricFilteredSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @filterByFabric@
filterByFabricSelector :: Selector '[] Bool
filterByFabricSelector = mkSelector "filterByFabric"

-- | @Selector@ for @setFilterByFabric:@
setFilterByFabricSelector :: Selector '[Bool] ()
setFilterByFabricSelector = mkSelector "setFilterByFabric:"

-- | @Selector@ for @minEventNumber@
minEventNumberSelector :: Selector '[] (Id NSNumber)
minEventNumberSelector = mkSelector "minEventNumber"

-- | @Selector@ for @setMinEventNumber:@
setMinEventNumberSelector :: Selector '[Id NSNumber] ()
setMinEventNumberSelector = mkSelector "setMinEventNumber:"

-- | @Selector@ for @assumeUnknownAttributesReportable@
assumeUnknownAttributesReportableSelector :: Selector '[] Bool
assumeUnknownAttributesReportableSelector = mkSelector "assumeUnknownAttributesReportable"

-- | @Selector@ for @setAssumeUnknownAttributesReportable:@
setAssumeUnknownAttributesReportableSelector :: Selector '[Bool] ()
setAssumeUnknownAttributesReportableSelector = mkSelector "setAssumeUnknownAttributesReportable:"

-- | @Selector@ for @fabricFiltered@
fabricFilteredSelector :: Selector '[] (Id NSNumber)
fabricFilteredSelector = mkSelector "fabricFiltered"

-- | @Selector@ for @setFabricFiltered:@
setFabricFilteredSelector :: Selector '[Id NSNumber] ()
setFabricFilteredSelector = mkSelector "setFabricFiltered:"

