{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKCorrelationQuery
--
-- A query to find HKCorrelations
--
-- Correlations are HKSamples that contain a set of correlated samples. HKCorrelationQuery                accepts a predicate to filter HKCorrelations and a dictionary of predicates to filter the                correlated samples.
--
-- Generated bindings for @HKCorrelationQuery@.
module ObjC.HealthKit.HKCorrelationQuery
  ( HKCorrelationQuery
  , IsHKCorrelationQuery(..)
  , correlationType
  , samplePredicates
  , correlationTypeSelector
  , samplePredicatesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- correlationType@
correlationType :: IsHKCorrelationQuery hkCorrelationQuery => hkCorrelationQuery -> IO (Id HKCorrelationType)
correlationType hkCorrelationQuery =
  sendMessage hkCorrelationQuery correlationTypeSelector

-- | samplePredicates
--
-- A dictionary of predicates for the HKCorrelation's objects
--
-- samplePredicates maps HKSampleTypes to NSPredicates. The predicate value will apply                to objects of the key type.
--
-- ObjC selector: @- samplePredicates@
samplePredicates :: IsHKCorrelationQuery hkCorrelationQuery => hkCorrelationQuery -> IO (Id NSDictionary)
samplePredicates hkCorrelationQuery =
  sendMessage hkCorrelationQuery samplePredicatesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @correlationType@
correlationTypeSelector :: Selector '[] (Id HKCorrelationType)
correlationTypeSelector = mkSelector "correlationType"

-- | @Selector@ for @samplePredicates@
samplePredicatesSelector :: Selector '[] (Id NSDictionary)
samplePredicatesSelector = mkSelector "samplePredicates"

