{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXMetric
--
-- An abstract class that describes a specific metric vended by MetricKit.
--
-- All supported metrics are subclasses of MXMetric.
--
-- Generated bindings for @MXMetric@.
module ObjC.MetricKit.MXMetric
  ( MXMetric
  , IsMXMetric(..)
  , jsonRepresentation
  , dictionaryRepresentation
  , dictionaryRepresentationSelector
  , jsonRepresentationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | JSONRepresentation
--
-- Convenience method to return a JSON representation of this metric.
--
-- Returns: An NSData object containing the JSON representation
--
-- ObjC selector: @- JSONRepresentation@
jsonRepresentation :: IsMXMetric mxMetric => mxMetric -> IO (Id NSData)
jsonRepresentation mxMetric =
  sendMessage mxMetric jsonRepresentationSelector

-- | DictionaryRepresentation
--
-- Convenience method to return a NSDictionary representation of this metric.
--
-- Returns: An NSDictionary object containing the dictionary representation
--
-- ObjC selector: @- DictionaryRepresentation@
dictionaryRepresentation :: IsMXMetric mxMetric => mxMetric -> IO (Id NSDictionary)
dictionaryRepresentation mxMetric =
  sendMessage mxMetric dictionaryRepresentationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @JSONRepresentation@
jsonRepresentationSelector :: Selector '[] (Id NSData)
jsonRepresentationSelector = mkSelector "JSONRepresentation"

-- | @Selector@ for @DictionaryRepresentation@
dictionaryRepresentationSelector :: Selector '[] (Id NSDictionary)
dictionaryRepresentationSelector = mkSelector "DictionaryRepresentation"

