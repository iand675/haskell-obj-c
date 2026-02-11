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
  , jsonRepresentationSelector
  , dictionaryRepresentationSelector


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
jsonRepresentation mxMetric  =
  sendMsg mxMetric (mkSelector "JSONRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | DictionaryRepresentation
--
-- Convenience method to return a NSDictionary representation of this metric.
--
-- Returns: An NSDictionary object containing the dictionary representation
--
-- ObjC selector: @- DictionaryRepresentation@
dictionaryRepresentation :: IsMXMetric mxMetric => mxMetric -> IO (Id NSDictionary)
dictionaryRepresentation mxMetric  =
  sendMsg mxMetric (mkSelector "DictionaryRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @JSONRepresentation@
jsonRepresentationSelector :: Selector
jsonRepresentationSelector = mkSelector "JSONRepresentation"

-- | @Selector@ for @DictionaryRepresentation@
dictionaryRepresentationSelector :: Selector
dictionaryRepresentationSelector = mkSelector "DictionaryRepresentation"

