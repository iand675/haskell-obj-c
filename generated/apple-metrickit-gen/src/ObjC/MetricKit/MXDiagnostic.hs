{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXDiagnostic
--
-- An abstract class that describes a diagnostic report vended by MetricKit.
--
-- All supported diagnostics are subclasses of MXDiagnostic.
--
-- Generated bindings for @MXDiagnostic@.
module ObjC.MetricKit.MXDiagnostic
  ( MXDiagnostic
  , IsMXDiagnostic(..)
  , jsonRepresentation
  , dictionaryRepresentation
  , metaData
  , applicationVersion
  , signpostData
  , applicationVersionSelector
  , dictionaryRepresentationSelector
  , jsonRepresentationSelector
  , metaDataSelector
  , signpostDataSelector


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
-- Convenience method to return a JSON representation of this diagnostic.
--
-- Returns: An NSData object containing the JSON representation
--
-- ObjC selector: @- JSONRepresentation@
jsonRepresentation :: IsMXDiagnostic mxDiagnostic => mxDiagnostic -> IO (Id NSData)
jsonRepresentation mxDiagnostic =
  sendMessage mxDiagnostic jsonRepresentationSelector

-- | dictionaryRepresentation
--
-- Convenience method to return a NSDictionary representation of this diagnostic.
--
-- Returns: An NSDictionary object containing the dictionary representation
--
-- ObjC selector: @- dictionaryRepresentation@
dictionaryRepresentation :: IsMXDiagnostic mxDiagnostic => mxDiagnostic -> IO (Id NSDictionary)
dictionaryRepresentation mxDiagnostic =
  sendMessage mxDiagnostic dictionaryRepresentationSelector

-- | @- metaData@
metaData :: IsMXDiagnostic mxDiagnostic => mxDiagnostic -> IO (Id MXMetaData)
metaData mxDiagnostic =
  sendMessage mxDiagnostic metaDataSelector

-- | applicationVersion
--
-- An NSString representation of the application version from which this diagnostic was generated.
--
-- ObjC selector: @- applicationVersion@
applicationVersion :: IsMXDiagnostic mxDiagnostic => mxDiagnostic -> IO (Id NSString)
applicationVersion mxDiagnostic =
  sendMessage mxDiagnostic applicationVersionSelector

-- | signpostData
--
-- An NSArray representing the list of signpost records.
--
-- ObjC selector: @- signpostData@
signpostData :: IsMXDiagnostic mxDiagnostic => mxDiagnostic -> IO (Id NSArray)
signpostData mxDiagnostic =
  sendMessage mxDiagnostic signpostDataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @JSONRepresentation@
jsonRepresentationSelector :: Selector '[] (Id NSData)
jsonRepresentationSelector = mkSelector "JSONRepresentation"

-- | @Selector@ for @dictionaryRepresentation@
dictionaryRepresentationSelector :: Selector '[] (Id NSDictionary)
dictionaryRepresentationSelector = mkSelector "dictionaryRepresentation"

-- | @Selector@ for @metaData@
metaDataSelector :: Selector '[] (Id MXMetaData)
metaDataSelector = mkSelector "metaData"

-- | @Selector@ for @applicationVersion@
applicationVersionSelector :: Selector '[] (Id NSString)
applicationVersionSelector = mkSelector "applicationVersion"

-- | @Selector@ for @signpostData@
signpostDataSelector :: Selector '[] (Id NSArray)
signpostDataSelector = mkSelector "signpostData"

