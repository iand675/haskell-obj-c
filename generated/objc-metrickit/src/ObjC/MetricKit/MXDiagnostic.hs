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
  , jsonRepresentationSelector
  , dictionaryRepresentationSelector
  , metaDataSelector
  , applicationVersionSelector
  , signpostDataSelector


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
-- Convenience method to return a JSON representation of this diagnostic.
--
-- Returns: An NSData object containing the JSON representation
--
-- ObjC selector: @- JSONRepresentation@
jsonRepresentation :: IsMXDiagnostic mxDiagnostic => mxDiagnostic -> IO (Id NSData)
jsonRepresentation mxDiagnostic  =
  sendMsg mxDiagnostic (mkSelector "JSONRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | dictionaryRepresentation
--
-- Convenience method to return a NSDictionary representation of this diagnostic.
--
-- Returns: An NSDictionary object containing the dictionary representation
--
-- ObjC selector: @- dictionaryRepresentation@
dictionaryRepresentation :: IsMXDiagnostic mxDiagnostic => mxDiagnostic -> IO (Id NSDictionary)
dictionaryRepresentation mxDiagnostic  =
  sendMsg mxDiagnostic (mkSelector "dictionaryRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- metaData@
metaData :: IsMXDiagnostic mxDiagnostic => mxDiagnostic -> IO (Id MXMetaData)
metaData mxDiagnostic  =
  sendMsg mxDiagnostic (mkSelector "metaData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | applicationVersion
--
-- An NSString representation of the application version from which this diagnostic was generated.
--
-- ObjC selector: @- applicationVersion@
applicationVersion :: IsMXDiagnostic mxDiagnostic => mxDiagnostic -> IO (Id NSString)
applicationVersion mxDiagnostic  =
  sendMsg mxDiagnostic (mkSelector "applicationVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | signpostData
--
-- An NSArray representing the list of signpost records.
--
-- ObjC selector: @- signpostData@
signpostData :: IsMXDiagnostic mxDiagnostic => mxDiagnostic -> IO (Id NSArray)
signpostData mxDiagnostic  =
  sendMsg mxDiagnostic (mkSelector "signpostData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @JSONRepresentation@
jsonRepresentationSelector :: Selector
jsonRepresentationSelector = mkSelector "JSONRepresentation"

-- | @Selector@ for @dictionaryRepresentation@
dictionaryRepresentationSelector :: Selector
dictionaryRepresentationSelector = mkSelector "dictionaryRepresentation"

-- | @Selector@ for @metaData@
metaDataSelector :: Selector
metaDataSelector = mkSelector "metaData"

-- | @Selector@ for @applicationVersion@
applicationVersionSelector :: Selector
applicationVersionSelector = mkSelector "applicationVersion"

-- | @Selector@ for @signpostData@
signpostDataSelector :: Selector
signpostDataSelector = mkSelector "signpostData"

