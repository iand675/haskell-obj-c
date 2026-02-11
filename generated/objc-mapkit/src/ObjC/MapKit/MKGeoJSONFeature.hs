{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKGeoJSONFeature@.
module ObjC.MapKit.MKGeoJSONFeature
  ( MKGeoJSONFeature
  , IsMKGeoJSONFeature(..)
  , identifier
  , properties
  , identifierSelector
  , propertiesSelector


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

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- identifier@
identifier :: IsMKGeoJSONFeature mkGeoJSONFeature => mkGeoJSONFeature -> IO (Id NSString)
identifier mkGeoJSONFeature  =
  sendMsg mkGeoJSONFeature (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- properties@
properties :: IsMKGeoJSONFeature mkGeoJSONFeature => mkGeoJSONFeature -> IO (Id NSData)
properties mkGeoJSONFeature  =
  sendMsg mkGeoJSONFeature (mkSelector "properties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @properties@
propertiesSelector :: Selector
propertiesSelector = mkSelector "properties"

