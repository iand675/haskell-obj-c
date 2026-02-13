{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKGeoJSONFeature@.
module ObjC.MapKit.MKGeoJSONFeature
  ( MKGeoJSONFeature
  , IsMKGeoJSONFeature(..)
  , identifier
  , properties
  , geometry
  , geometrySelector
  , identifierSelector
  , propertiesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- identifier@
identifier :: IsMKGeoJSONFeature mkGeoJSONFeature => mkGeoJSONFeature -> IO (Id NSString)
identifier mkGeoJSONFeature =
  sendMessage mkGeoJSONFeature identifierSelector

-- | @- properties@
properties :: IsMKGeoJSONFeature mkGeoJSONFeature => mkGeoJSONFeature -> IO (Id NSData)
properties mkGeoJSONFeature =
  sendMessage mkGeoJSONFeature propertiesSelector

-- | @- geometry@
geometry :: IsMKGeoJSONFeature mkGeoJSONFeature => mkGeoJSONFeature -> IO (Id NSArray)
geometry mkGeoJSONFeature =
  sendMessage mkGeoJSONFeature geometrySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @properties@
propertiesSelector :: Selector '[] (Id NSData)
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @geometry@
geometrySelector :: Selector '[] (Id NSArray)
geometrySelector = mkSelector "geometry"

