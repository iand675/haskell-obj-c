{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKGeoJSONDecoder@.
module ObjC.MapKit.MKGeoJSONDecoder
  ( MKGeoJSONDecoder
  , IsMKGeoJSONDecoder(..)
  , geoJSONObjectsWithData_error
  , geoJSONObjectsWithData_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- geoJSONObjectsWithData:error:@
geoJSONObjectsWithData_error :: (IsMKGeoJSONDecoder mkGeoJSONDecoder, IsNSData data_, IsNSError errorPtr) => mkGeoJSONDecoder -> data_ -> errorPtr -> IO (Id NSArray)
geoJSONObjectsWithData_error mkGeoJSONDecoder data_ errorPtr =
  sendMessage mkGeoJSONDecoder geoJSONObjectsWithData_errorSelector (toNSData data_) (toNSError errorPtr)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @geoJSONObjectsWithData:error:@
geoJSONObjectsWithData_errorSelector :: Selector '[Id NSData, Id NSError] (Id NSArray)
geoJSONObjectsWithData_errorSelector = mkSelector "geoJSONObjectsWithData:error:"

