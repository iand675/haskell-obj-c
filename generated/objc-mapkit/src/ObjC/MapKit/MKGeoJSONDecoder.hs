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

-- | @- geoJSONObjectsWithData:error:@
geoJSONObjectsWithData_error :: (IsMKGeoJSONDecoder mkGeoJSONDecoder, IsNSData data_, IsNSError errorPtr) => mkGeoJSONDecoder -> data_ -> errorPtr -> IO (Id NSArray)
geoJSONObjectsWithData_error mkGeoJSONDecoder  data_ errorPtr =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr errorPtr $ \raw_errorPtr ->
      sendMsg mkGeoJSONDecoder (mkSelector "geoJSONObjectsWithData:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_errorPtr :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @geoJSONObjectsWithData:error:@
geoJSONObjectsWithData_errorSelector :: Selector
geoJSONObjectsWithData_errorSelector = mkSelector "geoJSONObjectsWithData:error:"

