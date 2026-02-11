{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKReverseGeocodingRequest@.
module ObjC.MapKit.MKReverseGeocodingRequest
  ( MKReverseGeocodingRequest
  , IsMKReverseGeocodingRequest(..)
  , init_
  , new
  , initWithLocation
  , cancel
  , cancelled
  , loading
  , location
  , preferredLocale
  , setPreferredLocale
  , initSelector
  , newSelector
  , initWithLocationSelector
  , cancelSelector
  , cancelledSelector
  , loadingSelector
  , locationSelector
  , preferredLocaleSelector
  , setPreferredLocaleSelector


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
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMKReverseGeocodingRequest mkReverseGeocodingRequest => mkReverseGeocodingRequest -> IO (Id MKReverseGeocodingRequest)
init_ mkReverseGeocodingRequest  =
  sendMsg mkReverseGeocodingRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MKReverseGeocodingRequest)
new  =
  do
    cls' <- getRequiredClass "MKReverseGeocodingRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithLocation:@
initWithLocation :: (IsMKReverseGeocodingRequest mkReverseGeocodingRequest, IsCLLocation location) => mkReverseGeocodingRequest -> location -> IO (Id MKReverseGeocodingRequest)
initWithLocation mkReverseGeocodingRequest  location =
withObjCPtr location $ \raw_location ->
    sendMsg mkReverseGeocodingRequest (mkSelector "initWithLocation:") (retPtr retVoid) [argPtr (castPtr raw_location :: Ptr ())] >>= ownedObject . castPtr

-- | @- cancel@
cancel :: IsMKReverseGeocodingRequest mkReverseGeocodingRequest => mkReverseGeocodingRequest -> IO ()
cancel mkReverseGeocodingRequest  =
  sendMsg mkReverseGeocodingRequest (mkSelector "cancel") retVoid []

-- | @- cancelled@
cancelled :: IsMKReverseGeocodingRequest mkReverseGeocodingRequest => mkReverseGeocodingRequest -> IO Bool
cancelled mkReverseGeocodingRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkReverseGeocodingRequest (mkSelector "cancelled") retCULong []

-- | @- loading@
loading :: IsMKReverseGeocodingRequest mkReverseGeocodingRequest => mkReverseGeocodingRequest -> IO Bool
loading mkReverseGeocodingRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkReverseGeocodingRequest (mkSelector "loading") retCULong []

-- | @- location@
location :: IsMKReverseGeocodingRequest mkReverseGeocodingRequest => mkReverseGeocodingRequest -> IO (Id CLLocation)
location mkReverseGeocodingRequest  =
  sendMsg mkReverseGeocodingRequest (mkSelector "location") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- preferredLocale@
preferredLocale :: IsMKReverseGeocodingRequest mkReverseGeocodingRequest => mkReverseGeocodingRequest -> IO (Id NSLocale)
preferredLocale mkReverseGeocodingRequest  =
  sendMsg mkReverseGeocodingRequest (mkSelector "preferredLocale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreferredLocale:@
setPreferredLocale :: (IsMKReverseGeocodingRequest mkReverseGeocodingRequest, IsNSLocale value) => mkReverseGeocodingRequest -> value -> IO ()
setPreferredLocale mkReverseGeocodingRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkReverseGeocodingRequest (mkSelector "setPreferredLocale:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithLocation:@
initWithLocationSelector :: Selector
initWithLocationSelector = mkSelector "initWithLocation:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @cancelled@
cancelledSelector :: Selector
cancelledSelector = mkSelector "cancelled"

-- | @Selector@ for @loading@
loadingSelector :: Selector
loadingSelector = mkSelector "loading"

-- | @Selector@ for @location@
locationSelector :: Selector
locationSelector = mkSelector "location"

-- | @Selector@ for @preferredLocale@
preferredLocaleSelector :: Selector
preferredLocaleSelector = mkSelector "preferredLocale"

-- | @Selector@ for @setPreferredLocale:@
setPreferredLocaleSelector :: Selector
setPreferredLocaleSelector = mkSelector "setPreferredLocale:"

