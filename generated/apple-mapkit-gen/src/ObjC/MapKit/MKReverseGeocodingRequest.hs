{-# LANGUAGE DataKinds #-}
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
  , preferredLocale
  , setPreferredLocale
  , cancelSelector
  , cancelledSelector
  , initSelector
  , initWithLocationSelector
  , loadingSelector
  , newSelector
  , preferredLocaleSelector
  , setPreferredLocaleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMKReverseGeocodingRequest mkReverseGeocodingRequest => mkReverseGeocodingRequest -> IO (Id MKReverseGeocodingRequest)
init_ mkReverseGeocodingRequest =
  sendOwnedMessage mkReverseGeocodingRequest initSelector

-- | @+ new@
new :: IO (Id MKReverseGeocodingRequest)
new  =
  do
    cls' <- getRequiredClass "MKReverseGeocodingRequest"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithLocation:@
initWithLocation :: IsMKReverseGeocodingRequest mkReverseGeocodingRequest => mkReverseGeocodingRequest -> RawId -> IO (Id MKReverseGeocodingRequest)
initWithLocation mkReverseGeocodingRequest location =
  sendOwnedMessage mkReverseGeocodingRequest initWithLocationSelector location

-- | @- cancel@
cancel :: IsMKReverseGeocodingRequest mkReverseGeocodingRequest => mkReverseGeocodingRequest -> IO ()
cancel mkReverseGeocodingRequest =
  sendMessage mkReverseGeocodingRequest cancelSelector

-- | @- cancelled@
cancelled :: IsMKReverseGeocodingRequest mkReverseGeocodingRequest => mkReverseGeocodingRequest -> IO Bool
cancelled mkReverseGeocodingRequest =
  sendMessage mkReverseGeocodingRequest cancelledSelector

-- | @- loading@
loading :: IsMKReverseGeocodingRequest mkReverseGeocodingRequest => mkReverseGeocodingRequest -> IO Bool
loading mkReverseGeocodingRequest =
  sendMessage mkReverseGeocodingRequest loadingSelector

-- | @- preferredLocale@
preferredLocale :: IsMKReverseGeocodingRequest mkReverseGeocodingRequest => mkReverseGeocodingRequest -> IO (Id NSLocale)
preferredLocale mkReverseGeocodingRequest =
  sendMessage mkReverseGeocodingRequest preferredLocaleSelector

-- | @- setPreferredLocale:@
setPreferredLocale :: (IsMKReverseGeocodingRequest mkReverseGeocodingRequest, IsNSLocale value) => mkReverseGeocodingRequest -> value -> IO ()
setPreferredLocale mkReverseGeocodingRequest value =
  sendMessage mkReverseGeocodingRequest setPreferredLocaleSelector (toNSLocale value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MKReverseGeocodingRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MKReverseGeocodingRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithLocation:@
initWithLocationSelector :: Selector '[RawId] (Id MKReverseGeocodingRequest)
initWithLocationSelector = mkSelector "initWithLocation:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @cancelled@
cancelledSelector :: Selector '[] Bool
cancelledSelector = mkSelector "cancelled"

-- | @Selector@ for @loading@
loadingSelector :: Selector '[] Bool
loadingSelector = mkSelector "loading"

-- | @Selector@ for @preferredLocale@
preferredLocaleSelector :: Selector '[] (Id NSLocale)
preferredLocaleSelector = mkSelector "preferredLocale"

-- | @Selector@ for @setPreferredLocale:@
setPreferredLocaleSelector :: Selector '[Id NSLocale] ()
setPreferredLocaleSelector = mkSelector "setPreferredLocale:"

