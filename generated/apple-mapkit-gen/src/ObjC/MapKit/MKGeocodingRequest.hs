{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKGeocodingRequest@.
module ObjC.MapKit.MKGeocodingRequest
  ( MKGeocodingRequest
  , IsMKGeocodingRequest(..)
  , init_
  , new
  , initWithAddressString
  , cancel
  , cancelled
  , loading
  , addressString
  , preferredLocale
  , setPreferredLocale
  , addressStringSelector
  , cancelSelector
  , cancelledSelector
  , initSelector
  , initWithAddressStringSelector
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
init_ :: IsMKGeocodingRequest mkGeocodingRequest => mkGeocodingRequest -> IO (Id MKGeocodingRequest)
init_ mkGeocodingRequest =
  sendOwnedMessage mkGeocodingRequest initSelector

-- | @+ new@
new :: IO (Id MKGeocodingRequest)
new  =
  do
    cls' <- getRequiredClass "MKGeocodingRequest"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithAddressString:@
initWithAddressString :: (IsMKGeocodingRequest mkGeocodingRequest, IsNSString addressString) => mkGeocodingRequest -> addressString -> IO (Id MKGeocodingRequest)
initWithAddressString mkGeocodingRequest addressString =
  sendOwnedMessage mkGeocodingRequest initWithAddressStringSelector (toNSString addressString)

-- | @- cancel@
cancel :: IsMKGeocodingRequest mkGeocodingRequest => mkGeocodingRequest -> IO ()
cancel mkGeocodingRequest =
  sendMessage mkGeocodingRequest cancelSelector

-- | @- cancelled@
cancelled :: IsMKGeocodingRequest mkGeocodingRequest => mkGeocodingRequest -> IO Bool
cancelled mkGeocodingRequest =
  sendMessage mkGeocodingRequest cancelledSelector

-- | @- loading@
loading :: IsMKGeocodingRequest mkGeocodingRequest => mkGeocodingRequest -> IO Bool
loading mkGeocodingRequest =
  sendMessage mkGeocodingRequest loadingSelector

-- | @- addressString@
addressString :: IsMKGeocodingRequest mkGeocodingRequest => mkGeocodingRequest -> IO (Id NSString)
addressString mkGeocodingRequest =
  sendMessage mkGeocodingRequest addressStringSelector

-- | @- preferredLocale@
preferredLocale :: IsMKGeocodingRequest mkGeocodingRequest => mkGeocodingRequest -> IO (Id NSLocale)
preferredLocale mkGeocodingRequest =
  sendMessage mkGeocodingRequest preferredLocaleSelector

-- | @- setPreferredLocale:@
setPreferredLocale :: (IsMKGeocodingRequest mkGeocodingRequest, IsNSLocale value) => mkGeocodingRequest -> value -> IO ()
setPreferredLocale mkGeocodingRequest value =
  sendMessage mkGeocodingRequest setPreferredLocaleSelector (toNSLocale value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MKGeocodingRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MKGeocodingRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithAddressString:@
initWithAddressStringSelector :: Selector '[Id NSString] (Id MKGeocodingRequest)
initWithAddressStringSelector = mkSelector "initWithAddressString:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @cancelled@
cancelledSelector :: Selector '[] Bool
cancelledSelector = mkSelector "cancelled"

-- | @Selector@ for @loading@
loadingSelector :: Selector '[] Bool
loadingSelector = mkSelector "loading"

-- | @Selector@ for @addressString@
addressStringSelector :: Selector '[] (Id NSString)
addressStringSelector = mkSelector "addressString"

-- | @Selector@ for @preferredLocale@
preferredLocaleSelector :: Selector '[] (Id NSLocale)
preferredLocaleSelector = mkSelector "preferredLocale"

-- | @Selector@ for @setPreferredLocale:@
setPreferredLocaleSelector :: Selector '[Id NSLocale] ()
setPreferredLocaleSelector = mkSelector "setPreferredLocale:"

