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
  , initSelector
  , newSelector
  , initWithAddressStringSelector
  , cancelSelector
  , cancelledSelector
  , loadingSelector
  , addressStringSelector
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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMKGeocodingRequest mkGeocodingRequest => mkGeocodingRequest -> IO (Id MKGeocodingRequest)
init_ mkGeocodingRequest  =
  sendMsg mkGeocodingRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MKGeocodingRequest)
new  =
  do
    cls' <- getRequiredClass "MKGeocodingRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithAddressString:@
initWithAddressString :: (IsMKGeocodingRequest mkGeocodingRequest, IsNSString addressString) => mkGeocodingRequest -> addressString -> IO (Id MKGeocodingRequest)
initWithAddressString mkGeocodingRequest  addressString =
withObjCPtr addressString $ \raw_addressString ->
    sendMsg mkGeocodingRequest (mkSelector "initWithAddressString:") (retPtr retVoid) [argPtr (castPtr raw_addressString :: Ptr ())] >>= ownedObject . castPtr

-- | @- cancel@
cancel :: IsMKGeocodingRequest mkGeocodingRequest => mkGeocodingRequest -> IO ()
cancel mkGeocodingRequest  =
  sendMsg mkGeocodingRequest (mkSelector "cancel") retVoid []

-- | @- cancelled@
cancelled :: IsMKGeocodingRequest mkGeocodingRequest => mkGeocodingRequest -> IO Bool
cancelled mkGeocodingRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkGeocodingRequest (mkSelector "cancelled") retCULong []

-- | @- loading@
loading :: IsMKGeocodingRequest mkGeocodingRequest => mkGeocodingRequest -> IO Bool
loading mkGeocodingRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkGeocodingRequest (mkSelector "loading") retCULong []

-- | @- addressString@
addressString :: IsMKGeocodingRequest mkGeocodingRequest => mkGeocodingRequest -> IO (Id NSString)
addressString mkGeocodingRequest  =
  sendMsg mkGeocodingRequest (mkSelector "addressString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- preferredLocale@
preferredLocale :: IsMKGeocodingRequest mkGeocodingRequest => mkGeocodingRequest -> IO (Id NSLocale)
preferredLocale mkGeocodingRequest  =
  sendMsg mkGeocodingRequest (mkSelector "preferredLocale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreferredLocale:@
setPreferredLocale :: (IsMKGeocodingRequest mkGeocodingRequest, IsNSLocale value) => mkGeocodingRequest -> value -> IO ()
setPreferredLocale mkGeocodingRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkGeocodingRequest (mkSelector "setPreferredLocale:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithAddressString:@
initWithAddressStringSelector :: Selector
initWithAddressStringSelector = mkSelector "initWithAddressString:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @cancelled@
cancelledSelector :: Selector
cancelledSelector = mkSelector "cancelled"

-- | @Selector@ for @loading@
loadingSelector :: Selector
loadingSelector = mkSelector "loading"

-- | @Selector@ for @addressString@
addressStringSelector :: Selector
addressStringSelector = mkSelector "addressString"

-- | @Selector@ for @preferredLocale@
preferredLocaleSelector :: Selector
preferredLocaleSelector = mkSelector "preferredLocale"

-- | @Selector@ for @setPreferredLocale:@
setPreferredLocaleSelector :: Selector
setPreferredLocaleSelector = mkSelector "setPreferredLocale:"

