{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRestaurant@.
module ObjC.Intents.INRestaurant
  ( INRestaurant
  , IsINRestaurant(..)
  , initWithLocation_name_vendorIdentifier_restaurantIdentifier
  , location
  , setLocation
  , name
  , setName
  , vendorIdentifier
  , setVendorIdentifier
  , restaurantIdentifier
  , setRestaurantIdentifier
  , initWithLocation_name_vendorIdentifier_restaurantIdentifierSelector
  , locationSelector
  , setLocationSelector
  , nameSelector
  , setNameSelector
  , vendorIdentifierSelector
  , setVendorIdentifierSelector
  , restaurantIdentifierSelector
  , setRestaurantIdentifierSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithLocation:name:vendorIdentifier:restaurantIdentifier:@
initWithLocation_name_vendorIdentifier_restaurantIdentifier :: (IsINRestaurant inRestaurant, IsCLLocation location, IsNSString name, IsNSString vendorIdentifier, IsNSString restaurantIdentifier) => inRestaurant -> location -> name -> vendorIdentifier -> restaurantIdentifier -> IO (Id INRestaurant)
initWithLocation_name_vendorIdentifier_restaurantIdentifier inRestaurant  location name vendorIdentifier restaurantIdentifier =
withObjCPtr location $ \raw_location ->
  withObjCPtr name $ \raw_name ->
    withObjCPtr vendorIdentifier $ \raw_vendorIdentifier ->
      withObjCPtr restaurantIdentifier $ \raw_restaurantIdentifier ->
          sendMsg inRestaurant (mkSelector "initWithLocation:name:vendorIdentifier:restaurantIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_location :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_vendorIdentifier :: Ptr ()), argPtr (castPtr raw_restaurantIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- location@
location :: IsINRestaurant inRestaurant => inRestaurant -> IO (Id CLLocation)
location inRestaurant  =
  sendMsg inRestaurant (mkSelector "location") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocation:@
setLocation :: (IsINRestaurant inRestaurant, IsCLLocation value) => inRestaurant -> value -> IO ()
setLocation inRestaurant  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRestaurant (mkSelector "setLocation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsINRestaurant inRestaurant => inRestaurant -> IO (Id NSString)
name inRestaurant  =
  sendMsg inRestaurant (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsINRestaurant inRestaurant, IsNSString value) => inRestaurant -> value -> IO ()
setName inRestaurant  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRestaurant (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vendorIdentifier@
vendorIdentifier :: IsINRestaurant inRestaurant => inRestaurant -> IO (Id NSString)
vendorIdentifier inRestaurant  =
  sendMsg inRestaurant (mkSelector "vendorIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVendorIdentifier:@
setVendorIdentifier :: (IsINRestaurant inRestaurant, IsNSString value) => inRestaurant -> value -> IO ()
setVendorIdentifier inRestaurant  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRestaurant (mkSelector "setVendorIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- restaurantIdentifier@
restaurantIdentifier :: IsINRestaurant inRestaurant => inRestaurant -> IO (Id NSString)
restaurantIdentifier inRestaurant  =
  sendMsg inRestaurant (mkSelector "restaurantIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRestaurantIdentifier:@
setRestaurantIdentifier :: (IsINRestaurant inRestaurant, IsNSString value) => inRestaurant -> value -> IO ()
setRestaurantIdentifier inRestaurant  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRestaurant (mkSelector "setRestaurantIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLocation:name:vendorIdentifier:restaurantIdentifier:@
initWithLocation_name_vendorIdentifier_restaurantIdentifierSelector :: Selector
initWithLocation_name_vendorIdentifier_restaurantIdentifierSelector = mkSelector "initWithLocation:name:vendorIdentifier:restaurantIdentifier:"

-- | @Selector@ for @location@
locationSelector :: Selector
locationSelector = mkSelector "location"

-- | @Selector@ for @setLocation:@
setLocationSelector :: Selector
setLocationSelector = mkSelector "setLocation:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @vendorIdentifier@
vendorIdentifierSelector :: Selector
vendorIdentifierSelector = mkSelector "vendorIdentifier"

-- | @Selector@ for @setVendorIdentifier:@
setVendorIdentifierSelector :: Selector
setVendorIdentifierSelector = mkSelector "setVendorIdentifier:"

-- | @Selector@ for @restaurantIdentifier@
restaurantIdentifierSelector :: Selector
restaurantIdentifierSelector = mkSelector "restaurantIdentifier"

-- | @Selector@ for @setRestaurantIdentifier:@
setRestaurantIdentifierSelector :: Selector
setRestaurantIdentifierSelector = mkSelector "setRestaurantIdentifier:"

