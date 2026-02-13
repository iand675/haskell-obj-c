{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRestaurant@.
module ObjC.Intents.INRestaurant
  ( INRestaurant
  , IsINRestaurant(..)
  , initWithLocation_name_vendorIdentifier_restaurantIdentifier
  , name
  , setName
  , vendorIdentifier
  , setVendorIdentifier
  , restaurantIdentifier
  , setRestaurantIdentifier
  , initWithLocation_name_vendorIdentifier_restaurantIdentifierSelector
  , nameSelector
  , restaurantIdentifierSelector
  , setNameSelector
  , setRestaurantIdentifierSelector
  , setVendorIdentifierSelector
  , vendorIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithLocation:name:vendorIdentifier:restaurantIdentifier:@
initWithLocation_name_vendorIdentifier_restaurantIdentifier :: (IsINRestaurant inRestaurant, IsNSString name, IsNSString vendorIdentifier, IsNSString restaurantIdentifier) => inRestaurant -> RawId -> name -> vendorIdentifier -> restaurantIdentifier -> IO (Id INRestaurant)
initWithLocation_name_vendorIdentifier_restaurantIdentifier inRestaurant location name vendorIdentifier restaurantIdentifier =
  sendOwnedMessage inRestaurant initWithLocation_name_vendorIdentifier_restaurantIdentifierSelector location (toNSString name) (toNSString vendorIdentifier) (toNSString restaurantIdentifier)

-- | @- name@
name :: IsINRestaurant inRestaurant => inRestaurant -> IO (Id NSString)
name inRestaurant =
  sendMessage inRestaurant nameSelector

-- | @- setName:@
setName :: (IsINRestaurant inRestaurant, IsNSString value) => inRestaurant -> value -> IO ()
setName inRestaurant value =
  sendMessage inRestaurant setNameSelector (toNSString value)

-- | @- vendorIdentifier@
vendorIdentifier :: IsINRestaurant inRestaurant => inRestaurant -> IO (Id NSString)
vendorIdentifier inRestaurant =
  sendMessage inRestaurant vendorIdentifierSelector

-- | @- setVendorIdentifier:@
setVendorIdentifier :: (IsINRestaurant inRestaurant, IsNSString value) => inRestaurant -> value -> IO ()
setVendorIdentifier inRestaurant value =
  sendMessage inRestaurant setVendorIdentifierSelector (toNSString value)

-- | @- restaurantIdentifier@
restaurantIdentifier :: IsINRestaurant inRestaurant => inRestaurant -> IO (Id NSString)
restaurantIdentifier inRestaurant =
  sendMessage inRestaurant restaurantIdentifierSelector

-- | @- setRestaurantIdentifier:@
setRestaurantIdentifier :: (IsINRestaurant inRestaurant, IsNSString value) => inRestaurant -> value -> IO ()
setRestaurantIdentifier inRestaurant value =
  sendMessage inRestaurant setRestaurantIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLocation:name:vendorIdentifier:restaurantIdentifier:@
initWithLocation_name_vendorIdentifier_restaurantIdentifierSelector :: Selector '[RawId, Id NSString, Id NSString, Id NSString] (Id INRestaurant)
initWithLocation_name_vendorIdentifier_restaurantIdentifierSelector = mkSelector "initWithLocation:name:vendorIdentifier:restaurantIdentifier:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @vendorIdentifier@
vendorIdentifierSelector :: Selector '[] (Id NSString)
vendorIdentifierSelector = mkSelector "vendorIdentifier"

-- | @Selector@ for @setVendorIdentifier:@
setVendorIdentifierSelector :: Selector '[Id NSString] ()
setVendorIdentifierSelector = mkSelector "setVendorIdentifier:"

-- | @Selector@ for @restaurantIdentifier@
restaurantIdentifierSelector :: Selector '[] (Id NSString)
restaurantIdentifierSelector = mkSelector "restaurantIdentifier"

-- | @Selector@ for @setRestaurantIdentifier:@
setRestaurantIdentifierSelector :: Selector '[Id NSString] ()
setRestaurantIdentifierSelector = mkSelector "setRestaurantIdentifier:"

