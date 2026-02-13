{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRideOption@.
module ObjC.Intents.INRideOption
  ( INRideOption
  , IsINRideOption(..)
  , init_
  , initWithName_estimatedPickupDate
  , initWithCoder
  , name
  , setName
  , estimatedPickupDate
  , setEstimatedPickupDate
  , priceRange
  , setPriceRange
  , usesMeteredFare
  , setUsesMeteredFare
  , disclaimerMessage
  , setDisclaimerMessage
  , availablePartySizeOptions
  , setAvailablePartySizeOptions
  , availablePartySizeOptionsSelectionPrompt
  , setAvailablePartySizeOptionsSelectionPrompt
  , specialPricing
  , setSpecialPricing
  , specialPricingBadgeImage
  , setSpecialPricingBadgeImage
  , fareLineItems
  , setFareLineItems
  , userActivityForBookingInApplication
  , setUserActivityForBookingInApplication
  , identifier
  , setIdentifier
  , availablePartySizeOptionsSelectionPromptSelector
  , availablePartySizeOptionsSelector
  , disclaimerMessageSelector
  , estimatedPickupDateSelector
  , fareLineItemsSelector
  , identifierSelector
  , initSelector
  , initWithCoderSelector
  , initWithName_estimatedPickupDateSelector
  , nameSelector
  , priceRangeSelector
  , setAvailablePartySizeOptionsSelectionPromptSelector
  , setAvailablePartySizeOptionsSelector
  , setDisclaimerMessageSelector
  , setEstimatedPickupDateSelector
  , setFareLineItemsSelector
  , setIdentifierSelector
  , setNameSelector
  , setPriceRangeSelector
  , setSpecialPricingBadgeImageSelector
  , setSpecialPricingSelector
  , setUserActivityForBookingInApplicationSelector
  , setUsesMeteredFareSelector
  , specialPricingBadgeImageSelector
  , specialPricingSelector
  , userActivityForBookingInApplicationSelector
  , usesMeteredFareSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINRideOption inRideOption => inRideOption -> IO (Id INRideOption)
init_ inRideOption =
  sendOwnedMessage inRideOption initSelector

-- | @- initWithName:estimatedPickupDate:@
initWithName_estimatedPickupDate :: (IsINRideOption inRideOption, IsNSString name, IsNSDate estimatedPickupDate) => inRideOption -> name -> estimatedPickupDate -> IO (Id INRideOption)
initWithName_estimatedPickupDate inRideOption name estimatedPickupDate =
  sendOwnedMessage inRideOption initWithName_estimatedPickupDateSelector (toNSString name) (toNSDate estimatedPickupDate)

-- | @- initWithCoder:@
initWithCoder :: (IsINRideOption inRideOption, IsNSCoder decoder) => inRideOption -> decoder -> IO (Id INRideOption)
initWithCoder inRideOption decoder =
  sendOwnedMessage inRideOption initWithCoderSelector (toNSCoder decoder)

-- | @- name@
name :: IsINRideOption inRideOption => inRideOption -> IO (Id NSString)
name inRideOption =
  sendMessage inRideOption nameSelector

-- | @- setName:@
setName :: (IsINRideOption inRideOption, IsNSString value) => inRideOption -> value -> IO ()
setName inRideOption value =
  sendMessage inRideOption setNameSelector (toNSString value)

-- | @- estimatedPickupDate@
estimatedPickupDate :: IsINRideOption inRideOption => inRideOption -> IO (Id NSDate)
estimatedPickupDate inRideOption =
  sendMessage inRideOption estimatedPickupDateSelector

-- | @- setEstimatedPickupDate:@
setEstimatedPickupDate :: (IsINRideOption inRideOption, IsNSDate value) => inRideOption -> value -> IO ()
setEstimatedPickupDate inRideOption value =
  sendMessage inRideOption setEstimatedPickupDateSelector (toNSDate value)

-- | @- priceRange@
priceRange :: IsINRideOption inRideOption => inRideOption -> IO (Id INPriceRange)
priceRange inRideOption =
  sendMessage inRideOption priceRangeSelector

-- | @- setPriceRange:@
setPriceRange :: (IsINRideOption inRideOption, IsINPriceRange value) => inRideOption -> value -> IO ()
setPriceRange inRideOption value =
  sendMessage inRideOption setPriceRangeSelector (toINPriceRange value)

-- | @- usesMeteredFare@
usesMeteredFare :: IsINRideOption inRideOption => inRideOption -> IO (Id NSNumber)
usesMeteredFare inRideOption =
  sendMessage inRideOption usesMeteredFareSelector

-- | @- setUsesMeteredFare:@
setUsesMeteredFare :: (IsINRideOption inRideOption, IsNSNumber value) => inRideOption -> value -> IO ()
setUsesMeteredFare inRideOption value =
  sendMessage inRideOption setUsesMeteredFareSelector (toNSNumber value)

-- | @- disclaimerMessage@
disclaimerMessage :: IsINRideOption inRideOption => inRideOption -> IO (Id NSString)
disclaimerMessage inRideOption =
  sendMessage inRideOption disclaimerMessageSelector

-- | @- setDisclaimerMessage:@
setDisclaimerMessage :: (IsINRideOption inRideOption, IsNSString value) => inRideOption -> value -> IO ()
setDisclaimerMessage inRideOption value =
  sendMessage inRideOption setDisclaimerMessageSelector (toNSString value)

-- | @- availablePartySizeOptions@
availablePartySizeOptions :: IsINRideOption inRideOption => inRideOption -> IO (Id NSArray)
availablePartySizeOptions inRideOption =
  sendMessage inRideOption availablePartySizeOptionsSelector

-- | @- setAvailablePartySizeOptions:@
setAvailablePartySizeOptions :: (IsINRideOption inRideOption, IsNSArray value) => inRideOption -> value -> IO ()
setAvailablePartySizeOptions inRideOption value =
  sendMessage inRideOption setAvailablePartySizeOptionsSelector (toNSArray value)

-- | @- availablePartySizeOptionsSelectionPrompt@
availablePartySizeOptionsSelectionPrompt :: IsINRideOption inRideOption => inRideOption -> IO (Id NSString)
availablePartySizeOptionsSelectionPrompt inRideOption =
  sendMessage inRideOption availablePartySizeOptionsSelectionPromptSelector

-- | @- setAvailablePartySizeOptionsSelectionPrompt:@
setAvailablePartySizeOptionsSelectionPrompt :: (IsINRideOption inRideOption, IsNSString value) => inRideOption -> value -> IO ()
setAvailablePartySizeOptionsSelectionPrompt inRideOption value =
  sendMessage inRideOption setAvailablePartySizeOptionsSelectionPromptSelector (toNSString value)

-- | @- specialPricing@
specialPricing :: IsINRideOption inRideOption => inRideOption -> IO (Id NSString)
specialPricing inRideOption =
  sendMessage inRideOption specialPricingSelector

-- | @- setSpecialPricing:@
setSpecialPricing :: (IsINRideOption inRideOption, IsNSString value) => inRideOption -> value -> IO ()
setSpecialPricing inRideOption value =
  sendMessage inRideOption setSpecialPricingSelector (toNSString value)

-- | @- specialPricingBadgeImage@
specialPricingBadgeImage :: IsINRideOption inRideOption => inRideOption -> IO (Id INImage)
specialPricingBadgeImage inRideOption =
  sendMessage inRideOption specialPricingBadgeImageSelector

-- | @- setSpecialPricingBadgeImage:@
setSpecialPricingBadgeImage :: (IsINRideOption inRideOption, IsINImage value) => inRideOption -> value -> IO ()
setSpecialPricingBadgeImage inRideOption value =
  sendMessage inRideOption setSpecialPricingBadgeImageSelector (toINImage value)

-- | @- fareLineItems@
fareLineItems :: IsINRideOption inRideOption => inRideOption -> IO (Id NSArray)
fareLineItems inRideOption =
  sendMessage inRideOption fareLineItemsSelector

-- | @- setFareLineItems:@
setFareLineItems :: (IsINRideOption inRideOption, IsNSArray value) => inRideOption -> value -> IO ()
setFareLineItems inRideOption value =
  sendMessage inRideOption setFareLineItemsSelector (toNSArray value)

-- | @- userActivityForBookingInApplication@
userActivityForBookingInApplication :: IsINRideOption inRideOption => inRideOption -> IO (Id NSUserActivity)
userActivityForBookingInApplication inRideOption =
  sendMessage inRideOption userActivityForBookingInApplicationSelector

-- | @- setUserActivityForBookingInApplication:@
setUserActivityForBookingInApplication :: (IsINRideOption inRideOption, IsNSUserActivity value) => inRideOption -> value -> IO ()
setUserActivityForBookingInApplication inRideOption value =
  sendMessage inRideOption setUserActivityForBookingInApplicationSelector (toNSUserActivity value)

-- | @- identifier@
identifier :: IsINRideOption inRideOption => inRideOption -> IO (Id NSString)
identifier inRideOption =
  sendMessage inRideOption identifierSelector

-- | @- setIdentifier:@
setIdentifier :: (IsINRideOption inRideOption, IsNSString value) => inRideOption -> value -> IO ()
setIdentifier inRideOption value =
  sendMessage inRideOption setIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INRideOption)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithName:estimatedPickupDate:@
initWithName_estimatedPickupDateSelector :: Selector '[Id NSString, Id NSDate] (Id INRideOption)
initWithName_estimatedPickupDateSelector = mkSelector "initWithName:estimatedPickupDate:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id INRideOption)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @estimatedPickupDate@
estimatedPickupDateSelector :: Selector '[] (Id NSDate)
estimatedPickupDateSelector = mkSelector "estimatedPickupDate"

-- | @Selector@ for @setEstimatedPickupDate:@
setEstimatedPickupDateSelector :: Selector '[Id NSDate] ()
setEstimatedPickupDateSelector = mkSelector "setEstimatedPickupDate:"

-- | @Selector@ for @priceRange@
priceRangeSelector :: Selector '[] (Id INPriceRange)
priceRangeSelector = mkSelector "priceRange"

-- | @Selector@ for @setPriceRange:@
setPriceRangeSelector :: Selector '[Id INPriceRange] ()
setPriceRangeSelector = mkSelector "setPriceRange:"

-- | @Selector@ for @usesMeteredFare@
usesMeteredFareSelector :: Selector '[] (Id NSNumber)
usesMeteredFareSelector = mkSelector "usesMeteredFare"

-- | @Selector@ for @setUsesMeteredFare:@
setUsesMeteredFareSelector :: Selector '[Id NSNumber] ()
setUsesMeteredFareSelector = mkSelector "setUsesMeteredFare:"

-- | @Selector@ for @disclaimerMessage@
disclaimerMessageSelector :: Selector '[] (Id NSString)
disclaimerMessageSelector = mkSelector "disclaimerMessage"

-- | @Selector@ for @setDisclaimerMessage:@
setDisclaimerMessageSelector :: Selector '[Id NSString] ()
setDisclaimerMessageSelector = mkSelector "setDisclaimerMessage:"

-- | @Selector@ for @availablePartySizeOptions@
availablePartySizeOptionsSelector :: Selector '[] (Id NSArray)
availablePartySizeOptionsSelector = mkSelector "availablePartySizeOptions"

-- | @Selector@ for @setAvailablePartySizeOptions:@
setAvailablePartySizeOptionsSelector :: Selector '[Id NSArray] ()
setAvailablePartySizeOptionsSelector = mkSelector "setAvailablePartySizeOptions:"

-- | @Selector@ for @availablePartySizeOptionsSelectionPrompt@
availablePartySizeOptionsSelectionPromptSelector :: Selector '[] (Id NSString)
availablePartySizeOptionsSelectionPromptSelector = mkSelector "availablePartySizeOptionsSelectionPrompt"

-- | @Selector@ for @setAvailablePartySizeOptionsSelectionPrompt:@
setAvailablePartySizeOptionsSelectionPromptSelector :: Selector '[Id NSString] ()
setAvailablePartySizeOptionsSelectionPromptSelector = mkSelector "setAvailablePartySizeOptionsSelectionPrompt:"

-- | @Selector@ for @specialPricing@
specialPricingSelector :: Selector '[] (Id NSString)
specialPricingSelector = mkSelector "specialPricing"

-- | @Selector@ for @setSpecialPricing:@
setSpecialPricingSelector :: Selector '[Id NSString] ()
setSpecialPricingSelector = mkSelector "setSpecialPricing:"

-- | @Selector@ for @specialPricingBadgeImage@
specialPricingBadgeImageSelector :: Selector '[] (Id INImage)
specialPricingBadgeImageSelector = mkSelector "specialPricingBadgeImage"

-- | @Selector@ for @setSpecialPricingBadgeImage:@
setSpecialPricingBadgeImageSelector :: Selector '[Id INImage] ()
setSpecialPricingBadgeImageSelector = mkSelector "setSpecialPricingBadgeImage:"

-- | @Selector@ for @fareLineItems@
fareLineItemsSelector :: Selector '[] (Id NSArray)
fareLineItemsSelector = mkSelector "fareLineItems"

-- | @Selector@ for @setFareLineItems:@
setFareLineItemsSelector :: Selector '[Id NSArray] ()
setFareLineItemsSelector = mkSelector "setFareLineItems:"

-- | @Selector@ for @userActivityForBookingInApplication@
userActivityForBookingInApplicationSelector :: Selector '[] (Id NSUserActivity)
userActivityForBookingInApplicationSelector = mkSelector "userActivityForBookingInApplication"

-- | @Selector@ for @setUserActivityForBookingInApplication:@
setUserActivityForBookingInApplicationSelector :: Selector '[Id NSUserActivity] ()
setUserActivityForBookingInApplicationSelector = mkSelector "setUserActivityForBookingInApplication:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSString] ()
setIdentifierSelector = mkSelector "setIdentifier:"

