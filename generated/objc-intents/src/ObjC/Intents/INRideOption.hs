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
  , initSelector
  , initWithName_estimatedPickupDateSelector
  , initWithCoderSelector
  , nameSelector
  , setNameSelector
  , estimatedPickupDateSelector
  , setEstimatedPickupDateSelector
  , priceRangeSelector
  , setPriceRangeSelector
  , disclaimerMessageSelector
  , setDisclaimerMessageSelector
  , availablePartySizeOptionsSelector
  , setAvailablePartySizeOptionsSelector
  , availablePartySizeOptionsSelectionPromptSelector
  , setAvailablePartySizeOptionsSelectionPromptSelector
  , specialPricingSelector
  , setSpecialPricingSelector
  , specialPricingBadgeImageSelector
  , setSpecialPricingBadgeImageSelector
  , fareLineItemsSelector
  , setFareLineItemsSelector
  , userActivityForBookingInApplicationSelector
  , setUserActivityForBookingInApplicationSelector
  , identifierSelector
  , setIdentifierSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINRideOption inRideOption => inRideOption -> IO (Id INRideOption)
init_ inRideOption  =
  sendMsg inRideOption (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithName:estimatedPickupDate:@
initWithName_estimatedPickupDate :: (IsINRideOption inRideOption, IsNSString name, IsNSDate estimatedPickupDate) => inRideOption -> name -> estimatedPickupDate -> IO (Id INRideOption)
initWithName_estimatedPickupDate inRideOption  name estimatedPickupDate =
withObjCPtr name $ \raw_name ->
  withObjCPtr estimatedPickupDate $ \raw_estimatedPickupDate ->
      sendMsg inRideOption (mkSelector "initWithName:estimatedPickupDate:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_estimatedPickupDate :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsINRideOption inRideOption, IsNSCoder decoder) => inRideOption -> decoder -> IO (Id INRideOption)
initWithCoder inRideOption  decoder =
withObjCPtr decoder $ \raw_decoder ->
    sendMsg inRideOption (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_decoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- name@
name :: IsINRideOption inRideOption => inRideOption -> IO (Id NSString)
name inRideOption  =
  sendMsg inRideOption (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsINRideOption inRideOption, IsNSString value) => inRideOption -> value -> IO ()
setName inRideOption  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideOption (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- estimatedPickupDate@
estimatedPickupDate :: IsINRideOption inRideOption => inRideOption -> IO (Id NSDate)
estimatedPickupDate inRideOption  =
  sendMsg inRideOption (mkSelector "estimatedPickupDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEstimatedPickupDate:@
setEstimatedPickupDate :: (IsINRideOption inRideOption, IsNSDate value) => inRideOption -> value -> IO ()
setEstimatedPickupDate inRideOption  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideOption (mkSelector "setEstimatedPickupDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- priceRange@
priceRange :: IsINRideOption inRideOption => inRideOption -> IO (Id INPriceRange)
priceRange inRideOption  =
  sendMsg inRideOption (mkSelector "priceRange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPriceRange:@
setPriceRange :: (IsINRideOption inRideOption, IsINPriceRange value) => inRideOption -> value -> IO ()
setPriceRange inRideOption  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideOption (mkSelector "setPriceRange:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- disclaimerMessage@
disclaimerMessage :: IsINRideOption inRideOption => inRideOption -> IO (Id NSString)
disclaimerMessage inRideOption  =
  sendMsg inRideOption (mkSelector "disclaimerMessage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDisclaimerMessage:@
setDisclaimerMessage :: (IsINRideOption inRideOption, IsNSString value) => inRideOption -> value -> IO ()
setDisclaimerMessage inRideOption  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideOption (mkSelector "setDisclaimerMessage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- availablePartySizeOptions@
availablePartySizeOptions :: IsINRideOption inRideOption => inRideOption -> IO (Id NSArray)
availablePartySizeOptions inRideOption  =
  sendMsg inRideOption (mkSelector "availablePartySizeOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAvailablePartySizeOptions:@
setAvailablePartySizeOptions :: (IsINRideOption inRideOption, IsNSArray value) => inRideOption -> value -> IO ()
setAvailablePartySizeOptions inRideOption  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideOption (mkSelector "setAvailablePartySizeOptions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- availablePartySizeOptionsSelectionPrompt@
availablePartySizeOptionsSelectionPrompt :: IsINRideOption inRideOption => inRideOption -> IO (Id NSString)
availablePartySizeOptionsSelectionPrompt inRideOption  =
  sendMsg inRideOption (mkSelector "availablePartySizeOptionsSelectionPrompt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAvailablePartySizeOptionsSelectionPrompt:@
setAvailablePartySizeOptionsSelectionPrompt :: (IsINRideOption inRideOption, IsNSString value) => inRideOption -> value -> IO ()
setAvailablePartySizeOptionsSelectionPrompt inRideOption  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideOption (mkSelector "setAvailablePartySizeOptionsSelectionPrompt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- specialPricing@
specialPricing :: IsINRideOption inRideOption => inRideOption -> IO (Id NSString)
specialPricing inRideOption  =
  sendMsg inRideOption (mkSelector "specialPricing") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSpecialPricing:@
setSpecialPricing :: (IsINRideOption inRideOption, IsNSString value) => inRideOption -> value -> IO ()
setSpecialPricing inRideOption  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideOption (mkSelector "setSpecialPricing:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- specialPricingBadgeImage@
specialPricingBadgeImage :: IsINRideOption inRideOption => inRideOption -> IO (Id INImage)
specialPricingBadgeImage inRideOption  =
  sendMsg inRideOption (mkSelector "specialPricingBadgeImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSpecialPricingBadgeImage:@
setSpecialPricingBadgeImage :: (IsINRideOption inRideOption, IsINImage value) => inRideOption -> value -> IO ()
setSpecialPricingBadgeImage inRideOption  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideOption (mkSelector "setSpecialPricingBadgeImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fareLineItems@
fareLineItems :: IsINRideOption inRideOption => inRideOption -> IO (Id NSArray)
fareLineItems inRideOption  =
  sendMsg inRideOption (mkSelector "fareLineItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFareLineItems:@
setFareLineItems :: (IsINRideOption inRideOption, IsNSArray value) => inRideOption -> value -> IO ()
setFareLineItems inRideOption  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideOption (mkSelector "setFareLineItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userActivityForBookingInApplication@
userActivityForBookingInApplication :: IsINRideOption inRideOption => inRideOption -> IO (Id NSUserActivity)
userActivityForBookingInApplication inRideOption  =
  sendMsg inRideOption (mkSelector "userActivityForBookingInApplication") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserActivityForBookingInApplication:@
setUserActivityForBookingInApplication :: (IsINRideOption inRideOption, IsNSUserActivity value) => inRideOption -> value -> IO ()
setUserActivityForBookingInApplication inRideOption  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideOption (mkSelector "setUserActivityForBookingInApplication:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- identifier@
identifier :: IsINRideOption inRideOption => inRideOption -> IO (Id NSString)
identifier inRideOption  =
  sendMsg inRideOption (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIdentifier:@
setIdentifier :: (IsINRideOption inRideOption, IsNSString value) => inRideOption -> value -> IO ()
setIdentifier inRideOption  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideOption (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithName:estimatedPickupDate:@
initWithName_estimatedPickupDateSelector :: Selector
initWithName_estimatedPickupDateSelector = mkSelector "initWithName:estimatedPickupDate:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @estimatedPickupDate@
estimatedPickupDateSelector :: Selector
estimatedPickupDateSelector = mkSelector "estimatedPickupDate"

-- | @Selector@ for @setEstimatedPickupDate:@
setEstimatedPickupDateSelector :: Selector
setEstimatedPickupDateSelector = mkSelector "setEstimatedPickupDate:"

-- | @Selector@ for @priceRange@
priceRangeSelector :: Selector
priceRangeSelector = mkSelector "priceRange"

-- | @Selector@ for @setPriceRange:@
setPriceRangeSelector :: Selector
setPriceRangeSelector = mkSelector "setPriceRange:"

-- | @Selector@ for @disclaimerMessage@
disclaimerMessageSelector :: Selector
disclaimerMessageSelector = mkSelector "disclaimerMessage"

-- | @Selector@ for @setDisclaimerMessage:@
setDisclaimerMessageSelector :: Selector
setDisclaimerMessageSelector = mkSelector "setDisclaimerMessage:"

-- | @Selector@ for @availablePartySizeOptions@
availablePartySizeOptionsSelector :: Selector
availablePartySizeOptionsSelector = mkSelector "availablePartySizeOptions"

-- | @Selector@ for @setAvailablePartySizeOptions:@
setAvailablePartySizeOptionsSelector :: Selector
setAvailablePartySizeOptionsSelector = mkSelector "setAvailablePartySizeOptions:"

-- | @Selector@ for @availablePartySizeOptionsSelectionPrompt@
availablePartySizeOptionsSelectionPromptSelector :: Selector
availablePartySizeOptionsSelectionPromptSelector = mkSelector "availablePartySizeOptionsSelectionPrompt"

-- | @Selector@ for @setAvailablePartySizeOptionsSelectionPrompt:@
setAvailablePartySizeOptionsSelectionPromptSelector :: Selector
setAvailablePartySizeOptionsSelectionPromptSelector = mkSelector "setAvailablePartySizeOptionsSelectionPrompt:"

-- | @Selector@ for @specialPricing@
specialPricingSelector :: Selector
specialPricingSelector = mkSelector "specialPricing"

-- | @Selector@ for @setSpecialPricing:@
setSpecialPricingSelector :: Selector
setSpecialPricingSelector = mkSelector "setSpecialPricing:"

-- | @Selector@ for @specialPricingBadgeImage@
specialPricingBadgeImageSelector :: Selector
specialPricingBadgeImageSelector = mkSelector "specialPricingBadgeImage"

-- | @Selector@ for @setSpecialPricingBadgeImage:@
setSpecialPricingBadgeImageSelector :: Selector
setSpecialPricingBadgeImageSelector = mkSelector "setSpecialPricingBadgeImage:"

-- | @Selector@ for @fareLineItems@
fareLineItemsSelector :: Selector
fareLineItemsSelector = mkSelector "fareLineItems"

-- | @Selector@ for @setFareLineItems:@
setFareLineItemsSelector :: Selector
setFareLineItemsSelector = mkSelector "setFareLineItems:"

-- | @Selector@ for @userActivityForBookingInApplication@
userActivityForBookingInApplicationSelector :: Selector
userActivityForBookingInApplicationSelector = mkSelector "userActivityForBookingInApplication"

-- | @Selector@ for @setUserActivityForBookingInApplication:@
setUserActivityForBookingInApplicationSelector :: Selector
setUserActivityForBookingInApplicationSelector = mkSelector "setUserActivityForBookingInApplication:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

