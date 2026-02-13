{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRestaurantOffer@.
module ObjC.Intents.INRestaurantOffer
  ( INRestaurantOffer
  , IsINRestaurantOffer(..)
  , offerTitleText
  , setOfferTitleText
  , offerDetailText
  , setOfferDetailText
  , offerIdentifier
  , setOfferIdentifier
  , offerDetailTextSelector
  , offerIdentifierSelector
  , offerTitleTextSelector
  , setOfferDetailTextSelector
  , setOfferIdentifierSelector
  , setOfferTitleTextSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- offerTitleText@
offerTitleText :: IsINRestaurantOffer inRestaurantOffer => inRestaurantOffer -> IO (Id NSString)
offerTitleText inRestaurantOffer =
  sendMessage inRestaurantOffer offerTitleTextSelector

-- | @- setOfferTitleText:@
setOfferTitleText :: (IsINRestaurantOffer inRestaurantOffer, IsNSString value) => inRestaurantOffer -> value -> IO ()
setOfferTitleText inRestaurantOffer value =
  sendMessage inRestaurantOffer setOfferTitleTextSelector (toNSString value)

-- | @- offerDetailText@
offerDetailText :: IsINRestaurantOffer inRestaurantOffer => inRestaurantOffer -> IO (Id NSString)
offerDetailText inRestaurantOffer =
  sendMessage inRestaurantOffer offerDetailTextSelector

-- | @- setOfferDetailText:@
setOfferDetailText :: (IsINRestaurantOffer inRestaurantOffer, IsNSString value) => inRestaurantOffer -> value -> IO ()
setOfferDetailText inRestaurantOffer value =
  sendMessage inRestaurantOffer setOfferDetailTextSelector (toNSString value)

-- | @- offerIdentifier@
offerIdentifier :: IsINRestaurantOffer inRestaurantOffer => inRestaurantOffer -> IO (Id NSString)
offerIdentifier inRestaurantOffer =
  sendMessage inRestaurantOffer offerIdentifierSelector

-- | @- setOfferIdentifier:@
setOfferIdentifier :: (IsINRestaurantOffer inRestaurantOffer, IsNSString value) => inRestaurantOffer -> value -> IO ()
setOfferIdentifier inRestaurantOffer value =
  sendMessage inRestaurantOffer setOfferIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @offerTitleText@
offerTitleTextSelector :: Selector '[] (Id NSString)
offerTitleTextSelector = mkSelector "offerTitleText"

-- | @Selector@ for @setOfferTitleText:@
setOfferTitleTextSelector :: Selector '[Id NSString] ()
setOfferTitleTextSelector = mkSelector "setOfferTitleText:"

-- | @Selector@ for @offerDetailText@
offerDetailTextSelector :: Selector '[] (Id NSString)
offerDetailTextSelector = mkSelector "offerDetailText"

-- | @Selector@ for @setOfferDetailText:@
setOfferDetailTextSelector :: Selector '[Id NSString] ()
setOfferDetailTextSelector = mkSelector "setOfferDetailText:"

-- | @Selector@ for @offerIdentifier@
offerIdentifierSelector :: Selector '[] (Id NSString)
offerIdentifierSelector = mkSelector "offerIdentifier"

-- | @Selector@ for @setOfferIdentifier:@
setOfferIdentifierSelector :: Selector '[Id NSString] ()
setOfferIdentifierSelector = mkSelector "setOfferIdentifier:"

