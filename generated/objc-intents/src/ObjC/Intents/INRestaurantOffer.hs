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
  , offerTitleTextSelector
  , setOfferTitleTextSelector
  , offerDetailTextSelector
  , setOfferDetailTextSelector
  , offerIdentifierSelector
  , setOfferIdentifierSelector


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

-- | @- offerTitleText@
offerTitleText :: IsINRestaurantOffer inRestaurantOffer => inRestaurantOffer -> IO (Id NSString)
offerTitleText inRestaurantOffer  =
  sendMsg inRestaurantOffer (mkSelector "offerTitleText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOfferTitleText:@
setOfferTitleText :: (IsINRestaurantOffer inRestaurantOffer, IsNSString value) => inRestaurantOffer -> value -> IO ()
setOfferTitleText inRestaurantOffer  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRestaurantOffer (mkSelector "setOfferTitleText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- offerDetailText@
offerDetailText :: IsINRestaurantOffer inRestaurantOffer => inRestaurantOffer -> IO (Id NSString)
offerDetailText inRestaurantOffer  =
  sendMsg inRestaurantOffer (mkSelector "offerDetailText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOfferDetailText:@
setOfferDetailText :: (IsINRestaurantOffer inRestaurantOffer, IsNSString value) => inRestaurantOffer -> value -> IO ()
setOfferDetailText inRestaurantOffer  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRestaurantOffer (mkSelector "setOfferDetailText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- offerIdentifier@
offerIdentifier :: IsINRestaurantOffer inRestaurantOffer => inRestaurantOffer -> IO (Id NSString)
offerIdentifier inRestaurantOffer  =
  sendMsg inRestaurantOffer (mkSelector "offerIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOfferIdentifier:@
setOfferIdentifier :: (IsINRestaurantOffer inRestaurantOffer, IsNSString value) => inRestaurantOffer -> value -> IO ()
setOfferIdentifier inRestaurantOffer  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRestaurantOffer (mkSelector "setOfferIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @offerTitleText@
offerTitleTextSelector :: Selector
offerTitleTextSelector = mkSelector "offerTitleText"

-- | @Selector@ for @setOfferTitleText:@
setOfferTitleTextSelector :: Selector
setOfferTitleTextSelector = mkSelector "setOfferTitleText:"

-- | @Selector@ for @offerDetailText@
offerDetailTextSelector :: Selector
offerDetailTextSelector = mkSelector "offerDetailText"

-- | @Selector@ for @setOfferDetailText:@
setOfferDetailTextSelector :: Selector
setOfferDetailTextSelector = mkSelector "setOfferDetailText:"

-- | @Selector@ for @offerIdentifier@
offerIdentifierSelector :: Selector
offerIdentifierSelector = mkSelector "offerIdentifier"

-- | @Selector@ for @setOfferIdentifier:@
setOfferIdentifierSelector :: Selector
setOfferIdentifierSelector = mkSelector "setOfferIdentifier:"

