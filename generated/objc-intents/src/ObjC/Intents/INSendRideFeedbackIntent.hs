{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSendRideFeedbackIntent@.
module ObjC.Intents.INSendRideFeedbackIntent
  ( INSendRideFeedbackIntent
  , IsINSendRideFeedbackIntent(..)
  , init_
  , initWithRideIdentifier
  , rideIdentifier
  , rating
  , setRating
  , tip
  , setTip
  , initSelector
  , initWithRideIdentifierSelector
  , rideIdentifierSelector
  , ratingSelector
  , setRatingSelector
  , tipSelector
  , setTipSelector


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
init_ :: IsINSendRideFeedbackIntent inSendRideFeedbackIntent => inSendRideFeedbackIntent -> IO RawId
init_ inSendRideFeedbackIntent  =
  fmap (RawId . castPtr) $ sendMsg inSendRideFeedbackIntent (mkSelector "init") (retPtr retVoid) []

-- | @- initWithRideIdentifier:@
initWithRideIdentifier :: (IsINSendRideFeedbackIntent inSendRideFeedbackIntent, IsNSString rideIdentifier) => inSendRideFeedbackIntent -> rideIdentifier -> IO (Id INSendRideFeedbackIntent)
initWithRideIdentifier inSendRideFeedbackIntent  rideIdentifier =
withObjCPtr rideIdentifier $ \raw_rideIdentifier ->
    sendMsg inSendRideFeedbackIntent (mkSelector "initWithRideIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_rideIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- rideIdentifier@
rideIdentifier :: IsINSendRideFeedbackIntent inSendRideFeedbackIntent => inSendRideFeedbackIntent -> IO (Id NSString)
rideIdentifier inSendRideFeedbackIntent  =
  sendMsg inSendRideFeedbackIntent (mkSelector "rideIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rating@
rating :: IsINSendRideFeedbackIntent inSendRideFeedbackIntent => inSendRideFeedbackIntent -> IO (Id NSNumber)
rating inSendRideFeedbackIntent  =
  sendMsg inSendRideFeedbackIntent (mkSelector "rating") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRating:@
setRating :: (IsINSendRideFeedbackIntent inSendRideFeedbackIntent, IsNSNumber value) => inSendRideFeedbackIntent -> value -> IO ()
setRating inSendRideFeedbackIntent  value =
withObjCPtr value $ \raw_value ->
    sendMsg inSendRideFeedbackIntent (mkSelector "setRating:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tip@
tip :: IsINSendRideFeedbackIntent inSendRideFeedbackIntent => inSendRideFeedbackIntent -> IO (Id INCurrencyAmount)
tip inSendRideFeedbackIntent  =
  sendMsg inSendRideFeedbackIntent (mkSelector "tip") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTip:@
setTip :: (IsINSendRideFeedbackIntent inSendRideFeedbackIntent, IsINCurrencyAmount value) => inSendRideFeedbackIntent -> value -> IO ()
setTip inSendRideFeedbackIntent  value =
withObjCPtr value $ \raw_value ->
    sendMsg inSendRideFeedbackIntent (mkSelector "setTip:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRideIdentifier:@
initWithRideIdentifierSelector :: Selector
initWithRideIdentifierSelector = mkSelector "initWithRideIdentifier:"

-- | @Selector@ for @rideIdentifier@
rideIdentifierSelector :: Selector
rideIdentifierSelector = mkSelector "rideIdentifier"

-- | @Selector@ for @rating@
ratingSelector :: Selector
ratingSelector = mkSelector "rating"

-- | @Selector@ for @setRating:@
setRatingSelector :: Selector
setRatingSelector = mkSelector "setRating:"

-- | @Selector@ for @tip@
tipSelector :: Selector
tipSelector = mkSelector "tip"

-- | @Selector@ for @setTip:@
setTipSelector :: Selector
setTipSelector = mkSelector "setTip:"

