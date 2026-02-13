{-# LANGUAGE DataKinds #-}
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
  , ratingSelector
  , rideIdentifierSelector
  , setRatingSelector
  , setTipSelector
  , tipSelector


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
init_ :: IsINSendRideFeedbackIntent inSendRideFeedbackIntent => inSendRideFeedbackIntent -> IO RawId
init_ inSendRideFeedbackIntent =
  sendOwnedMessage inSendRideFeedbackIntent initSelector

-- | @- initWithRideIdentifier:@
initWithRideIdentifier :: (IsINSendRideFeedbackIntent inSendRideFeedbackIntent, IsNSString rideIdentifier) => inSendRideFeedbackIntent -> rideIdentifier -> IO (Id INSendRideFeedbackIntent)
initWithRideIdentifier inSendRideFeedbackIntent rideIdentifier =
  sendOwnedMessage inSendRideFeedbackIntent initWithRideIdentifierSelector (toNSString rideIdentifier)

-- | @- rideIdentifier@
rideIdentifier :: IsINSendRideFeedbackIntent inSendRideFeedbackIntent => inSendRideFeedbackIntent -> IO (Id NSString)
rideIdentifier inSendRideFeedbackIntent =
  sendMessage inSendRideFeedbackIntent rideIdentifierSelector

-- | @- rating@
rating :: IsINSendRideFeedbackIntent inSendRideFeedbackIntent => inSendRideFeedbackIntent -> IO (Id NSNumber)
rating inSendRideFeedbackIntent =
  sendMessage inSendRideFeedbackIntent ratingSelector

-- | @- setRating:@
setRating :: (IsINSendRideFeedbackIntent inSendRideFeedbackIntent, IsNSNumber value) => inSendRideFeedbackIntent -> value -> IO ()
setRating inSendRideFeedbackIntent value =
  sendMessage inSendRideFeedbackIntent setRatingSelector (toNSNumber value)

-- | @- tip@
tip :: IsINSendRideFeedbackIntent inSendRideFeedbackIntent => inSendRideFeedbackIntent -> IO (Id INCurrencyAmount)
tip inSendRideFeedbackIntent =
  sendMessage inSendRideFeedbackIntent tipSelector

-- | @- setTip:@
setTip :: (IsINSendRideFeedbackIntent inSendRideFeedbackIntent, IsINCurrencyAmount value) => inSendRideFeedbackIntent -> value -> IO ()
setTip inSendRideFeedbackIntent value =
  sendMessage inSendRideFeedbackIntent setTipSelector (toINCurrencyAmount value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRideIdentifier:@
initWithRideIdentifierSelector :: Selector '[Id NSString] (Id INSendRideFeedbackIntent)
initWithRideIdentifierSelector = mkSelector "initWithRideIdentifier:"

-- | @Selector@ for @rideIdentifier@
rideIdentifierSelector :: Selector '[] (Id NSString)
rideIdentifierSelector = mkSelector "rideIdentifier"

-- | @Selector@ for @rating@
ratingSelector :: Selector '[] (Id NSNumber)
ratingSelector = mkSelector "rating"

-- | @Selector@ for @setRating:@
setRatingSelector :: Selector '[Id NSNumber] ()
setRatingSelector = mkSelector "setRating:"

-- | @Selector@ for @tip@
tipSelector :: Selector '[] (Id INCurrencyAmount)
tipSelector = mkSelector "tip"

-- | @Selector@ for @setTip:@
setTipSelector :: Selector '[Id INCurrencyAmount] ()
setTipSelector = mkSelector "setTip:"

