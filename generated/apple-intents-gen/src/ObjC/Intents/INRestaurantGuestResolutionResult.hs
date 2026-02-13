{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRestaurantGuestResolutionResult@.
module ObjC.Intents.INRestaurantGuestResolutionResult
  ( INRestaurantGuestResolutionResult
  , IsINRestaurantGuestResolutionResult(..)
  , successWithResolvedRestaurantGuest
  , disambiguationWithRestaurantGuestsToDisambiguate
  , confirmationRequiredWithRestaurantGuestToConfirm
  , confirmationRequiredWithRestaurantGuestToConfirmSelector
  , disambiguationWithRestaurantGuestsToDisambiguateSelector
  , successWithResolvedRestaurantGuestSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedRestaurantGuest:@
successWithResolvedRestaurantGuest :: IsINRestaurantGuest resolvedRestaurantGuest => resolvedRestaurantGuest -> IO (Id INRestaurantGuestResolutionResult)
successWithResolvedRestaurantGuest resolvedRestaurantGuest =
  do
    cls' <- getRequiredClass "INRestaurantGuestResolutionResult"
    sendClassMessage cls' successWithResolvedRestaurantGuestSelector (toINRestaurantGuest resolvedRestaurantGuest)

-- | @+ disambiguationWithRestaurantGuestsToDisambiguate:@
disambiguationWithRestaurantGuestsToDisambiguate :: IsNSArray restaurantGuestsToDisambiguate => restaurantGuestsToDisambiguate -> IO (Id INRestaurantGuestResolutionResult)
disambiguationWithRestaurantGuestsToDisambiguate restaurantGuestsToDisambiguate =
  do
    cls' <- getRequiredClass "INRestaurantGuestResolutionResult"
    sendClassMessage cls' disambiguationWithRestaurantGuestsToDisambiguateSelector (toNSArray restaurantGuestsToDisambiguate)

-- | @+ confirmationRequiredWithRestaurantGuestToConfirm:@
confirmationRequiredWithRestaurantGuestToConfirm :: IsINRestaurantGuest restaurantGuestToConfirm => restaurantGuestToConfirm -> IO (Id INRestaurantGuestResolutionResult)
confirmationRequiredWithRestaurantGuestToConfirm restaurantGuestToConfirm =
  do
    cls' <- getRequiredClass "INRestaurantGuestResolutionResult"
    sendClassMessage cls' confirmationRequiredWithRestaurantGuestToConfirmSelector (toINRestaurantGuest restaurantGuestToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedRestaurantGuest:@
successWithResolvedRestaurantGuestSelector :: Selector '[Id INRestaurantGuest] (Id INRestaurantGuestResolutionResult)
successWithResolvedRestaurantGuestSelector = mkSelector "successWithResolvedRestaurantGuest:"

-- | @Selector@ for @disambiguationWithRestaurantGuestsToDisambiguate:@
disambiguationWithRestaurantGuestsToDisambiguateSelector :: Selector '[Id NSArray] (Id INRestaurantGuestResolutionResult)
disambiguationWithRestaurantGuestsToDisambiguateSelector = mkSelector "disambiguationWithRestaurantGuestsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithRestaurantGuestToConfirm:@
confirmationRequiredWithRestaurantGuestToConfirmSelector :: Selector '[Id INRestaurantGuest] (Id INRestaurantGuestResolutionResult)
confirmationRequiredWithRestaurantGuestToConfirmSelector = mkSelector "confirmationRequiredWithRestaurantGuestToConfirm:"

