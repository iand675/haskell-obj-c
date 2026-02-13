{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRestaurantResolutionResult@.
module ObjC.Intents.INRestaurantResolutionResult
  ( INRestaurantResolutionResult
  , IsINRestaurantResolutionResult(..)
  , successWithResolvedRestaurant
  , disambiguationWithRestaurantsToDisambiguate
  , confirmationRequiredWithRestaurantToConfirm
  , confirmationRequiredWithRestaurantToConfirmSelector
  , disambiguationWithRestaurantsToDisambiguateSelector
  , successWithResolvedRestaurantSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedRestaurant:@
successWithResolvedRestaurant :: IsINRestaurant resolvedRestaurant => resolvedRestaurant -> IO (Id INRestaurantResolutionResult)
successWithResolvedRestaurant resolvedRestaurant =
  do
    cls' <- getRequiredClass "INRestaurantResolutionResult"
    sendClassMessage cls' successWithResolvedRestaurantSelector (toINRestaurant resolvedRestaurant)

-- | @+ disambiguationWithRestaurantsToDisambiguate:@
disambiguationWithRestaurantsToDisambiguate :: IsNSArray restaurantsToDisambiguate => restaurantsToDisambiguate -> IO (Id INRestaurantResolutionResult)
disambiguationWithRestaurantsToDisambiguate restaurantsToDisambiguate =
  do
    cls' <- getRequiredClass "INRestaurantResolutionResult"
    sendClassMessage cls' disambiguationWithRestaurantsToDisambiguateSelector (toNSArray restaurantsToDisambiguate)

-- | @+ confirmationRequiredWithRestaurantToConfirm:@
confirmationRequiredWithRestaurantToConfirm :: IsINRestaurant restaurantToConfirm => restaurantToConfirm -> IO (Id INRestaurantResolutionResult)
confirmationRequiredWithRestaurantToConfirm restaurantToConfirm =
  do
    cls' <- getRequiredClass "INRestaurantResolutionResult"
    sendClassMessage cls' confirmationRequiredWithRestaurantToConfirmSelector (toINRestaurant restaurantToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedRestaurant:@
successWithResolvedRestaurantSelector :: Selector '[Id INRestaurant] (Id INRestaurantResolutionResult)
successWithResolvedRestaurantSelector = mkSelector "successWithResolvedRestaurant:"

-- | @Selector@ for @disambiguationWithRestaurantsToDisambiguate:@
disambiguationWithRestaurantsToDisambiguateSelector :: Selector '[Id NSArray] (Id INRestaurantResolutionResult)
disambiguationWithRestaurantsToDisambiguateSelector = mkSelector "disambiguationWithRestaurantsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithRestaurantToConfirm:@
confirmationRequiredWithRestaurantToConfirmSelector :: Selector '[Id INRestaurant] (Id INRestaurantResolutionResult)
confirmationRequiredWithRestaurantToConfirmSelector = mkSelector "confirmationRequiredWithRestaurantToConfirm:"

