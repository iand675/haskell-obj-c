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
  , successWithResolvedRestaurantSelector
  , disambiguationWithRestaurantsToDisambiguateSelector
  , confirmationRequiredWithRestaurantToConfirmSelector


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

-- | @+ successWithResolvedRestaurant:@
successWithResolvedRestaurant :: IsINRestaurant resolvedRestaurant => resolvedRestaurant -> IO (Id INRestaurantResolutionResult)
successWithResolvedRestaurant resolvedRestaurant =
  do
    cls' <- getRequiredClass "INRestaurantResolutionResult"
    withObjCPtr resolvedRestaurant $ \raw_resolvedRestaurant ->
      sendClassMsg cls' (mkSelector "successWithResolvedRestaurant:") (retPtr retVoid) [argPtr (castPtr raw_resolvedRestaurant :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithRestaurantsToDisambiguate:@
disambiguationWithRestaurantsToDisambiguate :: IsNSArray restaurantsToDisambiguate => restaurantsToDisambiguate -> IO (Id INRestaurantResolutionResult)
disambiguationWithRestaurantsToDisambiguate restaurantsToDisambiguate =
  do
    cls' <- getRequiredClass "INRestaurantResolutionResult"
    withObjCPtr restaurantsToDisambiguate $ \raw_restaurantsToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithRestaurantsToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_restaurantsToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithRestaurantToConfirm:@
confirmationRequiredWithRestaurantToConfirm :: IsINRestaurant restaurantToConfirm => restaurantToConfirm -> IO (Id INRestaurantResolutionResult)
confirmationRequiredWithRestaurantToConfirm restaurantToConfirm =
  do
    cls' <- getRequiredClass "INRestaurantResolutionResult"
    withObjCPtr restaurantToConfirm $ \raw_restaurantToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithRestaurantToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_restaurantToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedRestaurant:@
successWithResolvedRestaurantSelector :: Selector
successWithResolvedRestaurantSelector = mkSelector "successWithResolvedRestaurant:"

-- | @Selector@ for @disambiguationWithRestaurantsToDisambiguate:@
disambiguationWithRestaurantsToDisambiguateSelector :: Selector
disambiguationWithRestaurantsToDisambiguateSelector = mkSelector "disambiguationWithRestaurantsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithRestaurantToConfirm:@
confirmationRequiredWithRestaurantToConfirmSelector :: Selector
confirmationRequiredWithRestaurantToConfirmSelector = mkSelector "confirmationRequiredWithRestaurantToConfirm:"

