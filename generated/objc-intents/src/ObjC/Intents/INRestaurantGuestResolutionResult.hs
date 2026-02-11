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
  , successWithResolvedRestaurantGuestSelector
  , disambiguationWithRestaurantGuestsToDisambiguateSelector
  , confirmationRequiredWithRestaurantGuestToConfirmSelector


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

-- | @+ successWithResolvedRestaurantGuest:@
successWithResolvedRestaurantGuest :: IsINRestaurantGuest resolvedRestaurantGuest => resolvedRestaurantGuest -> IO (Id INRestaurantGuestResolutionResult)
successWithResolvedRestaurantGuest resolvedRestaurantGuest =
  do
    cls' <- getRequiredClass "INRestaurantGuestResolutionResult"
    withObjCPtr resolvedRestaurantGuest $ \raw_resolvedRestaurantGuest ->
      sendClassMsg cls' (mkSelector "successWithResolvedRestaurantGuest:") (retPtr retVoid) [argPtr (castPtr raw_resolvedRestaurantGuest :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithRestaurantGuestsToDisambiguate:@
disambiguationWithRestaurantGuestsToDisambiguate :: IsNSArray restaurantGuestsToDisambiguate => restaurantGuestsToDisambiguate -> IO (Id INRestaurantGuestResolutionResult)
disambiguationWithRestaurantGuestsToDisambiguate restaurantGuestsToDisambiguate =
  do
    cls' <- getRequiredClass "INRestaurantGuestResolutionResult"
    withObjCPtr restaurantGuestsToDisambiguate $ \raw_restaurantGuestsToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithRestaurantGuestsToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_restaurantGuestsToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithRestaurantGuestToConfirm:@
confirmationRequiredWithRestaurantGuestToConfirm :: IsINRestaurantGuest restaurantGuestToConfirm => restaurantGuestToConfirm -> IO (Id INRestaurantGuestResolutionResult)
confirmationRequiredWithRestaurantGuestToConfirm restaurantGuestToConfirm =
  do
    cls' <- getRequiredClass "INRestaurantGuestResolutionResult"
    withObjCPtr restaurantGuestToConfirm $ \raw_restaurantGuestToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithRestaurantGuestToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_restaurantGuestToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedRestaurantGuest:@
successWithResolvedRestaurantGuestSelector :: Selector
successWithResolvedRestaurantGuestSelector = mkSelector "successWithResolvedRestaurantGuest:"

-- | @Selector@ for @disambiguationWithRestaurantGuestsToDisambiguate:@
disambiguationWithRestaurantGuestsToDisambiguateSelector :: Selector
disambiguationWithRestaurantGuestsToDisambiguateSelector = mkSelector "disambiguationWithRestaurantGuestsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithRestaurantGuestToConfirm:@
confirmationRequiredWithRestaurantGuestToConfirmSelector :: Selector
confirmationRequiredWithRestaurantGuestToConfirmSelector = mkSelector "confirmationRequiredWithRestaurantGuestToConfirm:"

