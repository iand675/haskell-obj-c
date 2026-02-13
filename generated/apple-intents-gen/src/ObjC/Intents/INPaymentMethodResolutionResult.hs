{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPaymentMethodResolutionResult@.
module ObjC.Intents.INPaymentMethodResolutionResult
  ( INPaymentMethodResolutionResult
  , IsINPaymentMethodResolutionResult(..)
  , successWithResolvedPaymentMethod
  , disambiguationWithPaymentMethodsToDisambiguate
  , confirmationRequiredWithPaymentMethodToConfirm
  , confirmationRequiredWithPaymentMethodToConfirmSelector
  , disambiguationWithPaymentMethodsToDisambiguateSelector
  , successWithResolvedPaymentMethodSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedPaymentMethod:@
successWithResolvedPaymentMethod :: IsINPaymentMethod resolvedPaymentMethod => resolvedPaymentMethod -> IO (Id INPaymentMethodResolutionResult)
successWithResolvedPaymentMethod resolvedPaymentMethod =
  do
    cls' <- getRequiredClass "INPaymentMethodResolutionResult"
    sendClassMessage cls' successWithResolvedPaymentMethodSelector (toINPaymentMethod resolvedPaymentMethod)

-- | @+ disambiguationWithPaymentMethodsToDisambiguate:@
disambiguationWithPaymentMethodsToDisambiguate :: IsNSArray paymentMethodsToDisambiguate => paymentMethodsToDisambiguate -> IO (Id INPaymentMethodResolutionResult)
disambiguationWithPaymentMethodsToDisambiguate paymentMethodsToDisambiguate =
  do
    cls' <- getRequiredClass "INPaymentMethodResolutionResult"
    sendClassMessage cls' disambiguationWithPaymentMethodsToDisambiguateSelector (toNSArray paymentMethodsToDisambiguate)

-- | @+ confirmationRequiredWithPaymentMethodToConfirm:@
confirmationRequiredWithPaymentMethodToConfirm :: IsINPaymentMethod paymentMethodToConfirm => paymentMethodToConfirm -> IO (Id INPaymentMethodResolutionResult)
confirmationRequiredWithPaymentMethodToConfirm paymentMethodToConfirm =
  do
    cls' <- getRequiredClass "INPaymentMethodResolutionResult"
    sendClassMessage cls' confirmationRequiredWithPaymentMethodToConfirmSelector (toINPaymentMethod paymentMethodToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedPaymentMethod:@
successWithResolvedPaymentMethodSelector :: Selector '[Id INPaymentMethod] (Id INPaymentMethodResolutionResult)
successWithResolvedPaymentMethodSelector = mkSelector "successWithResolvedPaymentMethod:"

-- | @Selector@ for @disambiguationWithPaymentMethodsToDisambiguate:@
disambiguationWithPaymentMethodsToDisambiguateSelector :: Selector '[Id NSArray] (Id INPaymentMethodResolutionResult)
disambiguationWithPaymentMethodsToDisambiguateSelector = mkSelector "disambiguationWithPaymentMethodsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithPaymentMethodToConfirm:@
confirmationRequiredWithPaymentMethodToConfirmSelector :: Selector '[Id INPaymentMethod] (Id INPaymentMethodResolutionResult)
confirmationRequiredWithPaymentMethodToConfirmSelector = mkSelector "confirmationRequiredWithPaymentMethodToConfirm:"

