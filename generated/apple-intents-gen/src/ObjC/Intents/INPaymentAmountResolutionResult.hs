{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPaymentAmountResolutionResult@.
module ObjC.Intents.INPaymentAmountResolutionResult
  ( INPaymentAmountResolutionResult
  , IsINPaymentAmountResolutionResult(..)
  , successWithResolvedPaymentAmount
  , disambiguationWithPaymentAmountsToDisambiguate
  , confirmationRequiredWithPaymentAmountToConfirm
  , confirmationRequiredWithPaymentAmountToConfirmSelector
  , disambiguationWithPaymentAmountsToDisambiguateSelector
  , successWithResolvedPaymentAmountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedPaymentAmount:@
successWithResolvedPaymentAmount :: IsINPaymentAmount resolvedPaymentAmount => resolvedPaymentAmount -> IO (Id INPaymentAmountResolutionResult)
successWithResolvedPaymentAmount resolvedPaymentAmount =
  do
    cls' <- getRequiredClass "INPaymentAmountResolutionResult"
    sendClassMessage cls' successWithResolvedPaymentAmountSelector (toINPaymentAmount resolvedPaymentAmount)

-- | @+ disambiguationWithPaymentAmountsToDisambiguate:@
disambiguationWithPaymentAmountsToDisambiguate :: IsNSArray paymentAmountsToDisambiguate => paymentAmountsToDisambiguate -> IO (Id INPaymentAmountResolutionResult)
disambiguationWithPaymentAmountsToDisambiguate paymentAmountsToDisambiguate =
  do
    cls' <- getRequiredClass "INPaymentAmountResolutionResult"
    sendClassMessage cls' disambiguationWithPaymentAmountsToDisambiguateSelector (toNSArray paymentAmountsToDisambiguate)

-- | @+ confirmationRequiredWithPaymentAmountToConfirm:@
confirmationRequiredWithPaymentAmountToConfirm :: IsINPaymentAmount paymentAmountToConfirm => paymentAmountToConfirm -> IO (Id INPaymentAmountResolutionResult)
confirmationRequiredWithPaymentAmountToConfirm paymentAmountToConfirm =
  do
    cls' <- getRequiredClass "INPaymentAmountResolutionResult"
    sendClassMessage cls' confirmationRequiredWithPaymentAmountToConfirmSelector (toINPaymentAmount paymentAmountToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedPaymentAmount:@
successWithResolvedPaymentAmountSelector :: Selector '[Id INPaymentAmount] (Id INPaymentAmountResolutionResult)
successWithResolvedPaymentAmountSelector = mkSelector "successWithResolvedPaymentAmount:"

-- | @Selector@ for @disambiguationWithPaymentAmountsToDisambiguate:@
disambiguationWithPaymentAmountsToDisambiguateSelector :: Selector '[Id NSArray] (Id INPaymentAmountResolutionResult)
disambiguationWithPaymentAmountsToDisambiguateSelector = mkSelector "disambiguationWithPaymentAmountsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithPaymentAmountToConfirm:@
confirmationRequiredWithPaymentAmountToConfirmSelector :: Selector '[Id INPaymentAmount] (Id INPaymentAmountResolutionResult)
confirmationRequiredWithPaymentAmountToConfirmSelector = mkSelector "confirmationRequiredWithPaymentAmountToConfirm:"

