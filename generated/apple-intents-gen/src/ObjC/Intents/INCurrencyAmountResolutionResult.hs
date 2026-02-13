{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCurrencyAmountResolutionResult@.
module ObjC.Intents.INCurrencyAmountResolutionResult
  ( INCurrencyAmountResolutionResult
  , IsINCurrencyAmountResolutionResult(..)
  , successWithResolvedCurrencyAmount
  , disambiguationWithCurrencyAmountsToDisambiguate
  , confirmationRequiredWithCurrencyAmountToConfirm
  , confirmationRequiredWithCurrencyAmountToConfirmSelector
  , disambiguationWithCurrencyAmountsToDisambiguateSelector
  , successWithResolvedCurrencyAmountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedCurrencyAmount:@
successWithResolvedCurrencyAmount :: IsINCurrencyAmount resolvedCurrencyAmount => resolvedCurrencyAmount -> IO (Id INCurrencyAmountResolutionResult)
successWithResolvedCurrencyAmount resolvedCurrencyAmount =
  do
    cls' <- getRequiredClass "INCurrencyAmountResolutionResult"
    sendClassMessage cls' successWithResolvedCurrencyAmountSelector (toINCurrencyAmount resolvedCurrencyAmount)

-- | @+ disambiguationWithCurrencyAmountsToDisambiguate:@
disambiguationWithCurrencyAmountsToDisambiguate :: IsNSArray currencyAmountsToDisambiguate => currencyAmountsToDisambiguate -> IO (Id INCurrencyAmountResolutionResult)
disambiguationWithCurrencyAmountsToDisambiguate currencyAmountsToDisambiguate =
  do
    cls' <- getRequiredClass "INCurrencyAmountResolutionResult"
    sendClassMessage cls' disambiguationWithCurrencyAmountsToDisambiguateSelector (toNSArray currencyAmountsToDisambiguate)

-- | @+ confirmationRequiredWithCurrencyAmountToConfirm:@
confirmationRequiredWithCurrencyAmountToConfirm :: IsINCurrencyAmount currencyAmountToConfirm => currencyAmountToConfirm -> IO (Id INCurrencyAmountResolutionResult)
confirmationRequiredWithCurrencyAmountToConfirm currencyAmountToConfirm =
  do
    cls' <- getRequiredClass "INCurrencyAmountResolutionResult"
    sendClassMessage cls' confirmationRequiredWithCurrencyAmountToConfirmSelector (toINCurrencyAmount currencyAmountToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCurrencyAmount:@
successWithResolvedCurrencyAmountSelector :: Selector '[Id INCurrencyAmount] (Id INCurrencyAmountResolutionResult)
successWithResolvedCurrencyAmountSelector = mkSelector "successWithResolvedCurrencyAmount:"

-- | @Selector@ for @disambiguationWithCurrencyAmountsToDisambiguate:@
disambiguationWithCurrencyAmountsToDisambiguateSelector :: Selector '[Id NSArray] (Id INCurrencyAmountResolutionResult)
disambiguationWithCurrencyAmountsToDisambiguateSelector = mkSelector "disambiguationWithCurrencyAmountsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithCurrencyAmountToConfirm:@
confirmationRequiredWithCurrencyAmountToConfirmSelector :: Selector '[Id INCurrencyAmount] (Id INCurrencyAmountResolutionResult)
confirmationRequiredWithCurrencyAmountToConfirmSelector = mkSelector "confirmationRequiredWithCurrencyAmountToConfirm:"

