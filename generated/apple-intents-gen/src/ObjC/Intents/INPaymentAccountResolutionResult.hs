{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPaymentAccountResolutionResult@.
module ObjC.Intents.INPaymentAccountResolutionResult
  ( INPaymentAccountResolutionResult
  , IsINPaymentAccountResolutionResult(..)
  , successWithResolvedPaymentAccount
  , disambiguationWithPaymentAccountsToDisambiguate
  , confirmationRequiredWithPaymentAccountToConfirm
  , confirmationRequiredWithPaymentAccountToConfirmSelector
  , disambiguationWithPaymentAccountsToDisambiguateSelector
  , successWithResolvedPaymentAccountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedPaymentAccount:@
successWithResolvedPaymentAccount :: IsINPaymentAccount resolvedPaymentAccount => resolvedPaymentAccount -> IO (Id INPaymentAccountResolutionResult)
successWithResolvedPaymentAccount resolvedPaymentAccount =
  do
    cls' <- getRequiredClass "INPaymentAccountResolutionResult"
    sendClassMessage cls' successWithResolvedPaymentAccountSelector (toINPaymentAccount resolvedPaymentAccount)

-- | @+ disambiguationWithPaymentAccountsToDisambiguate:@
disambiguationWithPaymentAccountsToDisambiguate :: IsNSArray paymentAccountsToDisambiguate => paymentAccountsToDisambiguate -> IO (Id INPaymentAccountResolutionResult)
disambiguationWithPaymentAccountsToDisambiguate paymentAccountsToDisambiguate =
  do
    cls' <- getRequiredClass "INPaymentAccountResolutionResult"
    sendClassMessage cls' disambiguationWithPaymentAccountsToDisambiguateSelector (toNSArray paymentAccountsToDisambiguate)

-- | @+ confirmationRequiredWithPaymentAccountToConfirm:@
confirmationRequiredWithPaymentAccountToConfirm :: IsINPaymentAccount paymentAccountToConfirm => paymentAccountToConfirm -> IO (Id INPaymentAccountResolutionResult)
confirmationRequiredWithPaymentAccountToConfirm paymentAccountToConfirm =
  do
    cls' <- getRequiredClass "INPaymentAccountResolutionResult"
    sendClassMessage cls' confirmationRequiredWithPaymentAccountToConfirmSelector (toINPaymentAccount paymentAccountToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedPaymentAccount:@
successWithResolvedPaymentAccountSelector :: Selector '[Id INPaymentAccount] (Id INPaymentAccountResolutionResult)
successWithResolvedPaymentAccountSelector = mkSelector "successWithResolvedPaymentAccount:"

-- | @Selector@ for @disambiguationWithPaymentAccountsToDisambiguate:@
disambiguationWithPaymentAccountsToDisambiguateSelector :: Selector '[Id NSArray] (Id INPaymentAccountResolutionResult)
disambiguationWithPaymentAccountsToDisambiguateSelector = mkSelector "disambiguationWithPaymentAccountsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithPaymentAccountToConfirm:@
confirmationRequiredWithPaymentAccountToConfirmSelector :: Selector '[Id INPaymentAccount] (Id INPaymentAccountResolutionResult)
confirmationRequiredWithPaymentAccountToConfirmSelector = mkSelector "confirmationRequiredWithPaymentAccountToConfirm:"

