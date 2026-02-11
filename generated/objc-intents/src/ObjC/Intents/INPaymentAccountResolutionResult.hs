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
  , successWithResolvedPaymentAccountSelector
  , disambiguationWithPaymentAccountsToDisambiguateSelector
  , confirmationRequiredWithPaymentAccountToConfirmSelector


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

-- | @+ successWithResolvedPaymentAccount:@
successWithResolvedPaymentAccount :: IsINPaymentAccount resolvedPaymentAccount => resolvedPaymentAccount -> IO (Id INPaymentAccountResolutionResult)
successWithResolvedPaymentAccount resolvedPaymentAccount =
  do
    cls' <- getRequiredClass "INPaymentAccountResolutionResult"
    withObjCPtr resolvedPaymentAccount $ \raw_resolvedPaymentAccount ->
      sendClassMsg cls' (mkSelector "successWithResolvedPaymentAccount:") (retPtr retVoid) [argPtr (castPtr raw_resolvedPaymentAccount :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithPaymentAccountsToDisambiguate:@
disambiguationWithPaymentAccountsToDisambiguate :: IsNSArray paymentAccountsToDisambiguate => paymentAccountsToDisambiguate -> IO (Id INPaymentAccountResolutionResult)
disambiguationWithPaymentAccountsToDisambiguate paymentAccountsToDisambiguate =
  do
    cls' <- getRequiredClass "INPaymentAccountResolutionResult"
    withObjCPtr paymentAccountsToDisambiguate $ \raw_paymentAccountsToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithPaymentAccountsToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_paymentAccountsToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithPaymentAccountToConfirm:@
confirmationRequiredWithPaymentAccountToConfirm :: IsINPaymentAccount paymentAccountToConfirm => paymentAccountToConfirm -> IO (Id INPaymentAccountResolutionResult)
confirmationRequiredWithPaymentAccountToConfirm paymentAccountToConfirm =
  do
    cls' <- getRequiredClass "INPaymentAccountResolutionResult"
    withObjCPtr paymentAccountToConfirm $ \raw_paymentAccountToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithPaymentAccountToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_paymentAccountToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedPaymentAccount:@
successWithResolvedPaymentAccountSelector :: Selector
successWithResolvedPaymentAccountSelector = mkSelector "successWithResolvedPaymentAccount:"

-- | @Selector@ for @disambiguationWithPaymentAccountsToDisambiguate:@
disambiguationWithPaymentAccountsToDisambiguateSelector :: Selector
disambiguationWithPaymentAccountsToDisambiguateSelector = mkSelector "disambiguationWithPaymentAccountsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithPaymentAccountToConfirm:@
confirmationRequiredWithPaymentAccountToConfirmSelector :: Selector
confirmationRequiredWithPaymentAccountToConfirmSelector = mkSelector "confirmationRequiredWithPaymentAccountToConfirm:"

