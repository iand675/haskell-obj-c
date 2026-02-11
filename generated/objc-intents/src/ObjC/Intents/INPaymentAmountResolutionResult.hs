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
  , successWithResolvedPaymentAmountSelector
  , disambiguationWithPaymentAmountsToDisambiguateSelector
  , confirmationRequiredWithPaymentAmountToConfirmSelector


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

-- | @+ successWithResolvedPaymentAmount:@
successWithResolvedPaymentAmount :: IsINPaymentAmount resolvedPaymentAmount => resolvedPaymentAmount -> IO (Id INPaymentAmountResolutionResult)
successWithResolvedPaymentAmount resolvedPaymentAmount =
  do
    cls' <- getRequiredClass "INPaymentAmountResolutionResult"
    withObjCPtr resolvedPaymentAmount $ \raw_resolvedPaymentAmount ->
      sendClassMsg cls' (mkSelector "successWithResolvedPaymentAmount:") (retPtr retVoid) [argPtr (castPtr raw_resolvedPaymentAmount :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithPaymentAmountsToDisambiguate:@
disambiguationWithPaymentAmountsToDisambiguate :: IsNSArray paymentAmountsToDisambiguate => paymentAmountsToDisambiguate -> IO (Id INPaymentAmountResolutionResult)
disambiguationWithPaymentAmountsToDisambiguate paymentAmountsToDisambiguate =
  do
    cls' <- getRequiredClass "INPaymentAmountResolutionResult"
    withObjCPtr paymentAmountsToDisambiguate $ \raw_paymentAmountsToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithPaymentAmountsToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_paymentAmountsToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithPaymentAmountToConfirm:@
confirmationRequiredWithPaymentAmountToConfirm :: IsINPaymentAmount paymentAmountToConfirm => paymentAmountToConfirm -> IO (Id INPaymentAmountResolutionResult)
confirmationRequiredWithPaymentAmountToConfirm paymentAmountToConfirm =
  do
    cls' <- getRequiredClass "INPaymentAmountResolutionResult"
    withObjCPtr paymentAmountToConfirm $ \raw_paymentAmountToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithPaymentAmountToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_paymentAmountToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedPaymentAmount:@
successWithResolvedPaymentAmountSelector :: Selector
successWithResolvedPaymentAmountSelector = mkSelector "successWithResolvedPaymentAmount:"

-- | @Selector@ for @disambiguationWithPaymentAmountsToDisambiguate:@
disambiguationWithPaymentAmountsToDisambiguateSelector :: Selector
disambiguationWithPaymentAmountsToDisambiguateSelector = mkSelector "disambiguationWithPaymentAmountsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithPaymentAmountToConfirm:@
confirmationRequiredWithPaymentAmountToConfirmSelector :: Selector
confirmationRequiredWithPaymentAmountToConfirmSelector = mkSelector "confirmationRequiredWithPaymentAmountToConfirm:"

