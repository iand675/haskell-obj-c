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
  , successWithResolvedPaymentMethodSelector
  , disambiguationWithPaymentMethodsToDisambiguateSelector
  , confirmationRequiredWithPaymentMethodToConfirmSelector


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

-- | @+ successWithResolvedPaymentMethod:@
successWithResolvedPaymentMethod :: IsINPaymentMethod resolvedPaymentMethod => resolvedPaymentMethod -> IO (Id INPaymentMethodResolutionResult)
successWithResolvedPaymentMethod resolvedPaymentMethod =
  do
    cls' <- getRequiredClass "INPaymentMethodResolutionResult"
    withObjCPtr resolvedPaymentMethod $ \raw_resolvedPaymentMethod ->
      sendClassMsg cls' (mkSelector "successWithResolvedPaymentMethod:") (retPtr retVoid) [argPtr (castPtr raw_resolvedPaymentMethod :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithPaymentMethodsToDisambiguate:@
disambiguationWithPaymentMethodsToDisambiguate :: IsNSArray paymentMethodsToDisambiguate => paymentMethodsToDisambiguate -> IO (Id INPaymentMethodResolutionResult)
disambiguationWithPaymentMethodsToDisambiguate paymentMethodsToDisambiguate =
  do
    cls' <- getRequiredClass "INPaymentMethodResolutionResult"
    withObjCPtr paymentMethodsToDisambiguate $ \raw_paymentMethodsToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithPaymentMethodsToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_paymentMethodsToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithPaymentMethodToConfirm:@
confirmationRequiredWithPaymentMethodToConfirm :: IsINPaymentMethod paymentMethodToConfirm => paymentMethodToConfirm -> IO (Id INPaymentMethodResolutionResult)
confirmationRequiredWithPaymentMethodToConfirm paymentMethodToConfirm =
  do
    cls' <- getRequiredClass "INPaymentMethodResolutionResult"
    withObjCPtr paymentMethodToConfirm $ \raw_paymentMethodToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithPaymentMethodToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_paymentMethodToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedPaymentMethod:@
successWithResolvedPaymentMethodSelector :: Selector
successWithResolvedPaymentMethodSelector = mkSelector "successWithResolvedPaymentMethod:"

-- | @Selector@ for @disambiguationWithPaymentMethodsToDisambiguate:@
disambiguationWithPaymentMethodsToDisambiguateSelector :: Selector
disambiguationWithPaymentMethodsToDisambiguateSelector = mkSelector "disambiguationWithPaymentMethodsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithPaymentMethodToConfirm:@
confirmationRequiredWithPaymentMethodToConfirmSelector :: Selector
confirmationRequiredWithPaymentMethodToConfirmSelector = mkSelector "confirmationRequiredWithPaymentMethodToConfirm:"

