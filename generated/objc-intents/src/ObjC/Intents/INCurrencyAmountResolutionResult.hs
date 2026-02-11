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
  , successWithResolvedCurrencyAmountSelector
  , disambiguationWithCurrencyAmountsToDisambiguateSelector
  , confirmationRequiredWithCurrencyAmountToConfirmSelector


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

-- | @+ successWithResolvedCurrencyAmount:@
successWithResolvedCurrencyAmount :: IsINCurrencyAmount resolvedCurrencyAmount => resolvedCurrencyAmount -> IO (Id INCurrencyAmountResolutionResult)
successWithResolvedCurrencyAmount resolvedCurrencyAmount =
  do
    cls' <- getRequiredClass "INCurrencyAmountResolutionResult"
    withObjCPtr resolvedCurrencyAmount $ \raw_resolvedCurrencyAmount ->
      sendClassMsg cls' (mkSelector "successWithResolvedCurrencyAmount:") (retPtr retVoid) [argPtr (castPtr raw_resolvedCurrencyAmount :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithCurrencyAmountsToDisambiguate:@
disambiguationWithCurrencyAmountsToDisambiguate :: IsNSArray currencyAmountsToDisambiguate => currencyAmountsToDisambiguate -> IO (Id INCurrencyAmountResolutionResult)
disambiguationWithCurrencyAmountsToDisambiguate currencyAmountsToDisambiguate =
  do
    cls' <- getRequiredClass "INCurrencyAmountResolutionResult"
    withObjCPtr currencyAmountsToDisambiguate $ \raw_currencyAmountsToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithCurrencyAmountsToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_currencyAmountsToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithCurrencyAmountToConfirm:@
confirmationRequiredWithCurrencyAmountToConfirm :: IsINCurrencyAmount currencyAmountToConfirm => currencyAmountToConfirm -> IO (Id INCurrencyAmountResolutionResult)
confirmationRequiredWithCurrencyAmountToConfirm currencyAmountToConfirm =
  do
    cls' <- getRequiredClass "INCurrencyAmountResolutionResult"
    withObjCPtr currencyAmountToConfirm $ \raw_currencyAmountToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithCurrencyAmountToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_currencyAmountToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCurrencyAmount:@
successWithResolvedCurrencyAmountSelector :: Selector
successWithResolvedCurrencyAmountSelector = mkSelector "successWithResolvedCurrencyAmount:"

-- | @Selector@ for @disambiguationWithCurrencyAmountsToDisambiguate:@
disambiguationWithCurrencyAmountsToDisambiguateSelector :: Selector
disambiguationWithCurrencyAmountsToDisambiguateSelector = mkSelector "disambiguationWithCurrencyAmountsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithCurrencyAmountToConfirm:@
confirmationRequiredWithCurrencyAmountToConfirmSelector :: Selector
confirmationRequiredWithCurrencyAmountToConfirmSelector = mkSelector "confirmationRequiredWithCurrencyAmountToConfirm:"

