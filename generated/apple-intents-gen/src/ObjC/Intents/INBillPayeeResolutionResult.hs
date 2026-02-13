{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INBillPayeeResolutionResult@.
module ObjC.Intents.INBillPayeeResolutionResult
  ( INBillPayeeResolutionResult
  , IsINBillPayeeResolutionResult(..)
  , successWithResolvedBillPayee
  , disambiguationWithBillPayeesToDisambiguate
  , confirmationRequiredWithBillPayeeToConfirm
  , confirmationRequiredWithBillPayeeToConfirmSelector
  , disambiguationWithBillPayeesToDisambiguateSelector
  , successWithResolvedBillPayeeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedBillPayee:@
successWithResolvedBillPayee :: IsINBillPayee resolvedBillPayee => resolvedBillPayee -> IO (Id INBillPayeeResolutionResult)
successWithResolvedBillPayee resolvedBillPayee =
  do
    cls' <- getRequiredClass "INBillPayeeResolutionResult"
    sendClassMessage cls' successWithResolvedBillPayeeSelector (toINBillPayee resolvedBillPayee)

-- | @+ disambiguationWithBillPayeesToDisambiguate:@
disambiguationWithBillPayeesToDisambiguate :: IsNSArray billPayeesToDisambiguate => billPayeesToDisambiguate -> IO (Id INBillPayeeResolutionResult)
disambiguationWithBillPayeesToDisambiguate billPayeesToDisambiguate =
  do
    cls' <- getRequiredClass "INBillPayeeResolutionResult"
    sendClassMessage cls' disambiguationWithBillPayeesToDisambiguateSelector (toNSArray billPayeesToDisambiguate)

-- | @+ confirmationRequiredWithBillPayeeToConfirm:@
confirmationRequiredWithBillPayeeToConfirm :: IsINBillPayee billPayeeToConfirm => billPayeeToConfirm -> IO (Id INBillPayeeResolutionResult)
confirmationRequiredWithBillPayeeToConfirm billPayeeToConfirm =
  do
    cls' <- getRequiredClass "INBillPayeeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithBillPayeeToConfirmSelector (toINBillPayee billPayeeToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedBillPayee:@
successWithResolvedBillPayeeSelector :: Selector '[Id INBillPayee] (Id INBillPayeeResolutionResult)
successWithResolvedBillPayeeSelector = mkSelector "successWithResolvedBillPayee:"

-- | @Selector@ for @disambiguationWithBillPayeesToDisambiguate:@
disambiguationWithBillPayeesToDisambiguateSelector :: Selector '[Id NSArray] (Id INBillPayeeResolutionResult)
disambiguationWithBillPayeesToDisambiguateSelector = mkSelector "disambiguationWithBillPayeesToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithBillPayeeToConfirm:@
confirmationRequiredWithBillPayeeToConfirmSelector :: Selector '[Id INBillPayee] (Id INBillPayeeResolutionResult)
confirmationRequiredWithBillPayeeToConfirmSelector = mkSelector "confirmationRequiredWithBillPayeeToConfirm:"

