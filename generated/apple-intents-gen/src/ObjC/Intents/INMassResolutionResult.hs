{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INMassResolutionResult@.
module ObjC.Intents.INMassResolutionResult
  ( INMassResolutionResult
  , IsINMassResolutionResult(..)
  , successWithResolvedMass
  , disambiguationWithMassToDisambiguate
  , confirmationRequiredWithMassToConfirm
  , confirmationRequiredWithMassToConfirmSelector
  , disambiguationWithMassToDisambiguateSelector
  , successWithResolvedMassSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedMass:@
successWithResolvedMass :: IsNSMeasurement resolvedMass => resolvedMass -> IO (Id INMassResolutionResult)
successWithResolvedMass resolvedMass =
  do
    cls' <- getRequiredClass "INMassResolutionResult"
    sendClassMessage cls' successWithResolvedMassSelector (toNSMeasurement resolvedMass)

-- | @+ disambiguationWithMassToDisambiguate:@
disambiguationWithMassToDisambiguate :: IsNSArray massToDisambiguate => massToDisambiguate -> IO (Id INMassResolutionResult)
disambiguationWithMassToDisambiguate massToDisambiguate =
  do
    cls' <- getRequiredClass "INMassResolutionResult"
    sendClassMessage cls' disambiguationWithMassToDisambiguateSelector (toNSArray massToDisambiguate)

-- | @+ confirmationRequiredWithMassToConfirm:@
confirmationRequiredWithMassToConfirm :: IsNSMeasurement massToConfirm => massToConfirm -> IO (Id INMassResolutionResult)
confirmationRequiredWithMassToConfirm massToConfirm =
  do
    cls' <- getRequiredClass "INMassResolutionResult"
    sendClassMessage cls' confirmationRequiredWithMassToConfirmSelector (toNSMeasurement massToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedMass:@
successWithResolvedMassSelector :: Selector '[Id NSMeasurement] (Id INMassResolutionResult)
successWithResolvedMassSelector = mkSelector "successWithResolvedMass:"

-- | @Selector@ for @disambiguationWithMassToDisambiguate:@
disambiguationWithMassToDisambiguateSelector :: Selector '[Id NSArray] (Id INMassResolutionResult)
disambiguationWithMassToDisambiguateSelector = mkSelector "disambiguationWithMassToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithMassToConfirm:@
confirmationRequiredWithMassToConfirmSelector :: Selector '[Id NSMeasurement] (Id INMassResolutionResult)
confirmationRequiredWithMassToConfirmSelector = mkSelector "confirmationRequiredWithMassToConfirm:"

