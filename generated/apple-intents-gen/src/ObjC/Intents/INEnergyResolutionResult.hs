{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INEnergyResolutionResult@.
module ObjC.Intents.INEnergyResolutionResult
  ( INEnergyResolutionResult
  , IsINEnergyResolutionResult(..)
  , successWithResolvedEnergy
  , disambiguationWithEnergyToDisambiguate
  , confirmationRequiredWithEnergyToConfirm
  , confirmationRequiredWithEnergyToConfirmSelector
  , disambiguationWithEnergyToDisambiguateSelector
  , successWithResolvedEnergySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedEnergy:@
successWithResolvedEnergy :: IsNSMeasurement resolvedEnergy => resolvedEnergy -> IO (Id INEnergyResolutionResult)
successWithResolvedEnergy resolvedEnergy =
  do
    cls' <- getRequiredClass "INEnergyResolutionResult"
    sendClassMessage cls' successWithResolvedEnergySelector (toNSMeasurement resolvedEnergy)

-- | @+ disambiguationWithEnergyToDisambiguate:@
disambiguationWithEnergyToDisambiguate :: IsNSArray energyToDisambiguate => energyToDisambiguate -> IO (Id INEnergyResolutionResult)
disambiguationWithEnergyToDisambiguate energyToDisambiguate =
  do
    cls' <- getRequiredClass "INEnergyResolutionResult"
    sendClassMessage cls' disambiguationWithEnergyToDisambiguateSelector (toNSArray energyToDisambiguate)

-- | @+ confirmationRequiredWithEnergyToConfirm:@
confirmationRequiredWithEnergyToConfirm :: IsNSMeasurement energyToConfirm => energyToConfirm -> IO (Id INEnergyResolutionResult)
confirmationRequiredWithEnergyToConfirm energyToConfirm =
  do
    cls' <- getRequiredClass "INEnergyResolutionResult"
    sendClassMessage cls' confirmationRequiredWithEnergyToConfirmSelector (toNSMeasurement energyToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedEnergy:@
successWithResolvedEnergySelector :: Selector '[Id NSMeasurement] (Id INEnergyResolutionResult)
successWithResolvedEnergySelector = mkSelector "successWithResolvedEnergy:"

-- | @Selector@ for @disambiguationWithEnergyToDisambiguate:@
disambiguationWithEnergyToDisambiguateSelector :: Selector '[Id NSArray] (Id INEnergyResolutionResult)
disambiguationWithEnergyToDisambiguateSelector = mkSelector "disambiguationWithEnergyToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithEnergyToConfirm:@
confirmationRequiredWithEnergyToConfirmSelector :: Selector '[Id NSMeasurement] (Id INEnergyResolutionResult)
confirmationRequiredWithEnergyToConfirmSelector = mkSelector "confirmationRequiredWithEnergyToConfirm:"

