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
  , successWithResolvedEnergySelector
  , disambiguationWithEnergyToDisambiguateSelector
  , confirmationRequiredWithEnergyToConfirmSelector


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

-- | @+ successWithResolvedEnergy:@
successWithResolvedEnergy :: IsNSMeasurement resolvedEnergy => resolvedEnergy -> IO (Id INEnergyResolutionResult)
successWithResolvedEnergy resolvedEnergy =
  do
    cls' <- getRequiredClass "INEnergyResolutionResult"
    withObjCPtr resolvedEnergy $ \raw_resolvedEnergy ->
      sendClassMsg cls' (mkSelector "successWithResolvedEnergy:") (retPtr retVoid) [argPtr (castPtr raw_resolvedEnergy :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithEnergyToDisambiguate:@
disambiguationWithEnergyToDisambiguate :: IsNSArray energyToDisambiguate => energyToDisambiguate -> IO (Id INEnergyResolutionResult)
disambiguationWithEnergyToDisambiguate energyToDisambiguate =
  do
    cls' <- getRequiredClass "INEnergyResolutionResult"
    withObjCPtr energyToDisambiguate $ \raw_energyToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithEnergyToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_energyToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithEnergyToConfirm:@
confirmationRequiredWithEnergyToConfirm :: IsNSMeasurement energyToConfirm => energyToConfirm -> IO (Id INEnergyResolutionResult)
confirmationRequiredWithEnergyToConfirm energyToConfirm =
  do
    cls' <- getRequiredClass "INEnergyResolutionResult"
    withObjCPtr energyToConfirm $ \raw_energyToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithEnergyToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_energyToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedEnergy:@
successWithResolvedEnergySelector :: Selector
successWithResolvedEnergySelector = mkSelector "successWithResolvedEnergy:"

-- | @Selector@ for @disambiguationWithEnergyToDisambiguate:@
disambiguationWithEnergyToDisambiguateSelector :: Selector
disambiguationWithEnergyToDisambiguateSelector = mkSelector "disambiguationWithEnergyToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithEnergyToConfirm:@
confirmationRequiredWithEnergyToConfirmSelector :: Selector
confirmationRequiredWithEnergyToConfirmSelector = mkSelector "confirmationRequiredWithEnergyToConfirm:"

