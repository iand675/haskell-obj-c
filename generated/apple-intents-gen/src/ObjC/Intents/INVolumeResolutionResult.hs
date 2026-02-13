{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INVolumeResolutionResult@.
module ObjC.Intents.INVolumeResolutionResult
  ( INVolumeResolutionResult
  , IsINVolumeResolutionResult(..)
  , successWithResolvedVolume
  , disambiguationWithVolumeToDisambiguate
  , confirmationRequiredWithVolumeToConfirm
  , confirmationRequiredWithVolumeToConfirmSelector
  , disambiguationWithVolumeToDisambiguateSelector
  , successWithResolvedVolumeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedVolume:@
successWithResolvedVolume :: IsNSMeasurement resolvedVolume => resolvedVolume -> IO (Id INVolumeResolutionResult)
successWithResolvedVolume resolvedVolume =
  do
    cls' <- getRequiredClass "INVolumeResolutionResult"
    sendClassMessage cls' successWithResolvedVolumeSelector (toNSMeasurement resolvedVolume)

-- | @+ disambiguationWithVolumeToDisambiguate:@
disambiguationWithVolumeToDisambiguate :: IsNSArray volumeToDisambiguate => volumeToDisambiguate -> IO (Id INVolumeResolutionResult)
disambiguationWithVolumeToDisambiguate volumeToDisambiguate =
  do
    cls' <- getRequiredClass "INVolumeResolutionResult"
    sendClassMessage cls' disambiguationWithVolumeToDisambiguateSelector (toNSArray volumeToDisambiguate)

-- | @+ confirmationRequiredWithVolumeToConfirm:@
confirmationRequiredWithVolumeToConfirm :: IsNSMeasurement volumeToConfirm => volumeToConfirm -> IO (Id INVolumeResolutionResult)
confirmationRequiredWithVolumeToConfirm volumeToConfirm =
  do
    cls' <- getRequiredClass "INVolumeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithVolumeToConfirmSelector (toNSMeasurement volumeToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedVolume:@
successWithResolvedVolumeSelector :: Selector '[Id NSMeasurement] (Id INVolumeResolutionResult)
successWithResolvedVolumeSelector = mkSelector "successWithResolvedVolume:"

-- | @Selector@ for @disambiguationWithVolumeToDisambiguate:@
disambiguationWithVolumeToDisambiguateSelector :: Selector '[Id NSArray] (Id INVolumeResolutionResult)
disambiguationWithVolumeToDisambiguateSelector = mkSelector "disambiguationWithVolumeToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithVolumeToConfirm:@
confirmationRequiredWithVolumeToConfirmSelector :: Selector '[Id NSMeasurement] (Id INVolumeResolutionResult)
confirmationRequiredWithVolumeToConfirmSelector = mkSelector "confirmationRequiredWithVolumeToConfirm:"

