{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTemperatureResolutionResult@.
module ObjC.Intents.INTemperatureResolutionResult
  ( INTemperatureResolutionResult
  , IsINTemperatureResolutionResult(..)
  , successWithResolvedTemperature
  , disambiguationWithTemperaturesToDisambiguate
  , confirmationRequiredWithTemperatureToConfirm
  , confirmationRequiredWithTemperatureToConfirmSelector
  , disambiguationWithTemperaturesToDisambiguateSelector
  , successWithResolvedTemperatureSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedTemperature:@
successWithResolvedTemperature :: IsNSMeasurement resolvedTemperature => resolvedTemperature -> IO (Id INTemperatureResolutionResult)
successWithResolvedTemperature resolvedTemperature =
  do
    cls' <- getRequiredClass "INTemperatureResolutionResult"
    sendClassMessage cls' successWithResolvedTemperatureSelector (toNSMeasurement resolvedTemperature)

-- | @+ disambiguationWithTemperaturesToDisambiguate:@
disambiguationWithTemperaturesToDisambiguate :: IsNSArray temperaturesToDisambiguate => temperaturesToDisambiguate -> IO (Id INTemperatureResolutionResult)
disambiguationWithTemperaturesToDisambiguate temperaturesToDisambiguate =
  do
    cls' <- getRequiredClass "INTemperatureResolutionResult"
    sendClassMessage cls' disambiguationWithTemperaturesToDisambiguateSelector (toNSArray temperaturesToDisambiguate)

-- | @+ confirmationRequiredWithTemperatureToConfirm:@
confirmationRequiredWithTemperatureToConfirm :: IsNSMeasurement temperatureToConfirm => temperatureToConfirm -> IO (Id INTemperatureResolutionResult)
confirmationRequiredWithTemperatureToConfirm temperatureToConfirm =
  do
    cls' <- getRequiredClass "INTemperatureResolutionResult"
    sendClassMessage cls' confirmationRequiredWithTemperatureToConfirmSelector (toNSMeasurement temperatureToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedTemperature:@
successWithResolvedTemperatureSelector :: Selector '[Id NSMeasurement] (Id INTemperatureResolutionResult)
successWithResolvedTemperatureSelector = mkSelector "successWithResolvedTemperature:"

-- | @Selector@ for @disambiguationWithTemperaturesToDisambiguate:@
disambiguationWithTemperaturesToDisambiguateSelector :: Selector '[Id NSArray] (Id INTemperatureResolutionResult)
disambiguationWithTemperaturesToDisambiguateSelector = mkSelector "disambiguationWithTemperaturesToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithTemperatureToConfirm:@
confirmationRequiredWithTemperatureToConfirmSelector :: Selector '[Id NSMeasurement] (Id INTemperatureResolutionResult)
confirmationRequiredWithTemperatureToConfirmSelector = mkSelector "confirmationRequiredWithTemperatureToConfirm:"

