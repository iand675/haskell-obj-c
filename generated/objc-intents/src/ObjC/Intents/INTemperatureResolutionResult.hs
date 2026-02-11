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
  , successWithResolvedTemperatureSelector
  , disambiguationWithTemperaturesToDisambiguateSelector
  , confirmationRequiredWithTemperatureToConfirmSelector


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

-- | @+ successWithResolvedTemperature:@
successWithResolvedTemperature :: IsNSMeasurement resolvedTemperature => resolvedTemperature -> IO (Id INTemperatureResolutionResult)
successWithResolvedTemperature resolvedTemperature =
  do
    cls' <- getRequiredClass "INTemperatureResolutionResult"
    withObjCPtr resolvedTemperature $ \raw_resolvedTemperature ->
      sendClassMsg cls' (mkSelector "successWithResolvedTemperature:") (retPtr retVoid) [argPtr (castPtr raw_resolvedTemperature :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithTemperaturesToDisambiguate:@
disambiguationWithTemperaturesToDisambiguate :: IsNSArray temperaturesToDisambiguate => temperaturesToDisambiguate -> IO (Id INTemperatureResolutionResult)
disambiguationWithTemperaturesToDisambiguate temperaturesToDisambiguate =
  do
    cls' <- getRequiredClass "INTemperatureResolutionResult"
    withObjCPtr temperaturesToDisambiguate $ \raw_temperaturesToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithTemperaturesToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_temperaturesToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithTemperatureToConfirm:@
confirmationRequiredWithTemperatureToConfirm :: IsNSMeasurement temperatureToConfirm => temperatureToConfirm -> IO (Id INTemperatureResolutionResult)
confirmationRequiredWithTemperatureToConfirm temperatureToConfirm =
  do
    cls' <- getRequiredClass "INTemperatureResolutionResult"
    withObjCPtr temperatureToConfirm $ \raw_temperatureToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithTemperatureToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_temperatureToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedTemperature:@
successWithResolvedTemperatureSelector :: Selector
successWithResolvedTemperatureSelector = mkSelector "successWithResolvedTemperature:"

-- | @Selector@ for @disambiguationWithTemperaturesToDisambiguate:@
disambiguationWithTemperaturesToDisambiguateSelector :: Selector
disambiguationWithTemperaturesToDisambiguateSelector = mkSelector "disambiguationWithTemperaturesToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithTemperatureToConfirm:@
confirmationRequiredWithTemperatureToConfirmSelector :: Selector
confirmationRequiredWithTemperatureToConfirmSelector = mkSelector "confirmationRequiredWithTemperatureToConfirm:"

