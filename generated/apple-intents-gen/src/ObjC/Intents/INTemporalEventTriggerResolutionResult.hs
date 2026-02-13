{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTemporalEventTriggerResolutionResult@.
module ObjC.Intents.INTemporalEventTriggerResolutionResult
  ( INTemporalEventTriggerResolutionResult
  , IsINTemporalEventTriggerResolutionResult(..)
  , successWithResolvedTemporalEventTrigger
  , disambiguationWithTemporalEventTriggersToDisambiguate
  , confirmationRequiredWithTemporalEventTriggerToConfirm
  , confirmationRequiredWithTemporalEventTriggerToConfirmSelector
  , disambiguationWithTemporalEventTriggersToDisambiguateSelector
  , successWithResolvedTemporalEventTriggerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedTemporalEventTrigger:@
successWithResolvedTemporalEventTrigger :: IsINTemporalEventTrigger resolvedTemporalEventTrigger => resolvedTemporalEventTrigger -> IO (Id INTemporalEventTriggerResolutionResult)
successWithResolvedTemporalEventTrigger resolvedTemporalEventTrigger =
  do
    cls' <- getRequiredClass "INTemporalEventTriggerResolutionResult"
    sendClassMessage cls' successWithResolvedTemporalEventTriggerSelector (toINTemporalEventTrigger resolvedTemporalEventTrigger)

-- | @+ disambiguationWithTemporalEventTriggersToDisambiguate:@
disambiguationWithTemporalEventTriggersToDisambiguate :: IsNSArray temporalEventTriggersToDisambiguate => temporalEventTriggersToDisambiguate -> IO (Id INTemporalEventTriggerResolutionResult)
disambiguationWithTemporalEventTriggersToDisambiguate temporalEventTriggersToDisambiguate =
  do
    cls' <- getRequiredClass "INTemporalEventTriggerResolutionResult"
    sendClassMessage cls' disambiguationWithTemporalEventTriggersToDisambiguateSelector (toNSArray temporalEventTriggersToDisambiguate)

-- | @+ confirmationRequiredWithTemporalEventTriggerToConfirm:@
confirmationRequiredWithTemporalEventTriggerToConfirm :: IsINTemporalEventTrigger temporalEventTriggerToConfirm => temporalEventTriggerToConfirm -> IO (Id INTemporalEventTriggerResolutionResult)
confirmationRequiredWithTemporalEventTriggerToConfirm temporalEventTriggerToConfirm =
  do
    cls' <- getRequiredClass "INTemporalEventTriggerResolutionResult"
    sendClassMessage cls' confirmationRequiredWithTemporalEventTriggerToConfirmSelector (toINTemporalEventTrigger temporalEventTriggerToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedTemporalEventTrigger:@
successWithResolvedTemporalEventTriggerSelector :: Selector '[Id INTemporalEventTrigger] (Id INTemporalEventTriggerResolutionResult)
successWithResolvedTemporalEventTriggerSelector = mkSelector "successWithResolvedTemporalEventTrigger:"

-- | @Selector@ for @disambiguationWithTemporalEventTriggersToDisambiguate:@
disambiguationWithTemporalEventTriggersToDisambiguateSelector :: Selector '[Id NSArray] (Id INTemporalEventTriggerResolutionResult)
disambiguationWithTemporalEventTriggersToDisambiguateSelector = mkSelector "disambiguationWithTemporalEventTriggersToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithTemporalEventTriggerToConfirm:@
confirmationRequiredWithTemporalEventTriggerToConfirmSelector :: Selector '[Id INTemporalEventTrigger] (Id INTemporalEventTriggerResolutionResult)
confirmationRequiredWithTemporalEventTriggerToConfirmSelector = mkSelector "confirmationRequiredWithTemporalEventTriggerToConfirm:"

