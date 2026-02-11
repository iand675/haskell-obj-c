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
  , successWithResolvedTemporalEventTriggerSelector
  , disambiguationWithTemporalEventTriggersToDisambiguateSelector
  , confirmationRequiredWithTemporalEventTriggerToConfirmSelector


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

-- | @+ successWithResolvedTemporalEventTrigger:@
successWithResolvedTemporalEventTrigger :: IsINTemporalEventTrigger resolvedTemporalEventTrigger => resolvedTemporalEventTrigger -> IO (Id INTemporalEventTriggerResolutionResult)
successWithResolvedTemporalEventTrigger resolvedTemporalEventTrigger =
  do
    cls' <- getRequiredClass "INTemporalEventTriggerResolutionResult"
    withObjCPtr resolvedTemporalEventTrigger $ \raw_resolvedTemporalEventTrigger ->
      sendClassMsg cls' (mkSelector "successWithResolvedTemporalEventTrigger:") (retPtr retVoid) [argPtr (castPtr raw_resolvedTemporalEventTrigger :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithTemporalEventTriggersToDisambiguate:@
disambiguationWithTemporalEventTriggersToDisambiguate :: IsNSArray temporalEventTriggersToDisambiguate => temporalEventTriggersToDisambiguate -> IO (Id INTemporalEventTriggerResolutionResult)
disambiguationWithTemporalEventTriggersToDisambiguate temporalEventTriggersToDisambiguate =
  do
    cls' <- getRequiredClass "INTemporalEventTriggerResolutionResult"
    withObjCPtr temporalEventTriggersToDisambiguate $ \raw_temporalEventTriggersToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithTemporalEventTriggersToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_temporalEventTriggersToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithTemporalEventTriggerToConfirm:@
confirmationRequiredWithTemporalEventTriggerToConfirm :: IsINTemporalEventTrigger temporalEventTriggerToConfirm => temporalEventTriggerToConfirm -> IO (Id INTemporalEventTriggerResolutionResult)
confirmationRequiredWithTemporalEventTriggerToConfirm temporalEventTriggerToConfirm =
  do
    cls' <- getRequiredClass "INTemporalEventTriggerResolutionResult"
    withObjCPtr temporalEventTriggerToConfirm $ \raw_temporalEventTriggerToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithTemporalEventTriggerToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_temporalEventTriggerToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedTemporalEventTrigger:@
successWithResolvedTemporalEventTriggerSelector :: Selector
successWithResolvedTemporalEventTriggerSelector = mkSelector "successWithResolvedTemporalEventTrigger:"

-- | @Selector@ for @disambiguationWithTemporalEventTriggersToDisambiguate:@
disambiguationWithTemporalEventTriggersToDisambiguateSelector :: Selector
disambiguationWithTemporalEventTriggersToDisambiguateSelector = mkSelector "disambiguationWithTemporalEventTriggersToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithTemporalEventTriggerToConfirm:@
confirmationRequiredWithTemporalEventTriggerToConfirmSelector :: Selector
confirmationRequiredWithTemporalEventTriggerToConfirmSelector = mkSelector "confirmationRequiredWithTemporalEventTriggerToConfirm:"

