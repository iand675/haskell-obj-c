{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSpatialEventTriggerResolutionResult@.
module ObjC.Intents.INSpatialEventTriggerResolutionResult
  ( INSpatialEventTriggerResolutionResult
  , IsINSpatialEventTriggerResolutionResult(..)
  , successWithResolvedSpatialEventTrigger
  , disambiguationWithSpatialEventTriggersToDisambiguate
  , confirmationRequiredWithSpatialEventTriggerToConfirm
  , confirmationRequiredWithSpatialEventTriggerToConfirmSelector
  , disambiguationWithSpatialEventTriggersToDisambiguateSelector
  , successWithResolvedSpatialEventTriggerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedSpatialEventTrigger:@
successWithResolvedSpatialEventTrigger :: IsINSpatialEventTrigger resolvedSpatialEventTrigger => resolvedSpatialEventTrigger -> IO (Id INSpatialEventTriggerResolutionResult)
successWithResolvedSpatialEventTrigger resolvedSpatialEventTrigger =
  do
    cls' <- getRequiredClass "INSpatialEventTriggerResolutionResult"
    sendClassMessage cls' successWithResolvedSpatialEventTriggerSelector (toINSpatialEventTrigger resolvedSpatialEventTrigger)

-- | @+ disambiguationWithSpatialEventTriggersToDisambiguate:@
disambiguationWithSpatialEventTriggersToDisambiguate :: IsNSArray spatialEventTriggersToDisambiguate => spatialEventTriggersToDisambiguate -> IO (Id INSpatialEventTriggerResolutionResult)
disambiguationWithSpatialEventTriggersToDisambiguate spatialEventTriggersToDisambiguate =
  do
    cls' <- getRequiredClass "INSpatialEventTriggerResolutionResult"
    sendClassMessage cls' disambiguationWithSpatialEventTriggersToDisambiguateSelector (toNSArray spatialEventTriggersToDisambiguate)

-- | @+ confirmationRequiredWithSpatialEventTriggerToConfirm:@
confirmationRequiredWithSpatialEventTriggerToConfirm :: IsINSpatialEventTrigger spatialEventTriggerToConfirm => spatialEventTriggerToConfirm -> IO (Id INSpatialEventTriggerResolutionResult)
confirmationRequiredWithSpatialEventTriggerToConfirm spatialEventTriggerToConfirm =
  do
    cls' <- getRequiredClass "INSpatialEventTriggerResolutionResult"
    sendClassMessage cls' confirmationRequiredWithSpatialEventTriggerToConfirmSelector (toINSpatialEventTrigger spatialEventTriggerToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedSpatialEventTrigger:@
successWithResolvedSpatialEventTriggerSelector :: Selector '[Id INSpatialEventTrigger] (Id INSpatialEventTriggerResolutionResult)
successWithResolvedSpatialEventTriggerSelector = mkSelector "successWithResolvedSpatialEventTrigger:"

-- | @Selector@ for @disambiguationWithSpatialEventTriggersToDisambiguate:@
disambiguationWithSpatialEventTriggersToDisambiguateSelector :: Selector '[Id NSArray] (Id INSpatialEventTriggerResolutionResult)
disambiguationWithSpatialEventTriggersToDisambiguateSelector = mkSelector "disambiguationWithSpatialEventTriggersToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithSpatialEventTriggerToConfirm:@
confirmationRequiredWithSpatialEventTriggerToConfirmSelector :: Selector '[Id INSpatialEventTrigger] (Id INSpatialEventTriggerResolutionResult)
confirmationRequiredWithSpatialEventTriggerToConfirmSelector = mkSelector "confirmationRequiredWithSpatialEventTriggerToConfirm:"

