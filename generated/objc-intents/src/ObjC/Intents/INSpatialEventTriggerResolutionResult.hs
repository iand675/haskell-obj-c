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
  , successWithResolvedSpatialEventTriggerSelector
  , disambiguationWithSpatialEventTriggersToDisambiguateSelector
  , confirmationRequiredWithSpatialEventTriggerToConfirmSelector


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

-- | @+ successWithResolvedSpatialEventTrigger:@
successWithResolvedSpatialEventTrigger :: IsINSpatialEventTrigger resolvedSpatialEventTrigger => resolvedSpatialEventTrigger -> IO (Id INSpatialEventTriggerResolutionResult)
successWithResolvedSpatialEventTrigger resolvedSpatialEventTrigger =
  do
    cls' <- getRequiredClass "INSpatialEventTriggerResolutionResult"
    withObjCPtr resolvedSpatialEventTrigger $ \raw_resolvedSpatialEventTrigger ->
      sendClassMsg cls' (mkSelector "successWithResolvedSpatialEventTrigger:") (retPtr retVoid) [argPtr (castPtr raw_resolvedSpatialEventTrigger :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithSpatialEventTriggersToDisambiguate:@
disambiguationWithSpatialEventTriggersToDisambiguate :: IsNSArray spatialEventTriggersToDisambiguate => spatialEventTriggersToDisambiguate -> IO (Id INSpatialEventTriggerResolutionResult)
disambiguationWithSpatialEventTriggersToDisambiguate spatialEventTriggersToDisambiguate =
  do
    cls' <- getRequiredClass "INSpatialEventTriggerResolutionResult"
    withObjCPtr spatialEventTriggersToDisambiguate $ \raw_spatialEventTriggersToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithSpatialEventTriggersToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_spatialEventTriggersToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithSpatialEventTriggerToConfirm:@
confirmationRequiredWithSpatialEventTriggerToConfirm :: IsINSpatialEventTrigger spatialEventTriggerToConfirm => spatialEventTriggerToConfirm -> IO (Id INSpatialEventTriggerResolutionResult)
confirmationRequiredWithSpatialEventTriggerToConfirm spatialEventTriggerToConfirm =
  do
    cls' <- getRequiredClass "INSpatialEventTriggerResolutionResult"
    withObjCPtr spatialEventTriggerToConfirm $ \raw_spatialEventTriggerToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithSpatialEventTriggerToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_spatialEventTriggerToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedSpatialEventTrigger:@
successWithResolvedSpatialEventTriggerSelector :: Selector
successWithResolvedSpatialEventTriggerSelector = mkSelector "successWithResolvedSpatialEventTrigger:"

-- | @Selector@ for @disambiguationWithSpatialEventTriggersToDisambiguate:@
disambiguationWithSpatialEventTriggersToDisambiguateSelector :: Selector
disambiguationWithSpatialEventTriggersToDisambiguateSelector = mkSelector "disambiguationWithSpatialEventTriggersToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithSpatialEventTriggerToConfirm:@
confirmationRequiredWithSpatialEventTriggerToConfirmSelector :: Selector
confirmationRequiredWithSpatialEventTriggerToConfirmSelector = mkSelector "confirmationRequiredWithSpatialEventTriggerToConfirm:"

