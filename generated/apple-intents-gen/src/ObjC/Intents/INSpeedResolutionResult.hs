{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSpeedResolutionResult@.
module ObjC.Intents.INSpeedResolutionResult
  ( INSpeedResolutionResult
  , IsINSpeedResolutionResult(..)
  , successWithResolvedSpeed
  , disambiguationWithSpeedToDisambiguate
  , confirmationRequiredWithSpeedToConfirm
  , confirmationRequiredWithSpeedToConfirmSelector
  , disambiguationWithSpeedToDisambiguateSelector
  , successWithResolvedSpeedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedSpeed:@
successWithResolvedSpeed :: IsNSMeasurement resolvedSpeed => resolvedSpeed -> IO (Id INSpeedResolutionResult)
successWithResolvedSpeed resolvedSpeed =
  do
    cls' <- getRequiredClass "INSpeedResolutionResult"
    sendClassMessage cls' successWithResolvedSpeedSelector (toNSMeasurement resolvedSpeed)

-- | @+ disambiguationWithSpeedToDisambiguate:@
disambiguationWithSpeedToDisambiguate :: IsNSArray speedToDisambiguate => speedToDisambiguate -> IO (Id INSpeedResolutionResult)
disambiguationWithSpeedToDisambiguate speedToDisambiguate =
  do
    cls' <- getRequiredClass "INSpeedResolutionResult"
    sendClassMessage cls' disambiguationWithSpeedToDisambiguateSelector (toNSArray speedToDisambiguate)

-- | @+ confirmationRequiredWithSpeedToConfirm:@
confirmationRequiredWithSpeedToConfirm :: IsNSMeasurement speedToConfirm => speedToConfirm -> IO (Id INSpeedResolutionResult)
confirmationRequiredWithSpeedToConfirm speedToConfirm =
  do
    cls' <- getRequiredClass "INSpeedResolutionResult"
    sendClassMessage cls' confirmationRequiredWithSpeedToConfirmSelector (toNSMeasurement speedToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedSpeed:@
successWithResolvedSpeedSelector :: Selector '[Id NSMeasurement] (Id INSpeedResolutionResult)
successWithResolvedSpeedSelector = mkSelector "successWithResolvedSpeed:"

-- | @Selector@ for @disambiguationWithSpeedToDisambiguate:@
disambiguationWithSpeedToDisambiguateSelector :: Selector '[Id NSArray] (Id INSpeedResolutionResult)
disambiguationWithSpeedToDisambiguateSelector = mkSelector "disambiguationWithSpeedToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithSpeedToConfirm:@
confirmationRequiredWithSpeedToConfirmSelector :: Selector '[Id NSMeasurement] (Id INSpeedResolutionResult)
confirmationRequiredWithSpeedToConfirmSelector = mkSelector "confirmationRequiredWithSpeedToConfirm:"

