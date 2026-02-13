{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INLengthResolutionResult@.
module ObjC.Intents.INLengthResolutionResult
  ( INLengthResolutionResult
  , IsINLengthResolutionResult(..)
  , successWithResolvedLength
  , disambiguationWithLengthsToDisambiguate
  , confirmationRequiredWithLengthToConfirm
  , confirmationRequiredWithLengthToConfirmSelector
  , disambiguationWithLengthsToDisambiguateSelector
  , successWithResolvedLengthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedLength:@
successWithResolvedLength :: IsNSMeasurement resolvedLength => resolvedLength -> IO (Id INLengthResolutionResult)
successWithResolvedLength resolvedLength =
  do
    cls' <- getRequiredClass "INLengthResolutionResult"
    sendClassMessage cls' successWithResolvedLengthSelector (toNSMeasurement resolvedLength)

-- | @+ disambiguationWithLengthsToDisambiguate:@
disambiguationWithLengthsToDisambiguate :: IsNSArray lengthsToDisambiguate => lengthsToDisambiguate -> IO (Id INLengthResolutionResult)
disambiguationWithLengthsToDisambiguate lengthsToDisambiguate =
  do
    cls' <- getRequiredClass "INLengthResolutionResult"
    sendClassMessage cls' disambiguationWithLengthsToDisambiguateSelector (toNSArray lengthsToDisambiguate)

-- | @+ confirmationRequiredWithLengthToConfirm:@
confirmationRequiredWithLengthToConfirm :: IsNSMeasurement lengthToConfirm => lengthToConfirm -> IO (Id INLengthResolutionResult)
confirmationRequiredWithLengthToConfirm lengthToConfirm =
  do
    cls' <- getRequiredClass "INLengthResolutionResult"
    sendClassMessage cls' confirmationRequiredWithLengthToConfirmSelector (toNSMeasurement lengthToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedLength:@
successWithResolvedLengthSelector :: Selector '[Id NSMeasurement] (Id INLengthResolutionResult)
successWithResolvedLengthSelector = mkSelector "successWithResolvedLength:"

-- | @Selector@ for @disambiguationWithLengthsToDisambiguate:@
disambiguationWithLengthsToDisambiguateSelector :: Selector '[Id NSArray] (Id INLengthResolutionResult)
disambiguationWithLengthsToDisambiguateSelector = mkSelector "disambiguationWithLengthsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithLengthToConfirm:@
confirmationRequiredWithLengthToConfirmSelector :: Selector '[Id NSMeasurement] (Id INLengthResolutionResult)
confirmationRequiredWithLengthToConfirmSelector = mkSelector "confirmationRequiredWithLengthToConfirm:"

