{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INDateComponentsRangeResolutionResult@.
module ObjC.Intents.INDateComponentsRangeResolutionResult
  ( INDateComponentsRangeResolutionResult
  , IsINDateComponentsRangeResolutionResult(..)
  , successWithResolvedDateComponentsRange
  , disambiguationWithDateComponentsRangesToDisambiguate
  , confirmationRequiredWithDateComponentsRangeToConfirm
  , confirmationRequiredWithDateComponentsRangeToConfirmSelector
  , disambiguationWithDateComponentsRangesToDisambiguateSelector
  , successWithResolvedDateComponentsRangeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedDateComponentsRange:@
successWithResolvedDateComponentsRange :: IsINDateComponentsRange resolvedDateComponentsRange => resolvedDateComponentsRange -> IO (Id INDateComponentsRangeResolutionResult)
successWithResolvedDateComponentsRange resolvedDateComponentsRange =
  do
    cls' <- getRequiredClass "INDateComponentsRangeResolutionResult"
    sendClassMessage cls' successWithResolvedDateComponentsRangeSelector (toINDateComponentsRange resolvedDateComponentsRange)

-- | @+ disambiguationWithDateComponentsRangesToDisambiguate:@
disambiguationWithDateComponentsRangesToDisambiguate :: IsNSArray dateComponentsRangesToDisambiguate => dateComponentsRangesToDisambiguate -> IO (Id INDateComponentsRangeResolutionResult)
disambiguationWithDateComponentsRangesToDisambiguate dateComponentsRangesToDisambiguate =
  do
    cls' <- getRequiredClass "INDateComponentsRangeResolutionResult"
    sendClassMessage cls' disambiguationWithDateComponentsRangesToDisambiguateSelector (toNSArray dateComponentsRangesToDisambiguate)

-- | @+ confirmationRequiredWithDateComponentsRangeToConfirm:@
confirmationRequiredWithDateComponentsRangeToConfirm :: IsINDateComponentsRange dateComponentsRangeToConfirm => dateComponentsRangeToConfirm -> IO (Id INDateComponentsRangeResolutionResult)
confirmationRequiredWithDateComponentsRangeToConfirm dateComponentsRangeToConfirm =
  do
    cls' <- getRequiredClass "INDateComponentsRangeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithDateComponentsRangeToConfirmSelector (toINDateComponentsRange dateComponentsRangeToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedDateComponentsRange:@
successWithResolvedDateComponentsRangeSelector :: Selector '[Id INDateComponentsRange] (Id INDateComponentsRangeResolutionResult)
successWithResolvedDateComponentsRangeSelector = mkSelector "successWithResolvedDateComponentsRange:"

-- | @Selector@ for @disambiguationWithDateComponentsRangesToDisambiguate:@
disambiguationWithDateComponentsRangesToDisambiguateSelector :: Selector '[Id NSArray] (Id INDateComponentsRangeResolutionResult)
disambiguationWithDateComponentsRangesToDisambiguateSelector = mkSelector "disambiguationWithDateComponentsRangesToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithDateComponentsRangeToConfirm:@
confirmationRequiredWithDateComponentsRangeToConfirmSelector :: Selector '[Id INDateComponentsRange] (Id INDateComponentsRangeResolutionResult)
confirmationRequiredWithDateComponentsRangeToConfirmSelector = mkSelector "confirmationRequiredWithDateComponentsRangeToConfirm:"

