{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSpeakableStringResolutionResult@.
module ObjC.Intents.INSpeakableStringResolutionResult
  ( INSpeakableStringResolutionResult
  , IsINSpeakableStringResolutionResult(..)
  , successWithResolvedString
  , disambiguationWithStringsToDisambiguate
  , confirmationRequiredWithStringToConfirm
  , confirmationRequiredWithStringToConfirmSelector
  , disambiguationWithStringsToDisambiguateSelector
  , successWithResolvedStringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedString:@
successWithResolvedString :: IsINSpeakableString resolvedString => resolvedString -> IO (Id INSpeakableStringResolutionResult)
successWithResolvedString resolvedString =
  do
    cls' <- getRequiredClass "INSpeakableStringResolutionResult"
    sendClassMessage cls' successWithResolvedStringSelector (toINSpeakableString resolvedString)

-- | @+ disambiguationWithStringsToDisambiguate:@
disambiguationWithStringsToDisambiguate :: IsNSArray stringsToDisambiguate => stringsToDisambiguate -> IO (Id INSpeakableStringResolutionResult)
disambiguationWithStringsToDisambiguate stringsToDisambiguate =
  do
    cls' <- getRequiredClass "INSpeakableStringResolutionResult"
    sendClassMessage cls' disambiguationWithStringsToDisambiguateSelector (toNSArray stringsToDisambiguate)

-- | @+ confirmationRequiredWithStringToConfirm:@
confirmationRequiredWithStringToConfirm :: IsINSpeakableString stringToConfirm => stringToConfirm -> IO (Id INSpeakableStringResolutionResult)
confirmationRequiredWithStringToConfirm stringToConfirm =
  do
    cls' <- getRequiredClass "INSpeakableStringResolutionResult"
    sendClassMessage cls' confirmationRequiredWithStringToConfirmSelector (toINSpeakableString stringToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedString:@
successWithResolvedStringSelector :: Selector '[Id INSpeakableString] (Id INSpeakableStringResolutionResult)
successWithResolvedStringSelector = mkSelector "successWithResolvedString:"

-- | @Selector@ for @disambiguationWithStringsToDisambiguate:@
disambiguationWithStringsToDisambiguateSelector :: Selector '[Id NSArray] (Id INSpeakableStringResolutionResult)
disambiguationWithStringsToDisambiguateSelector = mkSelector "disambiguationWithStringsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithStringToConfirm:@
confirmationRequiredWithStringToConfirmSelector :: Selector '[Id INSpeakableString] (Id INSpeakableStringResolutionResult)
confirmationRequiredWithStringToConfirmSelector = mkSelector "confirmationRequiredWithStringToConfirm:"

