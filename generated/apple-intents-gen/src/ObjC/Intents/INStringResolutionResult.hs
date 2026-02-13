{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStringResolutionResult@.
module ObjC.Intents.INStringResolutionResult
  ( INStringResolutionResult
  , IsINStringResolutionResult(..)
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
successWithResolvedString :: IsNSString resolvedString => resolvedString -> IO (Id INStringResolutionResult)
successWithResolvedString resolvedString =
  do
    cls' <- getRequiredClass "INStringResolutionResult"
    sendClassMessage cls' successWithResolvedStringSelector (toNSString resolvedString)

-- | @+ disambiguationWithStringsToDisambiguate:@
disambiguationWithStringsToDisambiguate :: IsNSArray stringsToDisambiguate => stringsToDisambiguate -> IO (Id INStringResolutionResult)
disambiguationWithStringsToDisambiguate stringsToDisambiguate =
  do
    cls' <- getRequiredClass "INStringResolutionResult"
    sendClassMessage cls' disambiguationWithStringsToDisambiguateSelector (toNSArray stringsToDisambiguate)

-- | @+ confirmationRequiredWithStringToConfirm:@
confirmationRequiredWithStringToConfirm :: IsNSString stringToConfirm => stringToConfirm -> IO (Id INStringResolutionResult)
confirmationRequiredWithStringToConfirm stringToConfirm =
  do
    cls' <- getRequiredClass "INStringResolutionResult"
    sendClassMessage cls' confirmationRequiredWithStringToConfirmSelector (toNSString stringToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedString:@
successWithResolvedStringSelector :: Selector '[Id NSString] (Id INStringResolutionResult)
successWithResolvedStringSelector = mkSelector "successWithResolvedString:"

-- | @Selector@ for @disambiguationWithStringsToDisambiguate:@
disambiguationWithStringsToDisambiguateSelector :: Selector '[Id NSArray] (Id INStringResolutionResult)
disambiguationWithStringsToDisambiguateSelector = mkSelector "disambiguationWithStringsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithStringToConfirm:@
confirmationRequiredWithStringToConfirmSelector :: Selector '[Id NSString] (Id INStringResolutionResult)
confirmationRequiredWithStringToConfirmSelector = mkSelector "confirmationRequiredWithStringToConfirm:"

