{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INURLResolutionResult@.
module ObjC.Intents.INURLResolutionResult
  ( INURLResolutionResult
  , IsINURLResolutionResult(..)
  , successWithResolvedURL
  , disambiguationWithURLsToDisambiguate
  , confirmationRequiredWithURLToConfirm
  , confirmationRequiredWithURLToConfirmSelector
  , disambiguationWithURLsToDisambiguateSelector
  , successWithResolvedURLSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedURL:@
successWithResolvedURL :: IsNSURL resolvedURL => resolvedURL -> IO (Id INURLResolutionResult)
successWithResolvedURL resolvedURL =
  do
    cls' <- getRequiredClass "INURLResolutionResult"
    sendClassMessage cls' successWithResolvedURLSelector (toNSURL resolvedURL)

-- | @+ disambiguationWithURLsToDisambiguate:@
disambiguationWithURLsToDisambiguate :: IsNSArray urlsToDisambiguate => urlsToDisambiguate -> IO (Id INURLResolutionResult)
disambiguationWithURLsToDisambiguate urlsToDisambiguate =
  do
    cls' <- getRequiredClass "INURLResolutionResult"
    sendClassMessage cls' disambiguationWithURLsToDisambiguateSelector (toNSArray urlsToDisambiguate)

-- | @+ confirmationRequiredWithURLToConfirm:@
confirmationRequiredWithURLToConfirm :: IsNSURL urlToConfirm => urlToConfirm -> IO (Id INURLResolutionResult)
confirmationRequiredWithURLToConfirm urlToConfirm =
  do
    cls' <- getRequiredClass "INURLResolutionResult"
    sendClassMessage cls' confirmationRequiredWithURLToConfirmSelector (toNSURL urlToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedURL:@
successWithResolvedURLSelector :: Selector '[Id NSURL] (Id INURLResolutionResult)
successWithResolvedURLSelector = mkSelector "successWithResolvedURL:"

-- | @Selector@ for @disambiguationWithURLsToDisambiguate:@
disambiguationWithURLsToDisambiguateSelector :: Selector '[Id NSArray] (Id INURLResolutionResult)
disambiguationWithURLsToDisambiguateSelector = mkSelector "disambiguationWithURLsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithURLToConfirm:@
confirmationRequiredWithURLToConfirmSelector :: Selector '[Id NSURL] (Id INURLResolutionResult)
confirmationRequiredWithURLToConfirmSelector = mkSelector "confirmationRequiredWithURLToConfirm:"

