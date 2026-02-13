{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INNoteContentResolutionResult@.
module ObjC.Intents.INNoteContentResolutionResult
  ( INNoteContentResolutionResult
  , IsINNoteContentResolutionResult(..)
  , successWithResolvedNoteContent
  , disambiguationWithNoteContentsToDisambiguate
  , confirmationRequiredWithNoteContentToConfirm
  , confirmationRequiredWithNoteContentToConfirmSelector
  , disambiguationWithNoteContentsToDisambiguateSelector
  , successWithResolvedNoteContentSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedNoteContent:@
successWithResolvedNoteContent :: IsINNoteContent resolvedNoteContent => resolvedNoteContent -> IO (Id INNoteContentResolutionResult)
successWithResolvedNoteContent resolvedNoteContent =
  do
    cls' <- getRequiredClass "INNoteContentResolutionResult"
    sendClassMessage cls' successWithResolvedNoteContentSelector (toINNoteContent resolvedNoteContent)

-- | @+ disambiguationWithNoteContentsToDisambiguate:@
disambiguationWithNoteContentsToDisambiguate :: IsNSArray noteContentsToDisambiguate => noteContentsToDisambiguate -> IO (Id INNoteContentResolutionResult)
disambiguationWithNoteContentsToDisambiguate noteContentsToDisambiguate =
  do
    cls' <- getRequiredClass "INNoteContentResolutionResult"
    sendClassMessage cls' disambiguationWithNoteContentsToDisambiguateSelector (toNSArray noteContentsToDisambiguate)

-- | @+ confirmationRequiredWithNoteContentToConfirm:@
confirmationRequiredWithNoteContentToConfirm :: IsINNoteContent noteContentToConfirm => noteContentToConfirm -> IO (Id INNoteContentResolutionResult)
confirmationRequiredWithNoteContentToConfirm noteContentToConfirm =
  do
    cls' <- getRequiredClass "INNoteContentResolutionResult"
    sendClassMessage cls' confirmationRequiredWithNoteContentToConfirmSelector (toINNoteContent noteContentToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedNoteContent:@
successWithResolvedNoteContentSelector :: Selector '[Id INNoteContent] (Id INNoteContentResolutionResult)
successWithResolvedNoteContentSelector = mkSelector "successWithResolvedNoteContent:"

-- | @Selector@ for @disambiguationWithNoteContentsToDisambiguate:@
disambiguationWithNoteContentsToDisambiguateSelector :: Selector '[Id NSArray] (Id INNoteContentResolutionResult)
disambiguationWithNoteContentsToDisambiguateSelector = mkSelector "disambiguationWithNoteContentsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithNoteContentToConfirm:@
confirmationRequiredWithNoteContentToConfirmSelector :: Selector '[Id INNoteContent] (Id INNoteContentResolutionResult)
confirmationRequiredWithNoteContentToConfirmSelector = mkSelector "confirmationRequiredWithNoteContentToConfirm:"

