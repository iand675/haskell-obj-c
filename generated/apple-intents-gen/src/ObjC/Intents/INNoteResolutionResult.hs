{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INNoteResolutionResult@.
module ObjC.Intents.INNoteResolutionResult
  ( INNoteResolutionResult
  , IsINNoteResolutionResult(..)
  , successWithResolvedNote
  , disambiguationWithNotesToDisambiguate
  , confirmationRequiredWithNoteToConfirm
  , confirmationRequiredWithNoteToConfirmSelector
  , disambiguationWithNotesToDisambiguateSelector
  , successWithResolvedNoteSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedNote:@
successWithResolvedNote :: IsINNote resolvedNote => resolvedNote -> IO (Id INNoteResolutionResult)
successWithResolvedNote resolvedNote =
  do
    cls' <- getRequiredClass "INNoteResolutionResult"
    sendClassMessage cls' successWithResolvedNoteSelector (toINNote resolvedNote)

-- | @+ disambiguationWithNotesToDisambiguate:@
disambiguationWithNotesToDisambiguate :: IsNSArray notesToDisambiguate => notesToDisambiguate -> IO (Id INNoteResolutionResult)
disambiguationWithNotesToDisambiguate notesToDisambiguate =
  do
    cls' <- getRequiredClass "INNoteResolutionResult"
    sendClassMessage cls' disambiguationWithNotesToDisambiguateSelector (toNSArray notesToDisambiguate)

-- | @+ confirmationRequiredWithNoteToConfirm:@
confirmationRequiredWithNoteToConfirm :: IsINNote noteToConfirm => noteToConfirm -> IO (Id INNoteResolutionResult)
confirmationRequiredWithNoteToConfirm noteToConfirm =
  do
    cls' <- getRequiredClass "INNoteResolutionResult"
    sendClassMessage cls' confirmationRequiredWithNoteToConfirmSelector (toINNote noteToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedNote:@
successWithResolvedNoteSelector :: Selector '[Id INNote] (Id INNoteResolutionResult)
successWithResolvedNoteSelector = mkSelector "successWithResolvedNote:"

-- | @Selector@ for @disambiguationWithNotesToDisambiguate:@
disambiguationWithNotesToDisambiguateSelector :: Selector '[Id NSArray] (Id INNoteResolutionResult)
disambiguationWithNotesToDisambiguateSelector = mkSelector "disambiguationWithNotesToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithNoteToConfirm:@
confirmationRequiredWithNoteToConfirmSelector :: Selector '[Id INNote] (Id INNoteResolutionResult)
confirmationRequiredWithNoteToConfirmSelector = mkSelector "confirmationRequiredWithNoteToConfirm:"

