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
  , successWithResolvedNoteSelector
  , disambiguationWithNotesToDisambiguateSelector
  , confirmationRequiredWithNoteToConfirmSelector


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

-- | @+ successWithResolvedNote:@
successWithResolvedNote :: IsINNote resolvedNote => resolvedNote -> IO (Id INNoteResolutionResult)
successWithResolvedNote resolvedNote =
  do
    cls' <- getRequiredClass "INNoteResolutionResult"
    withObjCPtr resolvedNote $ \raw_resolvedNote ->
      sendClassMsg cls' (mkSelector "successWithResolvedNote:") (retPtr retVoid) [argPtr (castPtr raw_resolvedNote :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithNotesToDisambiguate:@
disambiguationWithNotesToDisambiguate :: IsNSArray notesToDisambiguate => notesToDisambiguate -> IO (Id INNoteResolutionResult)
disambiguationWithNotesToDisambiguate notesToDisambiguate =
  do
    cls' <- getRequiredClass "INNoteResolutionResult"
    withObjCPtr notesToDisambiguate $ \raw_notesToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithNotesToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_notesToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithNoteToConfirm:@
confirmationRequiredWithNoteToConfirm :: IsINNote noteToConfirm => noteToConfirm -> IO (Id INNoteResolutionResult)
confirmationRequiredWithNoteToConfirm noteToConfirm =
  do
    cls' <- getRequiredClass "INNoteResolutionResult"
    withObjCPtr noteToConfirm $ \raw_noteToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithNoteToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_noteToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedNote:@
successWithResolvedNoteSelector :: Selector
successWithResolvedNoteSelector = mkSelector "successWithResolvedNote:"

-- | @Selector@ for @disambiguationWithNotesToDisambiguate:@
disambiguationWithNotesToDisambiguateSelector :: Selector
disambiguationWithNotesToDisambiguateSelector = mkSelector "disambiguationWithNotesToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithNoteToConfirm:@
confirmationRequiredWithNoteToConfirmSelector :: Selector
confirmationRequiredWithNoteToConfirmSelector = mkSelector "confirmationRequiredWithNoteToConfirm:"

