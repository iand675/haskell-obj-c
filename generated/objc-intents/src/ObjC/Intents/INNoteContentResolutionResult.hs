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
  , successWithResolvedNoteContentSelector
  , disambiguationWithNoteContentsToDisambiguateSelector
  , confirmationRequiredWithNoteContentToConfirmSelector


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

-- | @+ successWithResolvedNoteContent:@
successWithResolvedNoteContent :: IsINNoteContent resolvedNoteContent => resolvedNoteContent -> IO (Id INNoteContentResolutionResult)
successWithResolvedNoteContent resolvedNoteContent =
  do
    cls' <- getRequiredClass "INNoteContentResolutionResult"
    withObjCPtr resolvedNoteContent $ \raw_resolvedNoteContent ->
      sendClassMsg cls' (mkSelector "successWithResolvedNoteContent:") (retPtr retVoid) [argPtr (castPtr raw_resolvedNoteContent :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithNoteContentsToDisambiguate:@
disambiguationWithNoteContentsToDisambiguate :: IsNSArray noteContentsToDisambiguate => noteContentsToDisambiguate -> IO (Id INNoteContentResolutionResult)
disambiguationWithNoteContentsToDisambiguate noteContentsToDisambiguate =
  do
    cls' <- getRequiredClass "INNoteContentResolutionResult"
    withObjCPtr noteContentsToDisambiguate $ \raw_noteContentsToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithNoteContentsToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_noteContentsToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithNoteContentToConfirm:@
confirmationRequiredWithNoteContentToConfirm :: IsINNoteContent noteContentToConfirm => noteContentToConfirm -> IO (Id INNoteContentResolutionResult)
confirmationRequiredWithNoteContentToConfirm noteContentToConfirm =
  do
    cls' <- getRequiredClass "INNoteContentResolutionResult"
    withObjCPtr noteContentToConfirm $ \raw_noteContentToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithNoteContentToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_noteContentToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedNoteContent:@
successWithResolvedNoteContentSelector :: Selector
successWithResolvedNoteContentSelector = mkSelector "successWithResolvedNoteContent:"

-- | @Selector@ for @disambiguationWithNoteContentsToDisambiguate:@
disambiguationWithNoteContentsToDisambiguateSelector :: Selector
disambiguationWithNoteContentsToDisambiguateSelector = mkSelector "disambiguationWithNoteContentsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithNoteContentToConfirm:@
confirmationRequiredWithNoteContentToConfirmSelector :: Selector
confirmationRequiredWithNoteContentToConfirmSelector = mkSelector "confirmationRequiredWithNoteContentToConfirm:"

