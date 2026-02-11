{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INNoteContentTypeResolutionResult@.
module ObjC.Intents.INNoteContentTypeResolutionResult
  ( INNoteContentTypeResolutionResult
  , IsINNoteContentTypeResolutionResult(..)
  , successWithResolvedNoteContentType
  , confirmationRequiredWithNoteContentTypeToConfirm
  , successWithResolvedNoteContentTypeSelector
  , confirmationRequiredWithNoteContentTypeToConfirmSelector

  -- * Enum types
  , INNoteContentType(INNoteContentType)
  , pattern INNoteContentTypeUnknown
  , pattern INNoteContentTypeText
  , pattern INNoteContentTypeImage

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
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedNoteContentType:@
successWithResolvedNoteContentType :: INNoteContentType -> IO (Id INNoteContentTypeResolutionResult)
successWithResolvedNoteContentType resolvedNoteContentType =
  do
    cls' <- getRequiredClass "INNoteContentTypeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedNoteContentType:") (retPtr retVoid) [argCLong (coerce resolvedNoteContentType)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithNoteContentTypeToConfirm:@
confirmationRequiredWithNoteContentTypeToConfirm :: INNoteContentType -> IO (Id INNoteContentTypeResolutionResult)
confirmationRequiredWithNoteContentTypeToConfirm noteContentTypeToConfirm =
  do
    cls' <- getRequiredClass "INNoteContentTypeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithNoteContentTypeToConfirm:") (retPtr retVoid) [argCLong (coerce noteContentTypeToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedNoteContentType:@
successWithResolvedNoteContentTypeSelector :: Selector
successWithResolvedNoteContentTypeSelector = mkSelector "successWithResolvedNoteContentType:"

-- | @Selector@ for @confirmationRequiredWithNoteContentTypeToConfirm:@
confirmationRequiredWithNoteContentTypeToConfirmSelector :: Selector
confirmationRequiredWithNoteContentTypeToConfirmSelector = mkSelector "confirmationRequiredWithNoteContentTypeToConfirm:"

