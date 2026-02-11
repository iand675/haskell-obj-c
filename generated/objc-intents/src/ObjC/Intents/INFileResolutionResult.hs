{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INFileResolutionResult@.
module ObjC.Intents.INFileResolutionResult
  ( INFileResolutionResult
  , IsINFileResolutionResult(..)
  , successWithResolvedFile
  , disambiguationWithFilesToDisambiguate
  , confirmationRequiredWithFileToConfirm
  , successWithResolvedFileSelector
  , disambiguationWithFilesToDisambiguateSelector
  , confirmationRequiredWithFileToConfirmSelector


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

-- | @+ successWithResolvedFile:@
successWithResolvedFile :: IsINFile resolvedFile => resolvedFile -> IO (Id INFileResolutionResult)
successWithResolvedFile resolvedFile =
  do
    cls' <- getRequiredClass "INFileResolutionResult"
    withObjCPtr resolvedFile $ \raw_resolvedFile ->
      sendClassMsg cls' (mkSelector "successWithResolvedFile:") (retPtr retVoid) [argPtr (castPtr raw_resolvedFile :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithFilesToDisambiguate:@
disambiguationWithFilesToDisambiguate :: IsNSArray filesToDisambiguate => filesToDisambiguate -> IO (Id INFileResolutionResult)
disambiguationWithFilesToDisambiguate filesToDisambiguate =
  do
    cls' <- getRequiredClass "INFileResolutionResult"
    withObjCPtr filesToDisambiguate $ \raw_filesToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithFilesToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_filesToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithFileToConfirm:@
confirmationRequiredWithFileToConfirm :: IsINFile fileToConfirm => fileToConfirm -> IO (Id INFileResolutionResult)
confirmationRequiredWithFileToConfirm fileToConfirm =
  do
    cls' <- getRequiredClass "INFileResolutionResult"
    withObjCPtr fileToConfirm $ \raw_fileToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithFileToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_fileToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedFile:@
successWithResolvedFileSelector :: Selector
successWithResolvedFileSelector = mkSelector "successWithResolvedFile:"

-- | @Selector@ for @disambiguationWithFilesToDisambiguate:@
disambiguationWithFilesToDisambiguateSelector :: Selector
disambiguationWithFilesToDisambiguateSelector = mkSelector "disambiguationWithFilesToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithFileToConfirm:@
confirmationRequiredWithFileToConfirmSelector :: Selector
confirmationRequiredWithFileToConfirmSelector = mkSelector "confirmationRequiredWithFileToConfirm:"

