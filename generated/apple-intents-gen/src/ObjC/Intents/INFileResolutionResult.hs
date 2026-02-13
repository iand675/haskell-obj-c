{-# LANGUAGE DataKinds #-}
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
  , confirmationRequiredWithFileToConfirmSelector
  , disambiguationWithFilesToDisambiguateSelector
  , successWithResolvedFileSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedFile:@
successWithResolvedFile :: IsINFile resolvedFile => resolvedFile -> IO (Id INFileResolutionResult)
successWithResolvedFile resolvedFile =
  do
    cls' <- getRequiredClass "INFileResolutionResult"
    sendClassMessage cls' successWithResolvedFileSelector (toINFile resolvedFile)

-- | @+ disambiguationWithFilesToDisambiguate:@
disambiguationWithFilesToDisambiguate :: IsNSArray filesToDisambiguate => filesToDisambiguate -> IO (Id INFileResolutionResult)
disambiguationWithFilesToDisambiguate filesToDisambiguate =
  do
    cls' <- getRequiredClass "INFileResolutionResult"
    sendClassMessage cls' disambiguationWithFilesToDisambiguateSelector (toNSArray filesToDisambiguate)

-- | @+ confirmationRequiredWithFileToConfirm:@
confirmationRequiredWithFileToConfirm :: IsINFile fileToConfirm => fileToConfirm -> IO (Id INFileResolutionResult)
confirmationRequiredWithFileToConfirm fileToConfirm =
  do
    cls' <- getRequiredClass "INFileResolutionResult"
    sendClassMessage cls' confirmationRequiredWithFileToConfirmSelector (toINFile fileToConfirm)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedFile:@
successWithResolvedFileSelector :: Selector '[Id INFile] (Id INFileResolutionResult)
successWithResolvedFileSelector = mkSelector "successWithResolvedFile:"

-- | @Selector@ for @disambiguationWithFilesToDisambiguate:@
disambiguationWithFilesToDisambiguateSelector :: Selector '[Id NSArray] (Id INFileResolutionResult)
disambiguationWithFilesToDisambiguateSelector = mkSelector "disambiguationWithFilesToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithFileToConfirm:@
confirmationRequiredWithFileToConfirmSelector :: Selector '[Id INFile] (Id INFileResolutionResult)
confirmationRequiredWithFileToConfirmSelector = mkSelector "confirmationRequiredWithFileToConfirm:"

