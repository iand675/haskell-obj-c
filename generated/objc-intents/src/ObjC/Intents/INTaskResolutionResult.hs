{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTaskResolutionResult@.
module ObjC.Intents.INTaskResolutionResult
  ( INTaskResolutionResult
  , IsINTaskResolutionResult(..)
  , successWithResolvedTask
  , disambiguationWithTasksToDisambiguate
  , confirmationRequiredWithTaskToConfirm
  , successWithResolvedTaskSelector
  , disambiguationWithTasksToDisambiguateSelector
  , confirmationRequiredWithTaskToConfirmSelector


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

-- | @+ successWithResolvedTask:@
successWithResolvedTask :: IsINTask resolvedTask => resolvedTask -> IO (Id INTaskResolutionResult)
successWithResolvedTask resolvedTask =
  do
    cls' <- getRequiredClass "INTaskResolutionResult"
    withObjCPtr resolvedTask $ \raw_resolvedTask ->
      sendClassMsg cls' (mkSelector "successWithResolvedTask:") (retPtr retVoid) [argPtr (castPtr raw_resolvedTask :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithTasksToDisambiguate:@
disambiguationWithTasksToDisambiguate :: IsNSArray tasksToDisambiguate => tasksToDisambiguate -> IO (Id INTaskResolutionResult)
disambiguationWithTasksToDisambiguate tasksToDisambiguate =
  do
    cls' <- getRequiredClass "INTaskResolutionResult"
    withObjCPtr tasksToDisambiguate $ \raw_tasksToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithTasksToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_tasksToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithTaskToConfirm:@
confirmationRequiredWithTaskToConfirm :: IsINTask taskToConfirm => taskToConfirm -> IO (Id INTaskResolutionResult)
confirmationRequiredWithTaskToConfirm taskToConfirm =
  do
    cls' <- getRequiredClass "INTaskResolutionResult"
    withObjCPtr taskToConfirm $ \raw_taskToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithTaskToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_taskToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedTask:@
successWithResolvedTaskSelector :: Selector
successWithResolvedTaskSelector = mkSelector "successWithResolvedTask:"

-- | @Selector@ for @disambiguationWithTasksToDisambiguate:@
disambiguationWithTasksToDisambiguateSelector :: Selector
disambiguationWithTasksToDisambiguateSelector = mkSelector "disambiguationWithTasksToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithTaskToConfirm:@
confirmationRequiredWithTaskToConfirmSelector :: Selector
confirmationRequiredWithTaskToConfirmSelector = mkSelector "confirmationRequiredWithTaskToConfirm:"

