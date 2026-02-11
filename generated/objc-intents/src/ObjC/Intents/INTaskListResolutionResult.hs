{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTaskListResolutionResult@.
module ObjC.Intents.INTaskListResolutionResult
  ( INTaskListResolutionResult
  , IsINTaskListResolutionResult(..)
  , successWithResolvedTaskList
  , disambiguationWithTaskListsToDisambiguate
  , confirmationRequiredWithTaskListToConfirm
  , successWithResolvedTaskListSelector
  , disambiguationWithTaskListsToDisambiguateSelector
  , confirmationRequiredWithTaskListToConfirmSelector


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

-- | @+ successWithResolvedTaskList:@
successWithResolvedTaskList :: IsINTaskList resolvedTaskList => resolvedTaskList -> IO (Id INTaskListResolutionResult)
successWithResolvedTaskList resolvedTaskList =
  do
    cls' <- getRequiredClass "INTaskListResolutionResult"
    withObjCPtr resolvedTaskList $ \raw_resolvedTaskList ->
      sendClassMsg cls' (mkSelector "successWithResolvedTaskList:") (retPtr retVoid) [argPtr (castPtr raw_resolvedTaskList :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disambiguationWithTaskListsToDisambiguate:@
disambiguationWithTaskListsToDisambiguate :: IsNSArray taskListsToDisambiguate => taskListsToDisambiguate -> IO (Id INTaskListResolutionResult)
disambiguationWithTaskListsToDisambiguate taskListsToDisambiguate =
  do
    cls' <- getRequiredClass "INTaskListResolutionResult"
    withObjCPtr taskListsToDisambiguate $ \raw_taskListsToDisambiguate ->
      sendClassMsg cls' (mkSelector "disambiguationWithTaskListsToDisambiguate:") (retPtr retVoid) [argPtr (castPtr raw_taskListsToDisambiguate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithTaskListToConfirm:@
confirmationRequiredWithTaskListToConfirm :: IsINTaskList taskListToConfirm => taskListToConfirm -> IO (Id INTaskListResolutionResult)
confirmationRequiredWithTaskListToConfirm taskListToConfirm =
  do
    cls' <- getRequiredClass "INTaskListResolutionResult"
    withObjCPtr taskListToConfirm $ \raw_taskListToConfirm ->
      sendClassMsg cls' (mkSelector "confirmationRequiredWithTaskListToConfirm:") (retPtr retVoid) [argPtr (castPtr raw_taskListToConfirm :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedTaskList:@
successWithResolvedTaskListSelector :: Selector
successWithResolvedTaskListSelector = mkSelector "successWithResolvedTaskList:"

-- | @Selector@ for @disambiguationWithTaskListsToDisambiguate:@
disambiguationWithTaskListsToDisambiguateSelector :: Selector
disambiguationWithTaskListsToDisambiguateSelector = mkSelector "disambiguationWithTaskListsToDisambiguate:"

-- | @Selector@ for @confirmationRequiredWithTaskListToConfirm:@
confirmationRequiredWithTaskListToConfirmSelector :: Selector
confirmationRequiredWithTaskListToConfirmSelector = mkSelector "confirmationRequiredWithTaskListToConfirm:"

