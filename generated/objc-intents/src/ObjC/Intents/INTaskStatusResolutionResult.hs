{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTaskStatusResolutionResult@.
module ObjC.Intents.INTaskStatusResolutionResult
  ( INTaskStatusResolutionResult
  , IsINTaskStatusResolutionResult(..)
  , successWithResolvedTaskStatus
  , confirmationRequiredWithTaskStatusToConfirm
  , successWithResolvedTaskStatusSelector
  , confirmationRequiredWithTaskStatusToConfirmSelector

  -- * Enum types
  , INTaskStatus(INTaskStatus)
  , pattern INTaskStatusUnknown
  , pattern INTaskStatusNotCompleted
  , pattern INTaskStatusCompleted

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

-- | @+ successWithResolvedTaskStatus:@
successWithResolvedTaskStatus :: INTaskStatus -> IO (Id INTaskStatusResolutionResult)
successWithResolvedTaskStatus resolvedTaskStatus =
  do
    cls' <- getRequiredClass "INTaskStatusResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedTaskStatus:") (retPtr retVoid) [argCLong (coerce resolvedTaskStatus)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithTaskStatusToConfirm:@
confirmationRequiredWithTaskStatusToConfirm :: INTaskStatus -> IO (Id INTaskStatusResolutionResult)
confirmationRequiredWithTaskStatusToConfirm taskStatusToConfirm =
  do
    cls' <- getRequiredClass "INTaskStatusResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithTaskStatusToConfirm:") (retPtr retVoid) [argCLong (coerce taskStatusToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedTaskStatus:@
successWithResolvedTaskStatusSelector :: Selector
successWithResolvedTaskStatusSelector = mkSelector "successWithResolvedTaskStatus:"

-- | @Selector@ for @confirmationRequiredWithTaskStatusToConfirm:@
confirmationRequiredWithTaskStatusToConfirmSelector :: Selector
confirmationRequiredWithTaskStatusToConfirmSelector = mkSelector "confirmationRequiredWithTaskStatusToConfirm:"

