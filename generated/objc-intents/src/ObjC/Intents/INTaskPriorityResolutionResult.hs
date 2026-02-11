{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTaskPriorityResolutionResult@.
module ObjC.Intents.INTaskPriorityResolutionResult
  ( INTaskPriorityResolutionResult
  , IsINTaskPriorityResolutionResult(..)
  , successWithResolvedTaskPriority
  , confirmationRequiredWithTaskPriorityToConfirm
  , successWithResolvedTaskPrioritySelector
  , confirmationRequiredWithTaskPriorityToConfirmSelector

  -- * Enum types
  , INTaskPriority(INTaskPriority)
  , pattern INTaskPriorityUnknown
  , pattern INTaskPriorityNotFlagged
  , pattern INTaskPriorityFlagged

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

-- | @+ successWithResolvedTaskPriority:@
successWithResolvedTaskPriority :: INTaskPriority -> IO (Id INTaskPriorityResolutionResult)
successWithResolvedTaskPriority resolvedTaskPriority =
  do
    cls' <- getRequiredClass "INTaskPriorityResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedTaskPriority:") (retPtr retVoid) [argCLong (coerce resolvedTaskPriority)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithTaskPriorityToConfirm:@
confirmationRequiredWithTaskPriorityToConfirm :: INTaskPriority -> IO (Id INTaskPriorityResolutionResult)
confirmationRequiredWithTaskPriorityToConfirm taskPriorityToConfirm =
  do
    cls' <- getRequiredClass "INTaskPriorityResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithTaskPriorityToConfirm:") (retPtr retVoid) [argCLong (coerce taskPriorityToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedTaskPriority:@
successWithResolvedTaskPrioritySelector :: Selector
successWithResolvedTaskPrioritySelector = mkSelector "successWithResolvedTaskPriority:"

-- | @Selector@ for @confirmationRequiredWithTaskPriorityToConfirm:@
confirmationRequiredWithTaskPriorityToConfirmSelector :: Selector
confirmationRequiredWithTaskPriorityToConfirmSelector = mkSelector "confirmationRequiredWithTaskPriorityToConfirm:"

