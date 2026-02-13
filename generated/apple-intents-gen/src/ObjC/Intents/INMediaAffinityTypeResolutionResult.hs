{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INMediaAffinityTypeResolutionResult@.
module ObjC.Intents.INMediaAffinityTypeResolutionResult
  ( INMediaAffinityTypeResolutionResult
  , IsINMediaAffinityTypeResolutionResult(..)
  , successWithResolvedMediaAffinityType
  , confirmationRequiredWithMediaAffinityTypeToConfirm
  , confirmationRequiredWithMediaAffinityTypeToConfirmSelector
  , successWithResolvedMediaAffinityTypeSelector

  -- * Enum types
  , INMediaAffinityType(INMediaAffinityType)
  , pattern INMediaAffinityTypeUnknown
  , pattern INMediaAffinityTypeLike
  , pattern INMediaAffinityTypeDislike

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedMediaAffinityType:@
successWithResolvedMediaAffinityType :: INMediaAffinityType -> IO (Id INMediaAffinityTypeResolutionResult)
successWithResolvedMediaAffinityType resolvedMediaAffinityType =
  do
    cls' <- getRequiredClass "INMediaAffinityTypeResolutionResult"
    sendClassMessage cls' successWithResolvedMediaAffinityTypeSelector resolvedMediaAffinityType

-- | @+ confirmationRequiredWithMediaAffinityTypeToConfirm:@
confirmationRequiredWithMediaAffinityTypeToConfirm :: INMediaAffinityType -> IO (Id INMediaAffinityTypeResolutionResult)
confirmationRequiredWithMediaAffinityTypeToConfirm mediaAffinityTypeToConfirm =
  do
    cls' <- getRequiredClass "INMediaAffinityTypeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithMediaAffinityTypeToConfirmSelector mediaAffinityTypeToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedMediaAffinityType:@
successWithResolvedMediaAffinityTypeSelector :: Selector '[INMediaAffinityType] (Id INMediaAffinityTypeResolutionResult)
successWithResolvedMediaAffinityTypeSelector = mkSelector "successWithResolvedMediaAffinityType:"

-- | @Selector@ for @confirmationRequiredWithMediaAffinityTypeToConfirm:@
confirmationRequiredWithMediaAffinityTypeToConfirmSelector :: Selector '[INMediaAffinityType] (Id INMediaAffinityTypeResolutionResult)
confirmationRequiredWithMediaAffinityTypeToConfirmSelector = mkSelector "confirmationRequiredWithMediaAffinityTypeToConfirm:"

