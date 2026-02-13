{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INLocationSearchTypeResolutionResult@.
module ObjC.Intents.INLocationSearchTypeResolutionResult
  ( INLocationSearchTypeResolutionResult
  , IsINLocationSearchTypeResolutionResult(..)
  , successWithResolvedLocationSearchType
  , confirmationRequiredWithLocationSearchTypeToConfirm
  , confirmationRequiredWithLocationSearchTypeToConfirmSelector
  , successWithResolvedLocationSearchTypeSelector

  -- * Enum types
  , INLocationSearchType(INLocationSearchType)
  , pattern INLocationSearchTypeUnknown
  , pattern INLocationSearchTypeByLocationTrigger

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

-- | @+ successWithResolvedLocationSearchType:@
successWithResolvedLocationSearchType :: INLocationSearchType -> IO (Id INLocationSearchTypeResolutionResult)
successWithResolvedLocationSearchType resolvedLocationSearchType =
  do
    cls' <- getRequiredClass "INLocationSearchTypeResolutionResult"
    sendClassMessage cls' successWithResolvedLocationSearchTypeSelector resolvedLocationSearchType

-- | @+ confirmationRequiredWithLocationSearchTypeToConfirm:@
confirmationRequiredWithLocationSearchTypeToConfirm :: INLocationSearchType -> IO (Id INLocationSearchTypeResolutionResult)
confirmationRequiredWithLocationSearchTypeToConfirm locationSearchTypeToConfirm =
  do
    cls' <- getRequiredClass "INLocationSearchTypeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithLocationSearchTypeToConfirmSelector locationSearchTypeToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedLocationSearchType:@
successWithResolvedLocationSearchTypeSelector :: Selector '[INLocationSearchType] (Id INLocationSearchTypeResolutionResult)
successWithResolvedLocationSearchTypeSelector = mkSelector "successWithResolvedLocationSearchType:"

-- | @Selector@ for @confirmationRequiredWithLocationSearchTypeToConfirm:@
confirmationRequiredWithLocationSearchTypeToConfirmSelector :: Selector '[INLocationSearchType] (Id INLocationSearchTypeResolutionResult)
confirmationRequiredWithLocationSearchTypeToConfirmSelector = mkSelector "confirmationRequiredWithLocationSearchTypeToConfirm:"

