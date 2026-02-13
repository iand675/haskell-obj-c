{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INVisualCodeTypeResolutionResult@.
module ObjC.Intents.INVisualCodeTypeResolutionResult
  ( INVisualCodeTypeResolutionResult
  , IsINVisualCodeTypeResolutionResult(..)
  , successWithResolvedVisualCodeType
  , confirmationRequiredWithVisualCodeTypeToConfirm
  , confirmationRequiredWithVisualCodeTypeToConfirmSelector
  , successWithResolvedVisualCodeTypeSelector

  -- * Enum types
  , INVisualCodeType(INVisualCodeType)
  , pattern INVisualCodeTypeUnknown
  , pattern INVisualCodeTypeContact
  , pattern INVisualCodeTypeRequestPayment
  , pattern INVisualCodeTypeSendPayment
  , pattern INVisualCodeTypeTransit
  , pattern INVisualCodeTypeBus
  , pattern INVisualCodeTypeSubway

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

-- | @+ successWithResolvedVisualCodeType:@
successWithResolvedVisualCodeType :: INVisualCodeType -> IO (Id INVisualCodeTypeResolutionResult)
successWithResolvedVisualCodeType resolvedVisualCodeType =
  do
    cls' <- getRequiredClass "INVisualCodeTypeResolutionResult"
    sendClassMessage cls' successWithResolvedVisualCodeTypeSelector resolvedVisualCodeType

-- | @+ confirmationRequiredWithVisualCodeTypeToConfirm:@
confirmationRequiredWithVisualCodeTypeToConfirm :: INVisualCodeType -> IO (Id INVisualCodeTypeResolutionResult)
confirmationRequiredWithVisualCodeTypeToConfirm visualCodeTypeToConfirm =
  do
    cls' <- getRequiredClass "INVisualCodeTypeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithVisualCodeTypeToConfirmSelector visualCodeTypeToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedVisualCodeType:@
successWithResolvedVisualCodeTypeSelector :: Selector '[INVisualCodeType] (Id INVisualCodeTypeResolutionResult)
successWithResolvedVisualCodeTypeSelector = mkSelector "successWithResolvedVisualCodeType:"

-- | @Selector@ for @confirmationRequiredWithVisualCodeTypeToConfirm:@
confirmationRequiredWithVisualCodeTypeToConfirmSelector :: Selector '[INVisualCodeType] (Id INVisualCodeTypeResolutionResult)
confirmationRequiredWithVisualCodeTypeToConfirmSelector = mkSelector "confirmationRequiredWithVisualCodeTypeToConfirm:"

