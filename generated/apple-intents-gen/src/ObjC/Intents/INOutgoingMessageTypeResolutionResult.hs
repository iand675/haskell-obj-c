{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INOutgoingMessageTypeResolutionResult@.
module ObjC.Intents.INOutgoingMessageTypeResolutionResult
  ( INOutgoingMessageTypeResolutionResult
  , IsINOutgoingMessageTypeResolutionResult(..)
  , successWithResolvedOutgoingMessageType
  , confirmationRequiredWithOutgoingMessageTypeToConfirm
  , confirmationRequiredWithOutgoingMessageTypeToConfirmSelector
  , successWithResolvedOutgoingMessageTypeSelector

  -- * Enum types
  , INOutgoingMessageType(INOutgoingMessageType)
  , pattern INOutgoingMessageTypeUnknown
  , pattern INOutgoingMessageTypeOutgoingMessageText
  , pattern INOutgoingMessageTypeOutgoingMessageAudio

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

-- | @+ successWithResolvedOutgoingMessageType:@
successWithResolvedOutgoingMessageType :: INOutgoingMessageType -> IO (Id INOutgoingMessageTypeResolutionResult)
successWithResolvedOutgoingMessageType resolvedOutgoingMessageType =
  do
    cls' <- getRequiredClass "INOutgoingMessageTypeResolutionResult"
    sendClassMessage cls' successWithResolvedOutgoingMessageTypeSelector resolvedOutgoingMessageType

-- | @+ confirmationRequiredWithOutgoingMessageTypeToConfirm:@
confirmationRequiredWithOutgoingMessageTypeToConfirm :: INOutgoingMessageType -> IO (Id INOutgoingMessageTypeResolutionResult)
confirmationRequiredWithOutgoingMessageTypeToConfirm outgoingMessageTypeToConfirm =
  do
    cls' <- getRequiredClass "INOutgoingMessageTypeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithOutgoingMessageTypeToConfirmSelector outgoingMessageTypeToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedOutgoingMessageType:@
successWithResolvedOutgoingMessageTypeSelector :: Selector '[INOutgoingMessageType] (Id INOutgoingMessageTypeResolutionResult)
successWithResolvedOutgoingMessageTypeSelector = mkSelector "successWithResolvedOutgoingMessageType:"

-- | @Selector@ for @confirmationRequiredWithOutgoingMessageTypeToConfirm:@
confirmationRequiredWithOutgoingMessageTypeToConfirmSelector :: Selector '[INOutgoingMessageType] (Id INOutgoingMessageTypeResolutionResult)
confirmationRequiredWithOutgoingMessageTypeToConfirmSelector = mkSelector "confirmationRequiredWithOutgoingMessageTypeToConfirm:"

