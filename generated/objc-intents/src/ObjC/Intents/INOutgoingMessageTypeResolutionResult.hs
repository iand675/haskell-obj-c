{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INOutgoingMessageTypeResolutionResult@.
module ObjC.Intents.INOutgoingMessageTypeResolutionResult
  ( INOutgoingMessageTypeResolutionResult
  , IsINOutgoingMessageTypeResolutionResult(..)
  , successWithResolvedOutgoingMessageType
  , confirmationRequiredWithOutgoingMessageTypeToConfirm
  , successWithResolvedOutgoingMessageTypeSelector
  , confirmationRequiredWithOutgoingMessageTypeToConfirmSelector

  -- * Enum types
  , INOutgoingMessageType(INOutgoingMessageType)
  , pattern INOutgoingMessageTypeUnknown
  , pattern INOutgoingMessageTypeOutgoingMessageText
  , pattern INOutgoingMessageTypeOutgoingMessageAudio

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

-- | @+ successWithResolvedOutgoingMessageType:@
successWithResolvedOutgoingMessageType :: INOutgoingMessageType -> IO (Id INOutgoingMessageTypeResolutionResult)
successWithResolvedOutgoingMessageType resolvedOutgoingMessageType =
  do
    cls' <- getRequiredClass "INOutgoingMessageTypeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedOutgoingMessageType:") (retPtr retVoid) [argCLong (coerce resolvedOutgoingMessageType)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithOutgoingMessageTypeToConfirm:@
confirmationRequiredWithOutgoingMessageTypeToConfirm :: INOutgoingMessageType -> IO (Id INOutgoingMessageTypeResolutionResult)
confirmationRequiredWithOutgoingMessageTypeToConfirm outgoingMessageTypeToConfirm =
  do
    cls' <- getRequiredClass "INOutgoingMessageTypeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithOutgoingMessageTypeToConfirm:") (retPtr retVoid) [argCLong (coerce outgoingMessageTypeToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedOutgoingMessageType:@
successWithResolvedOutgoingMessageTypeSelector :: Selector
successWithResolvedOutgoingMessageTypeSelector = mkSelector "successWithResolvedOutgoingMessageType:"

-- | @Selector@ for @confirmationRequiredWithOutgoingMessageTypeToConfirm:@
confirmationRequiredWithOutgoingMessageTypeToConfirmSelector :: Selector
confirmationRequiredWithOutgoingMessageTypeToConfirmSelector = mkSelector "confirmationRequiredWithOutgoingMessageTypeToConfirm:"

