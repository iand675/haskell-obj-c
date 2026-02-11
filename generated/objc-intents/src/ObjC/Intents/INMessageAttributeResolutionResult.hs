{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INMessageAttributeResolutionResult@.
module ObjC.Intents.INMessageAttributeResolutionResult
  ( INMessageAttributeResolutionResult
  , IsINMessageAttributeResolutionResult(..)
  , successWithResolvedMessageAttribute
  , successWithResolvedValue
  , confirmationRequiredWithMessageAttributeToConfirm
  , confirmationRequiredWithValueToConfirm
  , successWithResolvedMessageAttributeSelector
  , successWithResolvedValueSelector
  , confirmationRequiredWithMessageAttributeToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector

  -- * Enum types
  , INMessageAttribute(INMessageAttribute)
  , pattern INMessageAttributeUnknown
  , pattern INMessageAttributeRead
  , pattern INMessageAttributeUnread
  , pattern INMessageAttributeFlagged
  , pattern INMessageAttributeUnflagged
  , pattern INMessageAttributePlayed

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

-- | @+ successWithResolvedMessageAttribute:@
successWithResolvedMessageAttribute :: INMessageAttribute -> IO (Id INMessageAttributeResolutionResult)
successWithResolvedMessageAttribute resolvedMessageAttribute =
  do
    cls' <- getRequiredClass "INMessageAttributeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedMessageAttribute:") (retPtr retVoid) [argCLong (coerce resolvedMessageAttribute)] >>= retainedObject . castPtr

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INMessageAttribute -> IO (Id INMessageAttributeResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INMessageAttributeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedValue:") (retPtr retVoid) [argCLong (coerce resolvedValue)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithMessageAttributeToConfirm:@
confirmationRequiredWithMessageAttributeToConfirm :: INMessageAttribute -> IO (Id INMessageAttributeResolutionResult)
confirmationRequiredWithMessageAttributeToConfirm messageAttributeToConfirm =
  do
    cls' <- getRequiredClass "INMessageAttributeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithMessageAttributeToConfirm:") (retPtr retVoid) [argCLong (coerce messageAttributeToConfirm)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INMessageAttribute -> IO (Id INMessageAttributeResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INMessageAttributeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithValueToConfirm:") (retPtr retVoid) [argCLong (coerce valueToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedMessageAttribute:@
successWithResolvedMessageAttributeSelector :: Selector
successWithResolvedMessageAttributeSelector = mkSelector "successWithResolvedMessageAttribute:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithMessageAttributeToConfirm:@
confirmationRequiredWithMessageAttributeToConfirmSelector :: Selector
confirmationRequiredWithMessageAttributeToConfirmSelector = mkSelector "confirmationRequiredWithMessageAttributeToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

