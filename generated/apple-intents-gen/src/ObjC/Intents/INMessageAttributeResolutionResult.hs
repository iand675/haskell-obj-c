{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , confirmationRequiredWithMessageAttributeToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector
  , successWithResolvedMessageAttributeSelector
  , successWithResolvedValueSelector

  -- * Enum types
  , INMessageAttribute(INMessageAttribute)
  , pattern INMessageAttributeUnknown
  , pattern INMessageAttributeRead
  , pattern INMessageAttributeUnread
  , pattern INMessageAttributeFlagged
  , pattern INMessageAttributeUnflagged
  , pattern INMessageAttributePlayed

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

-- | @+ successWithResolvedMessageAttribute:@
successWithResolvedMessageAttribute :: INMessageAttribute -> IO (Id INMessageAttributeResolutionResult)
successWithResolvedMessageAttribute resolvedMessageAttribute =
  do
    cls' <- getRequiredClass "INMessageAttributeResolutionResult"
    sendClassMessage cls' successWithResolvedMessageAttributeSelector resolvedMessageAttribute

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INMessageAttribute -> IO (Id INMessageAttributeResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INMessageAttributeResolutionResult"
    sendClassMessage cls' successWithResolvedValueSelector resolvedValue

-- | @+ confirmationRequiredWithMessageAttributeToConfirm:@
confirmationRequiredWithMessageAttributeToConfirm :: INMessageAttribute -> IO (Id INMessageAttributeResolutionResult)
confirmationRequiredWithMessageAttributeToConfirm messageAttributeToConfirm =
  do
    cls' <- getRequiredClass "INMessageAttributeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithMessageAttributeToConfirmSelector messageAttributeToConfirm

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INMessageAttribute -> IO (Id INMessageAttributeResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INMessageAttributeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithValueToConfirmSelector valueToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedMessageAttribute:@
successWithResolvedMessageAttributeSelector :: Selector '[INMessageAttribute] (Id INMessageAttributeResolutionResult)
successWithResolvedMessageAttributeSelector = mkSelector "successWithResolvedMessageAttribute:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector '[INMessageAttribute] (Id INMessageAttributeResolutionResult)
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithMessageAttributeToConfirm:@
confirmationRequiredWithMessageAttributeToConfirmSelector :: Selector '[INMessageAttribute] (Id INMessageAttributeResolutionResult)
confirmationRequiredWithMessageAttributeToConfirmSelector = mkSelector "confirmationRequiredWithMessageAttributeToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector '[INMessageAttribute] (Id INMessageAttributeResolutionResult)
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

