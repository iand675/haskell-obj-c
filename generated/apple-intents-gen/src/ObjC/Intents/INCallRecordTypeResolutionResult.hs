{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCallRecordTypeResolutionResult@.
module ObjC.Intents.INCallRecordTypeResolutionResult
  ( INCallRecordTypeResolutionResult
  , IsINCallRecordTypeResolutionResult(..)
  , successWithResolvedCallRecordType
  , confirmationRequiredWithCallRecordTypeToConfirm
  , confirmationRequiredWithCallRecordTypeToConfirmSelector
  , successWithResolvedCallRecordTypeSelector

  -- * Enum types
  , INCallRecordType(INCallRecordType)
  , pattern INCallRecordTypeUnknown
  , pattern INCallRecordTypeOutgoing
  , pattern INCallRecordTypeMissed
  , pattern INCallRecordTypeReceived
  , pattern INCallRecordTypeLatest
  , pattern INCallRecordTypeVoicemail
  , pattern INCallRecordTypeRinging
  , pattern INCallRecordTypeInProgress
  , pattern INCallRecordTypeOnHold

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

-- | @+ successWithResolvedCallRecordType:@
successWithResolvedCallRecordType :: INCallRecordType -> IO (Id INCallRecordTypeResolutionResult)
successWithResolvedCallRecordType resolvedCallRecordType =
  do
    cls' <- getRequiredClass "INCallRecordTypeResolutionResult"
    sendClassMessage cls' successWithResolvedCallRecordTypeSelector resolvedCallRecordType

-- | @+ confirmationRequiredWithCallRecordTypeToConfirm:@
confirmationRequiredWithCallRecordTypeToConfirm :: INCallRecordType -> IO (Id INCallRecordTypeResolutionResult)
confirmationRequiredWithCallRecordTypeToConfirm callRecordTypeToConfirm =
  do
    cls' <- getRequiredClass "INCallRecordTypeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithCallRecordTypeToConfirmSelector callRecordTypeToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCallRecordType:@
successWithResolvedCallRecordTypeSelector :: Selector '[INCallRecordType] (Id INCallRecordTypeResolutionResult)
successWithResolvedCallRecordTypeSelector = mkSelector "successWithResolvedCallRecordType:"

-- | @Selector@ for @confirmationRequiredWithCallRecordTypeToConfirm:@
confirmationRequiredWithCallRecordTypeToConfirmSelector :: Selector '[INCallRecordType] (Id INCallRecordTypeResolutionResult)
confirmationRequiredWithCallRecordTypeToConfirmSelector = mkSelector "confirmationRequiredWithCallRecordTypeToConfirm:"

