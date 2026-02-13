{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCallRecordTypeOptionsResolutionResult@.
module ObjC.Intents.INCallRecordTypeOptionsResolutionResult
  ( INCallRecordTypeOptionsResolutionResult
  , IsINCallRecordTypeOptionsResolutionResult(..)
  , successWithResolvedCallRecordTypeOptions
  , confirmationRequiredWithCallRecordTypeOptionsToConfirm
  , confirmationRequiredWithCallRecordTypeOptionsToConfirmSelector
  , successWithResolvedCallRecordTypeOptionsSelector

  -- * Enum types
  , INCallRecordTypeOptions(INCallRecordTypeOptions)
  , pattern INCallRecordTypeOptionOutgoing
  , pattern INCallRecordTypeOptionMissed
  , pattern INCallRecordTypeOptionReceived
  , pattern INCallRecordTypeOptionLatest
  , pattern INCallRecordTypeOptionVoicemail
  , pattern INCallRecordTypeOptionRinging
  , pattern INCallRecordTypeOptionInProgress
  , pattern INCallRecordTypeOptionOnHold

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

-- | @+ successWithResolvedCallRecordTypeOptions:@
successWithResolvedCallRecordTypeOptions :: INCallRecordTypeOptions -> IO (Id INCallRecordTypeOptionsResolutionResult)
successWithResolvedCallRecordTypeOptions resolvedCallRecordTypeOptions =
  do
    cls' <- getRequiredClass "INCallRecordTypeOptionsResolutionResult"
    sendClassMessage cls' successWithResolvedCallRecordTypeOptionsSelector resolvedCallRecordTypeOptions

-- | @+ confirmationRequiredWithCallRecordTypeOptionsToConfirm:@
confirmationRequiredWithCallRecordTypeOptionsToConfirm :: INCallRecordTypeOptions -> IO (Id INCallRecordTypeOptionsResolutionResult)
confirmationRequiredWithCallRecordTypeOptionsToConfirm callRecordTypeOptionsToConfirm =
  do
    cls' <- getRequiredClass "INCallRecordTypeOptionsResolutionResult"
    sendClassMessage cls' confirmationRequiredWithCallRecordTypeOptionsToConfirmSelector callRecordTypeOptionsToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCallRecordTypeOptions:@
successWithResolvedCallRecordTypeOptionsSelector :: Selector '[INCallRecordTypeOptions] (Id INCallRecordTypeOptionsResolutionResult)
successWithResolvedCallRecordTypeOptionsSelector = mkSelector "successWithResolvedCallRecordTypeOptions:"

-- | @Selector@ for @confirmationRequiredWithCallRecordTypeOptionsToConfirm:@
confirmationRequiredWithCallRecordTypeOptionsToConfirmSelector :: Selector '[INCallRecordTypeOptions] (Id INCallRecordTypeOptionsResolutionResult)
confirmationRequiredWithCallRecordTypeOptionsToConfirmSelector = mkSelector "confirmationRequiredWithCallRecordTypeOptionsToConfirm:"

