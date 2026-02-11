{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCallRecordTypeOptionsResolutionResult@.
module ObjC.Intents.INCallRecordTypeOptionsResolutionResult
  ( INCallRecordTypeOptionsResolutionResult
  , IsINCallRecordTypeOptionsResolutionResult(..)
  , successWithResolvedCallRecordTypeOptions
  , confirmationRequiredWithCallRecordTypeOptionsToConfirm
  , successWithResolvedCallRecordTypeOptionsSelector
  , confirmationRequiredWithCallRecordTypeOptionsToConfirmSelector

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

-- | @+ successWithResolvedCallRecordTypeOptions:@
successWithResolvedCallRecordTypeOptions :: INCallRecordTypeOptions -> IO (Id INCallRecordTypeOptionsResolutionResult)
successWithResolvedCallRecordTypeOptions resolvedCallRecordTypeOptions =
  do
    cls' <- getRequiredClass "INCallRecordTypeOptionsResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedCallRecordTypeOptions:") (retPtr retVoid) [argCULong (coerce resolvedCallRecordTypeOptions)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithCallRecordTypeOptionsToConfirm:@
confirmationRequiredWithCallRecordTypeOptionsToConfirm :: INCallRecordTypeOptions -> IO (Id INCallRecordTypeOptionsResolutionResult)
confirmationRequiredWithCallRecordTypeOptionsToConfirm callRecordTypeOptionsToConfirm =
  do
    cls' <- getRequiredClass "INCallRecordTypeOptionsResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithCallRecordTypeOptionsToConfirm:") (retPtr retVoid) [argCULong (coerce callRecordTypeOptionsToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCallRecordTypeOptions:@
successWithResolvedCallRecordTypeOptionsSelector :: Selector
successWithResolvedCallRecordTypeOptionsSelector = mkSelector "successWithResolvedCallRecordTypeOptions:"

-- | @Selector@ for @confirmationRequiredWithCallRecordTypeOptionsToConfirm:@
confirmationRequiredWithCallRecordTypeOptionsToConfirmSelector :: Selector
confirmationRequiredWithCallRecordTypeOptionsToConfirmSelector = mkSelector "confirmationRequiredWithCallRecordTypeOptionsToConfirm:"

