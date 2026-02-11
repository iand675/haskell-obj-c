{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCallRecordTypeResolutionResult@.
module ObjC.Intents.INCallRecordTypeResolutionResult
  ( INCallRecordTypeResolutionResult
  , IsINCallRecordTypeResolutionResult(..)
  , successWithResolvedCallRecordType
  , confirmationRequiredWithCallRecordTypeToConfirm
  , successWithResolvedCallRecordTypeSelector
  , confirmationRequiredWithCallRecordTypeToConfirmSelector

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

-- | @+ successWithResolvedCallRecordType:@
successWithResolvedCallRecordType :: INCallRecordType -> IO (Id INCallRecordTypeResolutionResult)
successWithResolvedCallRecordType resolvedCallRecordType =
  do
    cls' <- getRequiredClass "INCallRecordTypeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedCallRecordType:") (retPtr retVoid) [argCLong (coerce resolvedCallRecordType)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithCallRecordTypeToConfirm:@
confirmationRequiredWithCallRecordTypeToConfirm :: INCallRecordType -> IO (Id INCallRecordTypeResolutionResult)
confirmationRequiredWithCallRecordTypeToConfirm callRecordTypeToConfirm =
  do
    cls' <- getRequiredClass "INCallRecordTypeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithCallRecordTypeToConfirm:") (retPtr retVoid) [argCLong (coerce callRecordTypeToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCallRecordType:@
successWithResolvedCallRecordTypeSelector :: Selector
successWithResolvedCallRecordTypeSelector = mkSelector "successWithResolvedCallRecordType:"

-- | @Selector@ for @confirmationRequiredWithCallRecordTypeToConfirm:@
confirmationRequiredWithCallRecordTypeToConfirmSelector :: Selector
confirmationRequiredWithCallRecordTypeToConfirmSelector = mkSelector "confirmationRequiredWithCallRecordTypeToConfirm:"

