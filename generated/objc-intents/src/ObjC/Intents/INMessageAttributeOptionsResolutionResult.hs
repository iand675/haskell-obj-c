{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INMessageAttributeOptionsResolutionResult@.
module ObjC.Intents.INMessageAttributeOptionsResolutionResult
  ( INMessageAttributeOptionsResolutionResult
  , IsINMessageAttributeOptionsResolutionResult(..)
  , successWithResolvedMessageAttributeOptions
  , successWithResolvedValue
  , confirmationRequiredWithMessageAttributeOptionsToConfirm
  , confirmationRequiredWithValueToConfirm
  , successWithResolvedMessageAttributeOptionsSelector
  , successWithResolvedValueSelector
  , confirmationRequiredWithMessageAttributeOptionsToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector

  -- * Enum types
  , INMessageAttributeOptions(INMessageAttributeOptions)
  , pattern INMessageAttributeOptionRead
  , pattern INMessageAttributeOptionUnread
  , pattern INMessageAttributeOptionFlagged
  , pattern INMessageAttributeOptionUnflagged
  , pattern INMessageAttributeOptionPlayed

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

-- | @+ successWithResolvedMessageAttributeOptions:@
successWithResolvedMessageAttributeOptions :: INMessageAttributeOptions -> IO (Id INMessageAttributeOptionsResolutionResult)
successWithResolvedMessageAttributeOptions resolvedMessageAttributeOptions =
  do
    cls' <- getRequiredClass "INMessageAttributeOptionsResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedMessageAttributeOptions:") (retPtr retVoid) [argCULong (coerce resolvedMessageAttributeOptions)] >>= retainedObject . castPtr

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INMessageAttributeOptions -> IO (Id INMessageAttributeOptionsResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INMessageAttributeOptionsResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedValue:") (retPtr retVoid) [argCULong (coerce resolvedValue)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithMessageAttributeOptionsToConfirm:@
confirmationRequiredWithMessageAttributeOptionsToConfirm :: INMessageAttributeOptions -> IO (Id INMessageAttributeOptionsResolutionResult)
confirmationRequiredWithMessageAttributeOptionsToConfirm messageAttributeOptionsToConfirm =
  do
    cls' <- getRequiredClass "INMessageAttributeOptionsResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithMessageAttributeOptionsToConfirm:") (retPtr retVoid) [argCULong (coerce messageAttributeOptionsToConfirm)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INMessageAttributeOptions -> IO (Id INMessageAttributeOptionsResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INMessageAttributeOptionsResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithValueToConfirm:") (retPtr retVoid) [argCULong (coerce valueToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedMessageAttributeOptions:@
successWithResolvedMessageAttributeOptionsSelector :: Selector
successWithResolvedMessageAttributeOptionsSelector = mkSelector "successWithResolvedMessageAttributeOptions:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithMessageAttributeOptionsToConfirm:@
confirmationRequiredWithMessageAttributeOptionsToConfirmSelector :: Selector
confirmationRequiredWithMessageAttributeOptionsToConfirmSelector = mkSelector "confirmationRequiredWithMessageAttributeOptionsToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

