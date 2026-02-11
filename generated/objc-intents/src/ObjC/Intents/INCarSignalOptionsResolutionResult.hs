{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCarSignalOptionsResolutionResult@.
module ObjC.Intents.INCarSignalOptionsResolutionResult
  ( INCarSignalOptionsResolutionResult
  , IsINCarSignalOptionsResolutionResult(..)
  , successWithResolvedCarSignalOptions
  , successWithResolvedValue
  , confirmationRequiredWithCarSignalOptionsToConfirm
  , confirmationRequiredWithValueToConfirm
  , successWithResolvedCarSignalOptionsSelector
  , successWithResolvedValueSelector
  , confirmationRequiredWithCarSignalOptionsToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector

  -- * Enum types
  , INCarSignalOptions(INCarSignalOptions)
  , pattern INCarSignalOptionAudible
  , pattern INCarSignalOptionVisible

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

-- | @+ successWithResolvedCarSignalOptions:@
successWithResolvedCarSignalOptions :: INCarSignalOptions -> IO (Id INCarSignalOptionsResolutionResult)
successWithResolvedCarSignalOptions resolvedCarSignalOptions =
  do
    cls' <- getRequiredClass "INCarSignalOptionsResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedCarSignalOptions:") (retPtr retVoid) [argCULong (coerce resolvedCarSignalOptions)] >>= retainedObject . castPtr

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INCarSignalOptions -> IO (Id INCarSignalOptionsResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INCarSignalOptionsResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedValue:") (retPtr retVoid) [argCULong (coerce resolvedValue)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithCarSignalOptionsToConfirm:@
confirmationRequiredWithCarSignalOptionsToConfirm :: INCarSignalOptions -> IO (Id INCarSignalOptionsResolutionResult)
confirmationRequiredWithCarSignalOptionsToConfirm carSignalOptionsToConfirm =
  do
    cls' <- getRequiredClass "INCarSignalOptionsResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithCarSignalOptionsToConfirm:") (retPtr retVoid) [argCULong (coerce carSignalOptionsToConfirm)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INCarSignalOptions -> IO (Id INCarSignalOptionsResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INCarSignalOptionsResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithValueToConfirm:") (retPtr retVoid) [argCULong (coerce valueToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCarSignalOptions:@
successWithResolvedCarSignalOptionsSelector :: Selector
successWithResolvedCarSignalOptionsSelector = mkSelector "successWithResolvedCarSignalOptions:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithCarSignalOptionsToConfirm:@
confirmationRequiredWithCarSignalOptionsToConfirmSelector :: Selector
confirmationRequiredWithCarSignalOptionsToConfirmSelector = mkSelector "confirmationRequiredWithCarSignalOptionsToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

