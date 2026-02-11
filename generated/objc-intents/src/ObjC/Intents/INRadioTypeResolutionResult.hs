{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRadioTypeResolutionResult@.
module ObjC.Intents.INRadioTypeResolutionResult
  ( INRadioTypeResolutionResult
  , IsINRadioTypeResolutionResult(..)
  , successWithResolvedRadioType
  , successWithResolvedValue
  , confirmationRequiredWithRadioTypeToConfirm
  , confirmationRequiredWithValueToConfirm
  , successWithResolvedRadioTypeSelector
  , successWithResolvedValueSelector
  , confirmationRequiredWithRadioTypeToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector

  -- * Enum types
  , INRadioType(INRadioType)
  , pattern INRadioTypeUnknown
  , pattern INRadioTypeAM
  , pattern INRadioTypeFM
  , pattern INRadioTypeHD
  , pattern INRadioTypeSatellite
  , pattern INRadioTypeDAB

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

-- | @+ successWithResolvedRadioType:@
successWithResolvedRadioType :: INRadioType -> IO (Id INRadioTypeResolutionResult)
successWithResolvedRadioType resolvedRadioType =
  do
    cls' <- getRequiredClass "INRadioTypeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedRadioType:") (retPtr retVoid) [argCLong (coerce resolvedRadioType)] >>= retainedObject . castPtr

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INRadioType -> IO (Id INRadioTypeResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INRadioTypeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedValue:") (retPtr retVoid) [argCLong (coerce resolvedValue)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithRadioTypeToConfirm:@
confirmationRequiredWithRadioTypeToConfirm :: INRadioType -> IO (Id INRadioTypeResolutionResult)
confirmationRequiredWithRadioTypeToConfirm radioTypeToConfirm =
  do
    cls' <- getRequiredClass "INRadioTypeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithRadioTypeToConfirm:") (retPtr retVoid) [argCLong (coerce radioTypeToConfirm)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INRadioType -> IO (Id INRadioTypeResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INRadioTypeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithValueToConfirm:") (retPtr retVoid) [argCLong (coerce valueToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedRadioType:@
successWithResolvedRadioTypeSelector :: Selector
successWithResolvedRadioTypeSelector = mkSelector "successWithResolvedRadioType:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithRadioTypeToConfirm:@
confirmationRequiredWithRadioTypeToConfirmSelector :: Selector
confirmationRequiredWithRadioTypeToConfirmSelector = mkSelector "confirmationRequiredWithRadioTypeToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

