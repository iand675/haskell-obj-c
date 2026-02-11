{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCarDefrosterResolutionResult@.
module ObjC.Intents.INCarDefrosterResolutionResult
  ( INCarDefrosterResolutionResult
  , IsINCarDefrosterResolutionResult(..)
  , successWithResolvedCarDefroster
  , successWithResolvedValue
  , confirmationRequiredWithCarDefrosterToConfirm
  , confirmationRequiredWithValueToConfirm
  , successWithResolvedCarDefrosterSelector
  , successWithResolvedValueSelector
  , confirmationRequiredWithCarDefrosterToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector

  -- * Enum types
  , INCarDefroster(INCarDefroster)
  , pattern INCarDefrosterUnknown
  , pattern INCarDefrosterFront
  , pattern INCarDefrosterRear
  , pattern INCarDefrosterAll

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

-- | @+ successWithResolvedCarDefroster:@
successWithResolvedCarDefroster :: INCarDefroster -> IO (Id INCarDefrosterResolutionResult)
successWithResolvedCarDefroster resolvedCarDefroster =
  do
    cls' <- getRequiredClass "INCarDefrosterResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedCarDefroster:") (retPtr retVoid) [argCLong (coerce resolvedCarDefroster)] >>= retainedObject . castPtr

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INCarDefroster -> IO (Id INCarDefrosterResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INCarDefrosterResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedValue:") (retPtr retVoid) [argCLong (coerce resolvedValue)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithCarDefrosterToConfirm:@
confirmationRequiredWithCarDefrosterToConfirm :: INCarDefroster -> IO (Id INCarDefrosterResolutionResult)
confirmationRequiredWithCarDefrosterToConfirm carDefrosterToConfirm =
  do
    cls' <- getRequiredClass "INCarDefrosterResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithCarDefrosterToConfirm:") (retPtr retVoid) [argCLong (coerce carDefrosterToConfirm)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INCarDefroster -> IO (Id INCarDefrosterResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INCarDefrosterResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithValueToConfirm:") (retPtr retVoid) [argCLong (coerce valueToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCarDefroster:@
successWithResolvedCarDefrosterSelector :: Selector
successWithResolvedCarDefrosterSelector = mkSelector "successWithResolvedCarDefroster:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithCarDefrosterToConfirm:@
confirmationRequiredWithCarDefrosterToConfirmSelector :: Selector
confirmationRequiredWithCarDefrosterToConfirmSelector = mkSelector "confirmationRequiredWithCarDefrosterToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

