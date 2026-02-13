{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , confirmationRequiredWithCarDefrosterToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector
  , successWithResolvedCarDefrosterSelector
  , successWithResolvedValueSelector

  -- * Enum types
  , INCarDefroster(INCarDefroster)
  , pattern INCarDefrosterUnknown
  , pattern INCarDefrosterFront
  , pattern INCarDefrosterRear
  , pattern INCarDefrosterAll

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

-- | @+ successWithResolvedCarDefroster:@
successWithResolvedCarDefroster :: INCarDefroster -> IO (Id INCarDefrosterResolutionResult)
successWithResolvedCarDefroster resolvedCarDefroster =
  do
    cls' <- getRequiredClass "INCarDefrosterResolutionResult"
    sendClassMessage cls' successWithResolvedCarDefrosterSelector resolvedCarDefroster

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INCarDefroster -> IO (Id INCarDefrosterResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INCarDefrosterResolutionResult"
    sendClassMessage cls' successWithResolvedValueSelector resolvedValue

-- | @+ confirmationRequiredWithCarDefrosterToConfirm:@
confirmationRequiredWithCarDefrosterToConfirm :: INCarDefroster -> IO (Id INCarDefrosterResolutionResult)
confirmationRequiredWithCarDefrosterToConfirm carDefrosterToConfirm =
  do
    cls' <- getRequiredClass "INCarDefrosterResolutionResult"
    sendClassMessage cls' confirmationRequiredWithCarDefrosterToConfirmSelector carDefrosterToConfirm

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INCarDefroster -> IO (Id INCarDefrosterResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INCarDefrosterResolutionResult"
    sendClassMessage cls' confirmationRequiredWithValueToConfirmSelector valueToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCarDefroster:@
successWithResolvedCarDefrosterSelector :: Selector '[INCarDefroster] (Id INCarDefrosterResolutionResult)
successWithResolvedCarDefrosterSelector = mkSelector "successWithResolvedCarDefroster:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector '[INCarDefroster] (Id INCarDefrosterResolutionResult)
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithCarDefrosterToConfirm:@
confirmationRequiredWithCarDefrosterToConfirmSelector :: Selector '[INCarDefroster] (Id INCarDefrosterResolutionResult)
confirmationRequiredWithCarDefrosterToConfirmSelector = mkSelector "confirmationRequiredWithCarDefrosterToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector '[INCarDefroster] (Id INCarDefrosterResolutionResult)
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

