{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCarSeatResolutionResult@.
module ObjC.Intents.INCarSeatResolutionResult
  ( INCarSeatResolutionResult
  , IsINCarSeatResolutionResult(..)
  , successWithResolvedCarSeat
  , successWithResolvedValue
  , confirmationRequiredWithCarSeatToConfirm
  , confirmationRequiredWithValueToConfirm
  , confirmationRequiredWithCarSeatToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector
  , successWithResolvedCarSeatSelector
  , successWithResolvedValueSelector

  -- * Enum types
  , INCarSeat(INCarSeat)
  , pattern INCarSeatUnknown
  , pattern INCarSeatDriver
  , pattern INCarSeatPassenger
  , pattern INCarSeatFrontLeft
  , pattern INCarSeatFrontRight
  , pattern INCarSeatFront
  , pattern INCarSeatRearLeft
  , pattern INCarSeatRearRight
  , pattern INCarSeatRear
  , pattern INCarSeatThirdRowLeft
  , pattern INCarSeatThirdRowRight
  , pattern INCarSeatThirdRow
  , pattern INCarSeatAll

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

-- | @+ successWithResolvedCarSeat:@
successWithResolvedCarSeat :: INCarSeat -> IO (Id INCarSeatResolutionResult)
successWithResolvedCarSeat resolvedCarSeat =
  do
    cls' <- getRequiredClass "INCarSeatResolutionResult"
    sendClassMessage cls' successWithResolvedCarSeatSelector resolvedCarSeat

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INCarSeat -> IO (Id INCarSeatResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INCarSeatResolutionResult"
    sendClassMessage cls' successWithResolvedValueSelector resolvedValue

-- | @+ confirmationRequiredWithCarSeatToConfirm:@
confirmationRequiredWithCarSeatToConfirm :: INCarSeat -> IO (Id INCarSeatResolutionResult)
confirmationRequiredWithCarSeatToConfirm carSeatToConfirm =
  do
    cls' <- getRequiredClass "INCarSeatResolutionResult"
    sendClassMessage cls' confirmationRequiredWithCarSeatToConfirmSelector carSeatToConfirm

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INCarSeat -> IO (Id INCarSeatResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INCarSeatResolutionResult"
    sendClassMessage cls' confirmationRequiredWithValueToConfirmSelector valueToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCarSeat:@
successWithResolvedCarSeatSelector :: Selector '[INCarSeat] (Id INCarSeatResolutionResult)
successWithResolvedCarSeatSelector = mkSelector "successWithResolvedCarSeat:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector '[INCarSeat] (Id INCarSeatResolutionResult)
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithCarSeatToConfirm:@
confirmationRequiredWithCarSeatToConfirmSelector :: Selector '[INCarSeat] (Id INCarSeatResolutionResult)
confirmationRequiredWithCarSeatToConfirmSelector = mkSelector "confirmationRequiredWithCarSeatToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector '[INCarSeat] (Id INCarSeatResolutionResult)
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

