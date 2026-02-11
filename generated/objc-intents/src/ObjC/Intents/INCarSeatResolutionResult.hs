{-# LANGUAGE PatternSynonyms #-}
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
  , successWithResolvedCarSeatSelector
  , successWithResolvedValueSelector
  , confirmationRequiredWithCarSeatToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector

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

-- | @+ successWithResolvedCarSeat:@
successWithResolvedCarSeat :: INCarSeat -> IO (Id INCarSeatResolutionResult)
successWithResolvedCarSeat resolvedCarSeat =
  do
    cls' <- getRequiredClass "INCarSeatResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedCarSeat:") (retPtr retVoid) [argCLong (coerce resolvedCarSeat)] >>= retainedObject . castPtr

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INCarSeat -> IO (Id INCarSeatResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INCarSeatResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedValue:") (retPtr retVoid) [argCLong (coerce resolvedValue)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithCarSeatToConfirm:@
confirmationRequiredWithCarSeatToConfirm :: INCarSeat -> IO (Id INCarSeatResolutionResult)
confirmationRequiredWithCarSeatToConfirm carSeatToConfirm =
  do
    cls' <- getRequiredClass "INCarSeatResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithCarSeatToConfirm:") (retPtr retVoid) [argCLong (coerce carSeatToConfirm)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INCarSeat -> IO (Id INCarSeatResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INCarSeatResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithValueToConfirm:") (retPtr retVoid) [argCLong (coerce valueToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCarSeat:@
successWithResolvedCarSeatSelector :: Selector
successWithResolvedCarSeatSelector = mkSelector "successWithResolvedCarSeat:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithCarSeatToConfirm:@
confirmationRequiredWithCarSeatToConfirmSelector :: Selector
confirmationRequiredWithCarSeatToConfirmSelector = mkSelector "confirmationRequiredWithCarSeatToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

