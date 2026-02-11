{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPaymentPass@.
module ObjC.PassKit.PKPaymentPass
  ( PKPaymentPass
  , IsPKPaymentPass(..)
  , activationState
  , activationStateSelector

  -- * Enum types
  , PKPaymentPassActivationState(PKPaymentPassActivationState)
  , pattern PKPaymentPassActivationStateActivated
  , pattern PKPaymentPassActivationStateRequiresActivation
  , pattern PKPaymentPassActivationStateActivating
  , pattern PKPaymentPassActivationStateSuspended
  , pattern PKPaymentPassActivationStateDeactivated

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

import ObjC.PassKit.Internal.Classes
import ObjC.PassKit.Internal.Enums

-- | @- activationState@
activationState :: IsPKPaymentPass pkPaymentPass => pkPaymentPass -> IO PKPaymentPassActivationState
activationState pkPaymentPass  =
  fmap (coerce :: CULong -> PKPaymentPassActivationState) $ sendMsg pkPaymentPass (mkSelector "activationState") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @activationState@
activationStateSelector :: Selector
activationStateSelector = mkSelector "activationState"

