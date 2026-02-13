{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.PassKit.Internal.Enums

-- | @- activationState@
activationState :: IsPKPaymentPass pkPaymentPass => pkPaymentPass -> IO PKPaymentPassActivationState
activationState pkPaymentPass =
  sendMessage pkPaymentPass activationStateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @activationState@
activationStateSelector :: Selector '[] PKPaymentPassActivationState
activationStateSelector = mkSelector "activationState"

