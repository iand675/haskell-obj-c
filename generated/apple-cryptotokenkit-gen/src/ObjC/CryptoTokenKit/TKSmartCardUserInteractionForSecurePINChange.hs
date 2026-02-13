{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | User interaction for the secure PIN change on the SmartCard reader.
--
-- Note: Result is available after the interaction has been successfully completed.
--
-- Generated bindings for @TKSmartCardUserInteractionForSecurePINChange@.
module ObjC.CryptoTokenKit.TKSmartCardUserInteractionForSecurePINChange
  ( TKSmartCardUserInteractionForSecurePINChange
  , IsTKSmartCardUserInteractionForSecurePINChange(..)
  , pinConfirmation
  , setPINConfirmation
  , pinConfirmationSelector
  , setPINConfirmationSelector

  -- * Enum types
  , TKSmartCardPINConfirmation(TKSmartCardPINConfirmation)
  , pattern TKSmartCardPINConfirmationNone
  , pattern TKSmartCardPINConfirmationNew
  , pattern TKSmartCardPINConfirmationCurrent

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.CryptoTokenKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Bitmask specifying whether PIN confirmation should be requested.
--
-- Note: Default value: TKSmartCardPINConfirmationNone
--
-- ObjC selector: @- PINConfirmation@
pinConfirmation :: IsTKSmartCardUserInteractionForSecurePINChange tkSmartCardUserInteractionForSecurePINChange => tkSmartCardUserInteractionForSecurePINChange -> IO TKSmartCardPINConfirmation
pinConfirmation tkSmartCardUserInteractionForSecurePINChange =
  sendMessage tkSmartCardUserInteractionForSecurePINChange pinConfirmationSelector

-- | Bitmask specifying whether PIN confirmation should be requested.
--
-- Note: Default value: TKSmartCardPINConfirmationNone
--
-- ObjC selector: @- setPINConfirmation:@
setPINConfirmation :: IsTKSmartCardUserInteractionForSecurePINChange tkSmartCardUserInteractionForSecurePINChange => tkSmartCardUserInteractionForSecurePINChange -> TKSmartCardPINConfirmation -> IO ()
setPINConfirmation tkSmartCardUserInteractionForSecurePINChange value =
  sendMessage tkSmartCardUserInteractionForSecurePINChange setPINConfirmationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @PINConfirmation@
pinConfirmationSelector :: Selector '[] TKSmartCardPINConfirmation
pinConfirmationSelector = mkSelector "PINConfirmation"

-- | @Selector@ for @setPINConfirmation:@
setPINConfirmationSelector :: Selector '[TKSmartCardPINConfirmation] ()
setPINConfirmationSelector = mkSelector "setPINConfirmation:"

