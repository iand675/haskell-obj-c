{-# LANGUAGE PatternSynonyms #-}
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

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.CryptoTokenKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Bitmask specifying whether PIN confirmation should be requested.
--
-- Note: Default value: TKSmartCardPINConfirmationNone
--
-- ObjC selector: @- PINConfirmation@
pinConfirmation :: IsTKSmartCardUserInteractionForSecurePINChange tkSmartCardUserInteractionForSecurePINChange => tkSmartCardUserInteractionForSecurePINChange -> IO TKSmartCardPINConfirmation
pinConfirmation tkSmartCardUserInteractionForSecurePINChange  =
  fmap (coerce :: CULong -> TKSmartCardPINConfirmation) $ sendMsg tkSmartCardUserInteractionForSecurePINChange (mkSelector "PINConfirmation") retCULong []

-- | Bitmask specifying whether PIN confirmation should be requested.
--
-- Note: Default value: TKSmartCardPINConfirmationNone
--
-- ObjC selector: @- setPINConfirmation:@
setPINConfirmation :: IsTKSmartCardUserInteractionForSecurePINChange tkSmartCardUserInteractionForSecurePINChange => tkSmartCardUserInteractionForSecurePINChange -> TKSmartCardPINConfirmation -> IO ()
setPINConfirmation tkSmartCardUserInteractionForSecurePINChange  value =
  sendMsg tkSmartCardUserInteractionForSecurePINChange (mkSelector "setPINConfirmation:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @PINConfirmation@
pinConfirmationSelector :: Selector
pinConfirmationSelector = mkSelector "PINConfirmation"

-- | @Selector@ for @setPINConfirmation:@
setPINConfirmationSelector :: Selector
setPINConfirmationSelector = mkSelector "setPINConfirmation:"

