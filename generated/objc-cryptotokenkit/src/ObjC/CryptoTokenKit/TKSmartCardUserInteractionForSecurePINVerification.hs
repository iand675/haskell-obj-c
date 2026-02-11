{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | User interaction for the secure PIN verification on the SmartCard reader.
--
-- Note: Result is available after the interaction has been successfully completed.
--
-- Generated bindings for @TKSmartCardUserInteractionForSecurePINVerification@.
module ObjC.CryptoTokenKit.TKSmartCardUserInteractionForSecurePINVerification
  ( TKSmartCardUserInteractionForSecurePINVerification
  , IsTKSmartCardUserInteractionForSecurePINVerification(..)


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
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

