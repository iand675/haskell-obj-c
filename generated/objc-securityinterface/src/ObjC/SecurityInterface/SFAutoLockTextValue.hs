{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SFKeychainSettingsPanel
--
-- SFKeychainSettingsPanel is a panel and sheet interface that allows users to change their keychain settings.
--
-- Generated bindings for @SFAutoLockTextValue@.
module ObjC.SecurityInterface.SFAutoLockTextValue
  ( SFAutoLockTextValue
  , IsSFAutoLockTextValue(..)


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

import ObjC.SecurityInterface.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

