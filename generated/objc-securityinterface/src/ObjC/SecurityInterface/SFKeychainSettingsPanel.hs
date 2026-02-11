{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFKeychainSettingsPanel@.
module ObjC.SecurityInterface.SFKeychainSettingsPanel
  ( SFKeychainSettingsPanel
  , IsSFKeychainSettingsPanel(..)
  , sharedKeychainSettingsPanel
  , sharedKeychainSettingsPanelSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | sharedKeychainSettingsPanel
--
-- Returns a global instance of SFKeychainSettingsPanel object.
--
-- ObjC selector: @+ sharedKeychainSettingsPanel@
sharedKeychainSettingsPanel :: IO (Id SFKeychainSettingsPanel)
sharedKeychainSettingsPanel  =
  do
    cls' <- getRequiredClass "SFKeychainSettingsPanel"
    sendClassMsg cls' (mkSelector "sharedKeychainSettingsPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedKeychainSettingsPanel@
sharedKeychainSettingsPanelSelector :: Selector
sharedKeychainSettingsPanelSelector = mkSelector "sharedKeychainSettingsPanel"

