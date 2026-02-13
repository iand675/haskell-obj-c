{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFKeychainSettingsPanel@.
module ObjC.SecurityInterface.SFKeychainSettingsPanel
  ( SFKeychainSettingsPanel
  , IsSFKeychainSettingsPanel(..)
  , sharedKeychainSettingsPanel
  , runModalForSettings_keychain
  , beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_settings_keychain
  , beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_settings_keychainSelector
  , runModalForSettings_keychainSelector
  , sharedKeychainSettingsPanelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' sharedKeychainSettingsPanelSelector

-- | runModalForSettings:keychain:
--
-- Changes the keychain settings using the settings panel. The panel contains settings such as "lock on sleep", "automatic lock", etc. If the user attempts to change the settings of a locked keychain, the unlock panel will be presented. It returns NSOKButton or NSCancelButton.
--
-- @settings@ — A pointer to a keychain settings structure. Since this structure is versioned, you must preallocate it and fill in the version of the structure.
--
-- @keychain@ — The keychain that will have its settings changed.
--
-- ObjC selector: @- runModalForSettings:keychain:@
runModalForSettings_keychain :: IsSFKeychainSettingsPanel sfKeychainSettingsPanel => sfKeychainSettingsPanel -> RawId -> Ptr () -> IO CLong
runModalForSettings_keychain sfKeychainSettingsPanel settings keychain =
  sendMessage sfKeychainSettingsPanel runModalForSettings_keychainSelector settings keychain

-- | beginSheetForWindow:settings:keychain:modalDelegate:didEndSelector:contextInfo:
--
-- Presents a sheet version of SFKeychainSettingsPanel. The didEndSelector returnCode will contain either NSOKButton or NSCancelButton.
--
-- @docWindow@ — The panel in which the settings sheet slides down; acting as a document modal window. If docWindow is nil, the behavior defaults to a standalone modal window.
--
-- @delegate@ — Delegate object in which didEndSelector is a method.
--
-- @didEndSelector@ — The didEndSelector method is optional. If implemented by the delegate, this method is invoked after the modal session has ended, but before dismissing the same panel. The didEndSelector may dismiss the keychain panel itself; otherwise it will be dismissed on return from the method. The didEndSelector should have the following signature:	- (void)settingsPanelDidEnd:(NSWindow *)sheet returnCode:(NSInteger)returnCode contextInfo:(void *)contextInfo;
--
-- @contextInfo@ — A client-defined context.
--
-- @settings@ — A pointer to a keychain settings structure. Since this structure is versioned, you must preallocate it and fill in the version of the structure.
--
-- @keychain@ — The keychain that will have its settings changed.
--
-- ObjC selector: @- beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:settings:keychain:@
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_settings_keychain :: (IsSFKeychainSettingsPanel sfKeychainSettingsPanel, IsNSWindow docWindow) => sfKeychainSettingsPanel -> docWindow -> RawId -> Sel -> Ptr () -> RawId -> Ptr () -> IO ()
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_settings_keychain sfKeychainSettingsPanel docWindow delegate didEndSelector contextInfo settings keychain =
  sendMessage sfKeychainSettingsPanel beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_settings_keychainSelector (toNSWindow docWindow) delegate didEndSelector contextInfo settings keychain

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedKeychainSettingsPanel@
sharedKeychainSettingsPanelSelector :: Selector '[] (Id SFKeychainSettingsPanel)
sharedKeychainSettingsPanelSelector = mkSelector "sharedKeychainSettingsPanel"

-- | @Selector@ for @runModalForSettings:keychain:@
runModalForSettings_keychainSelector :: Selector '[RawId, Ptr ()] CLong
runModalForSettings_keychainSelector = mkSelector "runModalForSettings:keychain:"

-- | @Selector@ for @beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:settings:keychain:@
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_settings_keychainSelector :: Selector '[Id NSWindow, RawId, Sel, Ptr (), RawId, Ptr ()] ()
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_settings_keychainSelector = mkSelector "beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:settings:keychain:"

