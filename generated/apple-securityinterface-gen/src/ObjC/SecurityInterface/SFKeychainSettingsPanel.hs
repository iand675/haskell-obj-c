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
  , sharedKeychainSettingsPanelSelector
  , runModalForSettings_keychainSelector
  , beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_settings_keychainSelector


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
runModalForSettings_keychain sfKeychainSettingsPanel  settings keychain =
    sendMsg sfKeychainSettingsPanel (mkSelector "runModalForSettings:keychain:") retCLong [argPtr (castPtr (unRawId settings) :: Ptr ()), argPtr keychain]

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
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_settings_keychain :: (IsSFKeychainSettingsPanel sfKeychainSettingsPanel, IsNSWindow docWindow) => sfKeychainSettingsPanel -> docWindow -> RawId -> Selector -> Ptr () -> RawId -> Ptr () -> IO ()
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_settings_keychain sfKeychainSettingsPanel  docWindow delegate didEndSelector contextInfo settings keychain =
  withObjCPtr docWindow $ \raw_docWindow ->
      sendMsg sfKeychainSettingsPanel (mkSelector "beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:settings:keychain:") retVoid [argPtr (castPtr raw_docWindow :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didEndSelector), argPtr contextInfo, argPtr (castPtr (unRawId settings) :: Ptr ()), argPtr keychain]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedKeychainSettingsPanel@
sharedKeychainSettingsPanelSelector :: Selector
sharedKeychainSettingsPanelSelector = mkSelector "sharedKeychainSettingsPanel"

-- | @Selector@ for @runModalForSettings:keychain:@
runModalForSettings_keychainSelector :: Selector
runModalForSettings_keychainSelector = mkSelector "runModalForSettings:keychain:"

-- | @Selector@ for @beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:settings:keychain:@
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_settings_keychainSelector :: Selector
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_settings_keychainSelector = mkSelector "beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:settings:keychain:"

