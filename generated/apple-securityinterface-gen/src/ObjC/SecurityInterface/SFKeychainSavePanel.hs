{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SFKeychainSavePanel
--
-- SFKeychainSavePanel is a panel and sheet interface used to create a keychain using the NSSavePanel UI.
--
-- Generated bindings for @SFKeychainSavePanel@.
module ObjC.SecurityInterface.SFKeychainSavePanel
  ( SFKeychainSavePanel
  , IsSFKeychainSavePanel(..)
  , sharedKeychainSavePanel
  , runModalForDirectory_file
  , setPassword
  , keychain
  , error_
  , beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfo
  , beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector
  , errorSelector
  , keychainSelector
  , runModalForDirectory_fileSelector
  , setPasswordSelector
  , sharedKeychainSavePanelSelector


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

-- | sharedKeychainSavePanel
--
-- Returns a global instance of SFKeychainSavePanel object.
--
-- ObjC selector: @+ sharedKeychainSavePanel@
sharedKeychainSavePanel :: IO (Id SFKeychainSavePanel)
sharedKeychainSavePanel  =
  do
    cls' <- getRequiredClass "SFKeychainSavePanel"
    sendClassMessage cls' sharedKeychainSavePanelSelector

-- | runModalForDirectory:file:
--
-- Displays a keychain save panel.
--
-- @path@ — The path to where the keychain is created (nil for ~/Library/Keychains).
--
-- @name@ — The keychain name to be automatically filled out in the NSSave panel.
--
-- Returns: a result code returned by NSSavePanel's runModalForDirectory method.
--
-- ObjC selector: @- runModalForDirectory:file:@
runModalForDirectory_file :: (IsSFKeychainSavePanel sfKeychainSavePanel, IsNSString path, IsNSString name) => sfKeychainSavePanel -> path -> name -> IO CLong
runModalForDirectory_file sfKeychainSavePanel path name =
  sendMessage sfKeychainSavePanel runModalForDirectory_fileSelector (toNSString path) (toNSString name)

-- | setPassword:
--
-- Specifies the password for the keychain that will be created.
--
-- @The@ — password string object.
--
-- ObjC selector: @- setPassword:@
setPassword :: (IsSFKeychainSavePanel sfKeychainSavePanel, IsNSString password) => sfKeychainSavePanel -> password -> IO ()
setPassword sfKeychainSavePanel password =
  sendMessage sfKeychainSavePanel setPasswordSelector (toNSString password)

-- | keychain
--
-- Returns the keychain created by the SFKeychainSavePanel.
--
-- Returns: The keychain object.
--
-- ObjC selector: @- keychain@
keychain :: IsSFKeychainSavePanel sfKeychainSavePanel => sfKeychainSavePanel -> IO (Ptr ())
keychain sfKeychainSavePanel =
  sendMessage sfKeychainSavePanel keychainSelector

-- | error
--
-- Returns the last error encountered by SFKeychainSavePanel.
--
-- Returns: The error object.
--
-- ObjC selector: @- error@
error_ :: IsSFKeychainSavePanel sfKeychainSavePanel => sfKeychainSavePanel -> IO (Id NSError)
error_ sfKeychainSavePanel =
  sendMessage sfKeychainSavePanel errorSelector

-- | beginSheetForDirectory:file:modalForWindow:modalDelegate:didEndSelector:contextInfo:
--
-- Displays a keychain save panel as a sheet.
--
-- @path@ — The path to where the keychain is created (nil for ~/Library/Keychains).
--
-- @name@ — The keychain name to be automatically filled out in the NSSave panel.
--
-- @docWindow@ — The panel in which the save sheet slides down; acting as a document modal window. If docWindow is nil, the behavior defaults to a standalone modal window.
--
-- @delegate@ — Delegate object in which didEndSelector is a method.
--
-- @didEndSelector@ — The didEndSelector method is optional. If implemented by the delegate, this method is invoked after the modal session has ended, but before dismissing the same panel. didEndSelector may dismiss the keychain panel itself; otherwise it will be dismissed on return from the method. didEndSelector should have the following signature:	- (void)createPanelDidEnd:(NSWindow *)sheet returnCode:(NSInteger)returnCode contextInfo:(void  *)contextInfo;
--
-- @contextInfo@ — Client-defined context pointer.
--
-- ObjC selector: @- beginSheetForDirectory:file:modalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfo :: (IsSFKeychainSavePanel sfKeychainSavePanel, IsNSString path, IsNSString name, IsNSWindow docWindow) => sfKeychainSavePanel -> path -> name -> docWindow -> RawId -> Sel -> Ptr () -> IO ()
beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfo sfKeychainSavePanel path name docWindow delegate didEndSelector contextInfo =
  sendMessage sfKeychainSavePanel beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector (toNSString path) (toNSString name) (toNSWindow docWindow) delegate didEndSelector contextInfo

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedKeychainSavePanel@
sharedKeychainSavePanelSelector :: Selector '[] (Id SFKeychainSavePanel)
sharedKeychainSavePanelSelector = mkSelector "sharedKeychainSavePanel"

-- | @Selector@ for @runModalForDirectory:file:@
runModalForDirectory_fileSelector :: Selector '[Id NSString, Id NSString] CLong
runModalForDirectory_fileSelector = mkSelector "runModalForDirectory:file:"

-- | @Selector@ for @setPassword:@
setPasswordSelector :: Selector '[Id NSString] ()
setPasswordSelector = mkSelector "setPassword:"

-- | @Selector@ for @keychain@
keychainSelector :: Selector '[] (Ptr ())
keychainSelector = mkSelector "keychain"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

-- | @Selector@ for @beginSheetForDirectory:file:modalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector :: Selector '[Id NSString, Id NSString, Id NSWindow, RawId, Sel, Ptr ()] ()
beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector = mkSelector "beginSheetForDirectory:file:modalForWindow:modalDelegate:didEndSelector:contextInfo:"

