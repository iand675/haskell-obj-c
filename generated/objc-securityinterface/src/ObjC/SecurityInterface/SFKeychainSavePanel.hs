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
  , sharedKeychainSavePanelSelector
  , runModalForDirectory_fileSelector
  , setPasswordSelector
  , keychainSelector
  , errorSelector
  , beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector


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

-- | sharedKeychainSavePanel
--
-- Returns a global instance of SFKeychainSavePanel object.
--
-- ObjC selector: @+ sharedKeychainSavePanel@
sharedKeychainSavePanel :: IO (Id SFKeychainSavePanel)
sharedKeychainSavePanel  =
  do
    cls' <- getRequiredClass "SFKeychainSavePanel"
    sendClassMsg cls' (mkSelector "sharedKeychainSavePanel") (retPtr retVoid) [] >>= retainedObject . castPtr

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
runModalForDirectory_file sfKeychainSavePanel  path name =
withObjCPtr path $ \raw_path ->
  withObjCPtr name $ \raw_name ->
      sendMsg sfKeychainSavePanel (mkSelector "runModalForDirectory:file:") retCLong [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_name :: Ptr ())]

-- | setPassword:
--
-- Specifies the password for the keychain that will be created.
--
-- @The@ — password string object.
--
-- ObjC selector: @- setPassword:@
setPassword :: (IsSFKeychainSavePanel sfKeychainSavePanel, IsNSString password) => sfKeychainSavePanel -> password -> IO ()
setPassword sfKeychainSavePanel  password =
withObjCPtr password $ \raw_password ->
    sendMsg sfKeychainSavePanel (mkSelector "setPassword:") retVoid [argPtr (castPtr raw_password :: Ptr ())]

-- | keychain
--
-- Returns the keychain created by the SFKeychainSavePanel.
--
-- Returns: The keychain object.
--
-- ObjC selector: @- keychain@
keychain :: IsSFKeychainSavePanel sfKeychainSavePanel => sfKeychainSavePanel -> IO (Ptr ())
keychain sfKeychainSavePanel  =
  fmap castPtr $ sendMsg sfKeychainSavePanel (mkSelector "keychain") (retPtr retVoid) []

-- | error
--
-- Returns the last error encountered by SFKeychainSavePanel.
--
-- Returns: The error object.
--
-- ObjC selector: @- error@
error_ :: IsSFKeychainSavePanel sfKeychainSavePanel => sfKeychainSavePanel -> IO (Id NSError)
error_ sfKeychainSavePanel  =
  sendMsg sfKeychainSavePanel (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

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
beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfo :: (IsSFKeychainSavePanel sfKeychainSavePanel, IsNSString path, IsNSString name, IsNSWindow docWindow) => sfKeychainSavePanel -> path -> name -> docWindow -> RawId -> Selector -> Ptr () -> IO ()
beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfo sfKeychainSavePanel  path name docWindow delegate didEndSelector contextInfo =
withObjCPtr path $ \raw_path ->
  withObjCPtr name $ \raw_name ->
    withObjCPtr docWindow $ \raw_docWindow ->
        sendMsg sfKeychainSavePanel (mkSelector "beginSheetForDirectory:file:modalForWindow:modalDelegate:didEndSelector:contextInfo:") retVoid [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_docWindow :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didEndSelector), argPtr contextInfo]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedKeychainSavePanel@
sharedKeychainSavePanelSelector :: Selector
sharedKeychainSavePanelSelector = mkSelector "sharedKeychainSavePanel"

-- | @Selector@ for @runModalForDirectory:file:@
runModalForDirectory_fileSelector :: Selector
runModalForDirectory_fileSelector = mkSelector "runModalForDirectory:file:"

-- | @Selector@ for @setPassword:@
setPasswordSelector :: Selector
setPasswordSelector = mkSelector "setPassword:"

-- | @Selector@ for @keychain@
keychainSelector :: Selector
keychainSelector = mkSelector "keychain"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

-- | @Selector@ for @beginSheetForDirectory:file:modalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector :: Selector
beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector = mkSelector "beginSheetForDirectory:file:modalForWindow:modalDelegate:didEndSelector:contextInfo:"

