{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SFAuthorizationPluginView
--
-- SFAuthorizationPluginView is a class that you can use to insert an NSView into AuthorizationPlugin interfaces.
--
-- SFAuthorizationPluginView provides AuthorizationPlugin writers with an easy way to provide a user interface for their AuthorizationPlugin without having to duplicate the standard authentication dialog or the login window dialog.  This class was designed to be subclassed by the AuthorizationPlugin writer.  The following methods were designed to be overridden: buttonPressed:, didActivate, willActivateWithUser:, didDeactivate, firstKeyView, firstResponder, lastKeyView, setEnabled:, and viewForType:.  In order to display the user interface, the AuthorizationPlugin should create an instance of your subclass and then call displayView.  That will cause the appropriate dialog to be displayed and when credentials are needed, the overridden methods will be called in order to display the NSView provided by the subclass.
--
-- Generated bindings for @SFAuthorizationPluginView@.
module ObjC.SecurityInterface.SFAuthorizationPluginView
  ( SFAuthorizationPluginView
  , IsSFAuthorizationPluginView(..)
  , initWithCallbacks_andEngineRef
  , engineRef
  , callbacks
  , lastError
  , didActivate
  , willActivateWithUser
  , didDeactivate
  , firstKeyView
  , firstResponder
  , lastKeyView
  , setEnabled
  , displayView
  , updateView
  , callbacksSelector
  , didActivateSelector
  , didDeactivateSelector
  , displayViewSelector
  , engineRefSelector
  , firstKeyViewSelector
  , firstResponderSelector
  , initWithCallbacks_andEngineRefSelector
  , lastErrorSelector
  , lastKeyViewSelector
  , setEnabledSelector
  , updateViewSelector
  , willActivateWithUserSelector


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

-- | initWithCallbacks:andEngineRef:
--
-- The initialization method of this class.  You must provide the callbacks and engineRef that were provided to the AuthorizationPlugin and AuthorizationMechanismRef.
--
-- @callbacks@ — The AuthorizationCallbacks provided to the AuthorizationPlugin.
--
-- @engineRef@ — The AuthorizationEngineRef provided to the AuthorizationMechanismRef.
--
-- ObjC selector: @- initWithCallbacks:andEngineRef:@
initWithCallbacks_andEngineRef :: IsSFAuthorizationPluginView sfAuthorizationPluginView => sfAuthorizationPluginView -> Const RawId -> Ptr () -> IO RawId
initWithCallbacks_andEngineRef sfAuthorizationPluginView callbacks engineRef =
  sendOwnedMessage sfAuthorizationPluginView initWithCallbacks_andEngineRefSelector callbacks engineRef

-- | engineRef
--
-- An accessor method to the AuthorizationEngineRef provided to the init method.  Use this value when calling the functions of the AuthorizationCallbacks when you need to set the result or set a context value.
--
-- ObjC selector: @- engineRef@
engineRef :: IsSFAuthorizationPluginView sfAuthorizationPluginView => sfAuthorizationPluginView -> IO (Ptr ())
engineRef sfAuthorizationPluginView =
  sendMessage sfAuthorizationPluginView engineRefSelector

-- | callbacks
--
-- An accessor method to the AuthorizationEngineRef provided to the init method.  Use this to get the function pointers to SetResult, SetContextValue, etc.  See the AuthorizationCallbacks documentation for more details.
--
-- ObjC selector: @- callbacks@
callbacks :: IsSFAuthorizationPluginView sfAuthorizationPluginView => sfAuthorizationPluginView -> IO (Const RawId)
callbacks sfAuthorizationPluginView =
  sendMessage sfAuthorizationPluginView callbacksSelector

-- | lastError
--
-- Called by the Apple provided SecurityAgent plugin to get a description of the error that occurred during evaluation.  If no error occurred, the method should return nil.
--
-- A downstream mechanism that works in cooperation with the SFAuthorizationPluginView can set a context value using the kAuthorizationContextFlagSticky flag to make it available to the SFAuthorizationPluginView in case of an error.
--
-- ObjC selector: @- lastError@
lastError :: IsSFAuthorizationPluginView sfAuthorizationPluginView => sfAuthorizationPluginView -> IO (Id NSError)
lastError sfAuthorizationPluginView =
  sendMessage sfAuthorizationPluginView lastErrorSelector

-- | didActivate
--
-- Called when the user interface was made active by the AuthorizationPlugin.
--
-- ObjC selector: @- didActivate@
didActivate :: IsSFAuthorizationPluginView sfAuthorizationPluginView => sfAuthorizationPluginView -> IO ()
didActivate sfAuthorizationPluginView =
  sendMessage sfAuthorizationPluginView didActivateSelector

-- | willActivateWithUser:
--
-- Called by the Apple provided SecurityAgent plugin before the UI is made active.
--
-- @inUserInformation@ — is a dictionary contains the following information:		kSFAuthorizationPluginViewUserNameKey - an NSString with the selected user's name		kSFAuthorizationPluginViewUserShortNameKey - an NSString with the selected user's short name
--
-- The user name can be used to pre-populate a Text Field.		NOTE: inUserInformation may be nil.
--
-- ObjC selector: @- willActivateWithUser:@
willActivateWithUser :: (IsSFAuthorizationPluginView sfAuthorizationPluginView, IsNSDictionary inUserInformation) => sfAuthorizationPluginView -> inUserInformation -> IO ()
willActivateWithUser sfAuthorizationPluginView inUserInformation =
  sendMessage sfAuthorizationPluginView willActivateWithUserSelector (toNSDictionary inUserInformation)

-- | didDeactivate
--
-- Called when the user interface is deactivated by the AuthorizationPlugin.
--
-- ObjC selector: @- didDeactivate@
didDeactivate :: IsSFAuthorizationPluginView sfAuthorizationPluginView => sfAuthorizationPluginView -> IO ()
didDeactivate sfAuthorizationPluginView =
  sendMessage sfAuthorizationPluginView didDeactivateSelector

-- | firstKeyView
--
-- When called by the AuthorizationPlugin, the subclass should return the first view in the keyboard loop of the NSView.  The default value returned is nil.
--
-- ObjC selector: @- firstKeyView@
firstKeyView :: IsSFAuthorizationPluginView sfAuthorizationPluginView => sfAuthorizationPluginView -> IO (Id NSView)
firstKeyView sfAuthorizationPluginView =
  sendMessage sfAuthorizationPluginView firstKeyViewSelector

-- | firstResponder
--
-- When called by the AuthorizationPlugin, the subclass should return the view that should get the focus for keyboard events.  The default value returned is nil.
--
-- ObjC selector: @- firstResponder@
firstResponder :: IsSFAuthorizationPluginView sfAuthorizationPluginView => sfAuthorizationPluginView -> IO (Id NSResponder)
firstResponder sfAuthorizationPluginView =
  sendMessage sfAuthorizationPluginView firstResponderSelector

-- | lastKeyView
--
-- When called by the AuthorizationPlugin, the subclass should return the last view in the keyboard loop for the view.  The default value returned is nil.
--
-- ObjC selector: @- lastKeyView@
lastKeyView :: IsSFAuthorizationPluginView sfAuthorizationPluginView => sfAuthorizationPluginView -> IO (Id NSView)
lastKeyView sfAuthorizationPluginView =
  sendMessage sfAuthorizationPluginView lastKeyViewSelector

-- | setEnabled:
--
-- When called by the AuthorizationPlugin, the subclass should call setEnabled: on the controls that are in its view.
--
-- @inEnabled@ — the state the controls should be in.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsSFAuthorizationPluginView sfAuthorizationPluginView => sfAuthorizationPluginView -> Bool -> IO ()
setEnabled sfAuthorizationPluginView inEnabled =
  sendMessage sfAuthorizationPluginView setEnabledSelector inEnabled

-- | displayView
--
-- displayView is called in order to display the user interface provided by the subclass.  If you subclass this method, be sure to call [super displayView].  displayView will raise an SFDisplayViewException exception if an error occurs displaying the authorization dialog.
--
-- ObjC selector: @- displayView@
displayView :: IsSFAuthorizationPluginView sfAuthorizationPluginView => sfAuthorizationPluginView -> IO ()
displayView sfAuthorizationPluginView =
  sendMessage sfAuthorizationPluginView displayViewSelector

-- | updateView
--
-- updateView is called in order to have a new or modified NSView loaded by the AuthorizationPlugin.
--
-- ObjC selector: @- updateView@
updateView :: IsSFAuthorizationPluginView sfAuthorizationPluginView => sfAuthorizationPluginView -> IO ()
updateView sfAuthorizationPluginView =
  sendMessage sfAuthorizationPluginView updateViewSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCallbacks:andEngineRef:@
initWithCallbacks_andEngineRefSelector :: Selector '[Const RawId, Ptr ()] RawId
initWithCallbacks_andEngineRefSelector = mkSelector "initWithCallbacks:andEngineRef:"

-- | @Selector@ for @engineRef@
engineRefSelector :: Selector '[] (Ptr ())
engineRefSelector = mkSelector "engineRef"

-- | @Selector@ for @callbacks@
callbacksSelector :: Selector '[] (Const RawId)
callbacksSelector = mkSelector "callbacks"

-- | @Selector@ for @lastError@
lastErrorSelector :: Selector '[] (Id NSError)
lastErrorSelector = mkSelector "lastError"

-- | @Selector@ for @didActivate@
didActivateSelector :: Selector '[] ()
didActivateSelector = mkSelector "didActivate"

-- | @Selector@ for @willActivateWithUser:@
willActivateWithUserSelector :: Selector '[Id NSDictionary] ()
willActivateWithUserSelector = mkSelector "willActivateWithUser:"

-- | @Selector@ for @didDeactivate@
didDeactivateSelector :: Selector '[] ()
didDeactivateSelector = mkSelector "didDeactivate"

-- | @Selector@ for @firstKeyView@
firstKeyViewSelector :: Selector '[] (Id NSView)
firstKeyViewSelector = mkSelector "firstKeyView"

-- | @Selector@ for @firstResponder@
firstResponderSelector :: Selector '[] (Id NSResponder)
firstResponderSelector = mkSelector "firstResponder"

-- | @Selector@ for @lastKeyView@
lastKeyViewSelector :: Selector '[] (Id NSView)
lastKeyViewSelector = mkSelector "lastKeyView"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @displayView@
displayViewSelector :: Selector '[] ()
displayViewSelector = mkSelector "displayView"

-- | @Selector@ for @updateView@
updateViewSelector :: Selector '[] ()
updateViewSelector = mkSelector "updateView"

