{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A modal dialog or sheet attached to a document window. The @NSAlert@ class is not designed for subclassing.
--
-- Generated bindings for @NSAlert@.
module ObjC.AppKit.NSAlert
  ( NSAlert
  , IsNSAlert(..)
  , alertWithError
  , addButtonWithTitle
  , layout
  , runModal
  , beginSheetModalForWindow_completionHandler
  , alertWithMessageText_defaultButton_alternateButton_otherButton_informativeTextWithFormat
  , beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfo
  , messageText
  , setMessageText
  , informativeText
  , setInformativeText
  , icon
  , setIcon
  , buttons
  , alertStyle
  , setAlertStyle
  , showsHelp
  , setShowsHelp
  , helpAnchor
  , setHelpAnchor
  , delegate
  , setDelegate
  , accessoryView
  , setAccessoryView
  , showsSuppressionButton
  , setShowsSuppressionButton
  , suppressionButton
  , window
  , accessoryViewSelector
  , addButtonWithTitleSelector
  , alertStyleSelector
  , alertWithErrorSelector
  , alertWithMessageText_defaultButton_alternateButton_otherButton_informativeTextWithFormatSelector
  , beginSheetModalForWindow_completionHandlerSelector
  , beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfoSelector
  , buttonsSelector
  , delegateSelector
  , helpAnchorSelector
  , iconSelector
  , informativeTextSelector
  , layoutSelector
  , messageTextSelector
  , runModalSelector
  , setAccessoryViewSelector
  , setAlertStyleSelector
  , setDelegateSelector
  , setHelpAnchorSelector
  , setIconSelector
  , setInformativeTextSelector
  , setMessageTextSelector
  , setShowsHelpSelector
  , setShowsSuppressionButtonSelector
  , showsHelpSelector
  , showsSuppressionButtonSelector
  , suppressionButtonSelector
  , windowSelector

  -- * Enum types
  , NSAlertStyle(NSAlertStyle)
  , pattern NSAlertStyleWarning
  , pattern NSAlertStyleInformational
  , pattern NSAlertStyleCritical

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Returns an alert initialized from information in an error object. - Parameter error: Error information to display. - Returns: An initialized alert. - Note: The @NSAlert@ class extracts the localized error description, recovery suggestion, and recovery options from the error parameter and uses them as the alert’s message text, informative text, and button titles, respectively.
--
-- ObjC selector: @+ alertWithError:@
alertWithError :: IsNSError error_ => error_ -> IO (Id NSAlert)
alertWithError error_ =
  do
    cls' <- getRequiredClass "NSAlert"
    sendClassMessage cls' alertWithErrorSelector (toNSError error_)

-- | Adds a button with a given title to the alert.
--
-- Buttons should be added from most-to-least prominent. The exact visual arrangement of the buttons is determined by @NSAlert@ internally. In general, they are arranged from trailing-to-leading edge when laid out horizontally, and top-to-bottom when laid out vertically, with some exceptions for buttons like “Cancel”.
--
-- The first three buttons are identified by the order in which they are added (not the order in which they may appear visually) as @NSAlertFirstButtonReturn@, @NSAlertSecondButtonReturn@, @NSAlertThirdButtonReturn@ in the return-code parameter. Subsequent buttons are identified as @NSAlertThirdButtonReturn@ + *n*, where *n* is an integer.
--
-- By default, the first button has a key equivalent of Return, any button with a title of “Cancel” has a key equivalent of Escape, and any button with the title “Don’t Save” has a key equivalent of Command-D (but only if it’s not the first button). You can also assign different key equivalents for the buttons using the @keyEquivalent@ method of the @NSButton@ class. In addition, you can use the @tag@ property of the @NSButton@ class to set the alert presentation’s return-code. The framework reserves the use of the button’s @target@ and @action@.
--
-- - Parameter title: Title of the button to add to the alert. - Returns: The button that was added to the alert.
--
-- ObjC selector: @- addButtonWithTitle:@
addButtonWithTitle :: (IsNSAlert nsAlert, IsNSString title) => nsAlert -> title -> IO (Id NSButton)
addButtonWithTitle nsAlert title =
  sendMessage nsAlert addButtonWithTitleSelector (toNSString title)

-- | Specifies that the alert must do immediate layout instead of lazily just before display. Used to indicate that the alert panel should do immediate layout, overriding the default behavior of laying out lazily just before showing panel. Only call this method if wanting to do custom layout after it returns. Call this method only after the alert’s other customization, including setting message and informative text, and adding buttons and an accessory view if needed. Layout changes can be made after this method returns, in particular to adjust the frame of an accessory view. Note that the standard layout of the alert may change in the future, so layout customization should be done with caution.
--
-- ObjC selector: @- layout@
layout :: IsNSAlert nsAlert => nsAlert -> IO ()
layout nsAlert =
  sendMessage nsAlert layoutSelector

-- | Runs the alert as an app-modal dialog and returns the constant that identifies the button clicked.
--
-- ObjC selector: @- runModal@
runModal :: IsNSAlert nsAlert => nsAlert -> IO CLong
runModal nsAlert =
  sendMessage nsAlert runModalSelector

-- | Runs the alert modally as a sheet attached to the specified window. - Parameters:   - sheetWindow: The window on which to display the sheet.   - handler: The completion handler that gets called when the sheet’s modal session ends.
--
-- This method uses the @NSWindow@ sheet methods to display the alert. If the alert has an alert style of @NSCriticalAlertStyle@, it is presented as a critical sheet, which means that it can display on top of other sheets that might already be attached to the window. Otherwise, it is presented--or queued for presentation--as a standard sheet. Note that @-orderOut:@ no longer needs to be called in the completion handler. If the alert isn’t  don’t dismiss the alert, it will be done for you after the completion handler finishes.
--
-- ObjC selector: @- beginSheetModalForWindow:completionHandler:@
beginSheetModalForWindow_completionHandler :: (IsNSAlert nsAlert, IsNSWindow sheetWindow) => nsAlert -> sheetWindow -> Ptr () -> IO ()
beginSheetModalForWindow_completionHandler nsAlert sheetWindow handler =
  sendMessage nsAlert beginSheetModalForWindow_completionHandlerSelector (toNSWindow sheetWindow) handler

-- | @+ alertWithMessageText:defaultButton:alternateButton:otherButton:informativeTextWithFormat:@
alertWithMessageText_defaultButton_alternateButton_otherButton_informativeTextWithFormat :: (IsNSString message, IsNSString defaultButton, IsNSString alternateButton, IsNSString otherButton, IsNSString format) => message -> defaultButton -> alternateButton -> otherButton -> format -> IO (Id NSAlert)
alertWithMessageText_defaultButton_alternateButton_otherButton_informativeTextWithFormat message defaultButton alternateButton otherButton format =
  do
    cls' <- getRequiredClass "NSAlert"
    sendClassMessage cls' alertWithMessageText_defaultButton_alternateButton_otherButton_informativeTextWithFormatSelector (toNSString message) (toNSString defaultButton) (toNSString alternateButton) (toNSString otherButton) (toNSString format)

-- | @- beginSheetModalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfo :: (IsNSAlert nsAlert, IsNSWindow window) => nsAlert -> window -> RawId -> Sel -> Ptr () -> IO ()
beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfo nsAlert window delegate didEndSelector contextInfo =
  sendMessage nsAlert beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfoSelector (toNSWindow window) delegate didEndSelector contextInfo

-- | The text that is displayed prominently in the alert. - Note: Use this string to get the user’s attention and communicate the reason for displaying the alert.
--
-- ObjC selector: @- messageText@
messageText :: IsNSAlert nsAlert => nsAlert -> IO (Id NSString)
messageText nsAlert =
  sendMessage nsAlert messageTextSelector

-- | The text that is displayed prominently in the alert. - Note: Use this string to get the user’s attention and communicate the reason for displaying the alert.
--
-- ObjC selector: @- setMessageText:@
setMessageText :: (IsNSAlert nsAlert, IsNSString value) => nsAlert -> value -> IO ()
setMessageText nsAlert value =
  sendMessage nsAlert setMessageTextSelector (toNSString value)

-- | The descriptive text that provides more details about the reason for the alert. - Note: The informative text string is displayed below the message text and is less prominent. Use this string to provide additional context about the reason for the alert or about the actions that the user might take.
--
-- ObjC selector: @- informativeText@
informativeText :: IsNSAlert nsAlert => nsAlert -> IO (Id NSString)
informativeText nsAlert =
  sendMessage nsAlert informativeTextSelector

-- | The descriptive text that provides more details about the reason for the alert. - Note: The informative text string is displayed below the message text and is less prominent. Use this string to provide additional context about the reason for the alert or about the actions that the user might take.
--
-- ObjC selector: @- setInformativeText:@
setInformativeText :: (IsNSAlert nsAlert, IsNSString value) => nsAlert -> value -> IO ()
setInformativeText nsAlert value =
  sendMessage nsAlert setInformativeTextSelector (toNSString value)

-- | The custom icon displayed in the alert.
--
-- By default, the image used in an alert is the app icon. If you set this property’s value, your specified custom image is used in place of the app icon.
--
-- If you’ve set a custom alert icon, you can clear it by setting this property’s value to @nil@, which restores use of the app icon for the alert.
--
-- - Note: AppKit may omit the icon from the alert if it’s the app icon and the alert’s context is clear, such as being presented as a sheet on an app window.
--
-- ObjC selector: @- icon@
icon :: IsNSAlert nsAlert => nsAlert -> IO (Id NSImage)
icon nsAlert =
  sendMessage nsAlert iconSelector

-- | The custom icon displayed in the alert.
--
-- By default, the image used in an alert is the app icon. If you set this property’s value, your specified custom image is used in place of the app icon.
--
-- If you’ve set a custom alert icon, you can clear it by setting this property’s value to @nil@, which restores use of the app icon for the alert.
--
-- - Note: AppKit may omit the icon from the alert if it’s the app icon and the alert’s context is clear, such as being presented as a sheet on an app window.
--
-- ObjC selector: @- setIcon:@
setIcon :: (IsNSAlert nsAlert, IsNSImage value) => nsAlert -> value -> IO ()
setIcon nsAlert value =
  sendMessage nsAlert setIconSelector (toNSImage value)

-- | The array of response buttons for the alert. The buttons are in the order in which they were added, and do not necessarily reflect the order they are arranged visually. The array does not include the default “OK” button that is shown in an alert presented without any buttons added with @-addButtonWithTitle:@.
--
-- ObjC selector: @- buttons@
buttons :: IsNSAlert nsAlert => nsAlert -> IO (Id NSArray)
buttons nsAlert =
  sendMessage nsAlert buttonsSelector

-- | Indicates the alert’s severity level. See the @NSAlertStyle@ enumeration for the list of alert style constants.
--
-- ObjC selector: @- alertStyle@
alertStyle :: IsNSAlert nsAlert => nsAlert -> IO NSAlertStyle
alertStyle nsAlert =
  sendMessage nsAlert alertStyleSelector

-- | Indicates the alert’s severity level. See the @NSAlertStyle@ enumeration for the list of alert style constants.
--
-- ObjC selector: @- setAlertStyle:@
setAlertStyle :: IsNSAlert nsAlert => nsAlert -> NSAlertStyle -> IO ()
setAlertStyle nsAlert value =
  sendMessage nsAlert setAlertStyleSelector value

-- | Specifies whether the alert has a help button.
--
-- Set this property’s value to @YES@ to specify that the alert has a help button, or @NO@ to specify it does not.
--
-- When a user clicks an alert’s help button, the alert delegate (@delegate@) receives an @alertShowHelp:@ message. The delegate is responsible for displaying the help information related to this particular alert.
--
-- Clicking an alert’s help button can alternately cause the @-openHelpAnchor:inBook:@ message to be sent to the app’s help manager with a @nil@ book and the anchor specified by the @helpAnchor@ property, if any of the following conditions are true: - There is no alert delegate. - The alert delegate does not implement @-alertShowHelp:@. - The alert delegate implements @-alertShowHelp:@ but returns @NO@. When this is the case, an exception is raised if no help anchor is set.
--
-- ObjC selector: @- showsHelp@
showsHelp :: IsNSAlert nsAlert => nsAlert -> IO Bool
showsHelp nsAlert =
  sendMessage nsAlert showsHelpSelector

-- | Specifies whether the alert has a help button.
--
-- Set this property’s value to @YES@ to specify that the alert has a help button, or @NO@ to specify it does not.
--
-- When a user clicks an alert’s help button, the alert delegate (@delegate@) receives an @alertShowHelp:@ message. The delegate is responsible for displaying the help information related to this particular alert.
--
-- Clicking an alert’s help button can alternately cause the @-openHelpAnchor:inBook:@ message to be sent to the app’s help manager with a @nil@ book and the anchor specified by the @helpAnchor@ property, if any of the following conditions are true: - There is no alert delegate. - The alert delegate does not implement @-alertShowHelp:@. - The alert delegate implements @-alertShowHelp:@ but returns @NO@. When this is the case, an exception is raised if no help anchor is set.
--
-- ObjC selector: @- setShowsHelp:@
setShowsHelp :: IsNSAlert nsAlert => nsAlert -> Bool -> IO ()
setShowsHelp nsAlert value =
  sendMessage nsAlert setShowsHelpSelector value

-- | The alert’s HTML help anchor used when the user clicks the alert’s help button
--
-- ObjC selector: @- helpAnchor@
helpAnchor :: IsNSAlert nsAlert => nsAlert -> IO (Id NSString)
helpAnchor nsAlert =
  sendMessage nsAlert helpAnchorSelector

-- | The alert’s HTML help anchor used when the user clicks the alert’s help button
--
-- ObjC selector: @- setHelpAnchor:@
setHelpAnchor :: (IsNSAlert nsAlert, IsNSString value) => nsAlert -> value -> IO ()
setHelpAnchor nsAlert value =
  sendMessage nsAlert setHelpAnchorSelector (toNSString value)

-- | The delegate of the receiver, currently only allows for custom help behavior of the alert. For apps linked against 10.12, this property has zeroing weak memory semantics. When linked against an older SDK this back to having @retain@ semantics, matching legacy behavior.
--
-- ObjC selector: @- delegate@
delegate :: IsNSAlert nsAlert => nsAlert -> IO RawId
delegate nsAlert =
  sendMessage nsAlert delegateSelector

-- | The delegate of the receiver, currently only allows for custom help behavior of the alert. For apps linked against 10.12, this property has zeroing weak memory semantics. When linked against an older SDK this back to having @retain@ semantics, matching legacy behavior.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsNSAlert nsAlert => nsAlert -> RawId -> IO ()
setDelegate nsAlert value =
  sendMessage nsAlert setDelegateSelector value

-- | The accessory view displayed in the alert, placed between the informative text or suppression checkbox (if present) and the response buttons. Before changing the location of the accessory view, first call the @-layout@ method.
--
-- ObjC selector: @- accessoryView@
accessoryView :: IsNSAlert nsAlert => nsAlert -> IO (Id NSView)
accessoryView nsAlert =
  sendMessage nsAlert accessoryViewSelector

-- | The accessory view displayed in the alert, placed between the informative text or suppression checkbox (if present) and the response buttons. Before changing the location of the accessory view, first call the @-layout@ method.
--
-- ObjC selector: @- setAccessoryView:@
setAccessoryView :: (IsNSAlert nsAlert, IsNSView value) => nsAlert -> value -> IO ()
setAccessoryView nsAlert value =
  sendMessage nsAlert setAccessoryViewSelector (toNSView value)

-- | Specifies whether the alert includes a suppression checkbox, which can be employed to allow a user to opt out of seeing the alert again. The default value of this property is @NO@, which specifies the absence of a suppression checkbox in the alert. Set the value to @YES@ to show a suppression checkbox in the alert. By default, a suppression checkbox has the title, “Do not show this message again.” In macOS 11.0 and later, if the alert displays multiple buttons that prompt the user to make a choice, the title is “Do not ask again.” To customize it, use the checkbox’s title property, as follows:
--
-- myAlert.suppressionButton.title = "Do not show this warning again";
--
-- To create an alert that responds to the selection state of the suppression checkbox, check @myAlert.suppressionButton.state@.
--
-- ObjC selector: @- showsSuppressionButton@
showsSuppressionButton :: IsNSAlert nsAlert => nsAlert -> IO Bool
showsSuppressionButton nsAlert =
  sendMessage nsAlert showsSuppressionButtonSelector

-- | Specifies whether the alert includes a suppression checkbox, which can be employed to allow a user to opt out of seeing the alert again. The default value of this property is @NO@, which specifies the absence of a suppression checkbox in the alert. Set the value to @YES@ to show a suppression checkbox in the alert. By default, a suppression checkbox has the title, “Do not show this message again.” In macOS 11.0 and later, if the alert displays multiple buttons that prompt the user to make a choice, the title is “Do not ask again.” To customize it, use the checkbox’s title property, as follows:
--
-- myAlert.suppressionButton.title = "Do not show this warning again";
--
-- To create an alert that responds to the selection state of the suppression checkbox, check @myAlert.suppressionButton.state@.
--
-- ObjC selector: @- setShowsSuppressionButton:@
setShowsSuppressionButton :: IsNSAlert nsAlert => nsAlert -> Bool -> IO ()
setShowsSuppressionButton nsAlert value =
  sendMessage nsAlert setShowsSuppressionButtonSelector value

-- | The alert’s suppression checkbox. The checkbox may be customized, including the title and the initial state. Additionally, use this method to get the state of the button after the alert is dismissed, which may be stored in user defaults and checked before showing the alert again. In order to show the suppression button in the alert panel, you must set @showsSuppressionButton@ to @YES@.
--
-- ObjC selector: @- suppressionButton@
suppressionButton :: IsNSAlert nsAlert => nsAlert -> IO (Id NSButton)
suppressionButton nsAlert =
  sendMessage nsAlert suppressionButtonSelector

-- | The app-modal panel or document-modal sheet that corresponds to the alert
--
-- ObjC selector: @- window@
window :: IsNSAlert nsAlert => nsAlert -> IO (Id NSWindow)
window nsAlert =
  sendMessage nsAlert windowSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alertWithError:@
alertWithErrorSelector :: Selector '[Id NSError] (Id NSAlert)
alertWithErrorSelector = mkSelector "alertWithError:"

-- | @Selector@ for @addButtonWithTitle:@
addButtonWithTitleSelector :: Selector '[Id NSString] (Id NSButton)
addButtonWithTitleSelector = mkSelector "addButtonWithTitle:"

-- | @Selector@ for @layout@
layoutSelector :: Selector '[] ()
layoutSelector = mkSelector "layout"

-- | @Selector@ for @runModal@
runModalSelector :: Selector '[] CLong
runModalSelector = mkSelector "runModal"

-- | @Selector@ for @beginSheetModalForWindow:completionHandler:@
beginSheetModalForWindow_completionHandlerSelector :: Selector '[Id NSWindow, Ptr ()] ()
beginSheetModalForWindow_completionHandlerSelector = mkSelector "beginSheetModalForWindow:completionHandler:"

-- | @Selector@ for @alertWithMessageText:defaultButton:alternateButton:otherButton:informativeTextWithFormat:@
alertWithMessageText_defaultButton_alternateButton_otherButton_informativeTextWithFormatSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSString, Id NSString] (Id NSAlert)
alertWithMessageText_defaultButton_alternateButton_otherButton_informativeTextWithFormatSelector = mkSelector "alertWithMessageText:defaultButton:alternateButton:otherButton:informativeTextWithFormat:"

-- | @Selector@ for @beginSheetModalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfoSelector :: Selector '[Id NSWindow, RawId, Sel, Ptr ()] ()
beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfoSelector = mkSelector "beginSheetModalForWindow:modalDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @messageText@
messageTextSelector :: Selector '[] (Id NSString)
messageTextSelector = mkSelector "messageText"

-- | @Selector@ for @setMessageText:@
setMessageTextSelector :: Selector '[Id NSString] ()
setMessageTextSelector = mkSelector "setMessageText:"

-- | @Selector@ for @informativeText@
informativeTextSelector :: Selector '[] (Id NSString)
informativeTextSelector = mkSelector "informativeText"

-- | @Selector@ for @setInformativeText:@
setInformativeTextSelector :: Selector '[Id NSString] ()
setInformativeTextSelector = mkSelector "setInformativeText:"

-- | @Selector@ for @icon@
iconSelector :: Selector '[] (Id NSImage)
iconSelector = mkSelector "icon"

-- | @Selector@ for @setIcon:@
setIconSelector :: Selector '[Id NSImage] ()
setIconSelector = mkSelector "setIcon:"

-- | @Selector@ for @buttons@
buttonsSelector :: Selector '[] (Id NSArray)
buttonsSelector = mkSelector "buttons"

-- | @Selector@ for @alertStyle@
alertStyleSelector :: Selector '[] NSAlertStyle
alertStyleSelector = mkSelector "alertStyle"

-- | @Selector@ for @setAlertStyle:@
setAlertStyleSelector :: Selector '[NSAlertStyle] ()
setAlertStyleSelector = mkSelector "setAlertStyle:"

-- | @Selector@ for @showsHelp@
showsHelpSelector :: Selector '[] Bool
showsHelpSelector = mkSelector "showsHelp"

-- | @Selector@ for @setShowsHelp:@
setShowsHelpSelector :: Selector '[Bool] ()
setShowsHelpSelector = mkSelector "setShowsHelp:"

-- | @Selector@ for @helpAnchor@
helpAnchorSelector :: Selector '[] (Id NSString)
helpAnchorSelector = mkSelector "helpAnchor"

-- | @Selector@ for @setHelpAnchor:@
setHelpAnchorSelector :: Selector '[Id NSString] ()
setHelpAnchorSelector = mkSelector "setHelpAnchor:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @accessoryView@
accessoryViewSelector :: Selector '[] (Id NSView)
accessoryViewSelector = mkSelector "accessoryView"

-- | @Selector@ for @setAccessoryView:@
setAccessoryViewSelector :: Selector '[Id NSView] ()
setAccessoryViewSelector = mkSelector "setAccessoryView:"

-- | @Selector@ for @showsSuppressionButton@
showsSuppressionButtonSelector :: Selector '[] Bool
showsSuppressionButtonSelector = mkSelector "showsSuppressionButton"

-- | @Selector@ for @setShowsSuppressionButton:@
setShowsSuppressionButtonSelector :: Selector '[Bool] ()
setShowsSuppressionButtonSelector = mkSelector "setShowsSuppressionButton:"

-- | @Selector@ for @suppressionButton@
suppressionButtonSelector :: Selector '[] (Id NSButton)
suppressionButtonSelector = mkSelector "suppressionButton"

-- | @Selector@ for @window@
windowSelector :: Selector '[] (Id NSWindow)
windowSelector = mkSelector "window"

