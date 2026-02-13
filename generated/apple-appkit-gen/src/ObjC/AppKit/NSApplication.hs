{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSApplication@.
module ObjC.AppKit.NSApplication
  ( NSApplication
  , IsNSApplication(..)
  , hide
  , unhide
  , unhideWithoutActivation
  , windowWithWindowNumber
  , deactivate
  , activateIgnoringOtherApps
  , activate
  , yieldActivationToApplication
  , yieldActivationToApplicationWithBundleIdentifier
  , hideOtherApplications
  , unhideAllApplications
  , finishLaunching
  , run
  , runModalForWindow
  , stop
  , stopModal
  , stopModalWithCode
  , abortModal
  , beginModalSessionForWindow
  , runModalSession
  , endModalSession
  , terminate
  , requestUserAttention
  , cancelUserAttentionRequest
  , enumerateWindowsWithOptions_usingBlock
  , preventWindowOrdering
  , setWindowsNeedUpdate
  , updateWindows
  , activationPolicy
  , setActivationPolicy
  , reportException
  , detachDrawingThread_toTarget_withObject
  , replyToApplicationShouldTerminate
  , replyToOpenOrPrint
  , orderFrontCharacterPalette
  , extendStateRestoration
  , completeStateRestoration
  , restoreWindowWithIdentifier_state_completionHandler
  , registerUserInterfaceItemSearchHandler
  , unregisterUserInterfaceItemSearchHandler
  , searchString_inUserInterfaceItemString_searchRange_foundRange
  , runPageLayout
  , orderFrontColorPanel
  , toggleTouchBarCustomizationPalette
  , activateContextHelpMode
  , showHelp
  , runModalForWindow_relativeToWindow
  , beginModalSessionForWindow_relativeToWindow
  , application_printFiles
  , beginSheet_modalForWindow_modalDelegate_didEndSelector_contextInfo
  , endSheet
  , endSheet_returnCode
  , makeWindowsPerform_inOrder
  , registerForRemoteNotifications
  , unregisterForRemoteNotifications
  , registerForRemoteNotificationTypes
  , disableRelaunchOnLogin
  , enableRelaunchOnLogin
  , orderFrontStandardAboutPanel
  , orderFrontStandardAboutPanelWithOptions
  , registerServicesMenuSendTypes_returnTypes
  , arrangeInFront
  , removeWindowsItem
  , addWindowsItem_title_filename
  , changeWindowsItem_title_filename
  , updateWindowsItem
  , miniaturizeAll
  , sendAction_to_from
  , targetForAction
  , targetForAction_to_from
  , tryToPerform_with
  , validRequestorForSendType_returnType
  , sendEvent
  , postEvent_atStart
  , nextEventMatchingMask_untilDate_inMode_dequeue
  , discardEventsMatchingMask_beforeEvent
  , sharedApplication
  , delegate
  , setDelegate
  , mainWindow
  , keyWindow
  , active
  , hidden
  , running
  , applicationShouldSuppressHighDynamicRangeContent
  , modalWindow
  , windows
  , mainMenu
  , setMainMenu
  , helpMenu
  , setHelpMenu
  , applicationIconImage
  , setApplicationIconImage
  , dockTile
  , presentationOptions
  , setPresentationOptions
  , currentSystemPresentationOptions
  , occlusionState
  , protectedDataAvailable
  , orderedDocuments
  , orderedWindows
  , automaticCustomizeTouchBarMenuItemEnabled
  , setAutomaticCustomizeTouchBarMenuItemEnabled
  , context
  , registeredForRemoteNotifications
  , enabledRemoteNotificationTypes
  , userInterfaceLayoutDirection
  , servicesProvider
  , setServicesProvider
  , servicesMenu
  , setServicesMenu
  , fullKeyboardAccessEnabled
  , windowsMenu
  , setWindowsMenu
  , currentEvent
  , appearance
  , setAppearance
  , effectiveAppearance
  , abortModalSelector
  , activateContextHelpModeSelector
  , activateIgnoringOtherAppsSelector
  , activateSelector
  , activationPolicySelector
  , activeSelector
  , addWindowsItem_title_filenameSelector
  , appearanceSelector
  , applicationIconImageSelector
  , applicationShouldSuppressHighDynamicRangeContentSelector
  , application_printFilesSelector
  , arrangeInFrontSelector
  , automaticCustomizeTouchBarMenuItemEnabledSelector
  , beginModalSessionForWindowSelector
  , beginModalSessionForWindow_relativeToWindowSelector
  , beginSheet_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector
  , cancelUserAttentionRequestSelector
  , changeWindowsItem_title_filenameSelector
  , completeStateRestorationSelector
  , contextSelector
  , currentEventSelector
  , currentSystemPresentationOptionsSelector
  , deactivateSelector
  , delegateSelector
  , detachDrawingThread_toTarget_withObjectSelector
  , disableRelaunchOnLoginSelector
  , discardEventsMatchingMask_beforeEventSelector
  , dockTileSelector
  , effectiveAppearanceSelector
  , enableRelaunchOnLoginSelector
  , enabledRemoteNotificationTypesSelector
  , endModalSessionSelector
  , endSheetSelector
  , endSheet_returnCodeSelector
  , enumerateWindowsWithOptions_usingBlockSelector
  , extendStateRestorationSelector
  , finishLaunchingSelector
  , fullKeyboardAccessEnabledSelector
  , helpMenuSelector
  , hiddenSelector
  , hideOtherApplicationsSelector
  , hideSelector
  , keyWindowSelector
  , mainMenuSelector
  , mainWindowSelector
  , makeWindowsPerform_inOrderSelector
  , miniaturizeAllSelector
  , modalWindowSelector
  , nextEventMatchingMask_untilDate_inMode_dequeueSelector
  , occlusionStateSelector
  , orderFrontCharacterPaletteSelector
  , orderFrontColorPanelSelector
  , orderFrontStandardAboutPanelSelector
  , orderFrontStandardAboutPanelWithOptionsSelector
  , orderedDocumentsSelector
  , orderedWindowsSelector
  , postEvent_atStartSelector
  , presentationOptionsSelector
  , preventWindowOrderingSelector
  , protectedDataAvailableSelector
  , registerForRemoteNotificationTypesSelector
  , registerForRemoteNotificationsSelector
  , registerServicesMenuSendTypes_returnTypesSelector
  , registerUserInterfaceItemSearchHandlerSelector
  , registeredForRemoteNotificationsSelector
  , removeWindowsItemSelector
  , replyToApplicationShouldTerminateSelector
  , replyToOpenOrPrintSelector
  , reportExceptionSelector
  , requestUserAttentionSelector
  , restoreWindowWithIdentifier_state_completionHandlerSelector
  , runModalForWindowSelector
  , runModalForWindow_relativeToWindowSelector
  , runModalSessionSelector
  , runPageLayoutSelector
  , runSelector
  , runningSelector
  , searchString_inUserInterfaceItemString_searchRange_foundRangeSelector
  , sendAction_to_fromSelector
  , sendEventSelector
  , servicesMenuSelector
  , servicesProviderSelector
  , setActivationPolicySelector
  , setAppearanceSelector
  , setApplicationIconImageSelector
  , setAutomaticCustomizeTouchBarMenuItemEnabledSelector
  , setDelegateSelector
  , setHelpMenuSelector
  , setMainMenuSelector
  , setPresentationOptionsSelector
  , setServicesMenuSelector
  , setServicesProviderSelector
  , setWindowsMenuSelector
  , setWindowsNeedUpdateSelector
  , sharedApplicationSelector
  , showHelpSelector
  , stopModalSelector
  , stopModalWithCodeSelector
  , stopSelector
  , targetForActionSelector
  , targetForAction_to_fromSelector
  , terminateSelector
  , toggleTouchBarCustomizationPaletteSelector
  , tryToPerform_withSelector
  , unhideAllApplicationsSelector
  , unhideSelector
  , unhideWithoutActivationSelector
  , unregisterForRemoteNotificationsSelector
  , unregisterUserInterfaceItemSearchHandlerSelector
  , updateWindowsItemSelector
  , updateWindowsSelector
  , userInterfaceLayoutDirectionSelector
  , validRequestorForSendType_returnTypeSelector
  , windowWithWindowNumberSelector
  , windowsMenuSelector
  , windowsSelector
  , yieldActivationToApplicationSelector
  , yieldActivationToApplicationWithBundleIdentifierSelector

  -- * Enum types
  , NSApplicationActivationPolicy(NSApplicationActivationPolicy)
  , pattern NSApplicationActivationPolicyRegular
  , pattern NSApplicationActivationPolicyAccessory
  , pattern NSApplicationActivationPolicyProhibited
  , NSApplicationDelegateReply(NSApplicationDelegateReply)
  , pattern NSApplicationDelegateReplySuccess
  , pattern NSApplicationDelegateReplyCancel
  , pattern NSApplicationDelegateReplyFailure
  , NSApplicationOcclusionState(NSApplicationOcclusionState)
  , pattern NSApplicationOcclusionStateVisible
  , NSApplicationPresentationOptions(NSApplicationPresentationOptions)
  , pattern NSApplicationPresentationDefault
  , pattern NSApplicationPresentationAutoHideDock
  , pattern NSApplicationPresentationHideDock
  , pattern NSApplicationPresentationAutoHideMenuBar
  , pattern NSApplicationPresentationHideMenuBar
  , pattern NSApplicationPresentationDisableAppleMenu
  , pattern NSApplicationPresentationDisableProcessSwitching
  , pattern NSApplicationPresentationDisableForceQuit
  , pattern NSApplicationPresentationDisableSessionTermination
  , pattern NSApplicationPresentationDisableHideApplication
  , pattern NSApplicationPresentationDisableMenuBarTransparency
  , pattern NSApplicationPresentationFullScreen
  , pattern NSApplicationPresentationAutoHideToolbar
  , pattern NSApplicationPresentationDisableCursorLocationAssistance
  , NSEventMask(NSEventMask)
  , pattern NSEventMaskLeftMouseDown
  , pattern NSEventMaskLeftMouseUp
  , pattern NSEventMaskRightMouseDown
  , pattern NSEventMaskRightMouseUp
  , pattern NSEventMaskMouseMoved
  , pattern NSEventMaskLeftMouseDragged
  , pattern NSEventMaskRightMouseDragged
  , pattern NSEventMaskMouseEntered
  , pattern NSEventMaskMouseExited
  , pattern NSEventMaskKeyDown
  , pattern NSEventMaskKeyUp
  , pattern NSEventMaskFlagsChanged
  , pattern NSEventMaskAppKitDefined
  , pattern NSEventMaskSystemDefined
  , pattern NSEventMaskApplicationDefined
  , pattern NSEventMaskPeriodic
  , pattern NSEventMaskCursorUpdate
  , pattern NSEventMaskScrollWheel
  , pattern NSEventMaskTabletPoint
  , pattern NSEventMaskTabletProximity
  , pattern NSEventMaskOtherMouseDown
  , pattern NSEventMaskOtherMouseUp
  , pattern NSEventMaskOtherMouseDragged
  , pattern NSEventMaskGesture
  , pattern NSEventMaskMagnify
  , pattern NSEventMaskSwipe
  , pattern NSEventMaskRotate
  , pattern NSEventMaskBeginGesture
  , pattern NSEventMaskEndGesture
  , pattern NSEventMaskSmartMagnify
  , pattern NSEventMaskPressure
  , pattern NSEventMaskDirectTouch
  , pattern NSEventMaskChangeMode
  , pattern NSEventMaskMouseCancelled
  , pattern NSEventMaskAny
  , NSRemoteNotificationType(NSRemoteNotificationType)
  , pattern NSRemoteNotificationTypeNone
  , pattern NSRemoteNotificationTypeBadge
  , pattern NSRemoteNotificationTypeSound
  , pattern NSRemoteNotificationTypeAlert
  , NSRequestUserAttentionType(NSRequestUserAttentionType)
  , pattern NSCriticalRequest
  , pattern NSInformationalRequest
  , NSUserInterfaceLayoutDirection(NSUserInterfaceLayoutDirection)
  , pattern NSUserInterfaceLayoutDirectionLeftToRight
  , pattern NSUserInterfaceLayoutDirectionRightToLeft
  , NSWindowListOptions(NSWindowListOptions)
  , pattern NSWindowListOrderedFrontToBack

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- hide:@
hide :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
hide nsApplication sender =
  sendMessage nsApplication hideSelector sender

-- | @- unhide:@
unhide :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
unhide nsApplication sender =
  sendMessage nsApplication unhideSelector sender

-- | @- unhideWithoutActivation@
unhideWithoutActivation :: IsNSApplication nsApplication => nsApplication -> IO ()
unhideWithoutActivation nsApplication =
  sendMessage nsApplication unhideWithoutActivationSelector

-- | @- windowWithWindowNumber:@
windowWithWindowNumber :: IsNSApplication nsApplication => nsApplication -> CLong -> IO (Id NSWindow)
windowWithWindowNumber nsApplication windowNum =
  sendMessage nsApplication windowWithWindowNumberSelector windowNum

-- | @- deactivate@
deactivate :: IsNSApplication nsApplication => nsApplication -> IO ()
deactivate nsApplication =
  sendMessage nsApplication deactivateSelector

-- | Makes the receiver the active app. - Parameter ignoreOtherApps: If @NO@, the app is activated only if no other app is currently active. If @YES@, the app activates regardless.
--
-- ObjC selector: @- activateIgnoringOtherApps:@
activateIgnoringOtherApps :: IsNSApplication nsApplication => nsApplication -> Bool -> IO ()
activateIgnoringOtherApps nsApplication ignoreOtherApps =
  sendMessage nsApplication activateIgnoringOtherAppsSelector ignoreOtherApps

-- | Makes the receiver the active app, if possible.
--
-- You shouldn’t assume the app will be active immediately after sending this message. The framework also does not guarantee that the app will be activated at all.
--
-- For cooperative activation, the other application should call @-yieldActivationToApplication:@ or equivalent prior to the target application invoking @-activate@.
--
-- Invoking @-activate@ on an already-active application cancels any pending activation yields by the receiver.
--
-- ObjC selector: @- activate@
activate :: IsNSApplication nsApplication => nsApplication -> IO ()
activate nsApplication =
  sendMessage nsApplication activateSelector

-- | Explicitly allows another application to make itself active.
--
-- Calling this method will not deactivate the current app, nor will it activate the other app. For cooperative or coordinated activation, the other app should request to be activated at some point in the future by calling @activate@ or equivalent.
--
-- ObjC selector: @- yieldActivationToApplication:@
yieldActivationToApplication :: (IsNSApplication nsApplication, IsNSRunningApplication application) => nsApplication -> application -> IO ()
yieldActivationToApplication nsApplication application =
  sendMessage nsApplication yieldActivationToApplicationSelector (toNSRunningApplication application)

-- | Same as @-yieldActivationToApplication:@, but the provided bundle identifier does not have to correspond to a currently running application.
--
-- This method should be used to yield activation to apps that may not be running at the time of invoking it. If it is known that the target application is currently running, use @-yieldActivationToApplication:@ instead.
--
-- ObjC selector: @- yieldActivationToApplicationWithBundleIdentifier:@
yieldActivationToApplicationWithBundleIdentifier :: (IsNSApplication nsApplication, IsNSString bundleIdentifier) => nsApplication -> bundleIdentifier -> IO ()
yieldActivationToApplicationWithBundleIdentifier nsApplication bundleIdentifier =
  sendMessage nsApplication yieldActivationToApplicationWithBundleIdentifierSelector (toNSString bundleIdentifier)

-- | @- hideOtherApplications:@
hideOtherApplications :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
hideOtherApplications nsApplication sender =
  sendMessage nsApplication hideOtherApplicationsSelector sender

-- | @- unhideAllApplications:@
unhideAllApplications :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
unhideAllApplications nsApplication sender =
  sendMessage nsApplication unhideAllApplicationsSelector sender

-- | @- finishLaunching@
finishLaunching :: IsNSApplication nsApplication => nsApplication -> IO ()
finishLaunching nsApplication =
  sendMessage nsApplication finishLaunchingSelector

-- | @- run@
run :: IsNSApplication nsApplication => nsApplication -> IO ()
run nsApplication =
  sendMessage nsApplication runSelector

-- | @- runModalForWindow:@
runModalForWindow :: (IsNSApplication nsApplication, IsNSWindow window) => nsApplication -> window -> IO CLong
runModalForWindow nsApplication window =
  sendMessage nsApplication runModalForWindowSelector (toNSWindow window)

-- | @- stop:@
stop :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
stop nsApplication sender =
  sendMessage nsApplication stopSelector sender

-- | @- stopModal@
stopModal :: IsNSApplication nsApplication => nsApplication -> IO ()
stopModal nsApplication =
  sendMessage nsApplication stopModalSelector

-- | @- stopModalWithCode:@
stopModalWithCode :: IsNSApplication nsApplication => nsApplication -> CLong -> IO ()
stopModalWithCode nsApplication returnCode =
  sendMessage nsApplication stopModalWithCodeSelector returnCode

-- | @- abortModal@
abortModal :: IsNSApplication nsApplication => nsApplication -> IO ()
abortModal nsApplication =
  sendMessage nsApplication abortModalSelector

-- | @- beginModalSessionForWindow:@
beginModalSessionForWindow :: (IsNSApplication nsApplication, IsNSWindow window) => nsApplication -> window -> IO (Ptr ())
beginModalSessionForWindow nsApplication window =
  sendMessage nsApplication beginModalSessionForWindowSelector (toNSWindow window)

-- | @- runModalSession:@
runModalSession :: IsNSApplication nsApplication => nsApplication -> Ptr () -> IO CLong
runModalSession nsApplication session =
  sendMessage nsApplication runModalSessionSelector session

-- | @- endModalSession:@
endModalSession :: IsNSApplication nsApplication => nsApplication -> Ptr () -> IO ()
endModalSession nsApplication session =
  sendMessage nsApplication endModalSessionSelector session

-- | @- terminate:@
terminate :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
terminate nsApplication sender =
  sendMessage nsApplication terminateSelector sender

-- | Inform the user that this application needs attention - call this method only if your application is not already active.
--
-- ObjC selector: @- requestUserAttention:@
requestUserAttention :: IsNSApplication nsApplication => nsApplication -> NSRequestUserAttentionType -> IO CLong
requestUserAttention nsApplication requestType =
  sendMessage nsApplication requestUserAttentionSelector requestType

-- | @- cancelUserAttentionRequest:@
cancelUserAttentionRequest :: IsNSApplication nsApplication => nsApplication -> CLong -> IO ()
cancelUserAttentionRequest nsApplication request =
  sendMessage nsApplication cancelUserAttentionRequestSelector request

-- | Execute a block for each of the app's windows. Set @*stop = YES@ if desired, to halt the enumeration early.
--
-- ObjC selector: @- enumerateWindowsWithOptions:usingBlock:@
enumerateWindowsWithOptions_usingBlock :: IsNSApplication nsApplication => nsApplication -> NSWindowListOptions -> Ptr () -> IO ()
enumerateWindowsWithOptions_usingBlock nsApplication options block =
  sendMessage nsApplication enumerateWindowsWithOptions_usingBlockSelector options block

-- | @- preventWindowOrdering@
preventWindowOrdering :: IsNSApplication nsApplication => nsApplication -> IO ()
preventWindowOrdering nsApplication =
  sendMessage nsApplication preventWindowOrderingSelector

-- | @- setWindowsNeedUpdate:@
setWindowsNeedUpdate :: IsNSApplication nsApplication => nsApplication -> Bool -> IO ()
setWindowsNeedUpdate nsApplication needUpdate =
  sendMessage nsApplication setWindowsNeedUpdateSelector needUpdate

-- | @- updateWindows@
updateWindows :: IsNSApplication nsApplication => nsApplication -> IO ()
updateWindows nsApplication =
  sendMessage nsApplication updateWindowsSelector

-- | Returns: The activation policy of the application.
--
-- ObjC selector: @- activationPolicy@
activationPolicy :: IsNSApplication nsApplication => nsApplication -> IO NSApplicationActivationPolicy
activationPolicy nsApplication =
  sendMessage nsApplication activationPolicySelector

-- | Attempts to modify the application's activation policy.  In OS X 10.9, any policy may be set; prior to 10.9, the activation policy may be changed to @NSApplicationActivationPolicyProhibited@ or @NSApplicationActivationPolicyRegular,@ but may not be changed to @NSApplicationActivationPolicyAccessory.@  This returns @YES@ if setting the activation policy is successful, and @NO@ if not.
--
-- ObjC selector: @- setActivationPolicy:@
setActivationPolicy :: IsNSApplication nsApplication => nsApplication -> NSApplicationActivationPolicy -> IO Bool
setActivationPolicy nsApplication activationPolicy =
  sendMessage nsApplication setActivationPolicySelector activationPolicy

-- | @- reportException:@
reportException :: (IsNSApplication nsApplication, IsNSException exception) => nsApplication -> exception -> IO ()
reportException nsApplication exception =
  sendMessage nsApplication reportExceptionSelector (toNSException exception)

-- | @+ detachDrawingThread:toTarget:withObject:@
detachDrawingThread_toTarget_withObject :: Sel -> RawId -> RawId -> IO ()
detachDrawingThread_toTarget_withObject selector target argument =
  do
    cls' <- getRequiredClass "NSApplication"
    sendClassMessage cls' detachDrawingThread_toTarget_withObjectSelector selector target argument

-- | If an application delegate returns NSTerminateLater from -applicationShouldTerminate:, -replyToApplicationShouldTerminate: must be called with YES or NO once the application decides if it can terminate.
--
-- ObjC selector: @- replyToApplicationShouldTerminate:@
replyToApplicationShouldTerminate :: IsNSApplication nsApplication => nsApplication -> Bool -> IO ()
replyToApplicationShouldTerminate nsApplication shouldTerminate =
  sendMessage nsApplication replyToApplicationShouldTerminateSelector shouldTerminate

-- | If an application delegate encounters an error while handling @-application:openFiles:@ or@ -application:printFiles:@, @-replyToOpenOrPrint:@ should be called with @NSApplicationDelegateReplyFailure.@  If the user cancels the operation, @NSApplicationDelegateReplyCancel@ should be used, and if the operation succeeds, @NSApplicationDelegateReplySuccess@ should be used .
--
-- ObjC selector: @- replyToOpenOrPrint:@
replyToOpenOrPrint :: IsNSApplication nsApplication => nsApplication -> NSApplicationDelegateReply -> IO ()
replyToOpenOrPrint nsApplication reply =
  sendMessage nsApplication replyToOpenOrPrintSelector reply

-- | Opens the character palette.
--
-- ObjC selector: @- orderFrontCharacterPalette:@
orderFrontCharacterPalette :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
orderFrontCharacterPalette nsApplication sender =
  sendMessage nsApplication orderFrontCharacterPaletteSelector sender

-- | @- extendStateRestoration@
extendStateRestoration :: IsNSApplication nsApplication => nsApplication -> IO ()
extendStateRestoration nsApplication =
  sendMessage nsApplication extendStateRestorationSelector

-- | @- completeStateRestoration@
completeStateRestoration :: IsNSApplication nsApplication => nsApplication -> IO ()
completeStateRestoration nsApplication =
  sendMessage nsApplication completeStateRestorationSelector

-- | @- restoreWindowWithIdentifier:state:completionHandler:@
restoreWindowWithIdentifier_state_completionHandler :: (IsNSApplication nsApplication, IsNSString identifier, IsNSCoder state) => nsApplication -> identifier -> state -> Ptr () -> IO Bool
restoreWindowWithIdentifier_state_completionHandler nsApplication identifier state completionHandler =
  sendMessage nsApplication restoreWindowWithIdentifier_state_completionHandlerSelector (toNSString identifier) (toNSCoder state) completionHandler

-- | @- registerUserInterfaceItemSearchHandler:@
registerUserInterfaceItemSearchHandler :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
registerUserInterfaceItemSearchHandler nsApplication handler =
  sendMessage nsApplication registerUserInterfaceItemSearchHandlerSelector handler

-- | @- unregisterUserInterfaceItemSearchHandler:@
unregisterUserInterfaceItemSearchHandler :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
unregisterUserInterfaceItemSearchHandler nsApplication handler =
  sendMessage nsApplication unregisterUserInterfaceItemSearchHandlerSelector handler

-- | @- searchString:inUserInterfaceItemString:searchRange:foundRange:@
searchString_inUserInterfaceItemString_searchRange_foundRange :: (IsNSApplication nsApplication, IsNSString searchString, IsNSString stringToSearch) => nsApplication -> searchString -> stringToSearch -> NSRange -> Ptr NSRange -> IO Bool
searchString_inUserInterfaceItemString_searchRange_foundRange nsApplication searchString stringToSearch searchRange foundRange =
  sendMessage nsApplication searchString_inUserInterfaceItemString_searchRange_foundRangeSelector (toNSString searchString) (toNSString stringToSearch) searchRange foundRange

-- | @- runPageLayout:@
runPageLayout :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
runPageLayout nsApplication sender =
  sendMessage nsApplication runPageLayoutSelector sender

-- | @- orderFrontColorPanel:@
orderFrontColorPanel :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
orderFrontColorPanel nsApplication sender =
  sendMessage nsApplication orderFrontColorPanelSelector sender

-- | Show or dismiss the customization palette for the currently displayed NSTouchBars. NSApplication validates this selector against whether the current NSTouchBars are customizable and, if configured on a menu item, will standardize and localize the title. If the current system does not have Touch Bar support, the menu item will be automatically hidden.
--
-- ObjC selector: @- toggleTouchBarCustomizationPalette:@
toggleTouchBarCustomizationPalette :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
toggleTouchBarCustomizationPalette nsApplication sender =
  sendMessage nsApplication toggleTouchBarCustomizationPaletteSelector sender

-- | @- activateContextHelpMode:@
activateContextHelpMode :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
activateContextHelpMode nsApplication sender =
  sendMessage nsApplication activateContextHelpModeSelector sender

-- | @- showHelp:@
showHelp :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
showHelp nsApplication sender =
  sendMessage nsApplication showHelpSelector sender

-- | @-runModalForWindow:relativeToWindow:@ was deprecated in Mac OS X 10.0. Please use @-[NSWindow beginSheet:completionHandler:]@ instead.
--
-- ObjC selector: @- runModalForWindow:relativeToWindow:@
runModalForWindow_relativeToWindow :: (IsNSApplication nsApplication, IsNSWindow window, IsNSWindow docWindow) => nsApplication -> window -> docWindow -> IO CLong
runModalForWindow_relativeToWindow nsApplication window docWindow =
  sendMessage nsApplication runModalForWindow_relativeToWindowSelector (toNSWindow window) (toNSWindow docWindow)

-- | @-beginModalSessionForWindow:relativeToWindow:@ was deprecated in Mac OS X 10.0. Please use @-[NSWindow beginSheet:completionHandler:]@ instead.
--
-- ObjC selector: @- beginModalSessionForWindow:relativeToWindow:@
beginModalSessionForWindow_relativeToWindow :: (IsNSApplication nsApplication, IsNSWindow window, IsNSWindow docWindow) => nsApplication -> window -> docWindow -> IO (Ptr ())
beginModalSessionForWindow_relativeToWindow nsApplication window docWindow =
  sendMessage nsApplication beginModalSessionForWindow_relativeToWindowSelector (toNSWindow window) (toNSWindow docWindow)

-- | @-application:printFiles:@ was deprecated in Mac OS X 10.4. Implement @-application:printFiles:withSettings:showPrintPanels:@ in your application delegate instead.
--
-- ObjC selector: @- application:printFiles:@
application_printFiles :: (IsNSApplication nsApplication, IsNSApplication sender, IsNSArray filenames) => nsApplication -> sender -> filenames -> IO ()
application_printFiles nsApplication sender filenames =
  sendMessage nsApplication application_printFilesSelector (toNSApplication sender) (toNSArray filenames)

-- | @NSWindow@'s @-beginSheet:completionHandler:@ and @-endSheet:returnCode:@ should be used instead.  @NSApplication@'s @-beginSheet:modalForWindow:modalDelegate:didEndSelector:contextInfo:@ will continue to work as it previously did, leaking contextInfo and failing when there is already an existing sheet.
--
-- ObjC selector: @- beginSheet:modalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheet_modalForWindow_modalDelegate_didEndSelector_contextInfo :: (IsNSApplication nsApplication, IsNSWindow sheet, IsNSWindow docWindow) => nsApplication -> sheet -> docWindow -> RawId -> Sel -> Ptr () -> IO ()
beginSheet_modalForWindow_modalDelegate_didEndSelector_contextInfo nsApplication sheet docWindow modalDelegate didEndSelector contextInfo =
  sendMessage nsApplication beginSheet_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector (toNSWindow sheet) (toNSWindow docWindow) modalDelegate didEndSelector contextInfo

-- | @- endSheet:@
endSheet :: (IsNSApplication nsApplication, IsNSWindow sheet) => nsApplication -> sheet -> IO ()
endSheet nsApplication sheet =
  sendMessage nsApplication endSheetSelector (toNSWindow sheet)

-- | @- endSheet:returnCode:@
endSheet_returnCode :: (IsNSApplication nsApplication, IsNSWindow sheet) => nsApplication -> sheet -> CLong -> IO ()
endSheet_returnCode nsApplication sheet returnCode =
  sendMessage nsApplication endSheet_returnCodeSelector (toNSWindow sheet) returnCode

-- | @- makeWindowsPerform:inOrder:@
makeWindowsPerform_inOrder :: IsNSApplication nsApplication => nsApplication -> Sel -> Bool -> IO (Id NSWindow)
makeWindowsPerform_inOrder nsApplication selector inOrder =
  sendMessage nsApplication makeWindowsPerform_inOrderSelector selector inOrder

-- | @- registerForRemoteNotifications@
registerForRemoteNotifications :: IsNSApplication nsApplication => nsApplication -> IO ()
registerForRemoteNotifications nsApplication =
  sendMessage nsApplication registerForRemoteNotificationsSelector

-- | @- unregisterForRemoteNotifications@
unregisterForRemoteNotifications :: IsNSApplication nsApplication => nsApplication -> IO ()
unregisterForRemoteNotifications nsApplication =
  sendMessage nsApplication unregisterForRemoteNotificationsSelector

-- | The following are soft deprecated. Please use the @-registerForRemoteNotifications@ above and @-requestAuthorizationWithOptions:@ from @UserNotifications.framework@.
--
-- ObjC selector: @- registerForRemoteNotificationTypes:@
registerForRemoteNotificationTypes :: IsNSApplication nsApplication => nsApplication -> NSRemoteNotificationType -> IO ()
registerForRemoteNotificationTypes nsApplication types =
  sendMessage nsApplication registerForRemoteNotificationTypesSelector types

-- | Disable or reenable relaunching this app on login, if the app was running at the time the user logged out.  These methods increment and decrement a counter respectively; if the counter is 0 at the time the user logs out, then the app may be relaunched when the user logs back in.  The counter is initially zero, so by default apps are relaunched.
--
-- If your app should not be relaunched because it launches via some other mechanism (e.g. launchd), then the recommended usage is to call @-[NSApp disableRelaunchOnLogin]@ once, and never pair it with an -enable call.
--
-- If your app should not be relaunched because it triggers a restart (e.g. an installer), then the recommended usage is to call @-[NSApp disableRelaunchOnLogin]@ immediately before you attempt to trigger a restart, and @-[NSApp enableRelaunchOnLogin]@ immediately after.  This is because the user may cancel restarting; if the user later restarts for another reason, then your app should be brought back.
--
-- These methods are thread safe.
--
-- ObjC selector: @- disableRelaunchOnLogin@
disableRelaunchOnLogin :: IsNSApplication nsApplication => nsApplication -> IO ()
disableRelaunchOnLogin nsApplication =
  sendMessage nsApplication disableRelaunchOnLoginSelector

-- | @- enableRelaunchOnLogin@
enableRelaunchOnLogin :: IsNSApplication nsApplication => nsApplication -> IO ()
enableRelaunchOnLogin nsApplication =
  sendMessage nsApplication enableRelaunchOnLoginSelector

-- | @- orderFrontStandardAboutPanel:@
orderFrontStandardAboutPanel :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
orderFrontStandardAboutPanel nsApplication sender =
  sendMessage nsApplication orderFrontStandardAboutPanelSelector sender

-- | @- orderFrontStandardAboutPanelWithOptions:@
orderFrontStandardAboutPanelWithOptions :: (IsNSApplication nsApplication, IsNSDictionary optionsDictionary) => nsApplication -> optionsDictionary -> IO ()
orderFrontStandardAboutPanelWithOptions nsApplication optionsDictionary =
  sendMessage nsApplication orderFrontStandardAboutPanelWithOptionsSelector (toNSDictionary optionsDictionary)

-- | @- registerServicesMenuSendTypes:returnTypes:@
registerServicesMenuSendTypes_returnTypes :: (IsNSApplication nsApplication, IsNSArray sendTypes, IsNSArray returnTypes) => nsApplication -> sendTypes -> returnTypes -> IO ()
registerServicesMenuSendTypes_returnTypes nsApplication sendTypes returnTypes =
  sendMessage nsApplication registerServicesMenuSendTypes_returnTypesSelector (toNSArray sendTypes) (toNSArray returnTypes)

-- | @- arrangeInFront:@
arrangeInFront :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
arrangeInFront nsApplication sender =
  sendMessage nsApplication arrangeInFrontSelector sender

-- | @- removeWindowsItem:@
removeWindowsItem :: (IsNSApplication nsApplication, IsNSWindow win) => nsApplication -> win -> IO ()
removeWindowsItem nsApplication win =
  sendMessage nsApplication removeWindowsItemSelector (toNSWindow win)

-- | @- addWindowsItem:title:filename:@
addWindowsItem_title_filename :: (IsNSApplication nsApplication, IsNSWindow win, IsNSString string) => nsApplication -> win -> string -> Bool -> IO ()
addWindowsItem_title_filename nsApplication win string isFilename =
  sendMessage nsApplication addWindowsItem_title_filenameSelector (toNSWindow win) (toNSString string) isFilename

-- | @- changeWindowsItem:title:filename:@
changeWindowsItem_title_filename :: (IsNSApplication nsApplication, IsNSWindow win, IsNSString string) => nsApplication -> win -> string -> Bool -> IO ()
changeWindowsItem_title_filename nsApplication win string isFilename =
  sendMessage nsApplication changeWindowsItem_title_filenameSelector (toNSWindow win) (toNSString string) isFilename

-- | @- updateWindowsItem:@
updateWindowsItem :: (IsNSApplication nsApplication, IsNSWindow win) => nsApplication -> win -> IO ()
updateWindowsItem nsApplication win =
  sendMessage nsApplication updateWindowsItemSelector (toNSWindow win)

-- | @- miniaturizeAll:@
miniaturizeAll :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
miniaturizeAll nsApplication sender =
  sendMessage nsApplication miniaturizeAllSelector sender

-- | @- sendAction:to:from:@
sendAction_to_from :: IsNSApplication nsApplication => nsApplication -> Sel -> RawId -> RawId -> IO Bool
sendAction_to_from nsApplication action target sender =
  sendMessage nsApplication sendAction_to_fromSelector action target sender

-- | @- targetForAction:@
targetForAction :: IsNSApplication nsApplication => nsApplication -> Sel -> IO RawId
targetForAction nsApplication action =
  sendMessage nsApplication targetForActionSelector action

-- | @- targetForAction:to:from:@
targetForAction_to_from :: IsNSApplication nsApplication => nsApplication -> Sel -> RawId -> RawId -> IO RawId
targetForAction_to_from nsApplication action target sender =
  sendMessage nsApplication targetForAction_to_fromSelector action target sender

-- | @- tryToPerform:with:@
tryToPerform_with :: IsNSApplication nsApplication => nsApplication -> Sel -> RawId -> IO Bool
tryToPerform_with nsApplication action object =
  sendMessage nsApplication tryToPerform_withSelector action object

-- | @- validRequestorForSendType:returnType:@
validRequestorForSendType_returnType :: (IsNSApplication nsApplication, IsNSString sendType, IsNSString returnType) => nsApplication -> sendType -> returnType -> IO RawId
validRequestorForSendType_returnType nsApplication sendType returnType =
  sendMessage nsApplication validRequestorForSendType_returnTypeSelector (toNSString sendType) (toNSString returnType)

-- | @- sendEvent:@
sendEvent :: (IsNSApplication nsApplication, IsNSEvent event) => nsApplication -> event -> IO ()
sendEvent nsApplication event =
  sendMessage nsApplication sendEventSelector (toNSEvent event)

-- | @- postEvent:atStart:@
postEvent_atStart :: (IsNSApplication nsApplication, IsNSEvent event) => nsApplication -> event -> Bool -> IO ()
postEvent_atStart nsApplication event atStart =
  sendMessage nsApplication postEvent_atStartSelector (toNSEvent event) atStart

-- | @- nextEventMatchingMask:untilDate:inMode:dequeue:@
nextEventMatchingMask_untilDate_inMode_dequeue :: (IsNSApplication nsApplication, IsNSDate expiration, IsNSString mode) => nsApplication -> NSEventMask -> expiration -> mode -> Bool -> IO (Id NSEvent)
nextEventMatchingMask_untilDate_inMode_dequeue nsApplication mask expiration mode deqFlag =
  sendMessage nsApplication nextEventMatchingMask_untilDate_inMode_dequeueSelector mask (toNSDate expiration) (toNSString mode) deqFlag

-- | @- discardEventsMatchingMask:beforeEvent:@
discardEventsMatchingMask_beforeEvent :: (IsNSApplication nsApplication, IsNSEvent lastEvent) => nsApplication -> NSEventMask -> lastEvent -> IO ()
discardEventsMatchingMask_beforeEvent nsApplication mask lastEvent =
  sendMessage nsApplication discardEventsMatchingMask_beforeEventSelector mask (toNSEvent lastEvent)

-- | @+ sharedApplication@
sharedApplication :: IO (Id NSApplication)
sharedApplication  =
  do
    cls' <- getRequiredClass "NSApplication"
    sendClassMessage cls' sharedApplicationSelector

-- | @- delegate@
delegate :: IsNSApplication nsApplication => nsApplication -> IO RawId
delegate nsApplication =
  sendMessage nsApplication delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
setDelegate nsApplication value =
  sendMessage nsApplication setDelegateSelector value

-- | @- mainWindow@
mainWindow :: IsNSApplication nsApplication => nsApplication -> IO (Id NSWindow)
mainWindow nsApplication =
  sendMessage nsApplication mainWindowSelector

-- | @- keyWindow@
keyWindow :: IsNSApplication nsApplication => nsApplication -> IO (Id NSWindow)
keyWindow nsApplication =
  sendMessage nsApplication keyWindowSelector

-- | @- active@
active :: IsNSApplication nsApplication => nsApplication -> IO Bool
active nsApplication =
  sendMessage nsApplication activeSelector

-- | @- hidden@
hidden :: IsNSApplication nsApplication => nsApplication -> IO Bool
hidden nsApplication =
  sendMessage nsApplication hiddenSelector

-- | @- running@
running :: IsNSApplication nsApplication => nsApplication -> IO Bool
running nsApplication =
  sendMessage nsApplication runningSelector

-- | A boolean value indicating whether your application should suppress HDR content based on established policy. Built-in AppKit components such as NSImageView will automatically behave correctly with HDR content. You should use this value in conjunction with notifications (@NSApplicationShouldBeginSuppressingHighDynamicRangeContentNotification@ and @NSApplicationShouldEndSuppressingHighDynamicRangeContentNotification@) to suppress HDR content in your application when signaled to do so.
--
-- ObjC selector: @- applicationShouldSuppressHighDynamicRangeContent@
applicationShouldSuppressHighDynamicRangeContent :: IsNSApplication nsApplication => nsApplication -> IO Bool
applicationShouldSuppressHighDynamicRangeContent nsApplication =
  sendMessage nsApplication applicationShouldSuppressHighDynamicRangeContentSelector

-- | @- modalWindow@
modalWindow :: IsNSApplication nsApplication => nsApplication -> IO (Id NSWindow)
modalWindow nsApplication =
  sendMessage nsApplication modalWindowSelector

-- | @- windows@
windows :: IsNSApplication nsApplication => nsApplication -> IO (Id NSArray)
windows nsApplication =
  sendMessage nsApplication windowsSelector

-- | @- mainMenu@
mainMenu :: IsNSApplication nsApplication => nsApplication -> IO (Id NSMenu)
mainMenu nsApplication =
  sendMessage nsApplication mainMenuSelector

-- | @- setMainMenu:@
setMainMenu :: (IsNSApplication nsApplication, IsNSMenu value) => nsApplication -> value -> IO ()
setMainMenu nsApplication value =
  sendMessage nsApplication setMainMenuSelector (toNSMenu value)

-- | Set or get the Help menu for the app.  If a non-nil menu is set as the Help menu, Spotlight for Help will be installed in it; otherwise AppKit will install Spotlight for Help into a menu of its choosing (and that menu is not returned from @-helpMenu@).  If you wish to completely suppress Spotlight for Help, you can set a menu that does not appear in the menu bar.  @NSApplication@ retains its Help menu and releases it when a different menu is set.
--
-- ObjC selector: @- helpMenu@
helpMenu :: IsNSApplication nsApplication => nsApplication -> IO (Id NSMenu)
helpMenu nsApplication =
  sendMessage nsApplication helpMenuSelector

-- | Set or get the Help menu for the app.  If a non-nil menu is set as the Help menu, Spotlight for Help will be installed in it; otherwise AppKit will install Spotlight for Help into a menu of its choosing (and that menu is not returned from @-helpMenu@).  If you wish to completely suppress Spotlight for Help, you can set a menu that does not appear in the menu bar.  @NSApplication@ retains its Help menu and releases it when a different menu is set.
--
-- ObjC selector: @- setHelpMenu:@
setHelpMenu :: (IsNSApplication nsApplication, IsNSMenu value) => nsApplication -> value -> IO ()
setHelpMenu nsApplication value =
  sendMessage nsApplication setHelpMenuSelector (toNSMenu value)

-- | @- applicationIconImage@
applicationIconImage :: IsNSApplication nsApplication => nsApplication -> IO (Id NSImage)
applicationIconImage nsApplication =
  sendMessage nsApplication applicationIconImageSelector

-- | @- setApplicationIconImage:@
setApplicationIconImage :: (IsNSApplication nsApplication, IsNSImage value) => nsApplication -> value -> IO ()
setApplicationIconImage nsApplication value =
  sendMessage nsApplication setApplicationIconImageSelector (toNSImage value)

-- | @- dockTile@
dockTile :: IsNSApplication nsApplication => nsApplication -> IO (Id NSDockTile)
dockTile nsApplication =
  sendMessage nsApplication dockTileSelector

-- | Gets or sets the @presentationOptions@ that should be in effect for the system when this application is the active application.  Only certain combinations of @NSApplicationPresentationOptions@ flags are allowed, as detailed in the AppKit Release Notes and the reference documentation for @-setPresentationOptions:@.  When given an invalid combination of option flags, @-setPresentationOptions:@ raises an exception.
--
-- ObjC selector: @- presentationOptions@
presentationOptions :: IsNSApplication nsApplication => nsApplication -> IO NSApplicationPresentationOptions
presentationOptions nsApplication =
  sendMessage nsApplication presentationOptionsSelector

-- | Gets or sets the @presentationOptions@ that should be in effect for the system when this application is the active application.  Only certain combinations of @NSApplicationPresentationOptions@ flags are allowed, as detailed in the AppKit Release Notes and the reference documentation for @-setPresentationOptions:@.  When given an invalid combination of option flags, @-setPresentationOptions:@ raises an exception.
--
-- ObjC selector: @- setPresentationOptions:@
setPresentationOptions :: IsNSApplication nsApplication => nsApplication -> NSApplicationPresentationOptions -> IO ()
setPresentationOptions nsApplication value =
  sendMessage nsApplication setPresentationOptionsSelector value

-- | Returns: The set of application presentation options that are currently in effect for the system. These are the presentation options that have been put into effect by the currently active application.
--
-- ObjC selector: @- currentSystemPresentationOptions@
currentSystemPresentationOptions :: IsNSApplication nsApplication => nsApplication -> IO NSApplicationPresentationOptions
currentSystemPresentationOptions nsApplication =
  sendMessage nsApplication currentSystemPresentationOptionsSelector

-- | @- occlusionState@
occlusionState :: IsNSApplication nsApplication => nsApplication -> IO NSApplicationOcclusionState
occlusionState nsApplication =
  sendMessage nsApplication occlusionStateSelector

-- | @- protectedDataAvailable@
protectedDataAvailable :: IsNSApplication nsApplication => nsApplication -> IO Bool
protectedDataAvailable nsApplication =
  sendMessage nsApplication protectedDataAvailableSelector

-- | @- orderedDocuments@
orderedDocuments :: IsNSApplication nsApplication => nsApplication -> IO (Id NSArray)
orderedDocuments nsApplication =
  sendMessage nsApplication orderedDocumentsSelector

-- | @- orderedWindows@
orderedWindows :: IsNSApplication nsApplication => nsApplication -> IO (Id NSArray)
orderedWindows nsApplication =
  sendMessage nsApplication orderedWindowsSelector

-- | Whether or not a menu item to customize the NSTouchBar can be automatically added to the main menu. It will only actually be added when Touch Bar hardware or simulator is present. Defaults to NO. Setting this property to YES is the recommended way to add the customization menu item. But if non-standard placement of the menu item is needed, creating a menu item with an action of @toggleTouchBarCustomizationPalette:@ can be used instead.
--
-- ObjC selector: @- automaticCustomizeTouchBarMenuItemEnabled@
automaticCustomizeTouchBarMenuItemEnabled :: IsNSApplication nsApplication => nsApplication -> IO Bool
automaticCustomizeTouchBarMenuItemEnabled nsApplication =
  sendMessage nsApplication automaticCustomizeTouchBarMenuItemEnabledSelector

-- | Whether or not a menu item to customize the NSTouchBar can be automatically added to the main menu. It will only actually be added when Touch Bar hardware or simulator is present. Defaults to NO. Setting this property to YES is the recommended way to add the customization menu item. But if non-standard placement of the menu item is needed, creating a menu item with an action of @toggleTouchBarCustomizationPalette:@ can be used instead.
--
-- ObjC selector: @- setAutomaticCustomizeTouchBarMenuItemEnabled:@
setAutomaticCustomizeTouchBarMenuItemEnabled :: IsNSApplication nsApplication => nsApplication -> Bool -> IO ()
setAutomaticCustomizeTouchBarMenuItemEnabled nsApplication value =
  sendMessage nsApplication setAutomaticCustomizeTouchBarMenuItemEnabledSelector value

-- | This method is deprecated as of macOS 10.12. Beginning in OS X 10.11 it would always return nil. Prior to this it would return an undefined graphics context that was not generally suitable for drawing.
--
-- ObjC selector: @- context@
context :: IsNSApplication nsApplication => nsApplication -> IO (Id NSGraphicsContext)
context nsApplication =
  sendMessage nsApplication contextSelector

-- | Returns: @YES@ if the application is currently registered for remote notifications, taking into account any systemwide settings; doesn't relate to connectivity.
--
-- ObjC selector: @- registeredForRemoteNotifications@
registeredForRemoteNotifications :: IsNSApplication nsApplication => nsApplication -> IO Bool
registeredForRemoteNotifications nsApplication =
  sendMessage nsApplication registeredForRemoteNotificationsSelector

-- | @- enabledRemoteNotificationTypes@
enabledRemoteNotificationTypes :: IsNSApplication nsApplication => nsApplication -> IO NSRemoteNotificationType
enabledRemoteNotificationTypes nsApplication =
  sendMessage nsApplication enabledRemoteNotificationTypesSelector

-- | @- userInterfaceLayoutDirection@
userInterfaceLayoutDirection :: IsNSApplication nsApplication => nsApplication -> IO NSUserInterfaceLayoutDirection
userInterfaceLayoutDirection nsApplication =
  sendMessage nsApplication userInterfaceLayoutDirectionSelector

-- | @- servicesProvider@
servicesProvider :: IsNSApplication nsApplication => nsApplication -> IO RawId
servicesProvider nsApplication =
  sendMessage nsApplication servicesProviderSelector

-- | @- setServicesProvider:@
setServicesProvider :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
setServicesProvider nsApplication value =
  sendMessage nsApplication setServicesProviderSelector value

-- | @- servicesMenu@
servicesMenu :: IsNSApplication nsApplication => nsApplication -> IO (Id NSMenu)
servicesMenu nsApplication =
  sendMessage nsApplication servicesMenuSelector

-- | @- setServicesMenu:@
setServicesMenu :: (IsNSApplication nsApplication, IsNSMenu value) => nsApplication -> value -> IO ()
setServicesMenu nsApplication value =
  sendMessage nsApplication setServicesMenuSelector (toNSMenu value)

-- | A Boolean value indicating whether keyboard navigation is enabled in System Settings > Keyboard. - Note: The value of this property is @YES@ if keyboard navigation is enabled or @NO@ if it’s not. You might use this value to implement your own key loop or to implement in-control tabbing behavior similar to @NSTableView@. Because of the nature of the preference storage, you won’t be notified of changes to this property if you attempt to observe it through key-value observing; however, accessing this property is fairly inexpensive, so you can access it directly rather than caching it. - Note: This property’s value isn’t necessarily reflective of the separate accessibility setting named “Full Keyboard Access” in System Settings > Accessibility > Keyboard.
--
-- ObjC selector: @- fullKeyboardAccessEnabled@
fullKeyboardAccessEnabled :: IsNSApplication nsApplication => nsApplication -> IO Bool
fullKeyboardAccessEnabled nsApplication =
  sendMessage nsApplication fullKeyboardAccessEnabledSelector

-- | @- windowsMenu@
windowsMenu :: IsNSApplication nsApplication => nsApplication -> IO (Id NSMenu)
windowsMenu nsApplication =
  sendMessage nsApplication windowsMenuSelector

-- | @- setWindowsMenu:@
setWindowsMenu :: (IsNSApplication nsApplication, IsNSMenu value) => nsApplication -> value -> IO ()
setWindowsMenu nsApplication value =
  sendMessage nsApplication setWindowsMenuSelector (toNSMenu value)

-- | @- currentEvent@
currentEvent :: IsNSApplication nsApplication => nsApplication -> IO (Id NSEvent)
currentEvent nsApplication =
  sendMessage nsApplication currentEventSelector

-- | @- appearance@
appearance :: IsNSApplication nsApplication => nsApplication -> IO (Id NSAppearance)
appearance nsApplication =
  sendMessage nsApplication appearanceSelector

-- | @- setAppearance:@
setAppearance :: (IsNSApplication nsApplication, IsNSAppearance value) => nsApplication -> value -> IO ()
setAppearance nsApplication value =
  sendMessage nsApplication setAppearanceSelector (toNSAppearance value)

-- | @- effectiveAppearance@
effectiveAppearance :: IsNSApplication nsApplication => nsApplication -> IO (Id NSAppearance)
effectiveAppearance nsApplication =
  sendMessage nsApplication effectiveAppearanceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hide:@
hideSelector :: Selector '[RawId] ()
hideSelector = mkSelector "hide:"

-- | @Selector@ for @unhide:@
unhideSelector :: Selector '[RawId] ()
unhideSelector = mkSelector "unhide:"

-- | @Selector@ for @unhideWithoutActivation@
unhideWithoutActivationSelector :: Selector '[] ()
unhideWithoutActivationSelector = mkSelector "unhideWithoutActivation"

-- | @Selector@ for @windowWithWindowNumber:@
windowWithWindowNumberSelector :: Selector '[CLong] (Id NSWindow)
windowWithWindowNumberSelector = mkSelector "windowWithWindowNumber:"

-- | @Selector@ for @deactivate@
deactivateSelector :: Selector '[] ()
deactivateSelector = mkSelector "deactivate"

-- | @Selector@ for @activateIgnoringOtherApps:@
activateIgnoringOtherAppsSelector :: Selector '[Bool] ()
activateIgnoringOtherAppsSelector = mkSelector "activateIgnoringOtherApps:"

-- | @Selector@ for @activate@
activateSelector :: Selector '[] ()
activateSelector = mkSelector "activate"

-- | @Selector@ for @yieldActivationToApplication:@
yieldActivationToApplicationSelector :: Selector '[Id NSRunningApplication] ()
yieldActivationToApplicationSelector = mkSelector "yieldActivationToApplication:"

-- | @Selector@ for @yieldActivationToApplicationWithBundleIdentifier:@
yieldActivationToApplicationWithBundleIdentifierSelector :: Selector '[Id NSString] ()
yieldActivationToApplicationWithBundleIdentifierSelector = mkSelector "yieldActivationToApplicationWithBundleIdentifier:"

-- | @Selector@ for @hideOtherApplications:@
hideOtherApplicationsSelector :: Selector '[RawId] ()
hideOtherApplicationsSelector = mkSelector "hideOtherApplications:"

-- | @Selector@ for @unhideAllApplications:@
unhideAllApplicationsSelector :: Selector '[RawId] ()
unhideAllApplicationsSelector = mkSelector "unhideAllApplications:"

-- | @Selector@ for @finishLaunching@
finishLaunchingSelector :: Selector '[] ()
finishLaunchingSelector = mkSelector "finishLaunching"

-- | @Selector@ for @run@
runSelector :: Selector '[] ()
runSelector = mkSelector "run"

-- | @Selector@ for @runModalForWindow:@
runModalForWindowSelector :: Selector '[Id NSWindow] CLong
runModalForWindowSelector = mkSelector "runModalForWindow:"

-- | @Selector@ for @stop:@
stopSelector :: Selector '[RawId] ()
stopSelector = mkSelector "stop:"

-- | @Selector@ for @stopModal@
stopModalSelector :: Selector '[] ()
stopModalSelector = mkSelector "stopModal"

-- | @Selector@ for @stopModalWithCode:@
stopModalWithCodeSelector :: Selector '[CLong] ()
stopModalWithCodeSelector = mkSelector "stopModalWithCode:"

-- | @Selector@ for @abortModal@
abortModalSelector :: Selector '[] ()
abortModalSelector = mkSelector "abortModal"

-- | @Selector@ for @beginModalSessionForWindow:@
beginModalSessionForWindowSelector :: Selector '[Id NSWindow] (Ptr ())
beginModalSessionForWindowSelector = mkSelector "beginModalSessionForWindow:"

-- | @Selector@ for @runModalSession:@
runModalSessionSelector :: Selector '[Ptr ()] CLong
runModalSessionSelector = mkSelector "runModalSession:"

-- | @Selector@ for @endModalSession:@
endModalSessionSelector :: Selector '[Ptr ()] ()
endModalSessionSelector = mkSelector "endModalSession:"

-- | @Selector@ for @terminate:@
terminateSelector :: Selector '[RawId] ()
terminateSelector = mkSelector "terminate:"

-- | @Selector@ for @requestUserAttention:@
requestUserAttentionSelector :: Selector '[NSRequestUserAttentionType] CLong
requestUserAttentionSelector = mkSelector "requestUserAttention:"

-- | @Selector@ for @cancelUserAttentionRequest:@
cancelUserAttentionRequestSelector :: Selector '[CLong] ()
cancelUserAttentionRequestSelector = mkSelector "cancelUserAttentionRequest:"

-- | @Selector@ for @enumerateWindowsWithOptions:usingBlock:@
enumerateWindowsWithOptions_usingBlockSelector :: Selector '[NSWindowListOptions, Ptr ()] ()
enumerateWindowsWithOptions_usingBlockSelector = mkSelector "enumerateWindowsWithOptions:usingBlock:"

-- | @Selector@ for @preventWindowOrdering@
preventWindowOrderingSelector :: Selector '[] ()
preventWindowOrderingSelector = mkSelector "preventWindowOrdering"

-- | @Selector@ for @setWindowsNeedUpdate:@
setWindowsNeedUpdateSelector :: Selector '[Bool] ()
setWindowsNeedUpdateSelector = mkSelector "setWindowsNeedUpdate:"

-- | @Selector@ for @updateWindows@
updateWindowsSelector :: Selector '[] ()
updateWindowsSelector = mkSelector "updateWindows"

-- | @Selector@ for @activationPolicy@
activationPolicySelector :: Selector '[] NSApplicationActivationPolicy
activationPolicySelector = mkSelector "activationPolicy"

-- | @Selector@ for @setActivationPolicy:@
setActivationPolicySelector :: Selector '[NSApplicationActivationPolicy] Bool
setActivationPolicySelector = mkSelector "setActivationPolicy:"

-- | @Selector@ for @reportException:@
reportExceptionSelector :: Selector '[Id NSException] ()
reportExceptionSelector = mkSelector "reportException:"

-- | @Selector@ for @detachDrawingThread:toTarget:withObject:@
detachDrawingThread_toTarget_withObjectSelector :: Selector '[Sel, RawId, RawId] ()
detachDrawingThread_toTarget_withObjectSelector = mkSelector "detachDrawingThread:toTarget:withObject:"

-- | @Selector@ for @replyToApplicationShouldTerminate:@
replyToApplicationShouldTerminateSelector :: Selector '[Bool] ()
replyToApplicationShouldTerminateSelector = mkSelector "replyToApplicationShouldTerminate:"

-- | @Selector@ for @replyToOpenOrPrint:@
replyToOpenOrPrintSelector :: Selector '[NSApplicationDelegateReply] ()
replyToOpenOrPrintSelector = mkSelector "replyToOpenOrPrint:"

-- | @Selector@ for @orderFrontCharacterPalette:@
orderFrontCharacterPaletteSelector :: Selector '[RawId] ()
orderFrontCharacterPaletteSelector = mkSelector "orderFrontCharacterPalette:"

-- | @Selector@ for @extendStateRestoration@
extendStateRestorationSelector :: Selector '[] ()
extendStateRestorationSelector = mkSelector "extendStateRestoration"

-- | @Selector@ for @completeStateRestoration@
completeStateRestorationSelector :: Selector '[] ()
completeStateRestorationSelector = mkSelector "completeStateRestoration"

-- | @Selector@ for @restoreWindowWithIdentifier:state:completionHandler:@
restoreWindowWithIdentifier_state_completionHandlerSelector :: Selector '[Id NSString, Id NSCoder, Ptr ()] Bool
restoreWindowWithIdentifier_state_completionHandlerSelector = mkSelector "restoreWindowWithIdentifier:state:completionHandler:"

-- | @Selector@ for @registerUserInterfaceItemSearchHandler:@
registerUserInterfaceItemSearchHandlerSelector :: Selector '[RawId] ()
registerUserInterfaceItemSearchHandlerSelector = mkSelector "registerUserInterfaceItemSearchHandler:"

-- | @Selector@ for @unregisterUserInterfaceItemSearchHandler:@
unregisterUserInterfaceItemSearchHandlerSelector :: Selector '[RawId] ()
unregisterUserInterfaceItemSearchHandlerSelector = mkSelector "unregisterUserInterfaceItemSearchHandler:"

-- | @Selector@ for @searchString:inUserInterfaceItemString:searchRange:foundRange:@
searchString_inUserInterfaceItemString_searchRange_foundRangeSelector :: Selector '[Id NSString, Id NSString, NSRange, Ptr NSRange] Bool
searchString_inUserInterfaceItemString_searchRange_foundRangeSelector = mkSelector "searchString:inUserInterfaceItemString:searchRange:foundRange:"

-- | @Selector@ for @runPageLayout:@
runPageLayoutSelector :: Selector '[RawId] ()
runPageLayoutSelector = mkSelector "runPageLayout:"

-- | @Selector@ for @orderFrontColorPanel:@
orderFrontColorPanelSelector :: Selector '[RawId] ()
orderFrontColorPanelSelector = mkSelector "orderFrontColorPanel:"

-- | @Selector@ for @toggleTouchBarCustomizationPalette:@
toggleTouchBarCustomizationPaletteSelector :: Selector '[RawId] ()
toggleTouchBarCustomizationPaletteSelector = mkSelector "toggleTouchBarCustomizationPalette:"

-- | @Selector@ for @activateContextHelpMode:@
activateContextHelpModeSelector :: Selector '[RawId] ()
activateContextHelpModeSelector = mkSelector "activateContextHelpMode:"

-- | @Selector@ for @showHelp:@
showHelpSelector :: Selector '[RawId] ()
showHelpSelector = mkSelector "showHelp:"

-- | @Selector@ for @runModalForWindow:relativeToWindow:@
runModalForWindow_relativeToWindowSelector :: Selector '[Id NSWindow, Id NSWindow] CLong
runModalForWindow_relativeToWindowSelector = mkSelector "runModalForWindow:relativeToWindow:"

-- | @Selector@ for @beginModalSessionForWindow:relativeToWindow:@
beginModalSessionForWindow_relativeToWindowSelector :: Selector '[Id NSWindow, Id NSWindow] (Ptr ())
beginModalSessionForWindow_relativeToWindowSelector = mkSelector "beginModalSessionForWindow:relativeToWindow:"

-- | @Selector@ for @application:printFiles:@
application_printFilesSelector :: Selector '[Id NSApplication, Id NSArray] ()
application_printFilesSelector = mkSelector "application:printFiles:"

-- | @Selector@ for @beginSheet:modalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheet_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector :: Selector '[Id NSWindow, Id NSWindow, RawId, Sel, Ptr ()] ()
beginSheet_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector = mkSelector "beginSheet:modalForWindow:modalDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @endSheet:@
endSheetSelector :: Selector '[Id NSWindow] ()
endSheetSelector = mkSelector "endSheet:"

-- | @Selector@ for @endSheet:returnCode:@
endSheet_returnCodeSelector :: Selector '[Id NSWindow, CLong] ()
endSheet_returnCodeSelector = mkSelector "endSheet:returnCode:"

-- | @Selector@ for @makeWindowsPerform:inOrder:@
makeWindowsPerform_inOrderSelector :: Selector '[Sel, Bool] (Id NSWindow)
makeWindowsPerform_inOrderSelector = mkSelector "makeWindowsPerform:inOrder:"

-- | @Selector@ for @registerForRemoteNotifications@
registerForRemoteNotificationsSelector :: Selector '[] ()
registerForRemoteNotificationsSelector = mkSelector "registerForRemoteNotifications"

-- | @Selector@ for @unregisterForRemoteNotifications@
unregisterForRemoteNotificationsSelector :: Selector '[] ()
unregisterForRemoteNotificationsSelector = mkSelector "unregisterForRemoteNotifications"

-- | @Selector@ for @registerForRemoteNotificationTypes:@
registerForRemoteNotificationTypesSelector :: Selector '[NSRemoteNotificationType] ()
registerForRemoteNotificationTypesSelector = mkSelector "registerForRemoteNotificationTypes:"

-- | @Selector@ for @disableRelaunchOnLogin@
disableRelaunchOnLoginSelector :: Selector '[] ()
disableRelaunchOnLoginSelector = mkSelector "disableRelaunchOnLogin"

-- | @Selector@ for @enableRelaunchOnLogin@
enableRelaunchOnLoginSelector :: Selector '[] ()
enableRelaunchOnLoginSelector = mkSelector "enableRelaunchOnLogin"

-- | @Selector@ for @orderFrontStandardAboutPanel:@
orderFrontStandardAboutPanelSelector :: Selector '[RawId] ()
orderFrontStandardAboutPanelSelector = mkSelector "orderFrontStandardAboutPanel:"

-- | @Selector@ for @orderFrontStandardAboutPanelWithOptions:@
orderFrontStandardAboutPanelWithOptionsSelector :: Selector '[Id NSDictionary] ()
orderFrontStandardAboutPanelWithOptionsSelector = mkSelector "orderFrontStandardAboutPanelWithOptions:"

-- | @Selector@ for @registerServicesMenuSendTypes:returnTypes:@
registerServicesMenuSendTypes_returnTypesSelector :: Selector '[Id NSArray, Id NSArray] ()
registerServicesMenuSendTypes_returnTypesSelector = mkSelector "registerServicesMenuSendTypes:returnTypes:"

-- | @Selector@ for @arrangeInFront:@
arrangeInFrontSelector :: Selector '[RawId] ()
arrangeInFrontSelector = mkSelector "arrangeInFront:"

-- | @Selector@ for @removeWindowsItem:@
removeWindowsItemSelector :: Selector '[Id NSWindow] ()
removeWindowsItemSelector = mkSelector "removeWindowsItem:"

-- | @Selector@ for @addWindowsItem:title:filename:@
addWindowsItem_title_filenameSelector :: Selector '[Id NSWindow, Id NSString, Bool] ()
addWindowsItem_title_filenameSelector = mkSelector "addWindowsItem:title:filename:"

-- | @Selector@ for @changeWindowsItem:title:filename:@
changeWindowsItem_title_filenameSelector :: Selector '[Id NSWindow, Id NSString, Bool] ()
changeWindowsItem_title_filenameSelector = mkSelector "changeWindowsItem:title:filename:"

-- | @Selector@ for @updateWindowsItem:@
updateWindowsItemSelector :: Selector '[Id NSWindow] ()
updateWindowsItemSelector = mkSelector "updateWindowsItem:"

-- | @Selector@ for @miniaturizeAll:@
miniaturizeAllSelector :: Selector '[RawId] ()
miniaturizeAllSelector = mkSelector "miniaturizeAll:"

-- | @Selector@ for @sendAction:to:from:@
sendAction_to_fromSelector :: Selector '[Sel, RawId, RawId] Bool
sendAction_to_fromSelector = mkSelector "sendAction:to:from:"

-- | @Selector@ for @targetForAction:@
targetForActionSelector :: Selector '[Sel] RawId
targetForActionSelector = mkSelector "targetForAction:"

-- | @Selector@ for @targetForAction:to:from:@
targetForAction_to_fromSelector :: Selector '[Sel, RawId, RawId] RawId
targetForAction_to_fromSelector = mkSelector "targetForAction:to:from:"

-- | @Selector@ for @tryToPerform:with:@
tryToPerform_withSelector :: Selector '[Sel, RawId] Bool
tryToPerform_withSelector = mkSelector "tryToPerform:with:"

-- | @Selector@ for @validRequestorForSendType:returnType:@
validRequestorForSendType_returnTypeSelector :: Selector '[Id NSString, Id NSString] RawId
validRequestorForSendType_returnTypeSelector = mkSelector "validRequestorForSendType:returnType:"

-- | @Selector@ for @sendEvent:@
sendEventSelector :: Selector '[Id NSEvent] ()
sendEventSelector = mkSelector "sendEvent:"

-- | @Selector@ for @postEvent:atStart:@
postEvent_atStartSelector :: Selector '[Id NSEvent, Bool] ()
postEvent_atStartSelector = mkSelector "postEvent:atStart:"

-- | @Selector@ for @nextEventMatchingMask:untilDate:inMode:dequeue:@
nextEventMatchingMask_untilDate_inMode_dequeueSelector :: Selector '[NSEventMask, Id NSDate, Id NSString, Bool] (Id NSEvent)
nextEventMatchingMask_untilDate_inMode_dequeueSelector = mkSelector "nextEventMatchingMask:untilDate:inMode:dequeue:"

-- | @Selector@ for @discardEventsMatchingMask:beforeEvent:@
discardEventsMatchingMask_beforeEventSelector :: Selector '[NSEventMask, Id NSEvent] ()
discardEventsMatchingMask_beforeEventSelector = mkSelector "discardEventsMatchingMask:beforeEvent:"

-- | @Selector@ for @sharedApplication@
sharedApplicationSelector :: Selector '[] (Id NSApplication)
sharedApplicationSelector = mkSelector "sharedApplication"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @mainWindow@
mainWindowSelector :: Selector '[] (Id NSWindow)
mainWindowSelector = mkSelector "mainWindow"

-- | @Selector@ for @keyWindow@
keyWindowSelector :: Selector '[] (Id NSWindow)
keyWindowSelector = mkSelector "keyWindow"

-- | @Selector@ for @active@
activeSelector :: Selector '[] Bool
activeSelector = mkSelector "active"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector '[] Bool
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @running@
runningSelector :: Selector '[] Bool
runningSelector = mkSelector "running"

-- | @Selector@ for @applicationShouldSuppressHighDynamicRangeContent@
applicationShouldSuppressHighDynamicRangeContentSelector :: Selector '[] Bool
applicationShouldSuppressHighDynamicRangeContentSelector = mkSelector "applicationShouldSuppressHighDynamicRangeContent"

-- | @Selector@ for @modalWindow@
modalWindowSelector :: Selector '[] (Id NSWindow)
modalWindowSelector = mkSelector "modalWindow"

-- | @Selector@ for @windows@
windowsSelector :: Selector '[] (Id NSArray)
windowsSelector = mkSelector "windows"

-- | @Selector@ for @mainMenu@
mainMenuSelector :: Selector '[] (Id NSMenu)
mainMenuSelector = mkSelector "mainMenu"

-- | @Selector@ for @setMainMenu:@
setMainMenuSelector :: Selector '[Id NSMenu] ()
setMainMenuSelector = mkSelector "setMainMenu:"

-- | @Selector@ for @helpMenu@
helpMenuSelector :: Selector '[] (Id NSMenu)
helpMenuSelector = mkSelector "helpMenu"

-- | @Selector@ for @setHelpMenu:@
setHelpMenuSelector :: Selector '[Id NSMenu] ()
setHelpMenuSelector = mkSelector "setHelpMenu:"

-- | @Selector@ for @applicationIconImage@
applicationIconImageSelector :: Selector '[] (Id NSImage)
applicationIconImageSelector = mkSelector "applicationIconImage"

-- | @Selector@ for @setApplicationIconImage:@
setApplicationIconImageSelector :: Selector '[Id NSImage] ()
setApplicationIconImageSelector = mkSelector "setApplicationIconImage:"

-- | @Selector@ for @dockTile@
dockTileSelector :: Selector '[] (Id NSDockTile)
dockTileSelector = mkSelector "dockTile"

-- | @Selector@ for @presentationOptions@
presentationOptionsSelector :: Selector '[] NSApplicationPresentationOptions
presentationOptionsSelector = mkSelector "presentationOptions"

-- | @Selector@ for @setPresentationOptions:@
setPresentationOptionsSelector :: Selector '[NSApplicationPresentationOptions] ()
setPresentationOptionsSelector = mkSelector "setPresentationOptions:"

-- | @Selector@ for @currentSystemPresentationOptions@
currentSystemPresentationOptionsSelector :: Selector '[] NSApplicationPresentationOptions
currentSystemPresentationOptionsSelector = mkSelector "currentSystemPresentationOptions"

-- | @Selector@ for @occlusionState@
occlusionStateSelector :: Selector '[] NSApplicationOcclusionState
occlusionStateSelector = mkSelector "occlusionState"

-- | @Selector@ for @protectedDataAvailable@
protectedDataAvailableSelector :: Selector '[] Bool
protectedDataAvailableSelector = mkSelector "protectedDataAvailable"

-- | @Selector@ for @orderedDocuments@
orderedDocumentsSelector :: Selector '[] (Id NSArray)
orderedDocumentsSelector = mkSelector "orderedDocuments"

-- | @Selector@ for @orderedWindows@
orderedWindowsSelector :: Selector '[] (Id NSArray)
orderedWindowsSelector = mkSelector "orderedWindows"

-- | @Selector@ for @automaticCustomizeTouchBarMenuItemEnabled@
automaticCustomizeTouchBarMenuItemEnabledSelector :: Selector '[] Bool
automaticCustomizeTouchBarMenuItemEnabledSelector = mkSelector "automaticCustomizeTouchBarMenuItemEnabled"

-- | @Selector@ for @setAutomaticCustomizeTouchBarMenuItemEnabled:@
setAutomaticCustomizeTouchBarMenuItemEnabledSelector :: Selector '[Bool] ()
setAutomaticCustomizeTouchBarMenuItemEnabledSelector = mkSelector "setAutomaticCustomizeTouchBarMenuItemEnabled:"

-- | @Selector@ for @context@
contextSelector :: Selector '[] (Id NSGraphicsContext)
contextSelector = mkSelector "context"

-- | @Selector@ for @registeredForRemoteNotifications@
registeredForRemoteNotificationsSelector :: Selector '[] Bool
registeredForRemoteNotificationsSelector = mkSelector "registeredForRemoteNotifications"

-- | @Selector@ for @enabledRemoteNotificationTypes@
enabledRemoteNotificationTypesSelector :: Selector '[] NSRemoteNotificationType
enabledRemoteNotificationTypesSelector = mkSelector "enabledRemoteNotificationTypes"

-- | @Selector@ for @userInterfaceLayoutDirection@
userInterfaceLayoutDirectionSelector :: Selector '[] NSUserInterfaceLayoutDirection
userInterfaceLayoutDirectionSelector = mkSelector "userInterfaceLayoutDirection"

-- | @Selector@ for @servicesProvider@
servicesProviderSelector :: Selector '[] RawId
servicesProviderSelector = mkSelector "servicesProvider"

-- | @Selector@ for @setServicesProvider:@
setServicesProviderSelector :: Selector '[RawId] ()
setServicesProviderSelector = mkSelector "setServicesProvider:"

-- | @Selector@ for @servicesMenu@
servicesMenuSelector :: Selector '[] (Id NSMenu)
servicesMenuSelector = mkSelector "servicesMenu"

-- | @Selector@ for @setServicesMenu:@
setServicesMenuSelector :: Selector '[Id NSMenu] ()
setServicesMenuSelector = mkSelector "setServicesMenu:"

-- | @Selector@ for @fullKeyboardAccessEnabled@
fullKeyboardAccessEnabledSelector :: Selector '[] Bool
fullKeyboardAccessEnabledSelector = mkSelector "fullKeyboardAccessEnabled"

-- | @Selector@ for @windowsMenu@
windowsMenuSelector :: Selector '[] (Id NSMenu)
windowsMenuSelector = mkSelector "windowsMenu"

-- | @Selector@ for @setWindowsMenu:@
setWindowsMenuSelector :: Selector '[Id NSMenu] ()
setWindowsMenuSelector = mkSelector "setWindowsMenu:"

-- | @Selector@ for @currentEvent@
currentEventSelector :: Selector '[] (Id NSEvent)
currentEventSelector = mkSelector "currentEvent"

-- | @Selector@ for @appearance@
appearanceSelector :: Selector '[] (Id NSAppearance)
appearanceSelector = mkSelector "appearance"

-- | @Selector@ for @setAppearance:@
setAppearanceSelector :: Selector '[Id NSAppearance] ()
setAppearanceSelector = mkSelector "setAppearance:"

-- | @Selector@ for @effectiveAppearance@
effectiveAppearanceSelector :: Selector '[] (Id NSAppearance)
effectiveAppearanceSelector = mkSelector "effectiveAppearance"

