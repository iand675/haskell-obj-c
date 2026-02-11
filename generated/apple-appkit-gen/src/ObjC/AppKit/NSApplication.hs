{-# LANGUAGE PatternSynonyms #-}
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
  , hideSelector
  , unhideSelector
  , unhideWithoutActivationSelector
  , windowWithWindowNumberSelector
  , deactivateSelector
  , activateIgnoringOtherAppsSelector
  , activateSelector
  , yieldActivationToApplicationSelector
  , yieldActivationToApplicationWithBundleIdentifierSelector
  , hideOtherApplicationsSelector
  , unhideAllApplicationsSelector
  , finishLaunchingSelector
  , runSelector
  , runModalForWindowSelector
  , stopSelector
  , stopModalSelector
  , stopModalWithCodeSelector
  , abortModalSelector
  , beginModalSessionForWindowSelector
  , runModalSessionSelector
  , endModalSessionSelector
  , terminateSelector
  , requestUserAttentionSelector
  , cancelUserAttentionRequestSelector
  , enumerateWindowsWithOptions_usingBlockSelector
  , preventWindowOrderingSelector
  , setWindowsNeedUpdateSelector
  , updateWindowsSelector
  , activationPolicySelector
  , setActivationPolicySelector
  , reportExceptionSelector
  , detachDrawingThread_toTarget_withObjectSelector
  , replyToApplicationShouldTerminateSelector
  , replyToOpenOrPrintSelector
  , orderFrontCharacterPaletteSelector
  , extendStateRestorationSelector
  , completeStateRestorationSelector
  , restoreWindowWithIdentifier_state_completionHandlerSelector
  , registerUserInterfaceItemSearchHandlerSelector
  , unregisterUserInterfaceItemSearchHandlerSelector
  , searchString_inUserInterfaceItemString_searchRange_foundRangeSelector
  , runPageLayoutSelector
  , orderFrontColorPanelSelector
  , toggleTouchBarCustomizationPaletteSelector
  , activateContextHelpModeSelector
  , showHelpSelector
  , runModalForWindow_relativeToWindowSelector
  , beginModalSessionForWindow_relativeToWindowSelector
  , application_printFilesSelector
  , beginSheet_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector
  , endSheetSelector
  , endSheet_returnCodeSelector
  , makeWindowsPerform_inOrderSelector
  , registerForRemoteNotificationsSelector
  , unregisterForRemoteNotificationsSelector
  , registerForRemoteNotificationTypesSelector
  , disableRelaunchOnLoginSelector
  , enableRelaunchOnLoginSelector
  , orderFrontStandardAboutPanelSelector
  , orderFrontStandardAboutPanelWithOptionsSelector
  , registerServicesMenuSendTypes_returnTypesSelector
  , arrangeInFrontSelector
  , removeWindowsItemSelector
  , addWindowsItem_title_filenameSelector
  , changeWindowsItem_title_filenameSelector
  , updateWindowsItemSelector
  , miniaturizeAllSelector
  , sendAction_to_fromSelector
  , targetForActionSelector
  , targetForAction_to_fromSelector
  , tryToPerform_withSelector
  , validRequestorForSendType_returnTypeSelector
  , sendEventSelector
  , postEvent_atStartSelector
  , nextEventMatchingMask_untilDate_inMode_dequeueSelector
  , discardEventsMatchingMask_beforeEventSelector
  , sharedApplicationSelector
  , delegateSelector
  , setDelegateSelector
  , mainWindowSelector
  , keyWindowSelector
  , activeSelector
  , hiddenSelector
  , runningSelector
  , applicationShouldSuppressHighDynamicRangeContentSelector
  , modalWindowSelector
  , windowsSelector
  , mainMenuSelector
  , setMainMenuSelector
  , helpMenuSelector
  , setHelpMenuSelector
  , applicationIconImageSelector
  , setApplicationIconImageSelector
  , dockTileSelector
  , presentationOptionsSelector
  , setPresentationOptionsSelector
  , currentSystemPresentationOptionsSelector
  , occlusionStateSelector
  , protectedDataAvailableSelector
  , orderedDocumentsSelector
  , orderedWindowsSelector
  , automaticCustomizeTouchBarMenuItemEnabledSelector
  , setAutomaticCustomizeTouchBarMenuItemEnabledSelector
  , contextSelector
  , registeredForRemoteNotificationsSelector
  , enabledRemoteNotificationTypesSelector
  , userInterfaceLayoutDirectionSelector
  , servicesProviderSelector
  , setServicesProviderSelector
  , servicesMenuSelector
  , setServicesMenuSelector
  , fullKeyboardAccessEnabledSelector
  , windowsMenuSelector
  , setWindowsMenuSelector
  , currentEventSelector
  , appearanceSelector
  , setAppearanceSelector
  , effectiveAppearanceSelector

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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- hide:@
hide :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
hide nsApplication  sender =
    sendMsg nsApplication (mkSelector "hide:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- unhide:@
unhide :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
unhide nsApplication  sender =
    sendMsg nsApplication (mkSelector "unhide:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- unhideWithoutActivation@
unhideWithoutActivation :: IsNSApplication nsApplication => nsApplication -> IO ()
unhideWithoutActivation nsApplication  =
    sendMsg nsApplication (mkSelector "unhideWithoutActivation") retVoid []

-- | @- windowWithWindowNumber:@
windowWithWindowNumber :: IsNSApplication nsApplication => nsApplication -> CLong -> IO (Id NSWindow)
windowWithWindowNumber nsApplication  windowNum =
    sendMsg nsApplication (mkSelector "windowWithWindowNumber:") (retPtr retVoid) [argCLong windowNum] >>= retainedObject . castPtr

-- | @- deactivate@
deactivate :: IsNSApplication nsApplication => nsApplication -> IO ()
deactivate nsApplication  =
    sendMsg nsApplication (mkSelector "deactivate") retVoid []

-- | Makes the receiver the active app. - Parameter ignoreOtherApps: If @NO@, the app is activated only if no other app is currently active. If @YES@, the app activates regardless.
--
-- ObjC selector: @- activateIgnoringOtherApps:@
activateIgnoringOtherApps :: IsNSApplication nsApplication => nsApplication -> Bool -> IO ()
activateIgnoringOtherApps nsApplication  ignoreOtherApps =
    sendMsg nsApplication (mkSelector "activateIgnoringOtherApps:") retVoid [argCULong (if ignoreOtherApps then 1 else 0)]

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
activate nsApplication  =
    sendMsg nsApplication (mkSelector "activate") retVoid []

-- | Explicitly allows another application to make itself active.
--
-- Calling this method will not deactivate the current app, nor will it activate the other app. For cooperative or coordinated activation, the other app should request to be activated at some point in the future by calling @activate@ or equivalent.
--
-- ObjC selector: @- yieldActivationToApplication:@
yieldActivationToApplication :: (IsNSApplication nsApplication, IsNSRunningApplication application) => nsApplication -> application -> IO ()
yieldActivationToApplication nsApplication  application =
  withObjCPtr application $ \raw_application ->
      sendMsg nsApplication (mkSelector "yieldActivationToApplication:") retVoid [argPtr (castPtr raw_application :: Ptr ())]

-- | Same as @-yieldActivationToApplication:@, but the provided bundle identifier does not have to correspond to a currently running application.
--
-- This method should be used to yield activation to apps that may not be running at the time of invoking it. If it is known that the target application is currently running, use @-yieldActivationToApplication:@ instead.
--
-- ObjC selector: @- yieldActivationToApplicationWithBundleIdentifier:@
yieldActivationToApplicationWithBundleIdentifier :: (IsNSApplication nsApplication, IsNSString bundleIdentifier) => nsApplication -> bundleIdentifier -> IO ()
yieldActivationToApplicationWithBundleIdentifier nsApplication  bundleIdentifier =
  withObjCPtr bundleIdentifier $ \raw_bundleIdentifier ->
      sendMsg nsApplication (mkSelector "yieldActivationToApplicationWithBundleIdentifier:") retVoid [argPtr (castPtr raw_bundleIdentifier :: Ptr ())]

-- | @- hideOtherApplications:@
hideOtherApplications :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
hideOtherApplications nsApplication  sender =
    sendMsg nsApplication (mkSelector "hideOtherApplications:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- unhideAllApplications:@
unhideAllApplications :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
unhideAllApplications nsApplication  sender =
    sendMsg nsApplication (mkSelector "unhideAllApplications:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- finishLaunching@
finishLaunching :: IsNSApplication nsApplication => nsApplication -> IO ()
finishLaunching nsApplication  =
    sendMsg nsApplication (mkSelector "finishLaunching") retVoid []

-- | @- run@
run :: IsNSApplication nsApplication => nsApplication -> IO ()
run nsApplication  =
    sendMsg nsApplication (mkSelector "run") retVoid []

-- | @- runModalForWindow:@
runModalForWindow :: (IsNSApplication nsApplication, IsNSWindow window) => nsApplication -> window -> IO CLong
runModalForWindow nsApplication  window =
  withObjCPtr window $ \raw_window ->
      sendMsg nsApplication (mkSelector "runModalForWindow:") retCLong [argPtr (castPtr raw_window :: Ptr ())]

-- | @- stop:@
stop :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
stop nsApplication  sender =
    sendMsg nsApplication (mkSelector "stop:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- stopModal@
stopModal :: IsNSApplication nsApplication => nsApplication -> IO ()
stopModal nsApplication  =
    sendMsg nsApplication (mkSelector "stopModal") retVoid []

-- | @- stopModalWithCode:@
stopModalWithCode :: IsNSApplication nsApplication => nsApplication -> CLong -> IO ()
stopModalWithCode nsApplication  returnCode =
    sendMsg nsApplication (mkSelector "stopModalWithCode:") retVoid [argCLong returnCode]

-- | @- abortModal@
abortModal :: IsNSApplication nsApplication => nsApplication -> IO ()
abortModal nsApplication  =
    sendMsg nsApplication (mkSelector "abortModal") retVoid []

-- | @- beginModalSessionForWindow:@
beginModalSessionForWindow :: (IsNSApplication nsApplication, IsNSWindow window) => nsApplication -> window -> IO (Ptr ())
beginModalSessionForWindow nsApplication  window =
  withObjCPtr window $ \raw_window ->
      fmap castPtr $ sendMsg nsApplication (mkSelector "beginModalSessionForWindow:") (retPtr retVoid) [argPtr (castPtr raw_window :: Ptr ())]

-- | @- runModalSession:@
runModalSession :: IsNSApplication nsApplication => nsApplication -> Ptr () -> IO CLong
runModalSession nsApplication  session =
    sendMsg nsApplication (mkSelector "runModalSession:") retCLong [argPtr session]

-- | @- endModalSession:@
endModalSession :: IsNSApplication nsApplication => nsApplication -> Ptr () -> IO ()
endModalSession nsApplication  session =
    sendMsg nsApplication (mkSelector "endModalSession:") retVoid [argPtr session]

-- | @- terminate:@
terminate :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
terminate nsApplication  sender =
    sendMsg nsApplication (mkSelector "terminate:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | Inform the user that this application needs attention - call this method only if your application is not already active.
--
-- ObjC selector: @- requestUserAttention:@
requestUserAttention :: IsNSApplication nsApplication => nsApplication -> NSRequestUserAttentionType -> IO CLong
requestUserAttention nsApplication  requestType =
    sendMsg nsApplication (mkSelector "requestUserAttention:") retCLong [argCULong (coerce requestType)]

-- | @- cancelUserAttentionRequest:@
cancelUserAttentionRequest :: IsNSApplication nsApplication => nsApplication -> CLong -> IO ()
cancelUserAttentionRequest nsApplication  request =
    sendMsg nsApplication (mkSelector "cancelUserAttentionRequest:") retVoid [argCLong request]

-- | Execute a block for each of the app's windows. Set @*stop = YES@ if desired, to halt the enumeration early.
--
-- ObjC selector: @- enumerateWindowsWithOptions:usingBlock:@
enumerateWindowsWithOptions_usingBlock :: IsNSApplication nsApplication => nsApplication -> NSWindowListOptions -> Ptr () -> IO ()
enumerateWindowsWithOptions_usingBlock nsApplication  options block =
    sendMsg nsApplication (mkSelector "enumerateWindowsWithOptions:usingBlock:") retVoid [argCLong (coerce options), argPtr (castPtr block :: Ptr ())]

-- | @- preventWindowOrdering@
preventWindowOrdering :: IsNSApplication nsApplication => nsApplication -> IO ()
preventWindowOrdering nsApplication  =
    sendMsg nsApplication (mkSelector "preventWindowOrdering") retVoid []

-- | @- setWindowsNeedUpdate:@
setWindowsNeedUpdate :: IsNSApplication nsApplication => nsApplication -> Bool -> IO ()
setWindowsNeedUpdate nsApplication  needUpdate =
    sendMsg nsApplication (mkSelector "setWindowsNeedUpdate:") retVoid [argCULong (if needUpdate then 1 else 0)]

-- | @- updateWindows@
updateWindows :: IsNSApplication nsApplication => nsApplication -> IO ()
updateWindows nsApplication  =
    sendMsg nsApplication (mkSelector "updateWindows") retVoid []

-- | Returns: The activation policy of the application.
--
-- ObjC selector: @- activationPolicy@
activationPolicy :: IsNSApplication nsApplication => nsApplication -> IO NSApplicationActivationPolicy
activationPolicy nsApplication  =
    fmap (coerce :: CLong -> NSApplicationActivationPolicy) $ sendMsg nsApplication (mkSelector "activationPolicy") retCLong []

-- | Attempts to modify the application's activation policy.  In OS X 10.9, any policy may be set; prior to 10.9, the activation policy may be changed to @NSApplicationActivationPolicyProhibited@ or @NSApplicationActivationPolicyRegular,@ but may not be changed to @NSApplicationActivationPolicyAccessory.@  This returns @YES@ if setting the activation policy is successful, and @NO@ if not.
--
-- ObjC selector: @- setActivationPolicy:@
setActivationPolicy :: IsNSApplication nsApplication => nsApplication -> NSApplicationActivationPolicy -> IO Bool
setActivationPolicy nsApplication  activationPolicy =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsApplication (mkSelector "setActivationPolicy:") retCULong [argCLong (coerce activationPolicy)]

-- | @- reportException:@
reportException :: (IsNSApplication nsApplication, IsNSException exception) => nsApplication -> exception -> IO ()
reportException nsApplication  exception =
  withObjCPtr exception $ \raw_exception ->
      sendMsg nsApplication (mkSelector "reportException:") retVoid [argPtr (castPtr raw_exception :: Ptr ())]

-- | @+ detachDrawingThread:toTarget:withObject:@
detachDrawingThread_toTarget_withObject :: Selector -> RawId -> RawId -> IO ()
detachDrawingThread_toTarget_withObject selector target argument =
  do
    cls' <- getRequiredClass "NSApplication"
    sendClassMsg cls' (mkSelector "detachDrawingThread:toTarget:withObject:") retVoid [argPtr (unSelector selector), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (castPtr (unRawId argument) :: Ptr ())]

-- | If an application delegate returns NSTerminateLater from -applicationShouldTerminate:, -replyToApplicationShouldTerminate: must be called with YES or NO once the application decides if it can terminate.
--
-- ObjC selector: @- replyToApplicationShouldTerminate:@
replyToApplicationShouldTerminate :: IsNSApplication nsApplication => nsApplication -> Bool -> IO ()
replyToApplicationShouldTerminate nsApplication  shouldTerminate =
    sendMsg nsApplication (mkSelector "replyToApplicationShouldTerminate:") retVoid [argCULong (if shouldTerminate then 1 else 0)]

-- | If an application delegate encounters an error while handling @-application:openFiles:@ or@ -application:printFiles:@, @-replyToOpenOrPrint:@ should be called with @NSApplicationDelegateReplyFailure.@  If the user cancels the operation, @NSApplicationDelegateReplyCancel@ should be used, and if the operation succeeds, @NSApplicationDelegateReplySuccess@ should be used .
--
-- ObjC selector: @- replyToOpenOrPrint:@
replyToOpenOrPrint :: IsNSApplication nsApplication => nsApplication -> NSApplicationDelegateReply -> IO ()
replyToOpenOrPrint nsApplication  reply =
    sendMsg nsApplication (mkSelector "replyToOpenOrPrint:") retVoid [argCULong (coerce reply)]

-- | Opens the character palette.
--
-- ObjC selector: @- orderFrontCharacterPalette:@
orderFrontCharacterPalette :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
orderFrontCharacterPalette nsApplication  sender =
    sendMsg nsApplication (mkSelector "orderFrontCharacterPalette:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- extendStateRestoration@
extendStateRestoration :: IsNSApplication nsApplication => nsApplication -> IO ()
extendStateRestoration nsApplication  =
    sendMsg nsApplication (mkSelector "extendStateRestoration") retVoid []

-- | @- completeStateRestoration@
completeStateRestoration :: IsNSApplication nsApplication => nsApplication -> IO ()
completeStateRestoration nsApplication  =
    sendMsg nsApplication (mkSelector "completeStateRestoration") retVoid []

-- | @- restoreWindowWithIdentifier:state:completionHandler:@
restoreWindowWithIdentifier_state_completionHandler :: (IsNSApplication nsApplication, IsNSString identifier, IsNSCoder state) => nsApplication -> identifier -> state -> Ptr () -> IO Bool
restoreWindowWithIdentifier_state_completionHandler nsApplication  identifier state completionHandler =
  withObjCPtr identifier $ \raw_identifier ->
    withObjCPtr state $ \raw_state ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsApplication (mkSelector "restoreWindowWithIdentifier:state:completionHandler:") retCULong [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_state :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- registerUserInterfaceItemSearchHandler:@
registerUserInterfaceItemSearchHandler :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
registerUserInterfaceItemSearchHandler nsApplication  handler =
    sendMsg nsApplication (mkSelector "registerUserInterfaceItemSearchHandler:") retVoid [argPtr (castPtr (unRawId handler) :: Ptr ())]

-- | @- unregisterUserInterfaceItemSearchHandler:@
unregisterUserInterfaceItemSearchHandler :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
unregisterUserInterfaceItemSearchHandler nsApplication  handler =
    sendMsg nsApplication (mkSelector "unregisterUserInterfaceItemSearchHandler:") retVoid [argPtr (castPtr (unRawId handler) :: Ptr ())]

-- | @- searchString:inUserInterfaceItemString:searchRange:foundRange:@
searchString_inUserInterfaceItemString_searchRange_foundRange :: (IsNSApplication nsApplication, IsNSString searchString, IsNSString stringToSearch) => nsApplication -> searchString -> stringToSearch -> NSRange -> Ptr NSRange -> IO Bool
searchString_inUserInterfaceItemString_searchRange_foundRange nsApplication  searchString stringToSearch searchRange foundRange =
  withObjCPtr searchString $ \raw_searchString ->
    withObjCPtr stringToSearch $ \raw_stringToSearch ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsApplication (mkSelector "searchString:inUserInterfaceItemString:searchRange:foundRange:") retCULong [argPtr (castPtr raw_searchString :: Ptr ()), argPtr (castPtr raw_stringToSearch :: Ptr ()), argNSRange searchRange, argPtr foundRange]

-- | @- runPageLayout:@
runPageLayout :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
runPageLayout nsApplication  sender =
    sendMsg nsApplication (mkSelector "runPageLayout:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- orderFrontColorPanel:@
orderFrontColorPanel :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
orderFrontColorPanel nsApplication  sender =
    sendMsg nsApplication (mkSelector "orderFrontColorPanel:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | Show or dismiss the customization palette for the currently displayed NSTouchBars. NSApplication validates this selector against whether the current NSTouchBars are customizable and, if configured on a menu item, will standardize and localize the title. If the current system does not have Touch Bar support, the menu item will be automatically hidden.
--
-- ObjC selector: @- toggleTouchBarCustomizationPalette:@
toggleTouchBarCustomizationPalette :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
toggleTouchBarCustomizationPalette nsApplication  sender =
    sendMsg nsApplication (mkSelector "toggleTouchBarCustomizationPalette:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- activateContextHelpMode:@
activateContextHelpMode :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
activateContextHelpMode nsApplication  sender =
    sendMsg nsApplication (mkSelector "activateContextHelpMode:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- showHelp:@
showHelp :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
showHelp nsApplication  sender =
    sendMsg nsApplication (mkSelector "showHelp:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @-runModalForWindow:relativeToWindow:@ was deprecated in Mac OS X 10.0. Please use @-[NSWindow beginSheet:completionHandler:]@ instead.
--
-- ObjC selector: @- runModalForWindow:relativeToWindow:@
runModalForWindow_relativeToWindow :: (IsNSApplication nsApplication, IsNSWindow window, IsNSWindow docWindow) => nsApplication -> window -> docWindow -> IO CLong
runModalForWindow_relativeToWindow nsApplication  window docWindow =
  withObjCPtr window $ \raw_window ->
    withObjCPtr docWindow $ \raw_docWindow ->
        sendMsg nsApplication (mkSelector "runModalForWindow:relativeToWindow:") retCLong [argPtr (castPtr raw_window :: Ptr ()), argPtr (castPtr raw_docWindow :: Ptr ())]

-- | @-beginModalSessionForWindow:relativeToWindow:@ was deprecated in Mac OS X 10.0. Please use @-[NSWindow beginSheet:completionHandler:]@ instead.
--
-- ObjC selector: @- beginModalSessionForWindow:relativeToWindow:@
beginModalSessionForWindow_relativeToWindow :: (IsNSApplication nsApplication, IsNSWindow window, IsNSWindow docWindow) => nsApplication -> window -> docWindow -> IO (Ptr ())
beginModalSessionForWindow_relativeToWindow nsApplication  window docWindow =
  withObjCPtr window $ \raw_window ->
    withObjCPtr docWindow $ \raw_docWindow ->
        fmap castPtr $ sendMsg nsApplication (mkSelector "beginModalSessionForWindow:relativeToWindow:") (retPtr retVoid) [argPtr (castPtr raw_window :: Ptr ()), argPtr (castPtr raw_docWindow :: Ptr ())]

-- | @-application:printFiles:@ was deprecated in Mac OS X 10.4. Implement @-application:printFiles:withSettings:showPrintPanels:@ in your application delegate instead.
--
-- ObjC selector: @- application:printFiles:@
application_printFiles :: (IsNSApplication nsApplication, IsNSApplication sender, IsNSArray filenames) => nsApplication -> sender -> filenames -> IO ()
application_printFiles nsApplication  sender filenames =
  withObjCPtr sender $ \raw_sender ->
    withObjCPtr filenames $ \raw_filenames ->
        sendMsg nsApplication (mkSelector "application:printFiles:") retVoid [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_filenames :: Ptr ())]

-- | @NSWindow@'s @-beginSheet:completionHandler:@ and @-endSheet:returnCode:@ should be used instead.  @NSApplication@'s @-beginSheet:modalForWindow:modalDelegate:didEndSelector:contextInfo:@ will continue to work as it previously did, leaking contextInfo and failing when there is already an existing sheet.
--
-- ObjC selector: @- beginSheet:modalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheet_modalForWindow_modalDelegate_didEndSelector_contextInfo :: (IsNSApplication nsApplication, IsNSWindow sheet, IsNSWindow docWindow) => nsApplication -> sheet -> docWindow -> RawId -> Selector -> Ptr () -> IO ()
beginSheet_modalForWindow_modalDelegate_didEndSelector_contextInfo nsApplication  sheet docWindow modalDelegate didEndSelector contextInfo =
  withObjCPtr sheet $ \raw_sheet ->
    withObjCPtr docWindow $ \raw_docWindow ->
        sendMsg nsApplication (mkSelector "beginSheet:modalForWindow:modalDelegate:didEndSelector:contextInfo:") retVoid [argPtr (castPtr raw_sheet :: Ptr ()), argPtr (castPtr raw_docWindow :: Ptr ()), argPtr (castPtr (unRawId modalDelegate) :: Ptr ()), argPtr (unSelector didEndSelector), argPtr contextInfo]

-- | @- endSheet:@
endSheet :: (IsNSApplication nsApplication, IsNSWindow sheet) => nsApplication -> sheet -> IO ()
endSheet nsApplication  sheet =
  withObjCPtr sheet $ \raw_sheet ->
      sendMsg nsApplication (mkSelector "endSheet:") retVoid [argPtr (castPtr raw_sheet :: Ptr ())]

-- | @- endSheet:returnCode:@
endSheet_returnCode :: (IsNSApplication nsApplication, IsNSWindow sheet) => nsApplication -> sheet -> CLong -> IO ()
endSheet_returnCode nsApplication  sheet returnCode =
  withObjCPtr sheet $ \raw_sheet ->
      sendMsg nsApplication (mkSelector "endSheet:returnCode:") retVoid [argPtr (castPtr raw_sheet :: Ptr ()), argCLong returnCode]

-- | @- makeWindowsPerform:inOrder:@
makeWindowsPerform_inOrder :: IsNSApplication nsApplication => nsApplication -> Selector -> Bool -> IO (Id NSWindow)
makeWindowsPerform_inOrder nsApplication  selector inOrder =
    sendMsg nsApplication (mkSelector "makeWindowsPerform:inOrder:") (retPtr retVoid) [argPtr (unSelector selector), argCULong (if inOrder then 1 else 0)] >>= retainedObject . castPtr

-- | @- registerForRemoteNotifications@
registerForRemoteNotifications :: IsNSApplication nsApplication => nsApplication -> IO ()
registerForRemoteNotifications nsApplication  =
    sendMsg nsApplication (mkSelector "registerForRemoteNotifications") retVoid []

-- | @- unregisterForRemoteNotifications@
unregisterForRemoteNotifications :: IsNSApplication nsApplication => nsApplication -> IO ()
unregisterForRemoteNotifications nsApplication  =
    sendMsg nsApplication (mkSelector "unregisterForRemoteNotifications") retVoid []

-- | The following are soft deprecated. Please use the @-registerForRemoteNotifications@ above and @-requestAuthorizationWithOptions:@ from @UserNotifications.framework@.
--
-- ObjC selector: @- registerForRemoteNotificationTypes:@
registerForRemoteNotificationTypes :: IsNSApplication nsApplication => nsApplication -> NSRemoteNotificationType -> IO ()
registerForRemoteNotificationTypes nsApplication  types =
    sendMsg nsApplication (mkSelector "registerForRemoteNotificationTypes:") retVoid [argCULong (coerce types)]

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
disableRelaunchOnLogin nsApplication  =
    sendMsg nsApplication (mkSelector "disableRelaunchOnLogin") retVoid []

-- | @- enableRelaunchOnLogin@
enableRelaunchOnLogin :: IsNSApplication nsApplication => nsApplication -> IO ()
enableRelaunchOnLogin nsApplication  =
    sendMsg nsApplication (mkSelector "enableRelaunchOnLogin") retVoid []

-- | @- orderFrontStandardAboutPanel:@
orderFrontStandardAboutPanel :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
orderFrontStandardAboutPanel nsApplication  sender =
    sendMsg nsApplication (mkSelector "orderFrontStandardAboutPanel:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- orderFrontStandardAboutPanelWithOptions:@
orderFrontStandardAboutPanelWithOptions :: (IsNSApplication nsApplication, IsNSDictionary optionsDictionary) => nsApplication -> optionsDictionary -> IO ()
orderFrontStandardAboutPanelWithOptions nsApplication  optionsDictionary =
  withObjCPtr optionsDictionary $ \raw_optionsDictionary ->
      sendMsg nsApplication (mkSelector "orderFrontStandardAboutPanelWithOptions:") retVoid [argPtr (castPtr raw_optionsDictionary :: Ptr ())]

-- | @- registerServicesMenuSendTypes:returnTypes:@
registerServicesMenuSendTypes_returnTypes :: (IsNSApplication nsApplication, IsNSArray sendTypes, IsNSArray returnTypes) => nsApplication -> sendTypes -> returnTypes -> IO ()
registerServicesMenuSendTypes_returnTypes nsApplication  sendTypes returnTypes =
  withObjCPtr sendTypes $ \raw_sendTypes ->
    withObjCPtr returnTypes $ \raw_returnTypes ->
        sendMsg nsApplication (mkSelector "registerServicesMenuSendTypes:returnTypes:") retVoid [argPtr (castPtr raw_sendTypes :: Ptr ()), argPtr (castPtr raw_returnTypes :: Ptr ())]

-- | @- arrangeInFront:@
arrangeInFront :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
arrangeInFront nsApplication  sender =
    sendMsg nsApplication (mkSelector "arrangeInFront:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- removeWindowsItem:@
removeWindowsItem :: (IsNSApplication nsApplication, IsNSWindow win) => nsApplication -> win -> IO ()
removeWindowsItem nsApplication  win =
  withObjCPtr win $ \raw_win ->
      sendMsg nsApplication (mkSelector "removeWindowsItem:") retVoid [argPtr (castPtr raw_win :: Ptr ())]

-- | @- addWindowsItem:title:filename:@
addWindowsItem_title_filename :: (IsNSApplication nsApplication, IsNSWindow win, IsNSString string) => nsApplication -> win -> string -> Bool -> IO ()
addWindowsItem_title_filename nsApplication  win string isFilename =
  withObjCPtr win $ \raw_win ->
    withObjCPtr string $ \raw_string ->
        sendMsg nsApplication (mkSelector "addWindowsItem:title:filename:") retVoid [argPtr (castPtr raw_win :: Ptr ()), argPtr (castPtr raw_string :: Ptr ()), argCULong (if isFilename then 1 else 0)]

-- | @- changeWindowsItem:title:filename:@
changeWindowsItem_title_filename :: (IsNSApplication nsApplication, IsNSWindow win, IsNSString string) => nsApplication -> win -> string -> Bool -> IO ()
changeWindowsItem_title_filename nsApplication  win string isFilename =
  withObjCPtr win $ \raw_win ->
    withObjCPtr string $ \raw_string ->
        sendMsg nsApplication (mkSelector "changeWindowsItem:title:filename:") retVoid [argPtr (castPtr raw_win :: Ptr ()), argPtr (castPtr raw_string :: Ptr ()), argCULong (if isFilename then 1 else 0)]

-- | @- updateWindowsItem:@
updateWindowsItem :: (IsNSApplication nsApplication, IsNSWindow win) => nsApplication -> win -> IO ()
updateWindowsItem nsApplication  win =
  withObjCPtr win $ \raw_win ->
      sendMsg nsApplication (mkSelector "updateWindowsItem:") retVoid [argPtr (castPtr raw_win :: Ptr ())]

-- | @- miniaturizeAll:@
miniaturizeAll :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
miniaturizeAll nsApplication  sender =
    sendMsg nsApplication (mkSelector "miniaturizeAll:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- sendAction:to:from:@
sendAction_to_from :: IsNSApplication nsApplication => nsApplication -> Selector -> RawId -> RawId -> IO Bool
sendAction_to_from nsApplication  action target sender =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsApplication (mkSelector "sendAction:to:from:") retCULong [argPtr (unSelector action), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- targetForAction:@
targetForAction :: IsNSApplication nsApplication => nsApplication -> Selector -> IO RawId
targetForAction nsApplication  action =
    fmap (RawId . castPtr) $ sendMsg nsApplication (mkSelector "targetForAction:") (retPtr retVoid) [argPtr (unSelector action)]

-- | @- targetForAction:to:from:@
targetForAction_to_from :: IsNSApplication nsApplication => nsApplication -> Selector -> RawId -> RawId -> IO RawId
targetForAction_to_from nsApplication  action target sender =
    fmap (RawId . castPtr) $ sendMsg nsApplication (mkSelector "targetForAction:to:from:") (retPtr retVoid) [argPtr (unSelector action), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- tryToPerform:with:@
tryToPerform_with :: IsNSApplication nsApplication => nsApplication -> Selector -> RawId -> IO Bool
tryToPerform_with nsApplication  action object =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsApplication (mkSelector "tryToPerform:with:") retCULong [argPtr (unSelector action), argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- validRequestorForSendType:returnType:@
validRequestorForSendType_returnType :: (IsNSApplication nsApplication, IsNSString sendType, IsNSString returnType) => nsApplication -> sendType -> returnType -> IO RawId
validRequestorForSendType_returnType nsApplication  sendType returnType =
  withObjCPtr sendType $ \raw_sendType ->
    withObjCPtr returnType $ \raw_returnType ->
        fmap (RawId . castPtr) $ sendMsg nsApplication (mkSelector "validRequestorForSendType:returnType:") (retPtr retVoid) [argPtr (castPtr raw_sendType :: Ptr ()), argPtr (castPtr raw_returnType :: Ptr ())]

-- | @- sendEvent:@
sendEvent :: (IsNSApplication nsApplication, IsNSEvent event) => nsApplication -> event -> IO ()
sendEvent nsApplication  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsApplication (mkSelector "sendEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- postEvent:atStart:@
postEvent_atStart :: (IsNSApplication nsApplication, IsNSEvent event) => nsApplication -> event -> Bool -> IO ()
postEvent_atStart nsApplication  event atStart =
  withObjCPtr event $ \raw_event ->
      sendMsg nsApplication (mkSelector "postEvent:atStart:") retVoid [argPtr (castPtr raw_event :: Ptr ()), argCULong (if atStart then 1 else 0)]

-- | @- nextEventMatchingMask:untilDate:inMode:dequeue:@
nextEventMatchingMask_untilDate_inMode_dequeue :: (IsNSApplication nsApplication, IsNSDate expiration, IsNSString mode) => nsApplication -> NSEventMask -> expiration -> mode -> Bool -> IO (Id NSEvent)
nextEventMatchingMask_untilDate_inMode_dequeue nsApplication  mask expiration mode deqFlag =
  withObjCPtr expiration $ \raw_expiration ->
    withObjCPtr mode $ \raw_mode ->
        sendMsg nsApplication (mkSelector "nextEventMatchingMask:untilDate:inMode:dequeue:") (retPtr retVoid) [argCULong (coerce mask), argPtr (castPtr raw_expiration :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ()), argCULong (if deqFlag then 1 else 0)] >>= retainedObject . castPtr

-- | @- discardEventsMatchingMask:beforeEvent:@
discardEventsMatchingMask_beforeEvent :: (IsNSApplication nsApplication, IsNSEvent lastEvent) => nsApplication -> NSEventMask -> lastEvent -> IO ()
discardEventsMatchingMask_beforeEvent nsApplication  mask lastEvent =
  withObjCPtr lastEvent $ \raw_lastEvent ->
      sendMsg nsApplication (mkSelector "discardEventsMatchingMask:beforeEvent:") retVoid [argCULong (coerce mask), argPtr (castPtr raw_lastEvent :: Ptr ())]

-- | @+ sharedApplication@
sharedApplication :: IO (Id NSApplication)
sharedApplication  =
  do
    cls' <- getRequiredClass "NSApplication"
    sendClassMsg cls' (mkSelector "sharedApplication") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- delegate@
delegate :: IsNSApplication nsApplication => nsApplication -> IO RawId
delegate nsApplication  =
    fmap (RawId . castPtr) $ sendMsg nsApplication (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
setDelegate nsApplication  value =
    sendMsg nsApplication (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- mainWindow@
mainWindow :: IsNSApplication nsApplication => nsApplication -> IO (Id NSWindow)
mainWindow nsApplication  =
    sendMsg nsApplication (mkSelector "mainWindow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- keyWindow@
keyWindow :: IsNSApplication nsApplication => nsApplication -> IO (Id NSWindow)
keyWindow nsApplication  =
    sendMsg nsApplication (mkSelector "keyWindow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- active@
active :: IsNSApplication nsApplication => nsApplication -> IO Bool
active nsApplication  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsApplication (mkSelector "active") retCULong []

-- | @- hidden@
hidden :: IsNSApplication nsApplication => nsApplication -> IO Bool
hidden nsApplication  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsApplication (mkSelector "hidden") retCULong []

-- | @- running@
running :: IsNSApplication nsApplication => nsApplication -> IO Bool
running nsApplication  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsApplication (mkSelector "running") retCULong []

-- | A boolean value indicating whether your application should suppress HDR content based on established policy. Built-in AppKit components such as NSImageView will automatically behave correctly with HDR content. You should use this value in conjunction with notifications (@NSApplicationShouldBeginSuppressingHighDynamicRangeContentNotification@ and @NSApplicationShouldEndSuppressingHighDynamicRangeContentNotification@) to suppress HDR content in your application when signaled to do so.
--
-- ObjC selector: @- applicationShouldSuppressHighDynamicRangeContent@
applicationShouldSuppressHighDynamicRangeContent :: IsNSApplication nsApplication => nsApplication -> IO Bool
applicationShouldSuppressHighDynamicRangeContent nsApplication  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsApplication (mkSelector "applicationShouldSuppressHighDynamicRangeContent") retCULong []

-- | @- modalWindow@
modalWindow :: IsNSApplication nsApplication => nsApplication -> IO (Id NSWindow)
modalWindow nsApplication  =
    sendMsg nsApplication (mkSelector "modalWindow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- windows@
windows :: IsNSApplication nsApplication => nsApplication -> IO (Id NSArray)
windows nsApplication  =
    sendMsg nsApplication (mkSelector "windows") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- mainMenu@
mainMenu :: IsNSApplication nsApplication => nsApplication -> IO (Id NSMenu)
mainMenu nsApplication  =
    sendMsg nsApplication (mkSelector "mainMenu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMainMenu:@
setMainMenu :: (IsNSApplication nsApplication, IsNSMenu value) => nsApplication -> value -> IO ()
setMainMenu nsApplication  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsApplication (mkSelector "setMainMenu:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Set or get the Help menu for the app.  If a non-nil menu is set as the Help menu, Spotlight for Help will be installed in it; otherwise AppKit will install Spotlight for Help into a menu of its choosing (and that menu is not returned from @-helpMenu@).  If you wish to completely suppress Spotlight for Help, you can set a menu that does not appear in the menu bar.  @NSApplication@ retains its Help menu and releases it when a different menu is set.
--
-- ObjC selector: @- helpMenu@
helpMenu :: IsNSApplication nsApplication => nsApplication -> IO (Id NSMenu)
helpMenu nsApplication  =
    sendMsg nsApplication (mkSelector "helpMenu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Set or get the Help menu for the app.  If a non-nil menu is set as the Help menu, Spotlight for Help will be installed in it; otherwise AppKit will install Spotlight for Help into a menu of its choosing (and that menu is not returned from @-helpMenu@).  If you wish to completely suppress Spotlight for Help, you can set a menu that does not appear in the menu bar.  @NSApplication@ retains its Help menu and releases it when a different menu is set.
--
-- ObjC selector: @- setHelpMenu:@
setHelpMenu :: (IsNSApplication nsApplication, IsNSMenu value) => nsApplication -> value -> IO ()
setHelpMenu nsApplication  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsApplication (mkSelector "setHelpMenu:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- applicationIconImage@
applicationIconImage :: IsNSApplication nsApplication => nsApplication -> IO (Id NSImage)
applicationIconImage nsApplication  =
    sendMsg nsApplication (mkSelector "applicationIconImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setApplicationIconImage:@
setApplicationIconImage :: (IsNSApplication nsApplication, IsNSImage value) => nsApplication -> value -> IO ()
setApplicationIconImage nsApplication  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsApplication (mkSelector "setApplicationIconImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dockTile@
dockTile :: IsNSApplication nsApplication => nsApplication -> IO (Id NSDockTile)
dockTile nsApplication  =
    sendMsg nsApplication (mkSelector "dockTile") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Gets or sets the @presentationOptions@ that should be in effect for the system when this application is the active application.  Only certain combinations of @NSApplicationPresentationOptions@ flags are allowed, as detailed in the AppKit Release Notes and the reference documentation for @-setPresentationOptions:@.  When given an invalid combination of option flags, @-setPresentationOptions:@ raises an exception.
--
-- ObjC selector: @- presentationOptions@
presentationOptions :: IsNSApplication nsApplication => nsApplication -> IO NSApplicationPresentationOptions
presentationOptions nsApplication  =
    fmap (coerce :: CULong -> NSApplicationPresentationOptions) $ sendMsg nsApplication (mkSelector "presentationOptions") retCULong []

-- | Gets or sets the @presentationOptions@ that should be in effect for the system when this application is the active application.  Only certain combinations of @NSApplicationPresentationOptions@ flags are allowed, as detailed in the AppKit Release Notes and the reference documentation for @-setPresentationOptions:@.  When given an invalid combination of option flags, @-setPresentationOptions:@ raises an exception.
--
-- ObjC selector: @- setPresentationOptions:@
setPresentationOptions :: IsNSApplication nsApplication => nsApplication -> NSApplicationPresentationOptions -> IO ()
setPresentationOptions nsApplication  value =
    sendMsg nsApplication (mkSelector "setPresentationOptions:") retVoid [argCULong (coerce value)]

-- | Returns: The set of application presentation options that are currently in effect for the system. These are the presentation options that have been put into effect by the currently active application.
--
-- ObjC selector: @- currentSystemPresentationOptions@
currentSystemPresentationOptions :: IsNSApplication nsApplication => nsApplication -> IO NSApplicationPresentationOptions
currentSystemPresentationOptions nsApplication  =
    fmap (coerce :: CULong -> NSApplicationPresentationOptions) $ sendMsg nsApplication (mkSelector "currentSystemPresentationOptions") retCULong []

-- | @- occlusionState@
occlusionState :: IsNSApplication nsApplication => nsApplication -> IO NSApplicationOcclusionState
occlusionState nsApplication  =
    fmap (coerce :: CULong -> NSApplicationOcclusionState) $ sendMsg nsApplication (mkSelector "occlusionState") retCULong []

-- | @- protectedDataAvailable@
protectedDataAvailable :: IsNSApplication nsApplication => nsApplication -> IO Bool
protectedDataAvailable nsApplication  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsApplication (mkSelector "protectedDataAvailable") retCULong []

-- | @- orderedDocuments@
orderedDocuments :: IsNSApplication nsApplication => nsApplication -> IO (Id NSArray)
orderedDocuments nsApplication  =
    sendMsg nsApplication (mkSelector "orderedDocuments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- orderedWindows@
orderedWindows :: IsNSApplication nsApplication => nsApplication -> IO (Id NSArray)
orderedWindows nsApplication  =
    sendMsg nsApplication (mkSelector "orderedWindows") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether or not a menu item to customize the NSTouchBar can be automatically added to the main menu. It will only actually be added when Touch Bar hardware or simulator is present. Defaults to NO. Setting this property to YES is the recommended way to add the customization menu item. But if non-standard placement of the menu item is needed, creating a menu item with an action of @toggleTouchBarCustomizationPalette:@ can be used instead.
--
-- ObjC selector: @- automaticCustomizeTouchBarMenuItemEnabled@
automaticCustomizeTouchBarMenuItemEnabled :: IsNSApplication nsApplication => nsApplication -> IO Bool
automaticCustomizeTouchBarMenuItemEnabled nsApplication  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsApplication (mkSelector "automaticCustomizeTouchBarMenuItemEnabled") retCULong []

-- | Whether or not a menu item to customize the NSTouchBar can be automatically added to the main menu. It will only actually be added when Touch Bar hardware or simulator is present. Defaults to NO. Setting this property to YES is the recommended way to add the customization menu item. But if non-standard placement of the menu item is needed, creating a menu item with an action of @toggleTouchBarCustomizationPalette:@ can be used instead.
--
-- ObjC selector: @- setAutomaticCustomizeTouchBarMenuItemEnabled:@
setAutomaticCustomizeTouchBarMenuItemEnabled :: IsNSApplication nsApplication => nsApplication -> Bool -> IO ()
setAutomaticCustomizeTouchBarMenuItemEnabled nsApplication  value =
    sendMsg nsApplication (mkSelector "setAutomaticCustomizeTouchBarMenuItemEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | This method is deprecated as of macOS 10.12. Beginning in OS X 10.11 it would always return nil. Prior to this it would return an undefined graphics context that was not generally suitable for drawing.
--
-- ObjC selector: @- context@
context :: IsNSApplication nsApplication => nsApplication -> IO (Id NSGraphicsContext)
context nsApplication  =
    sendMsg nsApplication (mkSelector "context") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns: @YES@ if the application is currently registered for remote notifications, taking into account any systemwide settings; doesn't relate to connectivity.
--
-- ObjC selector: @- registeredForRemoteNotifications@
registeredForRemoteNotifications :: IsNSApplication nsApplication => nsApplication -> IO Bool
registeredForRemoteNotifications nsApplication  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsApplication (mkSelector "registeredForRemoteNotifications") retCULong []

-- | @- enabledRemoteNotificationTypes@
enabledRemoteNotificationTypes :: IsNSApplication nsApplication => nsApplication -> IO NSRemoteNotificationType
enabledRemoteNotificationTypes nsApplication  =
    fmap (coerce :: CULong -> NSRemoteNotificationType) $ sendMsg nsApplication (mkSelector "enabledRemoteNotificationTypes") retCULong []

-- | @- userInterfaceLayoutDirection@
userInterfaceLayoutDirection :: IsNSApplication nsApplication => nsApplication -> IO NSUserInterfaceLayoutDirection
userInterfaceLayoutDirection nsApplication  =
    fmap (coerce :: CLong -> NSUserInterfaceLayoutDirection) $ sendMsg nsApplication (mkSelector "userInterfaceLayoutDirection") retCLong []

-- | @- servicesProvider@
servicesProvider :: IsNSApplication nsApplication => nsApplication -> IO RawId
servicesProvider nsApplication  =
    fmap (RawId . castPtr) $ sendMsg nsApplication (mkSelector "servicesProvider") (retPtr retVoid) []

-- | @- setServicesProvider:@
setServicesProvider :: IsNSApplication nsApplication => nsApplication -> RawId -> IO ()
setServicesProvider nsApplication  value =
    sendMsg nsApplication (mkSelector "setServicesProvider:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- servicesMenu@
servicesMenu :: IsNSApplication nsApplication => nsApplication -> IO (Id NSMenu)
servicesMenu nsApplication  =
    sendMsg nsApplication (mkSelector "servicesMenu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setServicesMenu:@
setServicesMenu :: (IsNSApplication nsApplication, IsNSMenu value) => nsApplication -> value -> IO ()
setServicesMenu nsApplication  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsApplication (mkSelector "setServicesMenu:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A Boolean value indicating whether keyboard navigation is enabled in System Settings > Keyboard. - Note: The value of this property is @YES@ if keyboard navigation is enabled or @NO@ if it’s not. You might use this value to implement your own key loop or to implement in-control tabbing behavior similar to @NSTableView@. Because of the nature of the preference storage, you won’t be notified of changes to this property if you attempt to observe it through key-value observing; however, accessing this property is fairly inexpensive, so you can access it directly rather than caching it. - Note: This property’s value isn’t necessarily reflective of the separate accessibility setting named “Full Keyboard Access” in System Settings > Accessibility > Keyboard.
--
-- ObjC selector: @- fullKeyboardAccessEnabled@
fullKeyboardAccessEnabled :: IsNSApplication nsApplication => nsApplication -> IO Bool
fullKeyboardAccessEnabled nsApplication  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsApplication (mkSelector "fullKeyboardAccessEnabled") retCULong []

-- | @- windowsMenu@
windowsMenu :: IsNSApplication nsApplication => nsApplication -> IO (Id NSMenu)
windowsMenu nsApplication  =
    sendMsg nsApplication (mkSelector "windowsMenu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWindowsMenu:@
setWindowsMenu :: (IsNSApplication nsApplication, IsNSMenu value) => nsApplication -> value -> IO ()
setWindowsMenu nsApplication  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsApplication (mkSelector "setWindowsMenu:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currentEvent@
currentEvent :: IsNSApplication nsApplication => nsApplication -> IO (Id NSEvent)
currentEvent nsApplication  =
    sendMsg nsApplication (mkSelector "currentEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- appearance@
appearance :: IsNSApplication nsApplication => nsApplication -> IO (Id NSAppearance)
appearance nsApplication  =
    sendMsg nsApplication (mkSelector "appearance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAppearance:@
setAppearance :: (IsNSApplication nsApplication, IsNSAppearance value) => nsApplication -> value -> IO ()
setAppearance nsApplication  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsApplication (mkSelector "setAppearance:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- effectiveAppearance@
effectiveAppearance :: IsNSApplication nsApplication => nsApplication -> IO (Id NSAppearance)
effectiveAppearance nsApplication  =
    sendMsg nsApplication (mkSelector "effectiveAppearance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hide:@
hideSelector :: Selector
hideSelector = mkSelector "hide:"

-- | @Selector@ for @unhide:@
unhideSelector :: Selector
unhideSelector = mkSelector "unhide:"

-- | @Selector@ for @unhideWithoutActivation@
unhideWithoutActivationSelector :: Selector
unhideWithoutActivationSelector = mkSelector "unhideWithoutActivation"

-- | @Selector@ for @windowWithWindowNumber:@
windowWithWindowNumberSelector :: Selector
windowWithWindowNumberSelector = mkSelector "windowWithWindowNumber:"

-- | @Selector@ for @deactivate@
deactivateSelector :: Selector
deactivateSelector = mkSelector "deactivate"

-- | @Selector@ for @activateIgnoringOtherApps:@
activateIgnoringOtherAppsSelector :: Selector
activateIgnoringOtherAppsSelector = mkSelector "activateIgnoringOtherApps:"

-- | @Selector@ for @activate@
activateSelector :: Selector
activateSelector = mkSelector "activate"

-- | @Selector@ for @yieldActivationToApplication:@
yieldActivationToApplicationSelector :: Selector
yieldActivationToApplicationSelector = mkSelector "yieldActivationToApplication:"

-- | @Selector@ for @yieldActivationToApplicationWithBundleIdentifier:@
yieldActivationToApplicationWithBundleIdentifierSelector :: Selector
yieldActivationToApplicationWithBundleIdentifierSelector = mkSelector "yieldActivationToApplicationWithBundleIdentifier:"

-- | @Selector@ for @hideOtherApplications:@
hideOtherApplicationsSelector :: Selector
hideOtherApplicationsSelector = mkSelector "hideOtherApplications:"

-- | @Selector@ for @unhideAllApplications:@
unhideAllApplicationsSelector :: Selector
unhideAllApplicationsSelector = mkSelector "unhideAllApplications:"

-- | @Selector@ for @finishLaunching@
finishLaunchingSelector :: Selector
finishLaunchingSelector = mkSelector "finishLaunching"

-- | @Selector@ for @run@
runSelector :: Selector
runSelector = mkSelector "run"

-- | @Selector@ for @runModalForWindow:@
runModalForWindowSelector :: Selector
runModalForWindowSelector = mkSelector "runModalForWindow:"

-- | @Selector@ for @stop:@
stopSelector :: Selector
stopSelector = mkSelector "stop:"

-- | @Selector@ for @stopModal@
stopModalSelector :: Selector
stopModalSelector = mkSelector "stopModal"

-- | @Selector@ for @stopModalWithCode:@
stopModalWithCodeSelector :: Selector
stopModalWithCodeSelector = mkSelector "stopModalWithCode:"

-- | @Selector@ for @abortModal@
abortModalSelector :: Selector
abortModalSelector = mkSelector "abortModal"

-- | @Selector@ for @beginModalSessionForWindow:@
beginModalSessionForWindowSelector :: Selector
beginModalSessionForWindowSelector = mkSelector "beginModalSessionForWindow:"

-- | @Selector@ for @runModalSession:@
runModalSessionSelector :: Selector
runModalSessionSelector = mkSelector "runModalSession:"

-- | @Selector@ for @endModalSession:@
endModalSessionSelector :: Selector
endModalSessionSelector = mkSelector "endModalSession:"

-- | @Selector@ for @terminate:@
terminateSelector :: Selector
terminateSelector = mkSelector "terminate:"

-- | @Selector@ for @requestUserAttention:@
requestUserAttentionSelector :: Selector
requestUserAttentionSelector = mkSelector "requestUserAttention:"

-- | @Selector@ for @cancelUserAttentionRequest:@
cancelUserAttentionRequestSelector :: Selector
cancelUserAttentionRequestSelector = mkSelector "cancelUserAttentionRequest:"

-- | @Selector@ for @enumerateWindowsWithOptions:usingBlock:@
enumerateWindowsWithOptions_usingBlockSelector :: Selector
enumerateWindowsWithOptions_usingBlockSelector = mkSelector "enumerateWindowsWithOptions:usingBlock:"

-- | @Selector@ for @preventWindowOrdering@
preventWindowOrderingSelector :: Selector
preventWindowOrderingSelector = mkSelector "preventWindowOrdering"

-- | @Selector@ for @setWindowsNeedUpdate:@
setWindowsNeedUpdateSelector :: Selector
setWindowsNeedUpdateSelector = mkSelector "setWindowsNeedUpdate:"

-- | @Selector@ for @updateWindows@
updateWindowsSelector :: Selector
updateWindowsSelector = mkSelector "updateWindows"

-- | @Selector@ for @activationPolicy@
activationPolicySelector :: Selector
activationPolicySelector = mkSelector "activationPolicy"

-- | @Selector@ for @setActivationPolicy:@
setActivationPolicySelector :: Selector
setActivationPolicySelector = mkSelector "setActivationPolicy:"

-- | @Selector@ for @reportException:@
reportExceptionSelector :: Selector
reportExceptionSelector = mkSelector "reportException:"

-- | @Selector@ for @detachDrawingThread:toTarget:withObject:@
detachDrawingThread_toTarget_withObjectSelector :: Selector
detachDrawingThread_toTarget_withObjectSelector = mkSelector "detachDrawingThread:toTarget:withObject:"

-- | @Selector@ for @replyToApplicationShouldTerminate:@
replyToApplicationShouldTerminateSelector :: Selector
replyToApplicationShouldTerminateSelector = mkSelector "replyToApplicationShouldTerminate:"

-- | @Selector@ for @replyToOpenOrPrint:@
replyToOpenOrPrintSelector :: Selector
replyToOpenOrPrintSelector = mkSelector "replyToOpenOrPrint:"

-- | @Selector@ for @orderFrontCharacterPalette:@
orderFrontCharacterPaletteSelector :: Selector
orderFrontCharacterPaletteSelector = mkSelector "orderFrontCharacterPalette:"

-- | @Selector@ for @extendStateRestoration@
extendStateRestorationSelector :: Selector
extendStateRestorationSelector = mkSelector "extendStateRestoration"

-- | @Selector@ for @completeStateRestoration@
completeStateRestorationSelector :: Selector
completeStateRestorationSelector = mkSelector "completeStateRestoration"

-- | @Selector@ for @restoreWindowWithIdentifier:state:completionHandler:@
restoreWindowWithIdentifier_state_completionHandlerSelector :: Selector
restoreWindowWithIdentifier_state_completionHandlerSelector = mkSelector "restoreWindowWithIdentifier:state:completionHandler:"

-- | @Selector@ for @registerUserInterfaceItemSearchHandler:@
registerUserInterfaceItemSearchHandlerSelector :: Selector
registerUserInterfaceItemSearchHandlerSelector = mkSelector "registerUserInterfaceItemSearchHandler:"

-- | @Selector@ for @unregisterUserInterfaceItemSearchHandler:@
unregisterUserInterfaceItemSearchHandlerSelector :: Selector
unregisterUserInterfaceItemSearchHandlerSelector = mkSelector "unregisterUserInterfaceItemSearchHandler:"

-- | @Selector@ for @searchString:inUserInterfaceItemString:searchRange:foundRange:@
searchString_inUserInterfaceItemString_searchRange_foundRangeSelector :: Selector
searchString_inUserInterfaceItemString_searchRange_foundRangeSelector = mkSelector "searchString:inUserInterfaceItemString:searchRange:foundRange:"

-- | @Selector@ for @runPageLayout:@
runPageLayoutSelector :: Selector
runPageLayoutSelector = mkSelector "runPageLayout:"

-- | @Selector@ for @orderFrontColorPanel:@
orderFrontColorPanelSelector :: Selector
orderFrontColorPanelSelector = mkSelector "orderFrontColorPanel:"

-- | @Selector@ for @toggleTouchBarCustomizationPalette:@
toggleTouchBarCustomizationPaletteSelector :: Selector
toggleTouchBarCustomizationPaletteSelector = mkSelector "toggleTouchBarCustomizationPalette:"

-- | @Selector@ for @activateContextHelpMode:@
activateContextHelpModeSelector :: Selector
activateContextHelpModeSelector = mkSelector "activateContextHelpMode:"

-- | @Selector@ for @showHelp:@
showHelpSelector :: Selector
showHelpSelector = mkSelector "showHelp:"

-- | @Selector@ for @runModalForWindow:relativeToWindow:@
runModalForWindow_relativeToWindowSelector :: Selector
runModalForWindow_relativeToWindowSelector = mkSelector "runModalForWindow:relativeToWindow:"

-- | @Selector@ for @beginModalSessionForWindow:relativeToWindow:@
beginModalSessionForWindow_relativeToWindowSelector :: Selector
beginModalSessionForWindow_relativeToWindowSelector = mkSelector "beginModalSessionForWindow:relativeToWindow:"

-- | @Selector@ for @application:printFiles:@
application_printFilesSelector :: Selector
application_printFilesSelector = mkSelector "application:printFiles:"

-- | @Selector@ for @beginSheet:modalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheet_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector :: Selector
beginSheet_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector = mkSelector "beginSheet:modalForWindow:modalDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @endSheet:@
endSheetSelector :: Selector
endSheetSelector = mkSelector "endSheet:"

-- | @Selector@ for @endSheet:returnCode:@
endSheet_returnCodeSelector :: Selector
endSheet_returnCodeSelector = mkSelector "endSheet:returnCode:"

-- | @Selector@ for @makeWindowsPerform:inOrder:@
makeWindowsPerform_inOrderSelector :: Selector
makeWindowsPerform_inOrderSelector = mkSelector "makeWindowsPerform:inOrder:"

-- | @Selector@ for @registerForRemoteNotifications@
registerForRemoteNotificationsSelector :: Selector
registerForRemoteNotificationsSelector = mkSelector "registerForRemoteNotifications"

-- | @Selector@ for @unregisterForRemoteNotifications@
unregisterForRemoteNotificationsSelector :: Selector
unregisterForRemoteNotificationsSelector = mkSelector "unregisterForRemoteNotifications"

-- | @Selector@ for @registerForRemoteNotificationTypes:@
registerForRemoteNotificationTypesSelector :: Selector
registerForRemoteNotificationTypesSelector = mkSelector "registerForRemoteNotificationTypes:"

-- | @Selector@ for @disableRelaunchOnLogin@
disableRelaunchOnLoginSelector :: Selector
disableRelaunchOnLoginSelector = mkSelector "disableRelaunchOnLogin"

-- | @Selector@ for @enableRelaunchOnLogin@
enableRelaunchOnLoginSelector :: Selector
enableRelaunchOnLoginSelector = mkSelector "enableRelaunchOnLogin"

-- | @Selector@ for @orderFrontStandardAboutPanel:@
orderFrontStandardAboutPanelSelector :: Selector
orderFrontStandardAboutPanelSelector = mkSelector "orderFrontStandardAboutPanel:"

-- | @Selector@ for @orderFrontStandardAboutPanelWithOptions:@
orderFrontStandardAboutPanelWithOptionsSelector :: Selector
orderFrontStandardAboutPanelWithOptionsSelector = mkSelector "orderFrontStandardAboutPanelWithOptions:"

-- | @Selector@ for @registerServicesMenuSendTypes:returnTypes:@
registerServicesMenuSendTypes_returnTypesSelector :: Selector
registerServicesMenuSendTypes_returnTypesSelector = mkSelector "registerServicesMenuSendTypes:returnTypes:"

-- | @Selector@ for @arrangeInFront:@
arrangeInFrontSelector :: Selector
arrangeInFrontSelector = mkSelector "arrangeInFront:"

-- | @Selector@ for @removeWindowsItem:@
removeWindowsItemSelector :: Selector
removeWindowsItemSelector = mkSelector "removeWindowsItem:"

-- | @Selector@ for @addWindowsItem:title:filename:@
addWindowsItem_title_filenameSelector :: Selector
addWindowsItem_title_filenameSelector = mkSelector "addWindowsItem:title:filename:"

-- | @Selector@ for @changeWindowsItem:title:filename:@
changeWindowsItem_title_filenameSelector :: Selector
changeWindowsItem_title_filenameSelector = mkSelector "changeWindowsItem:title:filename:"

-- | @Selector@ for @updateWindowsItem:@
updateWindowsItemSelector :: Selector
updateWindowsItemSelector = mkSelector "updateWindowsItem:"

-- | @Selector@ for @miniaturizeAll:@
miniaturizeAllSelector :: Selector
miniaturizeAllSelector = mkSelector "miniaturizeAll:"

-- | @Selector@ for @sendAction:to:from:@
sendAction_to_fromSelector :: Selector
sendAction_to_fromSelector = mkSelector "sendAction:to:from:"

-- | @Selector@ for @targetForAction:@
targetForActionSelector :: Selector
targetForActionSelector = mkSelector "targetForAction:"

-- | @Selector@ for @targetForAction:to:from:@
targetForAction_to_fromSelector :: Selector
targetForAction_to_fromSelector = mkSelector "targetForAction:to:from:"

-- | @Selector@ for @tryToPerform:with:@
tryToPerform_withSelector :: Selector
tryToPerform_withSelector = mkSelector "tryToPerform:with:"

-- | @Selector@ for @validRequestorForSendType:returnType:@
validRequestorForSendType_returnTypeSelector :: Selector
validRequestorForSendType_returnTypeSelector = mkSelector "validRequestorForSendType:returnType:"

-- | @Selector@ for @sendEvent:@
sendEventSelector :: Selector
sendEventSelector = mkSelector "sendEvent:"

-- | @Selector@ for @postEvent:atStart:@
postEvent_atStartSelector :: Selector
postEvent_atStartSelector = mkSelector "postEvent:atStart:"

-- | @Selector@ for @nextEventMatchingMask:untilDate:inMode:dequeue:@
nextEventMatchingMask_untilDate_inMode_dequeueSelector :: Selector
nextEventMatchingMask_untilDate_inMode_dequeueSelector = mkSelector "nextEventMatchingMask:untilDate:inMode:dequeue:"

-- | @Selector@ for @discardEventsMatchingMask:beforeEvent:@
discardEventsMatchingMask_beforeEventSelector :: Selector
discardEventsMatchingMask_beforeEventSelector = mkSelector "discardEventsMatchingMask:beforeEvent:"

-- | @Selector@ for @sharedApplication@
sharedApplicationSelector :: Selector
sharedApplicationSelector = mkSelector "sharedApplication"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @mainWindow@
mainWindowSelector :: Selector
mainWindowSelector = mkSelector "mainWindow"

-- | @Selector@ for @keyWindow@
keyWindowSelector :: Selector
keyWindowSelector = mkSelector "keyWindow"

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @running@
runningSelector :: Selector
runningSelector = mkSelector "running"

-- | @Selector@ for @applicationShouldSuppressHighDynamicRangeContent@
applicationShouldSuppressHighDynamicRangeContentSelector :: Selector
applicationShouldSuppressHighDynamicRangeContentSelector = mkSelector "applicationShouldSuppressHighDynamicRangeContent"

-- | @Selector@ for @modalWindow@
modalWindowSelector :: Selector
modalWindowSelector = mkSelector "modalWindow"

-- | @Selector@ for @windows@
windowsSelector :: Selector
windowsSelector = mkSelector "windows"

-- | @Selector@ for @mainMenu@
mainMenuSelector :: Selector
mainMenuSelector = mkSelector "mainMenu"

-- | @Selector@ for @setMainMenu:@
setMainMenuSelector :: Selector
setMainMenuSelector = mkSelector "setMainMenu:"

-- | @Selector@ for @helpMenu@
helpMenuSelector :: Selector
helpMenuSelector = mkSelector "helpMenu"

-- | @Selector@ for @setHelpMenu:@
setHelpMenuSelector :: Selector
setHelpMenuSelector = mkSelector "setHelpMenu:"

-- | @Selector@ for @applicationIconImage@
applicationIconImageSelector :: Selector
applicationIconImageSelector = mkSelector "applicationIconImage"

-- | @Selector@ for @setApplicationIconImage:@
setApplicationIconImageSelector :: Selector
setApplicationIconImageSelector = mkSelector "setApplicationIconImage:"

-- | @Selector@ for @dockTile@
dockTileSelector :: Selector
dockTileSelector = mkSelector "dockTile"

-- | @Selector@ for @presentationOptions@
presentationOptionsSelector :: Selector
presentationOptionsSelector = mkSelector "presentationOptions"

-- | @Selector@ for @setPresentationOptions:@
setPresentationOptionsSelector :: Selector
setPresentationOptionsSelector = mkSelector "setPresentationOptions:"

-- | @Selector@ for @currentSystemPresentationOptions@
currentSystemPresentationOptionsSelector :: Selector
currentSystemPresentationOptionsSelector = mkSelector "currentSystemPresentationOptions"

-- | @Selector@ for @occlusionState@
occlusionStateSelector :: Selector
occlusionStateSelector = mkSelector "occlusionState"

-- | @Selector@ for @protectedDataAvailable@
protectedDataAvailableSelector :: Selector
protectedDataAvailableSelector = mkSelector "protectedDataAvailable"

-- | @Selector@ for @orderedDocuments@
orderedDocumentsSelector :: Selector
orderedDocumentsSelector = mkSelector "orderedDocuments"

-- | @Selector@ for @orderedWindows@
orderedWindowsSelector :: Selector
orderedWindowsSelector = mkSelector "orderedWindows"

-- | @Selector@ for @automaticCustomizeTouchBarMenuItemEnabled@
automaticCustomizeTouchBarMenuItemEnabledSelector :: Selector
automaticCustomizeTouchBarMenuItemEnabledSelector = mkSelector "automaticCustomizeTouchBarMenuItemEnabled"

-- | @Selector@ for @setAutomaticCustomizeTouchBarMenuItemEnabled:@
setAutomaticCustomizeTouchBarMenuItemEnabledSelector :: Selector
setAutomaticCustomizeTouchBarMenuItemEnabledSelector = mkSelector "setAutomaticCustomizeTouchBarMenuItemEnabled:"

-- | @Selector@ for @context@
contextSelector :: Selector
contextSelector = mkSelector "context"

-- | @Selector@ for @registeredForRemoteNotifications@
registeredForRemoteNotificationsSelector :: Selector
registeredForRemoteNotificationsSelector = mkSelector "registeredForRemoteNotifications"

-- | @Selector@ for @enabledRemoteNotificationTypes@
enabledRemoteNotificationTypesSelector :: Selector
enabledRemoteNotificationTypesSelector = mkSelector "enabledRemoteNotificationTypes"

-- | @Selector@ for @userInterfaceLayoutDirection@
userInterfaceLayoutDirectionSelector :: Selector
userInterfaceLayoutDirectionSelector = mkSelector "userInterfaceLayoutDirection"

-- | @Selector@ for @servicesProvider@
servicesProviderSelector :: Selector
servicesProviderSelector = mkSelector "servicesProvider"

-- | @Selector@ for @setServicesProvider:@
setServicesProviderSelector :: Selector
setServicesProviderSelector = mkSelector "setServicesProvider:"

-- | @Selector@ for @servicesMenu@
servicesMenuSelector :: Selector
servicesMenuSelector = mkSelector "servicesMenu"

-- | @Selector@ for @setServicesMenu:@
setServicesMenuSelector :: Selector
setServicesMenuSelector = mkSelector "setServicesMenu:"

-- | @Selector@ for @fullKeyboardAccessEnabled@
fullKeyboardAccessEnabledSelector :: Selector
fullKeyboardAccessEnabledSelector = mkSelector "fullKeyboardAccessEnabled"

-- | @Selector@ for @windowsMenu@
windowsMenuSelector :: Selector
windowsMenuSelector = mkSelector "windowsMenu"

-- | @Selector@ for @setWindowsMenu:@
setWindowsMenuSelector :: Selector
setWindowsMenuSelector = mkSelector "setWindowsMenu:"

-- | @Selector@ for @currentEvent@
currentEventSelector :: Selector
currentEventSelector = mkSelector "currentEvent"

-- | @Selector@ for @appearance@
appearanceSelector :: Selector
appearanceSelector = mkSelector "appearance"

-- | @Selector@ for @setAppearance:@
setAppearanceSelector :: Selector
setAppearanceSelector = mkSelector "setAppearance:"

-- | @Selector@ for @effectiveAppearance@
effectiveAppearanceSelector :: Selector
effectiveAppearanceSelector = mkSelector "effectiveAppearance"

