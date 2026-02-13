{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | @NSRunningApplication@ is a class to manipulate and provide information for a single instance of an application.  Only user applications are tracked; this does not provide information about every process on the system.
--
-- Some properties of an application are fixed, such as the bundle identifier.  Other properties may vary over time, such as whether the app is hidden.  Properties that vary can be observed with KVO, in which case the description comment for the method will mention it.
--
-- Properties that vary over time are inherently race-prone.  For example, a hidden app may unhide itself at any time.  To ameliorate this, properties persist until the next turn of the main run loop in a common mode.  For example, if you repeatedly poll an unhidden app for its hidden property without allowing the run loop to run, it will continue to return @NO@, even if the app hides, until the next turn of the run loop.
--
-- @NSRunningApplication@ is thread safe, in that its properties are returned atomically.  However, it is still subject to the main run loop policy described above.  If you access an instance of @NSRunningApplication@ from a background thread, be aware that its time-varying properties may change from under you as the main run loop runs (or not).
--
-- An @NSRunningApplication@ instance remains valid after the application exits.  However, most properties lose their significance, and some properties may not be available on a terminated application.
--
-- To access the list of all running applications, use the @-runningApplications@ method on @NSWorkspace@.
--
-- Generated bindings for @NSRunningApplication@.
module ObjC.AppKit.NSRunningApplication
  ( NSRunningApplication
  , IsNSRunningApplication(..)
  , hide
  , unhide
  , activateFromApplication_options
  , activateWithOptions
  , terminate
  , forceTerminate
  , runningApplicationsWithBundleIdentifier
  , runningApplicationWithProcessIdentifier
  , terminateAutomaticallyTerminableApplications
  , terminated
  , finishedLaunching
  , hidden
  , active
  , ownsMenuBar
  , activationPolicy
  , localizedName
  , bundleIdentifier
  , bundleURL
  , executableURL
  , processIdentifier
  , launchDate
  , icon
  , executableArchitecture
  , currentApplication
  , activateFromApplication_optionsSelector
  , activateWithOptionsSelector
  , activationPolicySelector
  , activeSelector
  , bundleIdentifierSelector
  , bundleURLSelector
  , currentApplicationSelector
  , executableArchitectureSelector
  , executableURLSelector
  , finishedLaunchingSelector
  , forceTerminateSelector
  , hiddenSelector
  , hideSelector
  , iconSelector
  , launchDateSelector
  , localizedNameSelector
  , ownsMenuBarSelector
  , processIdentifierSelector
  , runningApplicationWithProcessIdentifierSelector
  , runningApplicationsWithBundleIdentifierSelector
  , terminateAutomaticallyTerminableApplicationsSelector
  , terminateSelector
  , terminatedSelector
  , unhideSelector

  -- * Enum types
  , NSApplicationActivationOptions(NSApplicationActivationOptions)
  , pattern NSApplicationActivateAllWindows
  , pattern NSApplicationActivateIgnoringOtherApps
  , NSApplicationActivationPolicy(NSApplicationActivationPolicy)
  , pattern NSApplicationActivationPolicyRegular
  , pattern NSApplicationActivationPolicyAccessory
  , pattern NSApplicationActivationPolicyProhibited

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

-- | Attempts to hide the receiver.
--
-- Returns: @YES@ if the request to hide or unhide was successfully sent, @NO@ if not (for example, if the application has quit, or is of a type that cannot be unhidden).
--
-- ObjC selector: @- hide@
hide :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO Bool
hide nsRunningApplication =
  sendMessage nsRunningApplication hideSelector

-- | Attempts to unhide the receiver.
--
-- Returns: @YES@ if the request to hide or unhide was successfully sent, @NO@ if not (for example, if the application has quit, or is of a type that cannot be unhidden).
--
-- ObjC selector: @- unhide@
unhide :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO Bool
unhide nsRunningApplication =
  sendMessage nsRunningApplication unhideSelector

-- | Attempts to activate the application using the specified options.
--
-- You shouldn’t assume the app will be active immediately after sending this message. The framework also does not guarantee that the app will be activated at all.
--
-- Additionally allows specifying another application to take the active status from, which can be used for coordinated or cooperative activation. The other application should call @-yieldActivationToApplication:@ or equivalent prior to this request being sent.
--
-- Returns: @YES@ if the request has been allowed by the system, otherwise @NO@.
--
-- ObjC selector: @- activateFromApplication:options:@
activateFromApplication_options :: (IsNSRunningApplication nsRunningApplication, IsNSRunningApplication application) => nsRunningApplication -> application -> NSApplicationActivationOptions -> IO Bool
activateFromApplication_options nsRunningApplication application options =
  sendMessage nsRunningApplication activateFromApplication_optionsSelector (toNSRunningApplication application) options

-- | Attempts to activate the receiver.
--
-- Returns: @YES@ if the request to activate was successfully sent, @NO@ if not (for example, if the application has quit, or is of a type that cannot be activated).
--
-- ObjC selector: @- activateWithOptions:@
activateWithOptions :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> NSApplicationActivationOptions -> IO Bool
activateWithOptions nsRunningApplication options =
  sendMessage nsRunningApplication activateWithOptionsSelector options

-- | Attempts to quit the receiver normally.
--
-- Returns: @YES@ if the request was successfully sent, @NO@ if not (for example, if the application is no longer running). This method may return before the receiver exits; you should observe the terminated property or listen for the notification to detect when the app has exited.
--
-- ObjC selector: @- terminate@
terminate :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO Bool
terminate nsRunningApplication =
  sendMessage nsRunningApplication terminateSelector

-- | Attempts to force the receiver to quit.
--
-- Returns: @YES@ if the request was successfully sent, @NO@ if not (for example, if the application is no longer running). This method may return before the receiver exits; you should observe the terminated property or listen for the notification to detect when the app has exited.
--
-- ObjC selector: @- forceTerminate@
forceTerminate :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO Bool
forceTerminate nsRunningApplication =
  sendMessage nsRunningApplication forceTerminateSelector

-- | Returns: An array of currently running applications with the given bundle identifier, or an empty array if no apps match.
--
-- ObjC selector: @+ runningApplicationsWithBundleIdentifier:@
runningApplicationsWithBundleIdentifier :: IsNSString bundleIdentifier => bundleIdentifier -> IO (Id NSArray)
runningApplicationsWithBundleIdentifier bundleIdentifier =
  do
    cls' <- getRequiredClass "NSRunningApplication"
    sendClassMessage cls' runningApplicationsWithBundleIdentifierSelector (toNSString bundleIdentifier)

-- | Returns: The running application with the given process identifier, or nil if no application has that pid. Applications that do not have PIDs cannot be returned from this method.
--
-- ObjC selector: @+ runningApplicationWithProcessIdentifier:@
runningApplicationWithProcessIdentifier :: CInt -> IO (Id NSRunningApplication)
runningApplicationWithProcessIdentifier pid =
  do
    cls' <- getRequiredClass "NSRunningApplication"
    sendClassMessage cls' runningApplicationWithProcessIdentifierSelector pid

-- | Cause any applications that are invisibly still running (see @NSProcessInfo.h@ automatic termination methods and docs) to terminate as if triggered by system memory pressure.  This is intended for installer apps and the like to make sure that nothing is unexpectedly relying on the files they're replacing.
--
-- ObjC selector: @+ terminateAutomaticallyTerminableApplications@
terminateAutomaticallyTerminableApplications :: IO ()
terminateAutomaticallyTerminableApplications  =
  do
    cls' <- getRequiredClass "NSRunningApplication"
    sendClassMessage cls' terminateAutomaticallyTerminableApplicationsSelector

-- | Indicates that the process is an exited application. This is observable through KVO.
--
-- ObjC selector: @- terminated@
terminated :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO Bool
terminated nsRunningApplication =
  sendMessage nsRunningApplication terminatedSelector

-- | Indicates that the process is finished launching, which corresponds to the @NSApplicationDidFinishLaunching@ internal notification.  This is observable through KVO.  Some applications do not post this notification and so are never reported as finished launching.
--
-- ObjC selector: @- finishedLaunching@
finishedLaunching :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO Bool
finishedLaunching nsRunningApplication =
  sendMessage nsRunningApplication finishedLaunchingSelector

-- | Indicates whether the application is currently hidden. This is observable through KVO.
--
-- ObjC selector: @- hidden@
hidden :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO Bool
hidden nsRunningApplication =
  sendMessage nsRunningApplication hiddenSelector

-- | Indicates whether the application is currently frontmost. This is observable through KVO.
--
-- ObjC selector: @- active@
active :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO Bool
active nsRunningApplication =
  sendMessage nsRunningApplication activeSelector

-- | Indicates whether the application currently owns the menu bar. This is observable through KVO.
--
-- ObjC selector: @- ownsMenuBar@
ownsMenuBar :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO Bool
ownsMenuBar nsRunningApplication =
  sendMessage nsRunningApplication ownsMenuBarSelector

-- | Indicates the activation policy of the application. This is observable through KVO (the type is usually fixed, but may be changed through a call to @-[NSApplication setActivationPolicy:]@).
--
-- ObjC selector: @- activationPolicy@
activationPolicy :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO NSApplicationActivationPolicy
activationPolicy nsRunningApplication =
  sendMessage nsRunningApplication activationPolicySelector

-- | Indicates the name of the application. This is dependent on the current localization of the referenced app, and is suitable for presentation to the user.
--
-- ObjC selector: @- localizedName@
localizedName :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO (Id NSString)
localizedName nsRunningApplication =
  sendMessage nsRunningApplication localizedNameSelector

-- | Indicates the @CFBundleIdentifier@ of the application, or nil if the application does not have an @Info.plist@.
--
-- ObjC selector: @- bundleIdentifier@
bundleIdentifier :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO (Id NSString)
bundleIdentifier nsRunningApplication =
  sendMessage nsRunningApplication bundleIdentifierSelector

-- | Indicates the URL to the application's bundle, or nil if the application does not have a bundle.
--
-- ObjC selector: @- bundleURL@
bundleURL :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO (Id NSURL)
bundleURL nsRunningApplication =
  sendMessage nsRunningApplication bundleURLSelector

-- | Indicates the URL to the application's executable.
--
-- ObjC selector: @- executableURL@
executableURL :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO (Id NSURL)
executableURL nsRunningApplication =
  sendMessage nsRunningApplication executableURLSelector

-- | Indicates the process identifier (pid) of the application. Do not rely on this for comparing processes.  Use @-isEqual:@ instead.
--
-- Note: Not all applications have a pid.  Applications without a pid return -1 from this method. This is observable through KVO (an application's pid may change if it is automatically terminated).
--
-- ObjC selector: @- processIdentifier@
processIdentifier :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO CInt
processIdentifier nsRunningApplication =
  sendMessage nsRunningApplication processIdentifierSelector

-- | Indicates the date when the application was launched.  This property is not available for all applications.  Specifically, it is not available for applications that were launched without going through @LaunchServices@.
--
-- ObjC selector: @- launchDate@
launchDate :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO (Id NSDate)
launchDate nsRunningApplication =
  sendMessage nsRunningApplication launchDateSelector

-- | Returns: The icon of the application.
--
-- ObjC selector: @- icon@
icon :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO (Id NSImage)
icon nsRunningApplication =
  sendMessage nsRunningApplication iconSelector

-- | Indicates the executing processor architecture for the application, as an @NSBundleExecutableArchitecture@ from @NSBundle.h@.
--
-- ObjC selector: @- executableArchitecture@
executableArchitecture :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO CLong
executableArchitecture nsRunningApplication =
  sendMessage nsRunningApplication executableArchitectureSelector

-- | Returns: An @NSRunningApplication@ representing this application.
--
-- ObjC selector: @+ currentApplication@
currentApplication :: IO (Id NSRunningApplication)
currentApplication  =
  do
    cls' <- getRequiredClass "NSRunningApplication"
    sendClassMessage cls' currentApplicationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hide@
hideSelector :: Selector '[] Bool
hideSelector = mkSelector "hide"

-- | @Selector@ for @unhide@
unhideSelector :: Selector '[] Bool
unhideSelector = mkSelector "unhide"

-- | @Selector@ for @activateFromApplication:options:@
activateFromApplication_optionsSelector :: Selector '[Id NSRunningApplication, NSApplicationActivationOptions] Bool
activateFromApplication_optionsSelector = mkSelector "activateFromApplication:options:"

-- | @Selector@ for @activateWithOptions:@
activateWithOptionsSelector :: Selector '[NSApplicationActivationOptions] Bool
activateWithOptionsSelector = mkSelector "activateWithOptions:"

-- | @Selector@ for @terminate@
terminateSelector :: Selector '[] Bool
terminateSelector = mkSelector "terminate"

-- | @Selector@ for @forceTerminate@
forceTerminateSelector :: Selector '[] Bool
forceTerminateSelector = mkSelector "forceTerminate"

-- | @Selector@ for @runningApplicationsWithBundleIdentifier:@
runningApplicationsWithBundleIdentifierSelector :: Selector '[Id NSString] (Id NSArray)
runningApplicationsWithBundleIdentifierSelector = mkSelector "runningApplicationsWithBundleIdentifier:"

-- | @Selector@ for @runningApplicationWithProcessIdentifier:@
runningApplicationWithProcessIdentifierSelector :: Selector '[CInt] (Id NSRunningApplication)
runningApplicationWithProcessIdentifierSelector = mkSelector "runningApplicationWithProcessIdentifier:"

-- | @Selector@ for @terminateAutomaticallyTerminableApplications@
terminateAutomaticallyTerminableApplicationsSelector :: Selector '[] ()
terminateAutomaticallyTerminableApplicationsSelector = mkSelector "terminateAutomaticallyTerminableApplications"

-- | @Selector@ for @terminated@
terminatedSelector :: Selector '[] Bool
terminatedSelector = mkSelector "terminated"

-- | @Selector@ for @finishedLaunching@
finishedLaunchingSelector :: Selector '[] Bool
finishedLaunchingSelector = mkSelector "finishedLaunching"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector '[] Bool
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @active@
activeSelector :: Selector '[] Bool
activeSelector = mkSelector "active"

-- | @Selector@ for @ownsMenuBar@
ownsMenuBarSelector :: Selector '[] Bool
ownsMenuBarSelector = mkSelector "ownsMenuBar"

-- | @Selector@ for @activationPolicy@
activationPolicySelector :: Selector '[] NSApplicationActivationPolicy
activationPolicySelector = mkSelector "activationPolicy"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector '[] (Id NSString)
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector '[] (Id NSString)
bundleIdentifierSelector = mkSelector "bundleIdentifier"

-- | @Selector@ for @bundleURL@
bundleURLSelector :: Selector '[] (Id NSURL)
bundleURLSelector = mkSelector "bundleURL"

-- | @Selector@ for @executableURL@
executableURLSelector :: Selector '[] (Id NSURL)
executableURLSelector = mkSelector "executableURL"

-- | @Selector@ for @processIdentifier@
processIdentifierSelector :: Selector '[] CInt
processIdentifierSelector = mkSelector "processIdentifier"

-- | @Selector@ for @launchDate@
launchDateSelector :: Selector '[] (Id NSDate)
launchDateSelector = mkSelector "launchDate"

-- | @Selector@ for @icon@
iconSelector :: Selector '[] (Id NSImage)
iconSelector = mkSelector "icon"

-- | @Selector@ for @executableArchitecture@
executableArchitectureSelector :: Selector '[] CLong
executableArchitectureSelector = mkSelector "executableArchitecture"

-- | @Selector@ for @currentApplication@
currentApplicationSelector :: Selector '[] (Id NSRunningApplication)
currentApplicationSelector = mkSelector "currentApplication"

