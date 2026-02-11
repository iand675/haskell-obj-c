{-# LANGUAGE PatternSynonyms #-}
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
  , hideSelector
  , unhideSelector
  , activateFromApplication_optionsSelector
  , activateWithOptionsSelector
  , terminateSelector
  , forceTerminateSelector
  , runningApplicationsWithBundleIdentifierSelector
  , runningApplicationWithProcessIdentifierSelector
  , terminateAutomaticallyTerminableApplicationsSelector
  , terminatedSelector
  , finishedLaunchingSelector
  , hiddenSelector
  , activeSelector
  , ownsMenuBarSelector
  , activationPolicySelector
  , localizedNameSelector
  , bundleIdentifierSelector
  , bundleURLSelector
  , executableURLSelector
  , processIdentifierSelector
  , launchDateSelector
  , iconSelector
  , executableArchitectureSelector
  , currentApplicationSelector

  -- * Enum types
  , NSApplicationActivationOptions(NSApplicationActivationOptions)
  , pattern NSApplicationActivateAllWindows
  , pattern NSApplicationActivateIgnoringOtherApps
  , NSApplicationActivationPolicy(NSApplicationActivationPolicy)
  , pattern NSApplicationActivationPolicyRegular
  , pattern NSApplicationActivationPolicyAccessory
  , pattern NSApplicationActivationPolicyProhibited

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Attempts to hide the receiver.
--
-- Returns: @YES@ if the request to hide or unhide was successfully sent, @NO@ if not (for example, if the application has quit, or is of a type that cannot be unhidden).
--
-- ObjC selector: @- hide@
hide :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO Bool
hide nsRunningApplication  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRunningApplication (mkSelector "hide") retCULong []

-- | Attempts to unhide the receiver.
--
-- Returns: @YES@ if the request to hide or unhide was successfully sent, @NO@ if not (for example, if the application has quit, or is of a type that cannot be unhidden).
--
-- ObjC selector: @- unhide@
unhide :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO Bool
unhide nsRunningApplication  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRunningApplication (mkSelector "unhide") retCULong []

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
activateFromApplication_options nsRunningApplication  application options =
withObjCPtr application $ \raw_application ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRunningApplication (mkSelector "activateFromApplication:options:") retCULong [argPtr (castPtr raw_application :: Ptr ()), argCULong (coerce options)]

-- | Attempts to activate the receiver.
--
-- Returns: @YES@ if the request to activate was successfully sent, @NO@ if not (for example, if the application has quit, or is of a type that cannot be activated).
--
-- ObjC selector: @- activateWithOptions:@
activateWithOptions :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> NSApplicationActivationOptions -> IO Bool
activateWithOptions nsRunningApplication  options =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRunningApplication (mkSelector "activateWithOptions:") retCULong [argCULong (coerce options)]

-- | Attempts to quit the receiver normally.
--
-- Returns: @YES@ if the request was successfully sent, @NO@ if not (for example, if the application is no longer running). This method may return before the receiver exits; you should observe the terminated property or listen for the notification to detect when the app has exited.
--
-- ObjC selector: @- terminate@
terminate :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO Bool
terminate nsRunningApplication  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRunningApplication (mkSelector "terminate") retCULong []

-- | Attempts to force the receiver to quit.
--
-- Returns: @YES@ if the request was successfully sent, @NO@ if not (for example, if the application is no longer running). This method may return before the receiver exits; you should observe the terminated property or listen for the notification to detect when the app has exited.
--
-- ObjC selector: @- forceTerminate@
forceTerminate :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO Bool
forceTerminate nsRunningApplication  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRunningApplication (mkSelector "forceTerminate") retCULong []

-- | Returns: An array of currently running applications with the given bundle identifier, or an empty array if no apps match.
--
-- ObjC selector: @+ runningApplicationsWithBundleIdentifier:@
runningApplicationsWithBundleIdentifier :: IsNSString bundleIdentifier => bundleIdentifier -> IO (Id NSArray)
runningApplicationsWithBundleIdentifier bundleIdentifier =
  do
    cls' <- getRequiredClass "NSRunningApplication"
    withObjCPtr bundleIdentifier $ \raw_bundleIdentifier ->
      sendClassMsg cls' (mkSelector "runningApplicationsWithBundleIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_bundleIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | Returns: The running application with the given process identifier, or nil if no application has that pid. Applications that do not have PIDs cannot be returned from this method.
--
-- ObjC selector: @+ runningApplicationWithProcessIdentifier:@
runningApplicationWithProcessIdentifier :: CInt -> IO (Id NSRunningApplication)
runningApplicationWithProcessIdentifier pid =
  do
    cls' <- getRequiredClass "NSRunningApplication"
    sendClassMsg cls' (mkSelector "runningApplicationWithProcessIdentifier:") (retPtr retVoid) [argCInt (fromIntegral pid)] >>= retainedObject . castPtr

-- | Cause any applications that are invisibly still running (see @NSProcessInfo.h@ automatic termination methods and docs) to terminate as if triggered by system memory pressure.  This is intended for installer apps and the like to make sure that nothing is unexpectedly relying on the files they're replacing.
--
-- ObjC selector: @+ terminateAutomaticallyTerminableApplications@
terminateAutomaticallyTerminableApplications :: IO ()
terminateAutomaticallyTerminableApplications  =
  do
    cls' <- getRequiredClass "NSRunningApplication"
    sendClassMsg cls' (mkSelector "terminateAutomaticallyTerminableApplications") retVoid []

-- | Indicates that the process is an exited application. This is observable through KVO.
--
-- ObjC selector: @- terminated@
terminated :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO Bool
terminated nsRunningApplication  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRunningApplication (mkSelector "terminated") retCULong []

-- | Indicates that the process is finished launching, which corresponds to the @NSApplicationDidFinishLaunching@ internal notification.  This is observable through KVO.  Some applications do not post this notification and so are never reported as finished launching.
--
-- ObjC selector: @- finishedLaunching@
finishedLaunching :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO Bool
finishedLaunching nsRunningApplication  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRunningApplication (mkSelector "finishedLaunching") retCULong []

-- | Indicates whether the application is currently hidden. This is observable through KVO.
--
-- ObjC selector: @- hidden@
hidden :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO Bool
hidden nsRunningApplication  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRunningApplication (mkSelector "hidden") retCULong []

-- | Indicates whether the application is currently frontmost. This is observable through KVO.
--
-- ObjC selector: @- active@
active :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO Bool
active nsRunningApplication  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRunningApplication (mkSelector "active") retCULong []

-- | Indicates whether the application currently owns the menu bar. This is observable through KVO.
--
-- ObjC selector: @- ownsMenuBar@
ownsMenuBar :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO Bool
ownsMenuBar nsRunningApplication  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRunningApplication (mkSelector "ownsMenuBar") retCULong []

-- | Indicates the activation policy of the application. This is observable through KVO (the type is usually fixed, but may be changed through a call to @-[NSApplication setActivationPolicy:]@).
--
-- ObjC selector: @- activationPolicy@
activationPolicy :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO NSApplicationActivationPolicy
activationPolicy nsRunningApplication  =
  fmap (coerce :: CLong -> NSApplicationActivationPolicy) $ sendMsg nsRunningApplication (mkSelector "activationPolicy") retCLong []

-- | Indicates the name of the application. This is dependent on the current localization of the referenced app, and is suitable for presentation to the user.
--
-- ObjC selector: @- localizedName@
localizedName :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO (Id NSString)
localizedName nsRunningApplication  =
  sendMsg nsRunningApplication (mkSelector "localizedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates the @CFBundleIdentifier@ of the application, or nil if the application does not have an @Info.plist@.
--
-- ObjC selector: @- bundleIdentifier@
bundleIdentifier :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO (Id NSString)
bundleIdentifier nsRunningApplication  =
  sendMsg nsRunningApplication (mkSelector "bundleIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates the URL to the application's bundle, or nil if the application does not have a bundle.
--
-- ObjC selector: @- bundleURL@
bundleURL :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO (Id NSURL)
bundleURL nsRunningApplication  =
  sendMsg nsRunningApplication (mkSelector "bundleURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates the URL to the application's executable.
--
-- ObjC selector: @- executableURL@
executableURL :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO (Id NSURL)
executableURL nsRunningApplication  =
  sendMsg nsRunningApplication (mkSelector "executableURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates the process identifier (pid) of the application. Do not rely on this for comparing processes.  Use @-isEqual:@ instead.
--
-- Note: Not all applications have a pid.  Applications without a pid return -1 from this method. This is observable through KVO (an application's pid may change if it is automatically terminated).
--
-- ObjC selector: @- processIdentifier@
processIdentifier :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO CInt
processIdentifier nsRunningApplication  =
  sendMsg nsRunningApplication (mkSelector "processIdentifier") retCInt []

-- | Indicates the date when the application was launched.  This property is not available for all applications.  Specifically, it is not available for applications that were launched without going through @LaunchServices@.
--
-- ObjC selector: @- launchDate@
launchDate :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO (Id NSDate)
launchDate nsRunningApplication  =
  sendMsg nsRunningApplication (mkSelector "launchDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns: The icon of the application.
--
-- ObjC selector: @- icon@
icon :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO (Id NSImage)
icon nsRunningApplication  =
  sendMsg nsRunningApplication (mkSelector "icon") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates the executing processor architecture for the application, as an @NSBundleExecutableArchitecture@ from @NSBundle.h@.
--
-- ObjC selector: @- executableArchitecture@
executableArchitecture :: IsNSRunningApplication nsRunningApplication => nsRunningApplication -> IO CLong
executableArchitecture nsRunningApplication  =
  sendMsg nsRunningApplication (mkSelector "executableArchitecture") retCLong []

-- | Returns: An @NSRunningApplication@ representing this application.
--
-- ObjC selector: @+ currentApplication@
currentApplication :: IO (Id NSRunningApplication)
currentApplication  =
  do
    cls' <- getRequiredClass "NSRunningApplication"
    sendClassMsg cls' (mkSelector "currentApplication") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hide@
hideSelector :: Selector
hideSelector = mkSelector "hide"

-- | @Selector@ for @unhide@
unhideSelector :: Selector
unhideSelector = mkSelector "unhide"

-- | @Selector@ for @activateFromApplication:options:@
activateFromApplication_optionsSelector :: Selector
activateFromApplication_optionsSelector = mkSelector "activateFromApplication:options:"

-- | @Selector@ for @activateWithOptions:@
activateWithOptionsSelector :: Selector
activateWithOptionsSelector = mkSelector "activateWithOptions:"

-- | @Selector@ for @terminate@
terminateSelector :: Selector
terminateSelector = mkSelector "terminate"

-- | @Selector@ for @forceTerminate@
forceTerminateSelector :: Selector
forceTerminateSelector = mkSelector "forceTerminate"

-- | @Selector@ for @runningApplicationsWithBundleIdentifier:@
runningApplicationsWithBundleIdentifierSelector :: Selector
runningApplicationsWithBundleIdentifierSelector = mkSelector "runningApplicationsWithBundleIdentifier:"

-- | @Selector@ for @runningApplicationWithProcessIdentifier:@
runningApplicationWithProcessIdentifierSelector :: Selector
runningApplicationWithProcessIdentifierSelector = mkSelector "runningApplicationWithProcessIdentifier:"

-- | @Selector@ for @terminateAutomaticallyTerminableApplications@
terminateAutomaticallyTerminableApplicationsSelector :: Selector
terminateAutomaticallyTerminableApplicationsSelector = mkSelector "terminateAutomaticallyTerminableApplications"

-- | @Selector@ for @terminated@
terminatedSelector :: Selector
terminatedSelector = mkSelector "terminated"

-- | @Selector@ for @finishedLaunching@
finishedLaunchingSelector :: Selector
finishedLaunchingSelector = mkSelector "finishedLaunching"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

-- | @Selector@ for @ownsMenuBar@
ownsMenuBarSelector :: Selector
ownsMenuBarSelector = mkSelector "ownsMenuBar"

-- | @Selector@ for @activationPolicy@
activationPolicySelector :: Selector
activationPolicySelector = mkSelector "activationPolicy"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector
bundleIdentifierSelector = mkSelector "bundleIdentifier"

-- | @Selector@ for @bundleURL@
bundleURLSelector :: Selector
bundleURLSelector = mkSelector "bundleURL"

-- | @Selector@ for @executableURL@
executableURLSelector :: Selector
executableURLSelector = mkSelector "executableURL"

-- | @Selector@ for @processIdentifier@
processIdentifierSelector :: Selector
processIdentifierSelector = mkSelector "processIdentifier"

-- | @Selector@ for @launchDate@
launchDateSelector :: Selector
launchDateSelector = mkSelector "launchDate"

-- | @Selector@ for @icon@
iconSelector :: Selector
iconSelector = mkSelector "icon"

-- | @Selector@ for @executableArchitecture@
executableArchitectureSelector :: Selector
executableArchitectureSelector = mkSelector "executableArchitecture"

-- | @Selector@ for @currentApplication@
currentApplicationSelector :: Selector
currentApplicationSelector = mkSelector "currentApplication"

