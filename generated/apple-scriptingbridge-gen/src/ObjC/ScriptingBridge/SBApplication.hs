{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The @SBApplication@ class provides a mechanism enabling an Objective-C program to send Apple events to a scriptable application and receive Apple events in response. It thereby makes it possible for that program to control the application and exchange data with it. Scripting Bridge works by bridging data types between Apple event descriptors and Cocoa objects.
--
-- Although @SBApplication@ includes methods that manually send and process Apple events, you should never have to call these methods directly. Instead, subclasses of @SBApplication@ implement application-specific methods that handle the sending of Apple events automatically.
--
-- For example, if you wanted to get the current iTunes track, you can simply use the @currentTrack@ method of the dynamically defined subclass for the iTunes application—which handles the details of sending the Apple event for you—rather than figuring out the more complicated, low-level alternative:
--
-- ```objc [iTunes propertyWithCode:'pTrk']; ```
--
-- If you do need to send Apple events manually, consider using the @NSAppleEventDescriptor@ class.
--
-- ## Subclassing Notes
--
-- You rarely instantiate @SBApplication@ objects directly. Instead, you get the shared instance of a application-specific subclass typically by calling one of the @applicationWith...@ class methods, using a bundle identifier, process identifier, or URL to identify the application.
--
-- Generated bindings for @SBApplication@.
module ObjC.ScriptingBridge.SBApplication
  ( SBApplication
  , IsSBApplication(..)
  , initWithBundleIdentifier
  , initWithURL
  , initWithProcessIdentifier
  , applicationWithBundleIdentifier
  , applicationWithURL
  , applicationWithProcessIdentifier
  , classForScriptingClass
  , activate
  , running
  , delegate
  , setDelegate
  , launchFlags
  , setLaunchFlags
  , sendMode
  , setSendMode
  , timeout
  , setTimeout
  , initWithBundleIdentifierSelector
  , initWithURLSelector
  , initWithProcessIdentifierSelector
  , applicationWithBundleIdentifierSelector
  , applicationWithURLSelector
  , applicationWithProcessIdentifierSelector
  , classForScriptingClassSelector
  , activateSelector
  , runningSelector
  , delegateSelector
  , setDelegateSelector
  , launchFlagsSelector
  , setLaunchFlagsSelector
  , sendModeSelector
  , setSendModeSelector
  , timeoutSelector
  , setTimeoutSelector


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

import ObjC.ScriptingBridge.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns an instance of an @SBApplication@ subclass that represents the target application identified by the given bundle identifier.
--
-- If you must initialize an @SBApplication@ object explictly, you should use this initializer if possible; unlike ``SBApplication/initWithProcessIdentifier:`` and ``SBApplication/initWithURL:``, this method is not dependent on changeable factors such as the target application's path or process ID. Even so, you should rarely have to initialize an @SBApplication@ object yourself; instead, you should initialize an application-specific subclass such as @iTunesApplication@.
--
-- Note that this method does not check whether an application with the given bundle identifier actually exists.
--
-- - Parameters:   - ident: A bundle identifier specifying an application that is OSA-compliant.
--
-- - Returns: An initialized shared instance of an @SBApplication@ subclass that represents a target application with the bundle identifier of @ident@. Returns @nil@ if no such application can be found or if the application does not have a scripting interface.
--
-- ObjC selector: @- initWithBundleIdentifier:@
initWithBundleIdentifier :: (IsSBApplication sbApplication, IsNSString ident) => sbApplication -> ident -> IO (Id SBApplication)
initWithBundleIdentifier sbApplication  ident =
  withObjCPtr ident $ \raw_ident ->
      sendMsg sbApplication (mkSelector "initWithBundleIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_ident :: Ptr ())] >>= ownedObject . castPtr

-- | Returns an instance of an @SBApplication@ subclass that represents the target application identified by the given URL.
--
-- This approach to initializing @SBApplication@ objects should be used only if you know for certain the URL of the target application. In most cases, it is better to use ``SBApplication/applicationWithBundleIdentifier:`` which dynamically locates the target application at runtime. Even so, you should rarely have to initialize an @SBApplication@ yourself.
--
-- This method currently supports file URLs (@file:@) and remote application URLs (@eppc:@). It checks whether a file exists at the specified path, but it does not check whether an application identified via @eppc:@ exists.
--
-- - Parameters:   - url: A Universal Resource Locator (URL) specifying an application that is OSA-compliant.
--
-- - Returns: An initialized @SBApplication@ that you can use to communicate with the target application specified by the process ID. Returns @nil@ if an application could not be found or if the application does not have a scripting interface.
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsSBApplication sbApplication, IsNSURL url) => sbApplication -> url -> IO (Id SBApplication)
initWithURL sbApplication  url =
  withObjCPtr url $ \raw_url ->
      sendMsg sbApplication (mkSelector "initWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | Returns an instance of an @SBApplication@ subclass that represents the target application identified by the given process identifier.
--
-- You should avoid using this method unless you know nothing about an external application but its PID. In most cases, it is better to use ``SBApplication/initWithBundleIdentifier:``, which will dynamically locate the external application's path at runtime, or ``SBApplication/initWithURL:``, which is not dependent on the external application being open at the time the method is called.
--
-- - Parameters:   - pid: A BSD process ID specifying an application that is OSA-compliant. Often you can get the process ID of a process using the <doc://com.apple.documentation/documentation/foundation/nstask/1412022-processidentifier> method of @NSTask@.
--
-- - Returns: An initialized @SBApplication@ that you can use to communicate with the target application specified by the process ID. Returns @nil@ if no such application can be found or if the application does not have a scripting interface.
--
-- ObjC selector: @- initWithProcessIdentifier:@
initWithProcessIdentifier :: IsSBApplication sbApplication => sbApplication -> CInt -> IO (Id SBApplication)
initWithProcessIdentifier sbApplication  pid =
    sendMsg sbApplication (mkSelector "initWithProcessIdentifier:") (retPtr retVoid) [argCInt pid] >>= ownedObject . castPtr

-- | Returns the shared instance representing the target application specified by its bundle identifier.
--
-- For applications that declare themselves to have a dynamic scripting interface, this method will launch the application if it is not already running.
--
-- - Parameters:   - ident: A bundle identifier specifying an application that is OSA-compliant.
--
-- - Returns: An instance of a @SBApplication@ subclass that represents the target application whose bundle identifier is @ident@. Returns @nil@ if no such application can be found or if the application does not have a scripting interface.
--
-- ObjC selector: @+ applicationWithBundleIdentifier:@
applicationWithBundleIdentifier :: IsNSString ident => ident -> IO (Id SBApplication)
applicationWithBundleIdentifier ident =
  do
    cls' <- getRequiredClass "SBApplication"
    withObjCPtr ident $ \raw_ident ->
      sendClassMsg cls' (mkSelector "applicationWithBundleIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_ident :: Ptr ())] >>= retainedObject . castPtr

-- | Returns the shared instance representing a target application specified by the given URL.
--
-- For applications that declare themselves to have a dynamic scripting interface, this method will launch the application if it is not already running. This approach to initializing @SBApplication@ objects should be used only if you know for certain the URL of the target application. In most cases, it is better to use ``SBApplication/applicationWithBundleIdentifier:`` which dynamically locates the target application at runtime.
--
-- This method currently supports file URLs (@file:@) and remote application URLs (@eppc:@). It checks whether a file exists at the specified path, but it does not check whether an application identified via @eppc:@ exists.
--
-- - Parameters:   - url: The Universal Resource Locator (URL) locating an OSA-compliant application.
--
-- - Returns: An @SBApplication@ subclass from which to generate a shared instance of the target application whose URL is @url@. Returns @nil@ if no such application can be found or if the application does not have a scripting interface.
--
-- ObjC selector: @+ applicationWithURL:@
applicationWithURL :: IsNSURL url => url -> IO (Id SBApplication)
applicationWithURL url =
  do
    cls' <- getRequiredClass "SBApplication"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "applicationWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | Returns the shared instance representing a target application specified by its process identifier.
--
-- You should avoid using this method unless you know nothing about a target application but its process ID. In most cases, it is better to use ``SBApplication/applicationWithBundleIdentifier:``, which will dynamically locate the application's path at runtime, or ``SBApplication/applicationWithURL:``, which is not dependent on the target application being open at the time the method is called.
--
-- - Parameters:   - pid: The BSD process ID of a OSA-compliant application. Often you can get the process ID of a process using the <doc://com.apple.documentation/documentation/foundation/nstask/1412022-processidentifier> method of @NSTask@.
--
-- - Returns: An instance of an @SBApplication@ subclass that represents the target application whose process identifier is @pid@. Returns @nil@ if no such application can be found or if the application does not have a scripting interface.
--
-- ObjC selector: @+ applicationWithProcessIdentifier:@
applicationWithProcessIdentifier :: CInt -> IO (Id SBApplication)
applicationWithProcessIdentifier pid =
  do
    cls' <- getRequiredClass "SBApplication"
    sendClassMsg cls' (mkSelector "applicationWithProcessIdentifier:") (retPtr retVoid) [argCInt pid] >>= retainedObject . castPtr

-- | Returns a class object that represents a particular class in the target application.
--
-- You invoke this method on an instance of a scriptable application. Once you have the class object, you may allocate an instance of the class and appropriately the raw instance. Or you may use it in a call to <doc://com.apple.documentation/documentation/objectivec/1418956-nsobject/1418511-iskindofclass> to determine the class type of an object.
--
-- - Parameters:   - className: The name of the scripting class, as it appears in the scripting interface. For example, "document".
--
-- - Returns: A @Class@ object representing the scripting class.
--
-- ObjC selector: @- classForScriptingClass:@
classForScriptingClass :: (IsSBApplication sbApplication, IsNSString className) => sbApplication -> className -> IO Class
classForScriptingClass sbApplication  className =
  withObjCPtr className $ \raw_className ->
      fmap (Class . castPtr) $ sendMsg sbApplication (mkSelector "classForScriptingClass:") (retPtr retVoid) [argPtr (castPtr raw_className :: Ptr ())]

-- | Moves the target application to the foreground immediately.
--
-- If the target application is not already running, this method launches it.
--
-- ObjC selector: @- activate@
activate :: IsSBApplication sbApplication => sbApplication -> IO ()
activate sbApplication  =
    sendMsg sbApplication (mkSelector "activate") retVoid []

-- | A Boolean that indicates whether the target application represented by the receiver is running.
--
-- <doc://com.apple.documentation/documentation/swift/true> if the application is running, <doc://com.apple.documentation/documentation/swift/false> otherwise.
--
-- This may be <doc://com.apple.documentation/documentation/swift/true> for instances initialized with a bundle identifier or URL because @SBApplication@ launches the application only when it's necessary to send it an event.
--
-- ObjC selector: @- running@
running :: IsSBApplication sbApplication => sbApplication -> IO Bool
running sbApplication  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg sbApplication (mkSelector "running") retCULong []

-- | The error-handling delegate of the receiver.
--
-- The delegate should implement the ``SBApplicationDelegate/eventDidFail:withError:`` method of the ``SBApplicationDelegate`` informal protocol.
--
-- ObjC selector: @- delegate@
delegate :: IsSBApplication sbApplication => sbApplication -> IO RawId
delegate sbApplication  =
    fmap (RawId . castPtr) $ sendMsg sbApplication (mkSelector "delegate") (retPtr retVoid) []

-- | The error-handling delegate of the receiver.
--
-- The delegate should implement the ``SBApplicationDelegate/eventDidFail:withError:`` method of the ``SBApplicationDelegate`` informal protocol.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsSBApplication sbApplication => sbApplication -> RawId -> IO ()
setDelegate sbApplication  value =
    sendMsg sbApplication (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The launch flags for the application represented by the receiver.
--
-- For more information, see <doc://com.apple.documentation/documentation/coreservices/launch_services>.
--
-- ObjC selector: @- launchFlags@
launchFlags :: IsSBApplication sbApplication => sbApplication -> IO CInt
launchFlags sbApplication  =
    sendMsg sbApplication (mkSelector "launchFlags") retCInt []

-- | The launch flags for the application represented by the receiver.
--
-- For more information, see <doc://com.apple.documentation/documentation/coreservices/launch_services>.
--
-- ObjC selector: @- setLaunchFlags:@
setLaunchFlags :: IsSBApplication sbApplication => sbApplication -> CInt -> IO ()
setLaunchFlags sbApplication  value =
    sendMsg sbApplication (mkSelector "setLaunchFlags:") retVoid [argCInt (fromIntegral value)]

-- | The mode for sending Apple events to the target application.
--
-- For more information, see <doc://com.apple.documentation/documentation/applicationservices/apple_event_manager>.
--
-- The default send mode is <doc://com.apple.documentation/documentation/coreservices/1542914-anonymous/kaewaitreply>. If the send mode is something other than @kAEWaitReply@, the receiver might not correctly handle reply events from the target application.
--
-- ObjC selector: @- sendMode@
sendMode :: IsSBApplication sbApplication => sbApplication -> IO CInt
sendMode sbApplication  =
    sendMsg sbApplication (mkSelector "sendMode") retCInt []

-- | The mode for sending Apple events to the target application.
--
-- For more information, see <doc://com.apple.documentation/documentation/applicationservices/apple_event_manager>.
--
-- The default send mode is <doc://com.apple.documentation/documentation/coreservices/1542914-anonymous/kaewaitreply>. If the send mode is something other than @kAEWaitReply@, the receiver might not correctly handle reply events from the target application.
--
-- ObjC selector: @- setSendMode:@
setSendMode :: IsSBApplication sbApplication => sbApplication -> CInt -> IO ()
setSendMode sbApplication  value =
    sendMsg sbApplication (mkSelector "setSendMode:") retVoid [argCInt value]

-- | The period the application will wait to receive reply Apple events.
--
-- For more information, see <doc://com.apple.documentation/documentation/applicationservices/apple_event_manager>.
--
-- The default timeout value is <doc://com.apple.documentation/documentation/coreservices/1542814-timeout_constants/kaedefaulttimeout>, which is about a minute. If you want the receiver to wait indefinitely for reply Apple events, use <doc://com.apple.documentation/documentation/coreservices/1542814-timeout_constants/knotimeout>. For more information, see <doc://com.apple.documentation/documentation/applicationservices/apple_event_manager>.
--
-- ObjC selector: @- timeout@
timeout :: IsSBApplication sbApplication => sbApplication -> IO CLong
timeout sbApplication  =
    sendMsg sbApplication (mkSelector "timeout") retCLong []

-- | The period the application will wait to receive reply Apple events.
--
-- For more information, see <doc://com.apple.documentation/documentation/applicationservices/apple_event_manager>.
--
-- The default timeout value is <doc://com.apple.documentation/documentation/coreservices/1542814-timeout_constants/kaedefaulttimeout>, which is about a minute. If you want the receiver to wait indefinitely for reply Apple events, use <doc://com.apple.documentation/documentation/coreservices/1542814-timeout_constants/knotimeout>. For more information, see <doc://com.apple.documentation/documentation/applicationservices/apple_event_manager>.
--
-- ObjC selector: @- setTimeout:@
setTimeout :: IsSBApplication sbApplication => sbApplication -> CLong -> IO ()
setTimeout sbApplication  value =
    sendMsg sbApplication (mkSelector "setTimeout:") retVoid [argCLong value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBundleIdentifier:@
initWithBundleIdentifierSelector :: Selector
initWithBundleIdentifierSelector = mkSelector "initWithBundleIdentifier:"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @initWithProcessIdentifier:@
initWithProcessIdentifierSelector :: Selector
initWithProcessIdentifierSelector = mkSelector "initWithProcessIdentifier:"

-- | @Selector@ for @applicationWithBundleIdentifier:@
applicationWithBundleIdentifierSelector :: Selector
applicationWithBundleIdentifierSelector = mkSelector "applicationWithBundleIdentifier:"

-- | @Selector@ for @applicationWithURL:@
applicationWithURLSelector :: Selector
applicationWithURLSelector = mkSelector "applicationWithURL:"

-- | @Selector@ for @applicationWithProcessIdentifier:@
applicationWithProcessIdentifierSelector :: Selector
applicationWithProcessIdentifierSelector = mkSelector "applicationWithProcessIdentifier:"

-- | @Selector@ for @classForScriptingClass:@
classForScriptingClassSelector :: Selector
classForScriptingClassSelector = mkSelector "classForScriptingClass:"

-- | @Selector@ for @activate@
activateSelector :: Selector
activateSelector = mkSelector "activate"

-- | @Selector@ for @running@
runningSelector :: Selector
runningSelector = mkSelector "running"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @launchFlags@
launchFlagsSelector :: Selector
launchFlagsSelector = mkSelector "launchFlags"

-- | @Selector@ for @setLaunchFlags:@
setLaunchFlagsSelector :: Selector
setLaunchFlagsSelector = mkSelector "setLaunchFlags:"

-- | @Selector@ for @sendMode@
sendModeSelector :: Selector
sendModeSelector = mkSelector "sendMode"

-- | @Selector@ for @setSendMode:@
setSendModeSelector :: Selector
setSendModeSelector = mkSelector "setSendMode:"

-- | @Selector@ for @timeout@
timeoutSelector :: Selector
timeoutSelector = mkSelector "timeout"

-- | @Selector@ for @setTimeout:@
setTimeoutSelector :: Selector
setTimeoutSelector = mkSelector "setTimeout:"

