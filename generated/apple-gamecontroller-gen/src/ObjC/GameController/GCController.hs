{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Controllers are available to an application that links to GameController.framework. There are 2 ways to access controllers paired to the system, adopt both to ensure the best user experience:
--
-- 1: Querying for the the current array or controllers using [GCController controllers]. 2: Registering for Connection/Disconnection notifications from NSNotificationCenter.
--
-- Only controllers that support one of the allowed profiles, such as GCExtendedGamepad, will be enumerated. Check for the profile supported before using a controller in your application. Ignore a controller that doesn't support a profile that suits your application, as the user will expect their controller to either be fully supported or not supported at all.
--
-- Generated bindings for @GCController@.
module ObjC.GameController.GCController
  ( GCController
  , IsGCController(..)
  , controllers
  , supportsHIDDevice
  , startWirelessControllerDiscoveryWithCompletionHandler
  , stopWirelessControllerDiscovery
  , capture
  , controllerWithMicroGamepad
  , controllerWithExtendedGamepad
  , current
  , controllerPausedHandler
  , setControllerPausedHandler
  , shouldMonitorBackgroundEvents
  , setShouldMonitorBackgroundEvents
  , attachedToDevice
  , playerIndex
  , setPlayerIndex
  , input
  , battery
  , physicalInputProfile
  , gamepad
  , microGamepad
  , extendedGamepad
  , motion
  , light
  , haptics
  , snapshot
  , attachedToDeviceSelector
  , batterySelector
  , captureSelector
  , controllerPausedHandlerSelector
  , controllerWithExtendedGamepadSelector
  , controllerWithMicroGamepadSelector
  , controllersSelector
  , currentSelector
  , extendedGamepadSelector
  , gamepadSelector
  , hapticsSelector
  , inputSelector
  , lightSelector
  , microGamepadSelector
  , motionSelector
  , physicalInputProfileSelector
  , playerIndexSelector
  , setControllerPausedHandlerSelector
  , setPlayerIndexSelector
  , setShouldMonitorBackgroundEventsSelector
  , shouldMonitorBackgroundEventsSelector
  , snapshotSelector
  , startWirelessControllerDiscoveryWithCompletionHandlerSelector
  , stopWirelessControllerDiscoverySelector
  , supportsHIDDeviceSelector

  -- * Enum types
  , GCControllerPlayerIndex(GCControllerPlayerIndex)
  , pattern GCControllerPlayerIndexUnset
  , pattern GCControllerPlayerIndex1
  , pattern GCControllerPlayerIndex2
  , pattern GCControllerPlayerIndex3
  , pattern GCControllerPlayerIndex4

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.GameController.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Get a list of controllers currently attached to the system.
--
-- See: GCControllerDidConnectNotification
--
-- See: GCControllerDidDisconnectNotification
--
-- ObjC selector: @+ controllers@
controllers :: IO (Id NSArray)
controllers  =
  do
    cls' <- getRequiredClass "GCController"
    sendClassMessage cls' controllersSelector

-- | Returns YES if the given HID device is supported by the Game Controller  framework, and will have an associated GCController instance.
--
-- Note: This is not cheap, be sure to cache the result
--
-- ObjC selector: @+ supportsHIDDevice:@
supportsHIDDevice :: Ptr () -> IO Bool
supportsHIDDevice device =
  do
    cls' <- getRequiredClass "GCController"
    sendClassMessage cls' supportsHIDDeviceSelector device

-- | Start discovery of new wireless controllers that are discoverable. This is an asynchronous and the supplied completionHandler will get called once no more devices can be found. If there are already multiple controllers available for use, there may be little reason to automatically start discovery of new wireless controllers. In this situation it may be best to allow the user to start discovery manually via in-game UI.
--
-- Once discovery has started new controllers will notify themselves as connected via GCControllerDidConnectNotification. As the notification arrives the controller is also available in the controllers array.
--
-- The completionHandler could be used to update UI and/or game state to indicate that no more controllers will be found and the current set of controllers is what is available for use in the game.
--
-- If a completionHandler was provided, it will be called once when discovery stops. Either from an explicit call to stopWirelessControllerDiscovery or from timing out or stopping in its natural course of operation. Thus the completionHandler will at most be called once per call to startWirelessControllerDiscoveryWithCompletionHandler:.
--
-- The completionHandler may also not get called at all, if for example startWirelessControllerDiscoveryWithCompletionHandler: is called multiple times during dicovery. For this case the net effect is that the completionHandler is replaced with each call and only the last one set before discovery stops will be called.
--
-- @completionHandler@ — an optional handler that is called when discovery stops. (may be nil, in which case you will not be notified when discovery stops)
--
-- See: stopWirelessControllerDiscovery
--
-- See: controllers
--
-- ObjC selector: @+ startWirelessControllerDiscoveryWithCompletionHandler:@
startWirelessControllerDiscoveryWithCompletionHandler :: Ptr () -> IO ()
startWirelessControllerDiscoveryWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "GCController"
    sendClassMessage cls' startWirelessControllerDiscoveryWithCompletionHandlerSelector completionHandler

-- | If no more controllers are needed, depending on game state or number of controllers supported by a game, the discovery process can be stopped. Calling stopWirelessControllerDiscovery when no discovery is currently in progress will return immediately without any effect, thus it is safe to call even if the completionHandler of startWirelessControllerDiscoveryWithCompletionHandler: has been called.
--
-- See: startWirelessControllerDiscoveryWithCompletionHandler:
--
-- ObjC selector: @+ stopWirelessControllerDiscovery@
stopWirelessControllerDiscovery :: IO ()
stopWirelessControllerDiscovery  =
  do
    cls' <- getRequiredClass "GCController"
    sendClassMessage cls' stopWirelessControllerDiscoverySelector

-- | Polls the state vector of the controller and saves it to a new and writable  instance of GCController.
--
-- If your application is heavily multithreaded this may also be useful to  guarantee atomicity of input handling as a snapshot will not change based  on user input once it is taken.
--
-- See: snapshot
--
-- Returns: A new controller with the duplicated state vector of the current  controller.
--
-- ObjC selector: @- capture@
capture :: IsGCController gcController => gcController -> IO (Id GCController)
capture gcController =
  sendMessage gcController captureSelector

-- | Creates a controller with a micro gamepad profile.
--
-- This controller will be considered a snapshot, allowing developers to write   to any GCControllerElement of its profiles.
--
-- See: snapshot
--
-- Returns: A new controller with a micro gamepad profile
--
-- ObjC selector: @+ controllerWithMicroGamepad@
controllerWithMicroGamepad :: IO (Id GCController)
controllerWithMicroGamepad  =
  do
    cls' <- getRequiredClass "GCController"
    sendClassMessage cls' controllerWithMicroGamepadSelector

-- | Creates a controller with an extended gamepad profile.
--
-- This controller will be considered a snapshot, allowing developers to write to any GCControllerElement of its profiles.
--
-- See: snapshot
--
-- Returns: A new controller with an extended gamepad profile
--
-- ObjC selector: @+ controllerWithExtendedGamepad@
controllerWithExtendedGamepad :: IO (Id GCController)
controllerWithExtendedGamepad  =
  do
    cls' <- getRequiredClass "GCController"
    sendClassMessage cls' controllerWithExtendedGamepadSelector

-- | The most recently used game controller. If a user actuates a game controller  input, that controller will become the current one.
--
-- Note: This is useful for single player games where you only care about whether an  input is pressed, and not where it came from.  You will still need to  register for changes to GCController.current so that your UI can remain  up-to-date with the current controller.
--
-- ObjC selector: @+ current@
current :: IO (Id GCController)
current  =
  do
    cls' <- getRequiredClass "GCController"
    sendClassMessage cls' currentSelector

-- | Set this block to be notified when a user intends to suspend or resume the current game state. A controller will have a button dedicated to suspending and resuming play and invoking context sensitive actions. During event handling the system will notify the application using this block such that the application can handle the suspension and resumption from the given controller.
--
-- Use this to implement your canonical transition to a pause menu for example if that is your application's desired handling of suspension in play. You may pause and resume based on game state as well so the event is only called each time the pause/resume button is pressed.
--
-- Note: This handler has been deprecated in favor of the Menu button found on GCMicroGamepad and GCExtendedGamepad.
--
-- See: microGamepad
--
-- See: extendedGamepad
--
-- ObjC selector: @- controllerPausedHandler@
controllerPausedHandler :: IsGCController gcController => gcController -> IO (Ptr ())
controllerPausedHandler gcController =
  sendMessage gcController controllerPausedHandlerSelector

-- | Set this block to be notified when a user intends to suspend or resume the current game state. A controller will have a button dedicated to suspending and resuming play and invoking context sensitive actions. During event handling the system will notify the application using this block such that the application can handle the suspension and resumption from the given controller.
--
-- Use this to implement your canonical transition to a pause menu for example if that is your application's desired handling of suspension in play. You may pause and resume based on game state as well so the event is only called each time the pause/resume button is pressed.
--
-- Note: This handler has been deprecated in favor of the Menu button found on GCMicroGamepad and GCExtendedGamepad.
--
-- See: microGamepad
--
-- See: extendedGamepad
--
-- ObjC selector: @- setControllerPausedHandler:@
setControllerPausedHandler :: IsGCController gcController => gcController -> Ptr () -> IO ()
setControllerPausedHandler gcController value =
  sendMessage gcController setControllerPausedHandlerSelector value

-- | Whether the current application should monitor and respond to game controller events when it is not the frontmost application.
--
-- If shouldMonitorBackgroundEvents is NO, and the application is not the frontmost application, any inputs from a game controller will
--
-- not be forwarded to the application. Once the application becomes the frontmost application, game controller events will be forwarded.
--
-- Note: Starting with macOS Big Sur 11.3, shouldMonitorBackgroundEvents will be NO by default. For applications built prior to macOS Big Sur 11.3, (or running on devices with an earlier version of macOS), shouldMonitorBackgroundEvents will be YES by default. On iOS and tvOS, this property is ignored.
--
-- ObjC selector: @+ shouldMonitorBackgroundEvents@
shouldMonitorBackgroundEvents :: IO Bool
shouldMonitorBackgroundEvents  =
  do
    cls' <- getRequiredClass "GCController"
    sendClassMessage cls' shouldMonitorBackgroundEventsSelector

-- | Whether the current application should monitor and respond to game controller events when it is not the frontmost application.
--
-- If shouldMonitorBackgroundEvents is NO, and the application is not the frontmost application, any inputs from a game controller will
--
-- not be forwarded to the application. Once the application becomes the frontmost application, game controller events will be forwarded.
--
-- Note: Starting with macOS Big Sur 11.3, shouldMonitorBackgroundEvents will be NO by default. For applications built prior to macOS Big Sur 11.3, (or running on devices with an earlier version of macOS), shouldMonitorBackgroundEvents will be YES by default. On iOS and tvOS, this property is ignored.
--
-- ObjC selector: @+ setShouldMonitorBackgroundEvents:@
setShouldMonitorBackgroundEvents :: Bool -> IO ()
setShouldMonitorBackgroundEvents value =
  do
    cls' <- getRequiredClass "GCController"
    sendClassMessage cls' setShouldMonitorBackgroundEventsSelector value

-- | A controller may be form fitting or otherwise closely attached to the device. This closeness to other inputs on the device may suggest that interaction with the device may use other inputs easily. This is presented to developers to allow them to make informed decisions about UI and interactions to choose for their game in this situation.
--
-- ObjC selector: @- attachedToDevice@
attachedToDevice :: IsGCController gcController => gcController -> IO Bool
attachedToDevice gcController =
  sendMessage gcController attachedToDeviceSelector

-- | A player index for the controller, defaults to GCControllerPlayerIndexUnset.
--
-- This can be set both for the application to keep track of controllers and as a signal to make a controller display a player index on a set of LEDs or some other mechanism.
--
-- A controller is not guaranteed to have a visual display of the playerIndex, playerIndex does not persist for a controller with regards to a system.
--
-- Negative values less than GCControllerPlayerIndexUnset will just map back to GCControllerPlayerIndexUnset when read back.
--
-- ObjC selector: @- playerIndex@
playerIndex :: IsGCController gcController => gcController -> IO GCControllerPlayerIndex
playerIndex gcController =
  sendMessage gcController playerIndexSelector

-- | A player index for the controller, defaults to GCControllerPlayerIndexUnset.
--
-- This can be set both for the application to keep track of controllers and as a signal to make a controller display a player index on a set of LEDs or some other mechanism.
--
-- A controller is not guaranteed to have a visual display of the playerIndex, playerIndex does not persist for a controller with regards to a system.
--
-- Negative values less than GCControllerPlayerIndexUnset will just map back to GCControllerPlayerIndexUnset when read back.
--
-- ObjC selector: @- setPlayerIndex:@
setPlayerIndex :: IsGCController gcController => gcController -> GCControllerPlayerIndex -> IO ()
setPlayerIndex gcController value =
  sendMessage gcController setPlayerIndexSelector value

-- | Gets the input profile for the controller.
--
-- ObjC selector: @- input@
input :: IsGCController gcController => gcController -> IO (Id GCControllerLiveInput)
input gcController =
  sendMessage gcController inputSelector

-- | Gets the battery information if controller supports one
--
-- This property is useful when you try to notify your user to change or charge controller before it runs out of battery life or simply display the current battery level and status.
--
-- ObjC selector: @- battery@
battery :: IsGCController gcController => gcController -> IO (Id GCDeviceBattery)
battery gcController =
  sendMessage gcController batterySelector

-- | Gets the physical input profile for the controller.
--
-- Note: This is equivalent to the controller's microGamepad, or extendedGamepad instance.
--
-- See: microGamepad
--
-- See: extendedGamepad
--
-- ObjC selector: @- physicalInputProfile@
physicalInputProfile :: IsGCController gcController => gcController -> IO (Id GCPhysicalInputProfile)
physicalInputProfile gcController =
  sendMessage gcController physicalInputProfileSelector

-- | Gets the profile for the controller that suits current application.
--
-- There are several supported profiles, with an additional optional profile for motion as well. Each controller may be able to map its inputs into all profiles or just one kind of profile. Query for the controller profile that suits your game, the simplest kind will be supported by the broadest variety of controllers. A controller supporting the Extended Gamepad profile for example supports the Gamepad profile and more. As such it can always be used just in the Gamepad profile if that suits the game.
--
-- A physical controller that supports a profile must support it completely. That means that all buttons and axis inputs must be valid inputs that a developer can utilize.
--
-- If a controller does not support the given profile the returned value will be nil. Use this to filter controllers if the application requires a specific kind of profile.
--
-- See: motion
--
-- ObjC selector: @- gamepad@
gamepad :: IsGCController gcController => gcController -> IO (Id GCGamepad)
gamepad gcController =
  sendMessage gcController gamepadSelector

-- | @- microGamepad@
microGamepad :: IsGCController gcController => gcController -> IO (Id GCMicroGamepad)
microGamepad gcController =
  sendMessage gcController microGamepadSelector

-- | @- extendedGamepad@
extendedGamepad :: IsGCController gcController => gcController -> IO (Id GCExtendedGamepad)
extendedGamepad gcController =
  sendMessage gcController extendedGamepadSelector

-- | Gets the motion input profile. This profile is optional and may be available if the controller is attached to a device that supports motion. If this is nil the controller does not support motion input and only the gamepad & extendedGamepad profiles are available.
--
-- See: gamepad
--
-- See: extendedGamepad
--
-- ObjC selector: @- motion@
motion :: IsGCController gcController => gcController -> IO (Id GCMotion)
motion gcController =
  sendMessage gcController motionSelector

-- | Gets the light for the controller, if one exists.
--
-- A controller's light can be used to signal information to the player, such as using different light colors based on the player index. It can also be used to react to in-game events and enhance user immersion.
--
-- ObjC selector: @- light@
light :: IsGCController gcController => gcController -> IO (Id GCDeviceLight)
light gcController =
  sendMessage gcController lightSelector

-- | Gets the haptics for the controller, if one exists.
--
-- Use this property to create CHHapticEngine instances according to your needs.
--
-- Note: Haptics are a drain on the controller's battery, and can be distracting when used excessively.
--
-- ObjC selector: @- haptics@
haptics :: IsGCController gcController => gcController -> IO (Id GCDeviceHaptics)
haptics gcController =
  sendMessage gcController hapticsSelector

-- | A controller may represent a real device managed by the operating system,  or a virtual snapshot created by the developer.  If a controller is created  by the developer, it is considered to be a snapshot, allowing direct writes  to any GCControllerElement of its profiles.  If the controller is not  snapshot, the system will reject any write requests to GCControllerElement.
--
-- See: controllerWithMicroGamepad
--
-- See: controllerWithExtendedGamepad
--
-- See: capture
--
-- ObjC selector: @- snapshot@
snapshot :: IsGCController gcController => gcController -> IO Bool
snapshot gcController =
  sendMessage gcController snapshotSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @controllers@
controllersSelector :: Selector '[] (Id NSArray)
controllersSelector = mkSelector "controllers"

-- | @Selector@ for @supportsHIDDevice:@
supportsHIDDeviceSelector :: Selector '[Ptr ()] Bool
supportsHIDDeviceSelector = mkSelector "supportsHIDDevice:"

-- | @Selector@ for @startWirelessControllerDiscoveryWithCompletionHandler:@
startWirelessControllerDiscoveryWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
startWirelessControllerDiscoveryWithCompletionHandlerSelector = mkSelector "startWirelessControllerDiscoveryWithCompletionHandler:"

-- | @Selector@ for @stopWirelessControllerDiscovery@
stopWirelessControllerDiscoverySelector :: Selector '[] ()
stopWirelessControllerDiscoverySelector = mkSelector "stopWirelessControllerDiscovery"

-- | @Selector@ for @capture@
captureSelector :: Selector '[] (Id GCController)
captureSelector = mkSelector "capture"

-- | @Selector@ for @controllerWithMicroGamepad@
controllerWithMicroGamepadSelector :: Selector '[] (Id GCController)
controllerWithMicroGamepadSelector = mkSelector "controllerWithMicroGamepad"

-- | @Selector@ for @controllerWithExtendedGamepad@
controllerWithExtendedGamepadSelector :: Selector '[] (Id GCController)
controllerWithExtendedGamepadSelector = mkSelector "controllerWithExtendedGamepad"

-- | @Selector@ for @current@
currentSelector :: Selector '[] (Id GCController)
currentSelector = mkSelector "current"

-- | @Selector@ for @controllerPausedHandler@
controllerPausedHandlerSelector :: Selector '[] (Ptr ())
controllerPausedHandlerSelector = mkSelector "controllerPausedHandler"

-- | @Selector@ for @setControllerPausedHandler:@
setControllerPausedHandlerSelector :: Selector '[Ptr ()] ()
setControllerPausedHandlerSelector = mkSelector "setControllerPausedHandler:"

-- | @Selector@ for @shouldMonitorBackgroundEvents@
shouldMonitorBackgroundEventsSelector :: Selector '[] Bool
shouldMonitorBackgroundEventsSelector = mkSelector "shouldMonitorBackgroundEvents"

-- | @Selector@ for @setShouldMonitorBackgroundEvents:@
setShouldMonitorBackgroundEventsSelector :: Selector '[Bool] ()
setShouldMonitorBackgroundEventsSelector = mkSelector "setShouldMonitorBackgroundEvents:"

-- | @Selector@ for @attachedToDevice@
attachedToDeviceSelector :: Selector '[] Bool
attachedToDeviceSelector = mkSelector "attachedToDevice"

-- | @Selector@ for @playerIndex@
playerIndexSelector :: Selector '[] GCControllerPlayerIndex
playerIndexSelector = mkSelector "playerIndex"

-- | @Selector@ for @setPlayerIndex:@
setPlayerIndexSelector :: Selector '[GCControllerPlayerIndex] ()
setPlayerIndexSelector = mkSelector "setPlayerIndex:"

-- | @Selector@ for @input@
inputSelector :: Selector '[] (Id GCControllerLiveInput)
inputSelector = mkSelector "input"

-- | @Selector@ for @battery@
batterySelector :: Selector '[] (Id GCDeviceBattery)
batterySelector = mkSelector "battery"

-- | @Selector@ for @physicalInputProfile@
physicalInputProfileSelector :: Selector '[] (Id GCPhysicalInputProfile)
physicalInputProfileSelector = mkSelector "physicalInputProfile"

-- | @Selector@ for @gamepad@
gamepadSelector :: Selector '[] (Id GCGamepad)
gamepadSelector = mkSelector "gamepad"

-- | @Selector@ for @microGamepad@
microGamepadSelector :: Selector '[] (Id GCMicroGamepad)
microGamepadSelector = mkSelector "microGamepad"

-- | @Selector@ for @extendedGamepad@
extendedGamepadSelector :: Selector '[] (Id GCExtendedGamepad)
extendedGamepadSelector = mkSelector "extendedGamepad"

-- | @Selector@ for @motion@
motionSelector :: Selector '[] (Id GCMotion)
motionSelector = mkSelector "motion"

-- | @Selector@ for @light@
lightSelector :: Selector '[] (Id GCDeviceLight)
lightSelector = mkSelector "light"

-- | @Selector@ for @haptics@
hapticsSelector :: Selector '[] (Id GCDeviceHaptics)
hapticsSelector = mkSelector "haptics"

-- | @Selector@ for @snapshot@
snapshotSelector :: Selector '[] Bool
snapshotSelector = mkSelector "snapshot"

