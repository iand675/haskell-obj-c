{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.GameController.Internal.Classes (
    module ObjC.GameController.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.CoreHaptics.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.CoreHaptics.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- GCColor ----------

-- | Represents a color used by a GCDeviceLight.
--
-- See: GCDeviceLight
-- 
-- Phantom type for @GCColor@.
data GCColor

instance IsObjCObject (Id GCColor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCColor"

class IsNSObject a => IsGCColor a where
  toGCColor :: a -> Id GCColor

instance IsGCColor (Id GCColor) where
  toGCColor = unsafeCastId

instance IsNSObject (Id GCColor) where
  toNSObject = unsafeCastId

-- ---------- GCController ----------

-- | Controllers are available to an application that links to GameController.framework. There are 2 ways to access controllers paired to the system, adopt both to ensure the best user experience:
--
-- 1: Querying for the the current array or controllers using [GCController controllers]. 2: Registering for Connection/Disconnection notifications from NSNotificationCenter.
--
-- Only controllers that support one of the allowed profiles, such as GCExtendedGamepad, will be enumerated. Check for the profile supported before using a controller in your application. Ignore a controller that doesn't support a profile that suits your application, as the user will expect their controller to either be fully supported or not supported at all.
-- 
-- Phantom type for @GCController@.
data GCController

instance IsObjCObject (Id GCController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCController"

class IsNSObject a => IsGCController a where
  toGCController :: a -> Id GCController

instance IsGCController (Id GCController) where
  toGCController = unsafeCastId

instance IsNSObject (Id GCController) where
  toNSObject = unsafeCastId

-- ---------- GCControllerElement ----------

-- | Every controller element knows which collection it belongs to and whether its input value is analog or digital.
-- 
-- Phantom type for @GCControllerElement@.
data GCControllerElement

instance IsObjCObject (Id GCControllerElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCControllerElement"

class IsNSObject a => IsGCControllerElement a where
  toGCControllerElement :: a -> Id GCControllerElement

instance IsGCControllerElement (Id GCControllerElement) where
  toGCControllerElement = unsafeCastId

instance IsNSObject (Id GCControllerElement) where
  toNSObject = unsafeCastId

-- ---------- GCControllerInputState ----------

-- | Phantom type for @GCControllerInputState@.
data GCControllerInputState

instance IsObjCObject (Id GCControllerInputState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCControllerInputState"

class IsNSObject a => IsGCControllerInputState a where
  toGCControllerInputState :: a -> Id GCControllerInputState

instance IsGCControllerInputState (Id GCControllerInputState) where
  toGCControllerInputState = unsafeCastId

instance IsNSObject (Id GCControllerInputState) where
  toNSObject = unsafeCastId

-- ---------- GCDeviceBattery ----------

-- | A controller battery is an abstract representation of the battery level and battery status of a GCController instance.
-- 
-- Phantom type for @GCDeviceBattery@.
data GCDeviceBattery

instance IsObjCObject (Id GCDeviceBattery) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCDeviceBattery"

class IsNSObject a => IsGCDeviceBattery a where
  toGCDeviceBattery :: a -> Id GCDeviceBattery

instance IsGCDeviceBattery (Id GCDeviceBattery) where
  toGCDeviceBattery = unsafeCastId

instance IsNSObject (Id GCDeviceBattery) where
  toNSObject = unsafeCastId

-- ---------- GCDeviceHaptics ----------

-- | Phantom type for @GCDeviceHaptics@.
data GCDeviceHaptics

instance IsObjCObject (Id GCDeviceHaptics) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCDeviceHaptics"

class IsNSObject a => IsGCDeviceHaptics a where
  toGCDeviceHaptics :: a -> Id GCDeviceHaptics

instance IsGCDeviceHaptics (Id GCDeviceHaptics) where
  toGCDeviceHaptics = unsafeCastId

instance IsNSObject (Id GCDeviceHaptics) where
  toNSObject = unsafeCastId

-- ---------- GCDeviceLight ----------

-- | A controller light is an abstract representation of the light-emitting capabilities of a GCController instance.
-- 
-- Phantom type for @GCDeviceLight@.
data GCDeviceLight

instance IsObjCObject (Id GCDeviceLight) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCDeviceLight"

class IsNSObject a => IsGCDeviceLight a where
  toGCDeviceLight :: a -> Id GCDeviceLight

instance IsGCDeviceLight (Id GCDeviceLight) where
  toGCDeviceLight = unsafeCastId

instance IsNSObject (Id GCDeviceLight) where
  toNSObject = unsafeCastId

-- ---------- GCGearShifterElement ----------

-- | A @GCGearShifterElement@ object represents an attached gear shifter.  Both pattern and sequential gear shifters are supported.
-- 
-- Phantom type for @GCGearShifterElement@.
data GCGearShifterElement

instance IsObjCObject (Id GCGearShifterElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCGearShifterElement"

class IsNSObject a => IsGCGearShifterElement a where
  toGCGearShifterElement :: a -> Id GCGearShifterElement

instance IsGCGearShifterElement (Id GCGearShifterElement) where
  toGCGearShifterElement = unsafeCastId

instance IsNSObject (Id GCGearShifterElement) where
  toNSObject = unsafeCastId

-- ---------- GCKeyboard ----------

-- | GCKeyboard is available to an application that links to GameController.framework There are 2 ways to access keyboard paired to the system: 1: Querying for the coalescedKeyboard using [GCKeyboard coalescedKeyboard] 2: Registering for Connection/Disconnection notifications from NSNotificationCenter
--
-- Note: All connected keyboards are coalesced into one keyboard object, so notification about connection/disconnection will only be delivered once.
-- 
-- Phantom type for @GCKeyboard@.
data GCKeyboard

instance IsObjCObject (Id GCKeyboard) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCKeyboard"

class IsNSObject a => IsGCKeyboard a where
  toGCKeyboard :: a -> Id GCKeyboard

instance IsGCKeyboard (Id GCKeyboard) where
  toGCKeyboard = unsafeCastId

instance IsNSObject (Id GCKeyboard) where
  toNSObject = unsafeCastId

-- ---------- GCMotion ----------

-- | A profile for getting motion input from a controller that has the ability to measure acceleration and rotation rate.
--
-- You check for the availablity of motion inputs by getting the motion property of a controller. If that returns a nil value; motion is not available. A non-nil value is a valid GCMotion profile that is able to provide motion input.
--
-- See: GCController.motion
-- 
-- Phantom type for @GCMotion@.
data GCMotion

instance IsObjCObject (Id GCMotion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCMotion"

class IsNSObject a => IsGCMotion a where
  toGCMotion :: a -> Id GCMotion

instance IsGCMotion (Id GCMotion) where
  toGCMotion = unsafeCastId

instance IsNSObject (Id GCMotion) where
  toNSObject = unsafeCastId

-- ---------- GCMouse ----------

-- | Mice are available to an application that links to GameController.framework. There are 2 ways to access mice paired to the system. Adopt both to ensure the best user experience:
--
-- 1: Querying for the current array of mice using [GCMouse mice] 2: Registering for Connection/Disconnection notifications from NSNotificationCenter.
-- 
-- Phantom type for @GCMouse@.
data GCMouse

instance IsObjCObject (Id GCMouse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCMouse"

class IsNSObject a => IsGCMouse a where
  toGCMouse :: a -> Id GCMouse

instance IsGCMouse (Id GCMouse) where
  toGCMouse = unsafeCastId

instance IsNSObject (Id GCMouse) where
  toNSObject = unsafeCastId

-- ---------- GCPhysicalInputElementCollection ----------

-- | An instance of @GCPhysicalInputElementCollection@ contains the collection of input elements found in a device's physical input profile.
-- 
-- Phantom type for @GCPhysicalInputElementCollection@.
data GCPhysicalInputElementCollection

instance IsObjCObject (Id GCPhysicalInputElementCollection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCPhysicalInputElementCollection"

class IsNSObject a => IsGCPhysicalInputElementCollection a where
  toGCPhysicalInputElementCollection :: a -> Id GCPhysicalInputElementCollection

instance IsGCPhysicalInputElementCollection (Id GCPhysicalInputElementCollection) where
  toGCPhysicalInputElementCollection = unsafeCastId

instance IsNSObject (Id GCPhysicalInputElementCollection) where
  toNSObject = unsafeCastId

-- ---------- GCPhysicalInputProfile ----------

-- | A game controller profile representing physical buttons, thumbsticks, dpads, etc... on a controller.
--
-- All controller profiles provide a base level of information about the controller they belong to.
--
-- A profile maps the hardware notion of a controller into a logical controller. One that a developer can design forand depend on, no matter the underlying hardware.
-- 
-- Phantom type for @GCPhysicalInputProfile@.
data GCPhysicalInputProfile

instance IsObjCObject (Id GCPhysicalInputProfile) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCPhysicalInputProfile"

class IsNSObject a => IsGCPhysicalInputProfile a where
  toGCPhysicalInputProfile :: a -> Id GCPhysicalInputProfile

instance IsGCPhysicalInputProfile (Id GCPhysicalInputProfile) where
  toGCPhysicalInputProfile = unsafeCastId

instance IsNSObject (Id GCPhysicalInputProfile) where
  toNSObject = unsafeCastId

-- ---------- GCRacingWheel ----------

-- | Phantom type for @GCRacingWheel@.
data GCRacingWheel

instance IsObjCObject (Id GCRacingWheel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCRacingWheel"

class IsNSObject a => IsGCRacingWheel a where
  toGCRacingWheel :: a -> Id GCRacingWheel

instance IsGCRacingWheel (Id GCRacingWheel) where
  toGCRacingWheel = unsafeCastId

instance IsNSObject (Id GCRacingWheel) where
  toNSObject = unsafeCastId

-- ---------- GCRacingWheelInputState ----------

-- | Phantom type for @GCRacingWheelInputState@.
data GCRacingWheelInputState

instance IsObjCObject (Id GCRacingWheelInputState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCRacingWheelInputState"

class IsNSObject a => IsGCRacingWheelInputState a where
  toGCRacingWheelInputState :: a -> Id GCRacingWheelInputState

instance IsGCRacingWheelInputState (Id GCRacingWheelInputState) where
  toGCRacingWheelInputState = unsafeCastId

instance IsNSObject (Id GCRacingWheelInputState) where
  toNSObject = unsafeCastId

-- ---------- GCSteeringWheelElement ----------

-- | Phantom type for @GCSteeringWheelElement@.
data GCSteeringWheelElement

instance IsObjCObject (Id GCSteeringWheelElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCSteeringWheelElement"

class IsNSObject a => IsGCSteeringWheelElement a where
  toGCSteeringWheelElement :: a -> Id GCSteeringWheelElement

instance IsGCSteeringWheelElement (Id GCSteeringWheelElement) where
  toGCSteeringWheelElement = unsafeCastId

instance IsNSObject (Id GCSteeringWheelElement) where
  toNSObject = unsafeCastId

-- ---------- GCStylus ----------

-- | An object that represents a physical stylus connected to the device.
--
-- Use the @styli@ property to get the currently connect stylus accessories  when your application starts.  Register for @GCStylusDidConnectNotification@  and @GCStylusDidDisconnectNotification@ to get notified when a stylus  connects of disconnects while your application is running.
--
-- ```  // Register for notifications  NotificationCenter.default.addObserver(self, selector: #selector(stylus(didConnect:)), name: NSNotification.Name.GCStylusDidConnect, object: nil)  NotificationCenter.default.addObserver(self, selector: #selector(stylus(didDisconnect:)), name: NSNotification.Name.GCStylusDidConnect, object: nil)
--
-- // Query current stylus devices  for stylus in GCStylus.styluses {      ...  }
--
-- // Later, handle connection   func stylus(didConnect notification: Notification) {      guard let stylus = notification.object as? GCStylus else { return }      ...  }  ```
--
-- Check the @productCategory@ to determine the type of stylus.  A spatial  stylus - capable of 6DoF tracking by Apple Vision Pro - has a  @GCProductCategorySpatialStylus@ category.
--
-- Use the @input@ property to get the input profile of the stylus.  A spatial  stylus includes a pressure sensitive tip and an input cluster composed of  two buttons.
--
-- - The primary button (@GCInputStylusPrimaryButton@) is the front button      (closest to the stylus tip) in the input cluster of the stylus.  This      button is frequently used grab virtual objects.
--
-- - The secondary button (@GCInputStylusSecondaryButton@) is the middle      button in the input cluster.  It can measures pressure/force levels.      It's intended to be used for controlling in-air drawing, selection,      and generic interactions.
--
-- - The tip is also represented as a button (@GCInputStylusTip@).
--
-- ```  guard let input = stylus.input else { return }  input.inputStateQueueDepth = 20  input.inputStateAvailableHandler = { input in      // This block will be enqueued for execution when the state of      // any stylus input changes.
--
-- // Iterate through all input state changes since last execution of      // the block.      while let nextState = input.nextInputState() {          // Use the value of @pressedInput.isPressed@ for binary          // interactions, such as object selection.          let primaryButtonPressed = nextState.buttons[.stylusPrimaryButton]?.pressedInput.isPressed          let secondaryButtonPressed = nextState.buttons[.stylusSecondaryButton]?.pressedInput.isPressed          // Use the normalized press value for analog actions such as          // controlling virtual ink flow.          let secondaryButtonPressure = nextState.buttons[.stylusSecondaryButton]?.pressedInput.value          let tipPressure = nextState.buttons[.stylusTip]?.pressedInput.value
--
-- ...      }  }  ```
--
-- Use the @haptics@ property to get the haptics profile of the stylus.  A  spatial stylus may optionally support haptic feedback to a single  locality - @GCHapticsLocalityDefault@.
-- 
-- Phantom type for @GCStylus@.
data GCStylus

instance IsObjCObject (Id GCStylus) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCStylus"

class IsNSObject a => IsGCStylus a where
  toGCStylus :: a -> Id GCStylus

instance IsGCStylus (Id GCStylus) where
  toGCStylus = unsafeCastId

instance IsNSObject (Id GCStylus) where
  toNSObject = unsafeCastId

-- ---------- GCControllerAxisInput ----------

-- | Phantom type for @GCControllerAxisInput@.
data GCControllerAxisInput

instance IsObjCObject (Id GCControllerAxisInput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCControllerAxisInput"

class IsGCControllerElement a => IsGCControllerAxisInput a where
  toGCControllerAxisInput :: a -> Id GCControllerAxisInput

instance IsGCControllerAxisInput (Id GCControllerAxisInput) where
  toGCControllerAxisInput = unsafeCastId

instance IsGCControllerElement (Id GCControllerAxisInput) where
  toGCControllerElement = unsafeCastId

instance IsNSObject (Id GCControllerAxisInput) where
  toNSObject = unsafeCastId

-- ---------- GCControllerButtonInput ----------

-- | Phantom type for @GCControllerButtonInput@.
data GCControllerButtonInput

instance IsObjCObject (Id GCControllerButtonInput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCControllerButtonInput"

class IsGCControllerElement a => IsGCControllerButtonInput a where
  toGCControllerButtonInput :: a -> Id GCControllerButtonInput

instance IsGCControllerButtonInput (Id GCControllerButtonInput) where
  toGCControllerButtonInput = unsafeCastId

instance IsGCControllerElement (Id GCControllerButtonInput) where
  toGCControllerElement = unsafeCastId

instance IsNSObject (Id GCControllerButtonInput) where
  toNSObject = unsafeCastId

-- ---------- GCControllerDirectionPad ----------

-- | A direction pad is a common grouping of 2 axis inputs where the input can also be interpreted as 2 sets of mutually exclusive button pairs. Only one button in each pair, {up, down} and {left, right}, can be pressed at any one time.
-- 
-- Phantom type for @GCControllerDirectionPad@.
data GCControllerDirectionPad

instance IsObjCObject (Id GCControllerDirectionPad) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCControllerDirectionPad"

class IsGCControllerElement a => IsGCControllerDirectionPad a where
  toGCControllerDirectionPad :: a -> Id GCControllerDirectionPad

instance IsGCControllerDirectionPad (Id GCControllerDirectionPad) where
  toGCControllerDirectionPad = unsafeCastId

instance IsGCControllerElement (Id GCControllerDirectionPad) where
  toGCControllerElement = unsafeCastId

instance IsNSObject (Id GCControllerDirectionPad) where
  toNSObject = unsafeCastId

-- ---------- GCControllerTouchpad ----------

-- | A touchpad is a touch-based two axis input with a notion of "touch state". It keeps track of whether the touchpad is actively being touched, and generates events based on a change in touch state.
-- 
-- Phantom type for @GCControllerTouchpad@.
data GCControllerTouchpad

instance IsObjCObject (Id GCControllerTouchpad) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCControllerTouchpad"

class IsGCControllerElement a => IsGCControllerTouchpad a where
  toGCControllerTouchpad :: a -> Id GCControllerTouchpad

instance IsGCControllerTouchpad (Id GCControllerTouchpad) where
  toGCControllerTouchpad = unsafeCastId

instance IsGCControllerElement (Id GCControllerTouchpad) where
  toGCControllerElement = unsafeCastId

instance IsNSObject (Id GCControllerTouchpad) where
  toNSObject = unsafeCastId

-- ---------- GCControllerLiveInput ----------

-- | Phantom type for @GCControllerLiveInput@.
data GCControllerLiveInput

instance IsObjCObject (Id GCControllerLiveInput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCControllerLiveInput"

class IsGCControllerInputState a => IsGCControllerLiveInput a where
  toGCControllerLiveInput :: a -> Id GCControllerLiveInput

instance IsGCControllerLiveInput (Id GCControllerLiveInput) where
  toGCControllerLiveInput = unsafeCastId

instance IsGCControllerInputState (Id GCControllerLiveInput) where
  toGCControllerInputState = unsafeCastId

instance IsNSObject (Id GCControllerLiveInput) where
  toNSObject = unsafeCastId

-- ---------- GCExtendedGamepad ----------

-- | Phantom type for @GCExtendedGamepad@.
data GCExtendedGamepad

instance IsObjCObject (Id GCExtendedGamepad) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCExtendedGamepad"

class IsGCPhysicalInputProfile a => IsGCExtendedGamepad a where
  toGCExtendedGamepad :: a -> Id GCExtendedGamepad

instance IsGCExtendedGamepad (Id GCExtendedGamepad) where
  toGCExtendedGamepad = unsafeCastId

instance IsGCPhysicalInputProfile (Id GCExtendedGamepad) where
  toGCPhysicalInputProfile = unsafeCastId

instance IsNSObject (Id GCExtendedGamepad) where
  toNSObject = unsafeCastId

-- ---------- GCGamepad ----------

-- | Phantom type for @GCGamepad@.
data GCGamepad

instance IsObjCObject (Id GCGamepad) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCGamepad"

class IsGCPhysicalInputProfile a => IsGCGamepad a where
  toGCGamepad :: a -> Id GCGamepad

instance IsGCGamepad (Id GCGamepad) where
  toGCGamepad = unsafeCastId

instance IsGCPhysicalInputProfile (Id GCGamepad) where
  toGCPhysicalInputProfile = unsafeCastId

instance IsNSObject (Id GCGamepad) where
  toNSObject = unsafeCastId

-- ---------- GCKeyboardInput ----------

-- | Keyboard profile. Contains the current state of buttons specified in GCKeyCodes.h.
--
-- GCKeyboardInput is designed primarly for input polling. For the best text input experience, UIKit/AppKit usage is recommended.
-- 
-- Phantom type for @GCKeyboardInput@.
data GCKeyboardInput

instance IsObjCObject (Id GCKeyboardInput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCKeyboardInput"

class IsGCPhysicalInputProfile a => IsGCKeyboardInput a where
  toGCKeyboardInput :: a -> Id GCKeyboardInput

instance IsGCKeyboardInput (Id GCKeyboardInput) where
  toGCKeyboardInput = unsafeCastId

instance IsGCPhysicalInputProfile (Id GCKeyboardInput) where
  toGCPhysicalInputProfile = unsafeCastId

instance IsNSObject (Id GCKeyboardInput) where
  toNSObject = unsafeCastId

-- ---------- GCMicroGamepad ----------

-- | Phantom type for @GCMicroGamepad@.
data GCMicroGamepad

instance IsObjCObject (Id GCMicroGamepad) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCMicroGamepad"

class IsGCPhysicalInputProfile a => IsGCMicroGamepad a where
  toGCMicroGamepad :: a -> Id GCMicroGamepad

instance IsGCMicroGamepad (Id GCMicroGamepad) where
  toGCMicroGamepad = unsafeCastId

instance IsGCPhysicalInputProfile (Id GCMicroGamepad) where
  toGCPhysicalInputProfile = unsafeCastId

instance IsNSObject (Id GCMicroGamepad) where
  toNSObject = unsafeCastId

-- ---------- GCMouseInput ----------

-- | Mouse profile that represent a physical mouse object with two axis cursor, two axis scroll, left button, optional right and middle buttons and optional set of auxiliary buttons.
--
-- It only provides information about raw mouse movement deltas. For the valid cursor position at given point in time, use UIHoverGestureRecognizer and NSEvent.mouseLocation.
-- 
-- Phantom type for @GCMouseInput@.
data GCMouseInput

instance IsObjCObject (Id GCMouseInput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCMouseInput"

class IsGCPhysicalInputProfile a => IsGCMouseInput a where
  toGCMouseInput :: a -> Id GCMouseInput

instance IsGCMouseInput (Id GCMouseInput) where
  toGCMouseInput = unsafeCastId

instance IsGCPhysicalInputProfile (Id GCMouseInput) where
  toGCPhysicalInputProfile = unsafeCastId

instance IsNSObject (Id GCMouseInput) where
  toNSObject = unsafeCastId

-- ---------- GCRacingWheelInput ----------

-- | Phantom type for @GCRacingWheelInput@.
data GCRacingWheelInput

instance IsObjCObject (Id GCRacingWheelInput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCRacingWheelInput"

class IsGCRacingWheelInputState a => IsGCRacingWheelInput a where
  toGCRacingWheelInput :: a -> Id GCRacingWheelInput

instance IsGCRacingWheelInput (Id GCRacingWheelInput) where
  toGCRacingWheelInput = unsafeCastId

instance IsGCRacingWheelInputState (Id GCRacingWheelInput) where
  toGCRacingWheelInputState = unsafeCastId

instance IsNSObject (Id GCRacingWheelInput) where
  toNSObject = unsafeCastId

-- ---------- GCDualSenseAdaptiveTrigger ----------

-- | DualSense triggers are required to be analog inputs. Common uses would be acceleration and decelleration in a driving game for example.
--
-- GCDualSenseAdaptiveTrigger represents an adaptive trigger on the Sony DualSense controller, allowing you to specify a dynamic resistance force that is applied when pulling the trigger. This can, for example, be used to emulate the feeling of pulling back a bow string, firing a weapon, or pulling a lever.
--
-- See: GCDualSenseGamepad
-- 
-- Phantom type for @GCDualSenseAdaptiveTrigger@.
data GCDualSenseAdaptiveTrigger

instance IsObjCObject (Id GCDualSenseAdaptiveTrigger) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCDualSenseAdaptiveTrigger"

class IsGCControllerButtonInput a => IsGCDualSenseAdaptiveTrigger a where
  toGCDualSenseAdaptiveTrigger :: a -> Id GCDualSenseAdaptiveTrigger

instance IsGCDualSenseAdaptiveTrigger (Id GCDualSenseAdaptiveTrigger) where
  toGCDualSenseAdaptiveTrigger = unsafeCastId

instance IsGCControllerButtonInput (Id GCDualSenseAdaptiveTrigger) where
  toGCControllerButtonInput = unsafeCastId

instance IsGCControllerElement (Id GCDualSenseAdaptiveTrigger) where
  toGCControllerElement = unsafeCastId

instance IsNSObject (Id GCDualSenseAdaptiveTrigger) where
  toNSObject = unsafeCastId

-- ---------- GCDeviceCursor ----------

-- | A cursor is a Direction Pad that has its axis extended from [-1; 1] to [width; height] range Up, down, left, right allows to use mouse to simulate DirectionaPad or Thumbstick since values are normalized for these elements
-- 
-- Phantom type for @GCDeviceCursor@.
data GCDeviceCursor

instance IsObjCObject (Id GCDeviceCursor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCDeviceCursor"

class IsGCControllerDirectionPad a => IsGCDeviceCursor a where
  toGCDeviceCursor :: a -> Id GCDeviceCursor

instance IsGCDeviceCursor (Id GCDeviceCursor) where
  toGCDeviceCursor = unsafeCastId

instance IsGCControllerDirectionPad (Id GCDeviceCursor) where
  toGCControllerDirectionPad = unsafeCastId

instance IsGCControllerElement (Id GCDeviceCursor) where
  toGCControllerElement = unsafeCastId

instance IsNSObject (Id GCDeviceCursor) where
  toNSObject = unsafeCastId

-- ---------- GCDualSenseGamepad ----------

-- | The GCDualSenseGamepad profile represents any supported DualSense controller.
--
-- See: GCExtendedGamepad
--
-- See: GCMotion
-- 
-- Phantom type for @GCDualSenseGamepad@.
data GCDualSenseGamepad

instance IsObjCObject (Id GCDualSenseGamepad) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCDualSenseGamepad"

class IsGCExtendedGamepad a => IsGCDualSenseGamepad a where
  toGCDualSenseGamepad :: a -> Id GCDualSenseGamepad

instance IsGCDualSenseGamepad (Id GCDualSenseGamepad) where
  toGCDualSenseGamepad = unsafeCastId

instance IsGCExtendedGamepad (Id GCDualSenseGamepad) where
  toGCExtendedGamepad = unsafeCastId

instance IsGCPhysicalInputProfile (Id GCDualSenseGamepad) where
  toGCPhysicalInputProfile = unsafeCastId

instance IsNSObject (Id GCDualSenseGamepad) where
  toNSObject = unsafeCastId

-- ---------- GCDualShockGamepad ----------

-- | The GCDualShockGamepad profile represents any supported DualShock 4 controller.
--
-- See: GCExtendedGamepad
--
-- See: GCMotion
-- 
-- Phantom type for @GCDualShockGamepad@.
data GCDualShockGamepad

instance IsObjCObject (Id GCDualShockGamepad) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCDualShockGamepad"

class IsGCExtendedGamepad a => IsGCDualShockGamepad a where
  toGCDualShockGamepad :: a -> Id GCDualShockGamepad

instance IsGCDualShockGamepad (Id GCDualShockGamepad) where
  toGCDualShockGamepad = unsafeCastId

instance IsGCExtendedGamepad (Id GCDualShockGamepad) where
  toGCExtendedGamepad = unsafeCastId

instance IsGCPhysicalInputProfile (Id GCDualShockGamepad) where
  toGCPhysicalInputProfile = unsafeCastId

instance IsNSObject (Id GCDualShockGamepad) where
  toNSObject = unsafeCastId

-- ---------- GCExtendedGamepadSnapshot ----------

-- | A GCExtendedGamepadSnapshot snapshot is a concrete GCExtendedGamepad implementation. It can be used directly in an application to implement controller input replays. It is also returned as the result of polling a controller.
--
-- The current snapshotData is readily available to access as NSData. A developer can serialize this to any destination necessary using the NSData API.
--
-- The data contains some version of a GCExtendedGamepadSnapShotData structure.
--
-- See: -[GCExtendedGamepad saveSnapshot]
-- 
-- Phantom type for @GCExtendedGamepadSnapshot@.
data GCExtendedGamepadSnapshot

instance IsObjCObject (Id GCExtendedGamepadSnapshot) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCExtendedGamepadSnapshot"

class IsGCExtendedGamepad a => IsGCExtendedGamepadSnapshot a where
  toGCExtendedGamepadSnapshot :: a -> Id GCExtendedGamepadSnapshot

instance IsGCExtendedGamepadSnapshot (Id GCExtendedGamepadSnapshot) where
  toGCExtendedGamepadSnapshot = unsafeCastId

instance IsGCExtendedGamepad (Id GCExtendedGamepadSnapshot) where
  toGCExtendedGamepad = unsafeCastId

instance IsGCPhysicalInputProfile (Id GCExtendedGamepadSnapshot) where
  toGCPhysicalInputProfile = unsafeCastId

instance IsNSObject (Id GCExtendedGamepadSnapshot) where
  toNSObject = unsafeCastId

-- ---------- GCXboxGamepad ----------

-- | The GCXboxGamepad profile represents any supported Xbox controller.
--
-- See: GCExtendedGamepad
-- 
-- Phantom type for @GCXboxGamepad@.
data GCXboxGamepad

instance IsObjCObject (Id GCXboxGamepad) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCXboxGamepad"

class IsGCExtendedGamepad a => IsGCXboxGamepad a where
  toGCXboxGamepad :: a -> Id GCXboxGamepad

instance IsGCXboxGamepad (Id GCXboxGamepad) where
  toGCXboxGamepad = unsafeCastId

instance IsGCExtendedGamepad (Id GCXboxGamepad) where
  toGCExtendedGamepad = unsafeCastId

instance IsGCPhysicalInputProfile (Id GCXboxGamepad) where
  toGCPhysicalInputProfile = unsafeCastId

instance IsNSObject (Id GCXboxGamepad) where
  toNSObject = unsafeCastId

-- ---------- GCGamepadSnapshot ----------

-- | A GCGamepadSnapshot snapshot is a concrete GCGamepad implementation. It can be used directly in an application to implement controller input replays. It is also returned as the result of polling a controller.
--
-- The current snapshotData is readily available to access as NSData. A developer can serialize this to any destination necessary using the NSData API.
--
-- The data contains some version of a GCGamepadSnapShotData structure.
--
-- See: -[GCGamepad saveSnapshot]
-- 
-- Phantom type for @GCGamepadSnapshot@.
data GCGamepadSnapshot

instance IsObjCObject (Id GCGamepadSnapshot) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCGamepadSnapshot"

class IsGCGamepad a => IsGCGamepadSnapshot a where
  toGCGamepadSnapshot :: a -> Id GCGamepadSnapshot

instance IsGCGamepadSnapshot (Id GCGamepadSnapshot) where
  toGCGamepadSnapshot = unsafeCastId

instance IsGCGamepad (Id GCGamepadSnapshot) where
  toGCGamepad = unsafeCastId

instance IsGCPhysicalInputProfile (Id GCGamepadSnapshot) where
  toGCPhysicalInputProfile = unsafeCastId

instance IsNSObject (Id GCGamepadSnapshot) where
  toNSObject = unsafeCastId

-- ---------- GCDirectionalGamepad ----------

-- | Directional Gamepad profile.
--
-- All controller profiles provide a base level of information about the controller they belong to. A directional gamepad features a subset of the possible inputs on a micro gamepad. It guarantees:    - The gamepad does not support motion, meaning        - -[GCController motion] is always nil        - -[GCDirectionalGamepad allowsRotation] is always NO
--
-- Additionally, the gamepad may have a digital or analog dpad.        - -[GCDirectionalGamepad dpad].analog may be YES or NO        - If -[GCDirectionalGamepad dpad].analog is NO, then -[GCDirectionalGamepad reportsAbsoluteDpadValues] is always YES
--
-- A profile maps the hardware notion of a controller into a logical controller. One that a developer can design for and depend on, no matter the underlying hardware. If your game supports GCMicroGamepad, but does not need the motion and analog dpad functionality of GCMicroGamepad, be sure to add Directional Gamepad to your project's supported Game Controller capabilities.
--
-- See: GCMicroGamepad
--
-- Note: If you want to use the additional functionality of GCDirectionalGamepad, you should set GCSupportsMultipleMicroGamepads to YES and handle microgamepad connections separately.
--
-- Note: This profile represents the 2021 2nd generation Siri Remote. Make sure you set GCSupportsMultipleMicroGamepads to YES to properly support the remote.
-- 
-- Phantom type for @GCDirectionalGamepad@.
data GCDirectionalGamepad

instance IsObjCObject (Id GCDirectionalGamepad) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCDirectionalGamepad"

class IsGCMicroGamepad a => IsGCDirectionalGamepad a where
  toGCDirectionalGamepad :: a -> Id GCDirectionalGamepad

instance IsGCDirectionalGamepad (Id GCDirectionalGamepad) where
  toGCDirectionalGamepad = unsafeCastId

instance IsGCMicroGamepad (Id GCDirectionalGamepad) where
  toGCMicroGamepad = unsafeCastId

instance IsGCPhysicalInputProfile (Id GCDirectionalGamepad) where
  toGCPhysicalInputProfile = unsafeCastId

instance IsNSObject (Id GCDirectionalGamepad) where
  toNSObject = unsafeCastId

-- ---------- GCMicroGamepadSnapshot ----------

-- | A GCMicroGamepadSnapshot snapshot is a concrete GCMicroGamepad implementation. It can be used directly in an application to implement controller input replays. It is also returned as the result of polling a controller.
--
-- The current snapshotData is readily available to access as NSData. A developer can serialize this to any destination necessary using the NSData API.
--
-- The data contains some version of a GCMicroGamepadSnapShotData structure.
--
-- See: -[GCMicroGamepad saveSnapshot]
-- 
-- Phantom type for @GCMicroGamepadSnapshot@.
data GCMicroGamepadSnapshot

instance IsObjCObject (Id GCMicroGamepadSnapshot) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCMicroGamepadSnapshot"

class IsGCMicroGamepad a => IsGCMicroGamepadSnapshot a where
  toGCMicroGamepadSnapshot :: a -> Id GCMicroGamepadSnapshot

instance IsGCMicroGamepadSnapshot (Id GCMicroGamepadSnapshot) where
  toGCMicroGamepadSnapshot = unsafeCastId

instance IsGCMicroGamepad (Id GCMicroGamepadSnapshot) where
  toGCMicroGamepad = unsafeCastId

instance IsGCPhysicalInputProfile (Id GCMicroGamepadSnapshot) where
  toGCPhysicalInputProfile = unsafeCastId

instance IsNSObject (Id GCMicroGamepadSnapshot) where
  toNSObject = unsafeCastId

-- ---------- GCEventViewController ----------

-- | A view controller subclass that allows fine grained control of the user interface system's handling of game controller events. Set an instance of this class as your root view controller if you intend to use GCController APIs for handling game controllers.
-- 
-- Phantom type for @GCEventViewController@.
data GCEventViewController

instance IsObjCObject (Id GCEventViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GCEventViewController"

class IsNSViewController a => IsGCEventViewController a where
  toGCEventViewController :: a -> Id GCEventViewController

instance IsGCEventViewController (Id GCEventViewController) where
  toGCEventViewController = unsafeCastId

instance IsNSObject (Id GCEventViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id GCEventViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id GCEventViewController) where
  toNSViewController = unsafeCastId
