{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A game controller profile representing physical buttons, thumbsticks, dpads, etc... on a controller.
--
-- All controller profiles provide a base level of information about the controller they belong to.
--
-- A profile maps the hardware notion of a controller into a logical controller. One that a developer can design forand depend on, no matter the underlying hardware.
--
-- Generated bindings for @GCPhysicalInputProfile@.
module ObjC.GameController.GCPhysicalInputProfile
  ( GCPhysicalInputProfile
  , IsGCPhysicalInputProfile(..)
  , objectForKeyedSubscript
  , capture
  , setStateFromPhysicalInput
  , mappedElementAliasForPhysicalInputName
  , mappedPhysicalInputNamesForElementAlias
  , device
  , lastEventTimestamp
  , hasRemappedElements
  , valueDidChangeHandler
  , setValueDidChangeHandler
  , elements
  , buttons
  , axes
  , dpads
  , touchpads
  , allElements
  , allButtons
  , allAxes
  , allDpads
  , allTouchpads
  , allAxesSelector
  , allButtonsSelector
  , allDpadsSelector
  , allElementsSelector
  , allTouchpadsSelector
  , axesSelector
  , buttonsSelector
  , captureSelector
  , deviceSelector
  , dpadsSelector
  , elementsSelector
  , hasRemappedElementsSelector
  , lastEventTimestampSelector
  , mappedElementAliasForPhysicalInputNameSelector
  , mappedPhysicalInputNamesForElementAliasSelector
  , objectForKeyedSubscriptSelector
  , setStateFromPhysicalInputSelector
  , setValueDidChangeHandlerSelector
  , touchpadsSelector
  , valueDidChangeHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Profile elements can be accessed using keyed subscript notation, with a valid alias of its inputs.
--
-- extendedGamepad["Button A"] == extendedGamepad.buttonA // YES
--
-- microGamepad["Button X"] == microGamepad.buttonX // YES
--
-- Note: Equivalent to -elements
--
-- ObjC selector: @- objectForKeyedSubscript:@
objectForKeyedSubscript :: (IsGCPhysicalInputProfile gcPhysicalInputProfile, IsNSString key) => gcPhysicalInputProfile -> key -> IO (Id GCControllerElement)
objectForKeyedSubscript gcPhysicalInputProfile key =
  sendMessage gcPhysicalInputProfile objectForKeyedSubscriptSelector (toNSString key)

-- | Polls the state vector of the physical input input and saves it to a new and writable instance of GCPhysicalInputProfile.
--
-- If your application is heavily multithreaded this may also be useful to guarantee atomicity of input handling as a snapshot will not change based on user input once it is taken.
--
-- See: snapshot
--
-- Returns: A new physical input profile with the duplicated state vector of the current physical input
--
-- ObjC selector: @- capture@
capture :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Id GCPhysicalInputProfile)
capture gcPhysicalInputProfile =
  sendMessage gcPhysicalInputProfile captureSelector

-- | Sets the state vector of the physical input profile to a copy of the passed in physical input profile's state vector.
--
-- Note: If the controller's snapshot flag is set to NO, this method has no effect.
--
-- See: GCController.snapshot
--
-- ObjC selector: @- setStateFromPhysicalInput:@
setStateFromPhysicalInput :: (IsGCPhysicalInputProfile gcPhysicalInputProfile, IsGCPhysicalInputProfile physicalInput) => gcPhysicalInputProfile -> physicalInput -> IO ()
setStateFromPhysicalInput gcPhysicalInputProfile physicalInput =
  sendMessage gcPhysicalInputProfile setStateFromPhysicalInputSelector (toGCPhysicalInputProfile physicalInput)

-- | Returns the primary alias of the GCControllerElement that a given physical input maps to.
--
-- If the user were to map a physical press of the A button of their game controller to the B button, then    -[GCPhysicalInputProfile  mappedElementAliasForPhysicalInputName: GCInputButtonA] would return GCInputButtonB.    Note that mappings can change anytime your app is backgrounded, so make sure you update any relevant visuals when    returning to foreground.
--
-- @inputName@ — A GCInput string corresponding to the physical button you want the mapped element alias for.
--
-- A GCInput string corresponding to the primary alias of the GCControllerElement that a given physical button maps to, or nil if there is no mapping.
--
-- ObjC selector: @- mappedElementAliasForPhysicalInputName:@
mappedElementAliasForPhysicalInputName :: (IsGCPhysicalInputProfile gcPhysicalInputProfile, IsNSString inputName) => gcPhysicalInputProfile -> inputName -> IO (Id NSString)
mappedElementAliasForPhysicalInputName gcPhysicalInputProfile inputName =
  sendMessage gcPhysicalInputProfile mappedElementAliasForPhysicalInputNameSelector (toNSString inputName)

-- | Returns a set of GCInput strings corresponding to physical inputs that are mapped to a given GCControllerElement.
--
-- If the user mapped the physical press of the A button, the B button, and the X button to the B button, then    -[GCPhysicalInputProfile mappedPhysicalInputNamesForElementAlias: GCInputButtonB] would return  [GCInputButtonA, GCInputButtonB, GCInputButtonX].    Note that mappings can change anytime your app is backgrounded, so make sure you update any relevant visuals when    returning to foreground.
--
-- @elementAlias@ — A GCInput string corresponding to an alias of the GCControllerElement you want the physical buttons for.
--
-- A set of GCInput strings corresponding to physical inputs that are mapped to a given GCControllerElement, or an empty set if there are no mappings.
--
-- ObjC selector: @- mappedPhysicalInputNamesForElementAlias:@
mappedPhysicalInputNamesForElementAlias :: (IsGCPhysicalInputProfile gcPhysicalInputProfile, IsNSString elementAlias) => gcPhysicalInputProfile -> elementAlias -> IO (Id NSSet)
mappedPhysicalInputNamesForElementAlias gcPhysicalInputProfile elementAlias =
  sendMessage gcPhysicalInputProfile mappedPhysicalInputNamesForElementAliasSelector (toNSString elementAlias)

-- | A profile keeps a reference to the device that this profile is mapping input from
--
-- ObjC selector: @- device@
device :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO RawId
device gcPhysicalInputProfile =
  sendMessage gcPhysicalInputProfile deviceSelector

-- | The last time elements of this profile were updated.
--
-- ObjC selector: @- lastEventTimestamp@
lastEventTimestamp :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO CDouble
lastEventTimestamp gcPhysicalInputProfile =
  sendMessage gcPhysicalInputProfile lastEventTimestampSelector

-- | Whether the user has remapped their physical input controls for this profile at the system level.
--
-- On iOS and tvOS, users can remap their game controller inputs in Settings.
--
-- ObjC selector: @- hasRemappedElements@
hasRemappedElements :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO Bool
hasRemappedElements gcPhysicalInputProfile =
  sendMessage gcPhysicalInputProfile hasRemappedElementsSelector

-- | Set this block if you want to be notified when a value on a element changed. If multiple elements have changed this block will be called for each element that changed.
--
-- @profile@ — this profile that is being used to map the raw input data into logical values on controller elements such as the dpad or the buttons.
--
-- @element@ — the element that has been modified.
--
-- ObjC selector: @- valueDidChangeHandler@
valueDidChangeHandler :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Ptr ())
valueDidChangeHandler gcPhysicalInputProfile =
  sendMessage gcPhysicalInputProfile valueDidChangeHandlerSelector

-- | Set this block if you want to be notified when a value on a element changed. If multiple elements have changed this block will be called for each element that changed.
--
-- @profile@ — this profile that is being used to map the raw input data into logical values on controller elements such as the dpad or the buttons.
--
-- @element@ — the element that has been modified.
--
-- ObjC selector: @- setValueDidChangeHandler:@
setValueDidChangeHandler :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> Ptr () -> IO ()
setValueDidChangeHandler gcPhysicalInputProfile value =
  sendMessage gcPhysicalInputProfile setValueDidChangeHandlerSelector value

-- | The following properties allow for runtime lookup of any input element on a profile, when provided with a valid alias.
--
-- extendedGamepad.elements["Button A"] == extendedGamepad.buttonA // YES
--
-- extendedGamepad.dpads["Left Thumbstick"] == extendedGamepad.leftThumbstick // YES
--
-- extendedGamepad.dpads["Button B"] // returns nil, "Button B" is not a GCControllerDirectionPad
--
-- ObjC selector: @- elements@
elements :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Id NSDictionary)
elements gcPhysicalInputProfile =
  sendMessage gcPhysicalInputProfile elementsSelector

-- | @- buttons@
buttons :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Id NSDictionary)
buttons gcPhysicalInputProfile =
  sendMessage gcPhysicalInputProfile buttonsSelector

-- | @- axes@
axes :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Id NSDictionary)
axes gcPhysicalInputProfile =
  sendMessage gcPhysicalInputProfile axesSelector

-- | @- dpads@
dpads :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Id NSDictionary)
dpads gcPhysicalInputProfile =
  sendMessage gcPhysicalInputProfile dpadsSelector

-- | @- touchpads@
touchpads :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Id NSDictionary)
touchpads gcPhysicalInputProfile =
  sendMessage gcPhysicalInputProfile touchpadsSelector

-- | The following properties allow for dynamic querying of the input elements available on a profile.
--
-- ObjC selector: @- allElements@
allElements :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Id NSSet)
allElements gcPhysicalInputProfile =
  sendMessage gcPhysicalInputProfile allElementsSelector

-- | @- allButtons@
allButtons :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Id NSSet)
allButtons gcPhysicalInputProfile =
  sendMessage gcPhysicalInputProfile allButtonsSelector

-- | @- allAxes@
allAxes :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Id NSSet)
allAxes gcPhysicalInputProfile =
  sendMessage gcPhysicalInputProfile allAxesSelector

-- | @- allDpads@
allDpads :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Id NSSet)
allDpads gcPhysicalInputProfile =
  sendMessage gcPhysicalInputProfile allDpadsSelector

-- | @- allTouchpads@
allTouchpads :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Id NSSet)
allTouchpads gcPhysicalInputProfile =
  sendMessage gcPhysicalInputProfile allTouchpadsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector '[Id NSString] (Id GCControllerElement)
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @capture@
captureSelector :: Selector '[] (Id GCPhysicalInputProfile)
captureSelector = mkSelector "capture"

-- | @Selector@ for @setStateFromPhysicalInput:@
setStateFromPhysicalInputSelector :: Selector '[Id GCPhysicalInputProfile] ()
setStateFromPhysicalInputSelector = mkSelector "setStateFromPhysicalInput:"

-- | @Selector@ for @mappedElementAliasForPhysicalInputName:@
mappedElementAliasForPhysicalInputNameSelector :: Selector '[Id NSString] (Id NSString)
mappedElementAliasForPhysicalInputNameSelector = mkSelector "mappedElementAliasForPhysicalInputName:"

-- | @Selector@ for @mappedPhysicalInputNamesForElementAlias:@
mappedPhysicalInputNamesForElementAliasSelector :: Selector '[Id NSString] (Id NSSet)
mappedPhysicalInputNamesForElementAliasSelector = mkSelector "mappedPhysicalInputNamesForElementAlias:"

-- | @Selector@ for @device@
deviceSelector :: Selector '[] RawId
deviceSelector = mkSelector "device"

-- | @Selector@ for @lastEventTimestamp@
lastEventTimestampSelector :: Selector '[] CDouble
lastEventTimestampSelector = mkSelector "lastEventTimestamp"

-- | @Selector@ for @hasRemappedElements@
hasRemappedElementsSelector :: Selector '[] Bool
hasRemappedElementsSelector = mkSelector "hasRemappedElements"

-- | @Selector@ for @valueDidChangeHandler@
valueDidChangeHandlerSelector :: Selector '[] (Ptr ())
valueDidChangeHandlerSelector = mkSelector "valueDidChangeHandler"

-- | @Selector@ for @setValueDidChangeHandler:@
setValueDidChangeHandlerSelector :: Selector '[Ptr ()] ()
setValueDidChangeHandlerSelector = mkSelector "setValueDidChangeHandler:"

-- | @Selector@ for @elements@
elementsSelector :: Selector '[] (Id NSDictionary)
elementsSelector = mkSelector "elements"

-- | @Selector@ for @buttons@
buttonsSelector :: Selector '[] (Id NSDictionary)
buttonsSelector = mkSelector "buttons"

-- | @Selector@ for @axes@
axesSelector :: Selector '[] (Id NSDictionary)
axesSelector = mkSelector "axes"

-- | @Selector@ for @dpads@
dpadsSelector :: Selector '[] (Id NSDictionary)
dpadsSelector = mkSelector "dpads"

-- | @Selector@ for @touchpads@
touchpadsSelector :: Selector '[] (Id NSDictionary)
touchpadsSelector = mkSelector "touchpads"

-- | @Selector@ for @allElements@
allElementsSelector :: Selector '[] (Id NSSet)
allElementsSelector = mkSelector "allElements"

-- | @Selector@ for @allButtons@
allButtonsSelector :: Selector '[] (Id NSSet)
allButtonsSelector = mkSelector "allButtons"

-- | @Selector@ for @allAxes@
allAxesSelector :: Selector '[] (Id NSSet)
allAxesSelector = mkSelector "allAxes"

-- | @Selector@ for @allDpads@
allDpadsSelector :: Selector '[] (Id NSSet)
allDpadsSelector = mkSelector "allDpads"

-- | @Selector@ for @allTouchpads@
allTouchpadsSelector :: Selector '[] (Id NSSet)
allTouchpadsSelector = mkSelector "allTouchpads"

