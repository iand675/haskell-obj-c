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
  , objectForKeyedSubscriptSelector
  , captureSelector
  , setStateFromPhysicalInputSelector
  , mappedElementAliasForPhysicalInputNameSelector
  , mappedPhysicalInputNamesForElementAliasSelector
  , deviceSelector
  , lastEventTimestampSelector
  , hasRemappedElementsSelector
  , valueDidChangeHandlerSelector
  , setValueDidChangeHandlerSelector
  , elementsSelector
  , buttonsSelector
  , axesSelector
  , dpadsSelector
  , touchpadsSelector
  , allElementsSelector
  , allButtonsSelector
  , allAxesSelector
  , allDpadsSelector
  , allTouchpadsSelector


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
objectForKeyedSubscript gcPhysicalInputProfile  key =
  withObjCPtr key $ \raw_key ->
      sendMsg gcPhysicalInputProfile (mkSelector "objectForKeyedSubscript:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

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
capture gcPhysicalInputProfile  =
    sendMsg gcPhysicalInputProfile (mkSelector "capture") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the state vector of the physical input profile to a copy of the passed in physical input profile's state vector.
--
-- Note: If the controller's snapshot flag is set to NO, this method has no effect.
--
-- See: GCController.snapshot
--
-- ObjC selector: @- setStateFromPhysicalInput:@
setStateFromPhysicalInput :: (IsGCPhysicalInputProfile gcPhysicalInputProfile, IsGCPhysicalInputProfile physicalInput) => gcPhysicalInputProfile -> physicalInput -> IO ()
setStateFromPhysicalInput gcPhysicalInputProfile  physicalInput =
  withObjCPtr physicalInput $ \raw_physicalInput ->
      sendMsg gcPhysicalInputProfile (mkSelector "setStateFromPhysicalInput:") retVoid [argPtr (castPtr raw_physicalInput :: Ptr ())]

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
mappedElementAliasForPhysicalInputName gcPhysicalInputProfile  inputName =
  withObjCPtr inputName $ \raw_inputName ->
      sendMsg gcPhysicalInputProfile (mkSelector "mappedElementAliasForPhysicalInputName:") (retPtr retVoid) [argPtr (castPtr raw_inputName :: Ptr ())] >>= retainedObject . castPtr

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
mappedPhysicalInputNamesForElementAlias gcPhysicalInputProfile  elementAlias =
  withObjCPtr elementAlias $ \raw_elementAlias ->
      sendMsg gcPhysicalInputProfile (mkSelector "mappedPhysicalInputNamesForElementAlias:") (retPtr retVoid) [argPtr (castPtr raw_elementAlias :: Ptr ())] >>= retainedObject . castPtr

-- | A profile keeps a reference to the device that this profile is mapping input from
--
-- ObjC selector: @- device@
device :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO RawId
device gcPhysicalInputProfile  =
    fmap (RawId . castPtr) $ sendMsg gcPhysicalInputProfile (mkSelector "device") (retPtr retVoid) []

-- | The last time elements of this profile were updated.
--
-- ObjC selector: @- lastEventTimestamp@
lastEventTimestamp :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO CDouble
lastEventTimestamp gcPhysicalInputProfile  =
    sendMsg gcPhysicalInputProfile (mkSelector "lastEventTimestamp") retCDouble []

-- | Whether the user has remapped their physical input controls for this profile at the system level.
--
-- On iOS and tvOS, users can remap their game controller inputs in Settings.
--
-- ObjC selector: @- hasRemappedElements@
hasRemappedElements :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO Bool
hasRemappedElements gcPhysicalInputProfile  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg gcPhysicalInputProfile (mkSelector "hasRemappedElements") retCULong []

-- | Set this block if you want to be notified when a value on a element changed. If multiple elements have changed this block will be called for each element that changed.
--
-- @profile@ — this profile that is being used to map the raw input data into logical values on controller elements such as the dpad or the buttons.
--
-- @element@ — the element that has been modified.
--
-- ObjC selector: @- valueDidChangeHandler@
valueDidChangeHandler :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Ptr ())
valueDidChangeHandler gcPhysicalInputProfile  =
    fmap castPtr $ sendMsg gcPhysicalInputProfile (mkSelector "valueDidChangeHandler") (retPtr retVoid) []

-- | Set this block if you want to be notified when a value on a element changed. If multiple elements have changed this block will be called for each element that changed.
--
-- @profile@ — this profile that is being used to map the raw input data into logical values on controller elements such as the dpad or the buttons.
--
-- @element@ — the element that has been modified.
--
-- ObjC selector: @- setValueDidChangeHandler:@
setValueDidChangeHandler :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> Ptr () -> IO ()
setValueDidChangeHandler gcPhysicalInputProfile  value =
    sendMsg gcPhysicalInputProfile (mkSelector "setValueDidChangeHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

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
elements gcPhysicalInputProfile  =
    sendMsg gcPhysicalInputProfile (mkSelector "elements") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- buttons@
buttons :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Id NSDictionary)
buttons gcPhysicalInputProfile  =
    sendMsg gcPhysicalInputProfile (mkSelector "buttons") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- axes@
axes :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Id NSDictionary)
axes gcPhysicalInputProfile  =
    sendMsg gcPhysicalInputProfile (mkSelector "axes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dpads@
dpads :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Id NSDictionary)
dpads gcPhysicalInputProfile  =
    sendMsg gcPhysicalInputProfile (mkSelector "dpads") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- touchpads@
touchpads :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Id NSDictionary)
touchpads gcPhysicalInputProfile  =
    sendMsg gcPhysicalInputProfile (mkSelector "touchpads") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The following properties allow for dynamic querying of the input elements available on a profile.
--
-- ObjC selector: @- allElements@
allElements :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Id NSSet)
allElements gcPhysicalInputProfile  =
    sendMsg gcPhysicalInputProfile (mkSelector "allElements") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- allButtons@
allButtons :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Id NSSet)
allButtons gcPhysicalInputProfile  =
    sendMsg gcPhysicalInputProfile (mkSelector "allButtons") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- allAxes@
allAxes :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Id NSSet)
allAxes gcPhysicalInputProfile  =
    sendMsg gcPhysicalInputProfile (mkSelector "allAxes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- allDpads@
allDpads :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Id NSSet)
allDpads gcPhysicalInputProfile  =
    sendMsg gcPhysicalInputProfile (mkSelector "allDpads") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- allTouchpads@
allTouchpads :: IsGCPhysicalInputProfile gcPhysicalInputProfile => gcPhysicalInputProfile -> IO (Id NSSet)
allTouchpads gcPhysicalInputProfile  =
    sendMsg gcPhysicalInputProfile (mkSelector "allTouchpads") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @capture@
captureSelector :: Selector
captureSelector = mkSelector "capture"

-- | @Selector@ for @setStateFromPhysicalInput:@
setStateFromPhysicalInputSelector :: Selector
setStateFromPhysicalInputSelector = mkSelector "setStateFromPhysicalInput:"

-- | @Selector@ for @mappedElementAliasForPhysicalInputName:@
mappedElementAliasForPhysicalInputNameSelector :: Selector
mappedElementAliasForPhysicalInputNameSelector = mkSelector "mappedElementAliasForPhysicalInputName:"

-- | @Selector@ for @mappedPhysicalInputNamesForElementAlias:@
mappedPhysicalInputNamesForElementAliasSelector :: Selector
mappedPhysicalInputNamesForElementAliasSelector = mkSelector "mappedPhysicalInputNamesForElementAlias:"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

-- | @Selector@ for @lastEventTimestamp@
lastEventTimestampSelector :: Selector
lastEventTimestampSelector = mkSelector "lastEventTimestamp"

-- | @Selector@ for @hasRemappedElements@
hasRemappedElementsSelector :: Selector
hasRemappedElementsSelector = mkSelector "hasRemappedElements"

-- | @Selector@ for @valueDidChangeHandler@
valueDidChangeHandlerSelector :: Selector
valueDidChangeHandlerSelector = mkSelector "valueDidChangeHandler"

-- | @Selector@ for @setValueDidChangeHandler:@
setValueDidChangeHandlerSelector :: Selector
setValueDidChangeHandlerSelector = mkSelector "setValueDidChangeHandler:"

-- | @Selector@ for @elements@
elementsSelector :: Selector
elementsSelector = mkSelector "elements"

-- | @Selector@ for @buttons@
buttonsSelector :: Selector
buttonsSelector = mkSelector "buttons"

-- | @Selector@ for @axes@
axesSelector :: Selector
axesSelector = mkSelector "axes"

-- | @Selector@ for @dpads@
dpadsSelector :: Selector
dpadsSelector = mkSelector "dpads"

-- | @Selector@ for @touchpads@
touchpadsSelector :: Selector
touchpadsSelector = mkSelector "touchpads"

-- | @Selector@ for @allElements@
allElementsSelector :: Selector
allElementsSelector = mkSelector "allElements"

-- | @Selector@ for @allButtons@
allButtonsSelector :: Selector
allButtonsSelector = mkSelector "allButtons"

-- | @Selector@ for @allAxes@
allAxesSelector :: Selector
allAxesSelector = mkSelector "allAxes"

-- | @Selector@ for @allDpads@
allDpadsSelector :: Selector
allDpadsSelector = mkSelector "allDpads"

-- | @Selector@ for @allTouchpads@
allTouchpadsSelector :: Selector
allTouchpadsSelector = mkSelector "allTouchpads"

