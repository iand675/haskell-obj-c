{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DRBurn@.
module ObjC.DiscRecording.DRBurn
  ( DRBurn
  , IsDRBurn(..)
  , burnForDevice
  , initWithDevice
  , writeLayout
  , status
  , abort
  , properties
  , setProperties
  , device
  , layoutForImageFile
  , requestedBurnSpeed
  , setRequestedBurnSpeed
  , appendable
  , setAppendable
  , verifyDisc
  , setVerifyDisc
  , completionAction
  , setCompletionAction
  , burnForDeviceSelector
  , initWithDeviceSelector
  , writeLayoutSelector
  , statusSelector
  , abortSelector
  , propertiesSelector
  , setPropertiesSelector
  , deviceSelector
  , layoutForImageFileSelector
  , requestedBurnSpeedSelector
  , setRequestedBurnSpeedSelector
  , appendableSelector
  , setAppendableSelector
  , verifyDiscSelector
  , setVerifyDiscSelector
  , completionActionSelector
  , setCompletionActionSelector


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

import ObjC.DiscRecording.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | burnForDevice:
--
-- Creates an autoreleased burn object.
--
-- Once a burn is created with this method, the object is ready to write data to the disc.
--
-- @device@ — Device to use for the burn
--
-- Returns: An autoreleased DRBurn object.
--
-- ObjC selector: @+ burnForDevice:@
burnForDevice :: IsDRDevice device => device -> IO (Id DRBurn)
burnForDevice device =
  do
    cls' <- getRequiredClass "DRBurn"
    withObjCPtr device $ \raw_device ->
      sendClassMsg cls' (mkSelector "burnForDevice:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ())] >>= retainedObject . castPtr

-- | initWithDevice:
--
-- Initializes the burn object.
--
-- Once a burn is initialized with this method, the object is ready to write data to the disc.
--
-- @device@ — Device to use for the burn
--
-- Returns: A DRBurn object.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: (IsDRBurn drBurn, IsDRDevice device) => drBurn -> device -> IO RawId
initWithDevice drBurn  device =
withObjCPtr device $ \raw_device ->
    fmap (RawId . castPtr) $ sendMsg drBurn (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ())]

-- | writeLayout:
--
-- Begin the process of burning a disc layout.
--
-- This method only begins the burning process. Once the burn					has been started, control returns to the caller and it is up to the caller to 					monitor the progress of the burn (by listening for
--
-- //apple_ref/occ/data/DRBurnStatusChangedNotification DRBurnStatusChangedNotification
--
-- or polling
--
-- //apple_ref/occ/instm/DRBurn/status status
--
-- ) to know when the burn completes (either successfully 					or with an error).
--
-- @layout@ — The data to be burned to disc.
--
-- There are three configurations of the object passed in:
--
-- For a multi-session burn, layout must be a valid NSArray containing 						 one or more NSArrays, each of which contains one or more valid
--
-- //apple_ref/occ/cl/DRTrack DRTrack
--
-- objects.					For a single-session multi-track burn, layout must be a valid NSArray 						 containing one or more valid
--
-- //apple_ref/occ/cl/DRTrack DRTrack
--
-- objects.					For a single-session single-track burn, layout must be a valid
--
-- //apple_ref/occ/cl/DRTrack DRTrack
--
-- object.										If none of these configurations are met or the leaf values contained in					layout are not valid
--
-- //apple_ref/occ/cl/DRTrack DRTrack
--
-- objects, an exception is thrown.
--
-- ObjC selector: @- writeLayout:@
writeLayout :: IsDRBurn drBurn => drBurn -> RawId -> IO ()
writeLayout drBurn  layout =
  sendMsg drBurn (mkSelector "writeLayout:") retVoid [argPtr (castPtr (unRawId layout) :: Ptr ())]

-- | status
--
-- Returns a dictionary describing the status of the burn.
--
-- The same dictionary is returned through the
--
-- //apple_ref/occ/data/DRBurnStatusChangedNotification DRBurnStatusChangedNotification
--
-- notification
--
-- Returns: An NSDictionary	reporting the status of the burn.
--
-- ObjC selector: @- status@
status :: IsDRBurn drBurn => drBurn -> IO (Id NSDictionary)
status drBurn  =
  sendMsg drBurn (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | abort
--
-- Stops the burn.
--
-- When this method returns the burn might not actually be fully stopped					but it has been cancelled and only cleanup is going on. If a burn					has not completed writing data to disc, you just made a coaster.
--
-- Typically this method is only used as a result of the user hitting					a cancel/stop button somewhere in the user interface.
--
-- ObjC selector: @- abort@
abort :: IsDRBurn drBurn => drBurn -> IO ()
abort drBurn  =
  sendMsg drBurn (mkSelector "abort") retVoid []

-- | properties
--
-- Returns the properties dictionary of the burn.
--
-- Returns: An NSDictionary containing the properties of the burn.
--
-- ObjC selector: @- properties@
properties :: IsDRBurn drBurn => drBurn -> IO (Id NSDictionary)
properties drBurn  =
  sendMsg drBurn (mkSelector "properties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setProperties:
--
-- Sets the properties dictionary of the burn.
--
-- @properties@ — NSDictionary of the properties to set.
--
-- ObjC selector: @- setProperties:@
setProperties :: (IsDRBurn drBurn, IsNSDictionary properties) => drBurn -> properties -> IO ()
setProperties drBurn  properties =
withObjCPtr properties $ \raw_properties ->
    sendMsg drBurn (mkSelector "setProperties:") retVoid [argPtr (castPtr raw_properties :: Ptr ())]

-- | device
--
-- Returns the device being used for the burn.
--
-- Returns: The DRDevice the burn will use.
--
-- ObjC selector: @- device@
device :: IsDRBurn drBurn => drBurn -> IO (Id DRDevice)
device drBurn  =
  sendMsg drBurn (mkSelector "device") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | layoutForImageFile:
--
-- Creates a layout capable of burning an image to disc.
--
-- The layout created by this method may be any type of object. No assumptions 					should be made as to what sort of object may be returned based on the  					input image type.
--
-- @path@ — The path to the image file. This file must be one that can							be read by DiscRecording. The supported image types include: 							.dmg, .iso, .cue, and .toc. For .cue and .toc files the  							corresponding data files (.bin, .img, etc) must also be present 							and correctly referenced in the .cue/.toc file.
--
-- Returns: An autoreleased layout object
--
-- ObjC selector: @+ layoutForImageFile:@
layoutForImageFile :: IsNSString path => path -> IO RawId
layoutForImageFile path =
  do
    cls' <- getRequiredClass "DRBurn"
    withObjCPtr path $ \raw_path ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "layoutForImageFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())]

-- | requestedBurnSpeed
--
-- Returns the speed at which this burn will attempt to write data.
--
-- The actual speed also depends on the capabilities of the bus the device is on, 					the maximum speed of the device itself, and the media used.
--
-- Returns: A float indicating the speed the burn should run at in kilobytes per second.
--
-- ObjC selector: @- requestedBurnSpeed@
requestedBurnSpeed :: IsDRBurn drBurn => drBurn -> IO CFloat
requestedBurnSpeed drBurn  =
  sendMsg drBurn (mkSelector "requestedBurnSpeed") retCFloat []

-- | setRequestedBurnSpeed:
--
-- Sets the speed at which the burn will be attempted to be performed at
--
-- The actual speed also depends on the capabilities of the bus the device is on, 					the maximum speed of the device itself, and the media used.
--
-- @speed@ — The speed that the burn should run at in kilobytes per second.
--
-- ObjC selector: @- setRequestedBurnSpeed:@
setRequestedBurnSpeed :: IsDRBurn drBurn => drBurn -> CFloat -> IO ()
setRequestedBurnSpeed drBurn  speed =
  sendMsg drBurn (mkSelector "setRequestedBurnSpeed:") retVoid [argCFloat (fromIntegral speed)]

-- | appendable
--
-- Indicates if the burn is appendable.
--
-- When a burn completes, it can mark the disc so that no more data can be 					written to it. This creates a closed or non-appendable disc (which is the					most compatible with audio CD players). If this method returns NO, then 					the disc will be marked as closed and no data can be appended to it. A					return value of YES indicates further burns can be appended to 					the disc.
--
-- Returns: A BOOL indicating if the burn is appendable.
--
-- ObjC selector: @- appendable@
appendable :: IsDRBurn drBurn => drBurn -> IO Bool
appendable drBurn  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg drBurn (mkSelector "appendable") retCULong []

-- | setAppendable:
--
-- Sets the burn to be appendable or non-appendable.
--
-- When a burn completes, it can mark the disc so that no more data can be 					written to it. This creates a closed or non-appendable disc (which is the					most compatible with audio CD players).
--
-- @appendable@ — A BOOL indicating if the burn is appendable. Passing in YES    					indicates further burns can be appended to the disc, while passing in NO,    					marks the disc as closed and no data can be appended to it.
--
-- ObjC selector: @- setAppendable:@
setAppendable :: IsDRBurn drBurn => drBurn -> Bool -> IO ()
setAppendable drBurn  appendable =
  sendMsg drBurn (mkSelector "setAppendable:") retVoid [argCULong (if appendable then 1 else 0)]

-- | verifyDisc
--
-- Indicates if the resulting disc will be verified.
--
-- After data is written to disc, the data can be verified. The verification   					process will read the data on the disc back into memory and compare it to the    					data originally used to write to disc. The type of verification is determined   					by a track property on a track-by-track basis. See the
--
-- //apple_ref/occ/cl/DRTrack DRTrack
--
-- documentation for more information on verification types.
--
-- Returns: A BOOL indicating if the disc will be verified.
--
-- ObjC selector: @- verifyDisc@
verifyDisc :: IsDRBurn drBurn => drBurn -> IO Bool
verifyDisc drBurn  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg drBurn (mkSelector "verifyDisc") retCULong []

-- | setVerifyDisc:
--
-- Sets the burn to verify or not verify the disc.
--
-- @verify@ — A BOOL indicating if the disc is to be verified. Passing in YES    							(the default) indicates that the data written to disc will be verified    							against the source data once the burn complete. 							Passing in NO indicates that no verification will take place.
--
-- ObjC selector: @- setVerifyDisc:@
setVerifyDisc :: IsDRBurn drBurn => drBurn -> Bool -> IO ()
setVerifyDisc drBurn  verify =
  sendMsg drBurn (mkSelector "setVerifyDisc:") retVoid [argCULong (if verify then 1 else 0)]

-- | completionAction
--
-- Returns the action to be performed at the end of the burn.
--
-- Returns: An NSString
--
-- ObjC selector: @- completionAction@
completionAction :: IsDRBurn drBurn => drBurn -> IO (Id NSString)
completionAction drBurn  =
  sendMsg drBurn (mkSelector "completionAction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setCompletionAction:
--
-- Sets the action to be performed at the end of the burn.
--
-- @action@ — An NSString for the action to perform.
--
-- ObjC selector: @- setCompletionAction:@
setCompletionAction :: (IsDRBurn drBurn, IsNSString action) => drBurn -> action -> IO ()
setCompletionAction drBurn  action =
withObjCPtr action $ \raw_action ->
    sendMsg drBurn (mkSelector "setCompletionAction:") retVoid [argPtr (castPtr raw_action :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @burnForDevice:@
burnForDeviceSelector :: Selector
burnForDeviceSelector = mkSelector "burnForDevice:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @writeLayout:@
writeLayoutSelector :: Selector
writeLayoutSelector = mkSelector "writeLayout:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @abort@
abortSelector :: Selector
abortSelector = mkSelector "abort"

-- | @Selector@ for @properties@
propertiesSelector :: Selector
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @setProperties:@
setPropertiesSelector :: Selector
setPropertiesSelector = mkSelector "setProperties:"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

-- | @Selector@ for @layoutForImageFile:@
layoutForImageFileSelector :: Selector
layoutForImageFileSelector = mkSelector "layoutForImageFile:"

-- | @Selector@ for @requestedBurnSpeed@
requestedBurnSpeedSelector :: Selector
requestedBurnSpeedSelector = mkSelector "requestedBurnSpeed"

-- | @Selector@ for @setRequestedBurnSpeed:@
setRequestedBurnSpeedSelector :: Selector
setRequestedBurnSpeedSelector = mkSelector "setRequestedBurnSpeed:"

-- | @Selector@ for @appendable@
appendableSelector :: Selector
appendableSelector = mkSelector "appendable"

-- | @Selector@ for @setAppendable:@
setAppendableSelector :: Selector
setAppendableSelector = mkSelector "setAppendable:"

-- | @Selector@ for @verifyDisc@
verifyDiscSelector :: Selector
verifyDiscSelector = mkSelector "verifyDisc"

-- | @Selector@ for @setVerifyDisc:@
setVerifyDiscSelector :: Selector
setVerifyDiscSelector = mkSelector "setVerifyDisc:"

-- | @Selector@ for @completionAction@
completionActionSelector :: Selector
completionActionSelector = mkSelector "completionAction"

-- | @Selector@ for @setCompletionAction:@
setCompletionActionSelector :: Selector
setCompletionActionSelector = mkSelector "setCompletionAction:"

