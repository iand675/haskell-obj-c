{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVOutputSettingsAssistant
--
-- A class, each instance of which specifies a set of parameters for configuring objects that use output settings dictionaries, for example AVAssetWriter & AVAssetWriterInput, so that the resulting media file conforms to some specific criteria
--
-- Instances of AVOutputSettingsAssistant are typically created using a string constant representing a specific preset configuration, such as AVOutputSettingsPreset1280x720.  Once you have an instance, its properties can be used as a guide for creating and configuring an AVAssetWriter object and one or more AVAssetWriterInput objects.  If all the suggested properties are respected, the resulting media file will conform to the criteria implied by the preset.  Alternatively, the properties of an instance can be used as a "base" configuration which can be customized to suit your individual needs.
--
-- The recommendations made by an instance get better as you tell it more about the format of your source data.  For example, if you set the sourceVideoFormat property, the recommendation made by the videoSettings property will ensure that your video frames are not scaled up from a smaller size.
--
-- Generated bindings for @AVOutputSettingsAssistant@.
module ObjC.AVFoundation.AVOutputSettingsAssistant
  ( AVOutputSettingsAssistant
  , IsAVOutputSettingsAssistant(..)
  , init_
  , new
  , availableOutputSettingsPresets
  , outputSettingsAssistantWithPreset
  , audioSettings
  , videoSettings
  , outputFileType
  , sourceAudioFormat
  , setSourceAudioFormat
  , sourceVideoFormat
  , setSourceVideoFormat
  , initSelector
  , newSelector
  , availableOutputSettingsPresetsSelector
  , outputSettingsAssistantWithPresetSelector
  , audioSettingsSelector
  , videoSettingsSelector
  , outputFileTypeSelector
  , sourceAudioFormatSelector
  , setSourceAudioFormatSelector
  , sourceVideoFormatSelector
  , setSourceVideoFormatSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVOutputSettingsAssistant avOutputSettingsAssistant => avOutputSettingsAssistant -> IO (Id AVOutputSettingsAssistant)
init_ avOutputSettingsAssistant  =
  sendMsg avOutputSettingsAssistant (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVOutputSettingsAssistant)
new  =
  do
    cls' <- getRequiredClass "AVOutputSettingsAssistant"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | availableOutputSettingsPresets
--
-- Returns the list of presets that can be used to create an instance of AVOutputSettingsAssistant
--
-- Returns: An NSArray of NSString objects, each of which is a preset identifier
--
-- Each preset in the returned list can be passed in to +outputSettingsAssistantWithPreset: to create a new instance of AVOutputSettingsAssistant.
--
-- On iOS, the returned array may be different between different device models.
--
-- ObjC selector: @+ availableOutputSettingsPresets@
availableOutputSettingsPresets :: IO (Id NSArray)
availableOutputSettingsPresets  =
  do
    cls' <- getRequiredClass "AVOutputSettingsAssistant"
    sendClassMsg cls' (mkSelector "availableOutputSettingsPresets") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | outputSettingsAssistantWithPreset:
--
-- Returns an instance of AVOutputSettingsAssistant corresponding to the given preset
--
-- @presetIdentifier@ — The string identifier, for example AVOutputSettingsPreset1280x720, for the desired preset
--
-- Returns: An instance of AVOutputSettingsAssistant with properties corresponding to the given preset, or nil if there is no such available preset.
--
-- The properties of the returned object can be used as a guide for creating and configuring an AVAssetWriter object and one or more AVAssetWriterInput objects.  If all the suggested properties are respected in creating the AVAssetWriter, the resulting media file will conform to the criteria implied by the preset.
--
-- Use +availableOutputSettingsPresets to get a list of presets identifiers that can be used with this method.
--
-- ObjC selector: @+ outputSettingsAssistantWithPreset:@
outputSettingsAssistantWithPreset :: IsNSString presetIdentifier => presetIdentifier -> IO (Id AVOutputSettingsAssistant)
outputSettingsAssistantWithPreset presetIdentifier =
  do
    cls' <- getRequiredClass "AVOutputSettingsAssistant"
    withObjCPtr presetIdentifier $ \raw_presetIdentifier ->
      sendClassMsg cls' (mkSelector "outputSettingsAssistantWithPreset:") (retPtr retVoid) [argPtr (castPtr raw_presetIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | audioSettings
--
-- A dictionary of key/value pairs, as specified in AVAudioSettings.h, to be used when e.g. creating an instance of AVAssetWriterInput
--
-- The value of this property may change as a result of setting a new value for the sourceAudioFormat property.
--
-- ObjC selector: @- audioSettings@
audioSettings :: IsAVOutputSettingsAssistant avOutputSettingsAssistant => avOutputSettingsAssistant -> IO (Id NSDictionary)
audioSettings avOutputSettingsAssistant  =
  sendMsg avOutputSettingsAssistant (mkSelector "audioSettings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | videoSettings
--
-- A dictionary of key/value pairs, as specified in AVVideoSettings.h, to be used when e.g. creating an instance of AVAssetWriterInput
--
-- The value of this property may change as a result of setting a new value for the sourceVideoFormat property.
--
-- ObjC selector: @- videoSettings@
videoSettings :: IsAVOutputSettingsAssistant avOutputSettingsAssistant => avOutputSettingsAssistant -> IO (Id NSDictionary)
videoSettings avOutputSettingsAssistant  =
  sendMsg avOutputSettingsAssistant (mkSelector "videoSettings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | outputFileType
--
-- A UTI indicating the type of file to be written, to be used when e.g. creating an instance of AVAssetWriter
--
-- Use [[UTType typeWithIdentifier:outputFileType] preferredFilenameExtension] to get a suitable file extension for a given file type.
--
-- ObjC selector: @- outputFileType@
outputFileType :: IsAVOutputSettingsAssistant avOutputSettingsAssistant => avOutputSettingsAssistant -> IO (Id NSString)
outputFileType avOutputSettingsAssistant  =
  sendMsg avOutputSettingsAssistant (mkSelector "outputFileType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sourceAudioFormat
--
-- A CMAudioFormatDescription object describing the format of you audio data
--
-- Setting this property will allow the receiver to make a more informed recommendation for the audio settings that should be used.  After setting this property, you should re-query the audioSettings property to get the new recommendation.  The default value is NULL, which means that the receiver does not know anything about the format of your audio data.
--
-- If you set a non-NULL value for this property, and are using the receiver to initialize an AVAssetWriterInput, the same format description should be used to initialize the AVAssetWriterInput, along with the dictionary from the audioSettings property.
--
-- ObjC selector: @- sourceAudioFormat@
sourceAudioFormat :: IsAVOutputSettingsAssistant avOutputSettingsAssistant => avOutputSettingsAssistant -> IO RawId
sourceAudioFormat avOutputSettingsAssistant  =
  fmap (RawId . castPtr) $ sendMsg avOutputSettingsAssistant (mkSelector "sourceAudioFormat") (retPtr retVoid) []

-- | sourceAudioFormat
--
-- A CMAudioFormatDescription object describing the format of you audio data
--
-- Setting this property will allow the receiver to make a more informed recommendation for the audio settings that should be used.  After setting this property, you should re-query the audioSettings property to get the new recommendation.  The default value is NULL, which means that the receiver does not know anything about the format of your audio data.
--
-- If you set a non-NULL value for this property, and are using the receiver to initialize an AVAssetWriterInput, the same format description should be used to initialize the AVAssetWriterInput, along with the dictionary from the audioSettings property.
--
-- ObjC selector: @- setSourceAudioFormat:@
setSourceAudioFormat :: IsAVOutputSettingsAssistant avOutputSettingsAssistant => avOutputSettingsAssistant -> RawId -> IO ()
setSourceAudioFormat avOutputSettingsAssistant  value =
  sendMsg avOutputSettingsAssistant (mkSelector "setSourceAudioFormat:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | sourceVideoFormat
--
-- A CMVideoFormatDescription object describing the format of your video data
--
-- Setting this property will allow the receiver to make a more informed recommendation for the video settings that should be used.  After setting this property, you should re-query the videoSettings property to get the new recommendation.  The default value is NULL, which means that the receiver does not know anything about the format of your video data.
--
-- If you set a non-NULL value for this property, and are using the receiver to initialize an AVAssetWriterInput, the same format description should be used to initialize the AVAssetWriterInput, along with the dictionary from the videoSettings property.
--
-- ObjC selector: @- sourceVideoFormat@
sourceVideoFormat :: IsAVOutputSettingsAssistant avOutputSettingsAssistant => avOutputSettingsAssistant -> IO RawId
sourceVideoFormat avOutputSettingsAssistant  =
  fmap (RawId . castPtr) $ sendMsg avOutputSettingsAssistant (mkSelector "sourceVideoFormat") (retPtr retVoid) []

-- | sourceVideoFormat
--
-- A CMVideoFormatDescription object describing the format of your video data
--
-- Setting this property will allow the receiver to make a more informed recommendation for the video settings that should be used.  After setting this property, you should re-query the videoSettings property to get the new recommendation.  The default value is NULL, which means that the receiver does not know anything about the format of your video data.
--
-- If you set a non-NULL value for this property, and are using the receiver to initialize an AVAssetWriterInput, the same format description should be used to initialize the AVAssetWriterInput, along with the dictionary from the videoSettings property.
--
-- ObjC selector: @- setSourceVideoFormat:@
setSourceVideoFormat :: IsAVOutputSettingsAssistant avOutputSettingsAssistant => avOutputSettingsAssistant -> RawId -> IO ()
setSourceVideoFormat avOutputSettingsAssistant  value =
  sendMsg avOutputSettingsAssistant (mkSelector "setSourceVideoFormat:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @availableOutputSettingsPresets@
availableOutputSettingsPresetsSelector :: Selector
availableOutputSettingsPresetsSelector = mkSelector "availableOutputSettingsPresets"

-- | @Selector@ for @outputSettingsAssistantWithPreset:@
outputSettingsAssistantWithPresetSelector :: Selector
outputSettingsAssistantWithPresetSelector = mkSelector "outputSettingsAssistantWithPreset:"

-- | @Selector@ for @audioSettings@
audioSettingsSelector :: Selector
audioSettingsSelector = mkSelector "audioSettings"

-- | @Selector@ for @videoSettings@
videoSettingsSelector :: Selector
videoSettingsSelector = mkSelector "videoSettings"

-- | @Selector@ for @outputFileType@
outputFileTypeSelector :: Selector
outputFileTypeSelector = mkSelector "outputFileType"

-- | @Selector@ for @sourceAudioFormat@
sourceAudioFormatSelector :: Selector
sourceAudioFormatSelector = mkSelector "sourceAudioFormat"

-- | @Selector@ for @setSourceAudioFormat:@
setSourceAudioFormatSelector :: Selector
setSourceAudioFormatSelector = mkSelector "setSourceAudioFormat:"

-- | @Selector@ for @sourceVideoFormat@
sourceVideoFormatSelector :: Selector
sourceVideoFormatSelector = mkSelector "sourceVideoFormat"

-- | @Selector@ for @setSourceVideoFormat:@
setSourceVideoFormatSelector :: Selector
setSourceVideoFormatSelector = mkSelector "setSourceVideoFormat:"

