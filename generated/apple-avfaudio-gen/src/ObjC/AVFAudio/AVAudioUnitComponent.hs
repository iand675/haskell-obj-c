{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioUnitComponent
--
-- Provides details about an audio unit such as type, subtype, manufacturer, location etc. User	 tags can be added to the AVAudioUnitComponent which can be queried later for display.
--
-- Generated bindings for @AVAudioUnitComponent@.
module ObjC.AVFAudio.AVAudioUnitComponent
  ( AVAudioUnitComponent
  , IsAVAudioUnitComponent(..)
  , supportsNumberInputChannels_outputChannels
  , name
  , typeName
  , localizedTypeName
  , manufacturerName
  , version
  , versionString
  , componentURL
  , availableArchitectures
  , sandboxSafe
  , hasMIDIInput
  , hasMIDIOutput
  , audioComponent
  , userTagNames
  , setUserTagNames
  , allTagNames
  , audioComponentDescription
  , iconURL
  , icon
  , passesAUVal
  , hasCustomView
  , configurationDictionary
  , supportsNumberInputChannels_outputChannelsSelector
  , nameSelector
  , typeNameSelector
  , localizedTypeNameSelector
  , manufacturerNameSelector
  , versionSelector
  , versionStringSelector
  , componentURLSelector
  , availableArchitecturesSelector
  , sandboxSafeSelector
  , hasMIDIInputSelector
  , hasMIDIOutputSelector
  , audioComponentSelector
  , userTagNamesSelector
  , setUserTagNamesSelector
  , allTagNamesSelector
  , audioComponentDescriptionSelector
  , iconURLSelector
  , iconSelector
  , passesAUValSelector
  , hasCustomViewSelector
  , configurationDictionarySelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.AudioToolbox.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | supportsNumberInputChannels:outputChannels:
--
-- returns YES if the AudioComponent supports the input/output channel configuration
--
-- ObjC selector: @- supportsNumberInputChannels:outputChannels:@
supportsNumberInputChannels_outputChannels :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> CLong -> CLong -> IO Bool
supportsNumberInputChannels_outputChannels avAudioUnitComponent  numInputChannels numOutputChannels =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioUnitComponent (mkSelector "supportsNumberInputChannels:outputChannels:") retCULong [argCLong numInputChannels, argCLong numOutputChannels]

-- | name
--
-- the name of an audio component
--
-- ObjC selector: @- name@
name :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Id NSString)
name avAudioUnitComponent  =
    sendMsg avAudioUnitComponent (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | typeName
--
-- standard audio component types returned as strings
--
-- ObjC selector: @- typeName@
typeName :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Id NSString)
typeName avAudioUnitComponent  =
    sendMsg avAudioUnitComponent (mkSelector "typeName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | localizedTypeName
--
-- localized string of typeName for display
--
-- ObjC selector: @- localizedTypeName@
localizedTypeName :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Id NSString)
localizedTypeName avAudioUnitComponent  =
    sendMsg avAudioUnitComponent (mkSelector "localizedTypeName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | manufacturerName
--
-- the manufacturer name, extracted from the manufacturer key defined in Info.plist dictionary
--
-- ObjC selector: @- manufacturerName@
manufacturerName :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Id NSString)
manufacturerName avAudioUnitComponent  =
    sendMsg avAudioUnitComponent (mkSelector "manufacturerName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | version
--
-- version number comprised of a hexadecimal number with major, minor, dot-release format: 0xMMMMmmDD
--
-- ObjC selector: @- version@
version :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO CULong
version avAudioUnitComponent  =
    sendMsg avAudioUnitComponent (mkSelector "version") retCULong []

-- | versionString
--
-- version number as string
--
-- ObjC selector: @- versionString@
versionString :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Id NSString)
versionString avAudioUnitComponent  =
    sendMsg avAudioUnitComponent (mkSelector "versionString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | componentURL
--
-- URL representing location of component
--
-- ObjC selector: @- componentURL@
componentURL :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO RawId
componentURL avAudioUnitComponent  =
    fmap (RawId . castPtr) $ sendMsg avAudioUnitComponent (mkSelector "componentURL") (retPtr retVoid) []

-- | availableArchitectures
--
-- NSArray of NSNumbers each of which corresponds to one of the constants in Mach-O Architecture in NSBundle Class Reference
--
-- ObjC selector: @- availableArchitectures@
availableArchitectures :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Id NSArray)
availableArchitectures avAudioUnitComponent  =
    sendMsg avAudioUnitComponent (mkSelector "availableArchitectures") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sandboxSafe
--
-- On OSX, YES if the AudioComponent can be loaded into a sandboxed process otherwise NO.			  On iOS, this is always YES.
--
-- ObjC selector: @- sandboxSafe@
sandboxSafe :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO Bool
sandboxSafe avAudioUnitComponent  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioUnitComponent (mkSelector "sandboxSafe") retCULong []

-- | hasMIDIInput
--
-- YES if AudioComponent has midi input, otherwise NO
--
-- ObjC selector: @- hasMIDIInput@
hasMIDIInput :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO Bool
hasMIDIInput avAudioUnitComponent  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioUnitComponent (mkSelector "hasMIDIInput") retCULong []

-- | hasMIDIOutput
--
-- YES if AudioComponent has midi output, otherwise NO
--
-- ObjC selector: @- hasMIDIOutput@
hasMIDIOutput :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO Bool
hasMIDIOutput avAudioUnitComponent  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioUnitComponent (mkSelector "hasMIDIOutput") retCULong []

-- | audioComponent
--
-- the audioComponent that can be used in AudioComponent APIs.
--
-- ObjC selector: @- audioComponent@
audioComponent :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Ptr ())
audioComponent avAudioUnitComponent  =
    fmap castPtr $ sendMsg avAudioUnitComponent (mkSelector "audioComponent") (retPtr retVoid) []

-- | userTagNames
--
-- User tags represent the tags from the current user.
--
-- ObjC selector: @- userTagNames@
userTagNames :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Id NSArray)
userTagNames avAudioUnitComponent  =
    sendMsg avAudioUnitComponent (mkSelector "userTagNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | userTagNames
--
-- User tags represent the tags from the current user.
--
-- ObjC selector: @- setUserTagNames:@
setUserTagNames :: (IsAVAudioUnitComponent avAudioUnitComponent, IsNSArray value) => avAudioUnitComponent -> value -> IO ()
setUserTagNames avAudioUnitComponent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avAudioUnitComponent (mkSelector "setUserTagNames:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | allTagNames
--
-- represent the tags from the current user and the system tags defined by AudioComponent.
--
-- ObjC selector: @- allTagNames@
allTagNames :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Id NSArray)
allTagNames avAudioUnitComponent  =
    sendMsg avAudioUnitComponent (mkSelector "allTagNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | audioComponentDescription
--
-- description of the audio component that can be used in AudioComponent APIs.
--
-- ObjC selector: @- audioComponentDescription@
audioComponentDescription :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO AudioComponentDescription
audioComponentDescription avAudioUnitComponent  =
    sendMsgStret avAudioUnitComponent (mkSelector "audioComponentDescription") retAudioComponentDescription []

-- | iconURL
--
-- A URL that will specify the location of an icon file that can be used when presenting UI for this audio component.
--
-- ObjC selector: @- iconURL@
iconURL :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Id NSURL)
iconURL avAudioUnitComponent  =
    sendMsg avAudioUnitComponent (mkSelector "iconURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- icon@
icon :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO RawId
icon avAudioUnitComponent  =
    fmap (RawId . castPtr) $ sendMsg avAudioUnitComponent (mkSelector "icon") (retPtr retVoid) []

-- | passesAUVal
--
-- YES if the AudioComponent has passed the AU validation tests, otherwise NO
--
-- ObjC selector: @- passesAUVal@
passesAUVal :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO Bool
passesAUVal avAudioUnitComponent  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioUnitComponent (mkSelector "passesAUVal") retCULong []

-- | hasCustomView
--
-- YES if the AudioComponent provides custom view, otherwise NO
--
-- ObjC selector: @- hasCustomView@
hasCustomView :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO Bool
hasCustomView avAudioUnitComponent  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioUnitComponent (mkSelector "hasCustomView") retCULong []

-- | configurationDictionary
--
-- A NSDictionary that contains information describing the capabilities of the AudioComponent.	The specific information depends on the type and the keys are defined in AudioUnitProperties.h
--
-- ObjC selector: @- configurationDictionary@
configurationDictionary :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Id NSDictionary)
configurationDictionary avAudioUnitComponent  =
    sendMsg avAudioUnitComponent (mkSelector "configurationDictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportsNumberInputChannels:outputChannels:@
supportsNumberInputChannels_outputChannelsSelector :: Selector
supportsNumberInputChannels_outputChannelsSelector = mkSelector "supportsNumberInputChannels:outputChannels:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @typeName@
typeNameSelector :: Selector
typeNameSelector = mkSelector "typeName"

-- | @Selector@ for @localizedTypeName@
localizedTypeNameSelector :: Selector
localizedTypeNameSelector = mkSelector "localizedTypeName"

-- | @Selector@ for @manufacturerName@
manufacturerNameSelector :: Selector
manufacturerNameSelector = mkSelector "manufacturerName"

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

-- | @Selector@ for @versionString@
versionStringSelector :: Selector
versionStringSelector = mkSelector "versionString"

-- | @Selector@ for @componentURL@
componentURLSelector :: Selector
componentURLSelector = mkSelector "componentURL"

-- | @Selector@ for @availableArchitectures@
availableArchitecturesSelector :: Selector
availableArchitecturesSelector = mkSelector "availableArchitectures"

-- | @Selector@ for @sandboxSafe@
sandboxSafeSelector :: Selector
sandboxSafeSelector = mkSelector "sandboxSafe"

-- | @Selector@ for @hasMIDIInput@
hasMIDIInputSelector :: Selector
hasMIDIInputSelector = mkSelector "hasMIDIInput"

-- | @Selector@ for @hasMIDIOutput@
hasMIDIOutputSelector :: Selector
hasMIDIOutputSelector = mkSelector "hasMIDIOutput"

-- | @Selector@ for @audioComponent@
audioComponentSelector :: Selector
audioComponentSelector = mkSelector "audioComponent"

-- | @Selector@ for @userTagNames@
userTagNamesSelector :: Selector
userTagNamesSelector = mkSelector "userTagNames"

-- | @Selector@ for @setUserTagNames:@
setUserTagNamesSelector :: Selector
setUserTagNamesSelector = mkSelector "setUserTagNames:"

-- | @Selector@ for @allTagNames@
allTagNamesSelector :: Selector
allTagNamesSelector = mkSelector "allTagNames"

-- | @Selector@ for @audioComponentDescription@
audioComponentDescriptionSelector :: Selector
audioComponentDescriptionSelector = mkSelector "audioComponentDescription"

-- | @Selector@ for @iconURL@
iconURLSelector :: Selector
iconURLSelector = mkSelector "iconURL"

-- | @Selector@ for @icon@
iconSelector :: Selector
iconSelector = mkSelector "icon"

-- | @Selector@ for @passesAUVal@
passesAUValSelector :: Selector
passesAUValSelector = mkSelector "passesAUVal"

-- | @Selector@ for @hasCustomView@
hasCustomViewSelector :: Selector
hasCustomViewSelector = mkSelector "hasCustomView"

-- | @Selector@ for @configurationDictionary@
configurationDictionarySelector :: Selector
configurationDictionarySelector = mkSelector "configurationDictionary"

