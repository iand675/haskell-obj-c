{-# LANGUAGE DataKinds #-}
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
  , allTagNamesSelector
  , audioComponentDescriptionSelector
  , audioComponentSelector
  , availableArchitecturesSelector
  , componentURLSelector
  , configurationDictionarySelector
  , hasCustomViewSelector
  , hasMIDIInputSelector
  , hasMIDIOutputSelector
  , iconSelector
  , iconURLSelector
  , localizedTypeNameSelector
  , manufacturerNameSelector
  , nameSelector
  , passesAUValSelector
  , sandboxSafeSelector
  , setUserTagNamesSelector
  , supportsNumberInputChannels_outputChannelsSelector
  , typeNameSelector
  , userTagNamesSelector
  , versionSelector
  , versionStringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
supportsNumberInputChannels_outputChannels avAudioUnitComponent numInputChannels numOutputChannels =
  sendMessage avAudioUnitComponent supportsNumberInputChannels_outputChannelsSelector numInputChannels numOutputChannels

-- | name
--
-- the name of an audio component
--
-- ObjC selector: @- name@
name :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Id NSString)
name avAudioUnitComponent =
  sendMessage avAudioUnitComponent nameSelector

-- | typeName
--
-- standard audio component types returned as strings
--
-- ObjC selector: @- typeName@
typeName :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Id NSString)
typeName avAudioUnitComponent =
  sendMessage avAudioUnitComponent typeNameSelector

-- | localizedTypeName
--
-- localized string of typeName for display
--
-- ObjC selector: @- localizedTypeName@
localizedTypeName :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Id NSString)
localizedTypeName avAudioUnitComponent =
  sendMessage avAudioUnitComponent localizedTypeNameSelector

-- | manufacturerName
--
-- the manufacturer name, extracted from the manufacturer key defined in Info.plist dictionary
--
-- ObjC selector: @- manufacturerName@
manufacturerName :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Id NSString)
manufacturerName avAudioUnitComponent =
  sendMessage avAudioUnitComponent manufacturerNameSelector

-- | version
--
-- version number comprised of a hexadecimal number with major, minor, dot-release format: 0xMMMMmmDD
--
-- ObjC selector: @- version@
version :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO CULong
version avAudioUnitComponent =
  sendMessage avAudioUnitComponent versionSelector

-- | versionString
--
-- version number as string
--
-- ObjC selector: @- versionString@
versionString :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Id NSString)
versionString avAudioUnitComponent =
  sendMessage avAudioUnitComponent versionStringSelector

-- | componentURL
--
-- URL representing location of component
--
-- ObjC selector: @- componentURL@
componentURL :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO RawId
componentURL avAudioUnitComponent =
  sendMessage avAudioUnitComponent componentURLSelector

-- | availableArchitectures
--
-- NSArray of NSNumbers each of which corresponds to one of the constants in Mach-O Architecture in NSBundle Class Reference
--
-- ObjC selector: @- availableArchitectures@
availableArchitectures :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Id NSArray)
availableArchitectures avAudioUnitComponent =
  sendMessage avAudioUnitComponent availableArchitecturesSelector

-- | sandboxSafe
--
-- On OSX, YES if the AudioComponent can be loaded into a sandboxed process otherwise NO.			  On iOS, this is always YES.
--
-- ObjC selector: @- sandboxSafe@
sandboxSafe :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO Bool
sandboxSafe avAudioUnitComponent =
  sendMessage avAudioUnitComponent sandboxSafeSelector

-- | hasMIDIInput
--
-- YES if AudioComponent has midi input, otherwise NO
--
-- ObjC selector: @- hasMIDIInput@
hasMIDIInput :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO Bool
hasMIDIInput avAudioUnitComponent =
  sendMessage avAudioUnitComponent hasMIDIInputSelector

-- | hasMIDIOutput
--
-- YES if AudioComponent has midi output, otherwise NO
--
-- ObjC selector: @- hasMIDIOutput@
hasMIDIOutput :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO Bool
hasMIDIOutput avAudioUnitComponent =
  sendMessage avAudioUnitComponent hasMIDIOutputSelector

-- | audioComponent
--
-- the audioComponent that can be used in AudioComponent APIs.
--
-- ObjC selector: @- audioComponent@
audioComponent :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Ptr ())
audioComponent avAudioUnitComponent =
  sendMessage avAudioUnitComponent audioComponentSelector

-- | userTagNames
--
-- User tags represent the tags from the current user.
--
-- ObjC selector: @- userTagNames@
userTagNames :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Id NSArray)
userTagNames avAudioUnitComponent =
  sendMessage avAudioUnitComponent userTagNamesSelector

-- | userTagNames
--
-- User tags represent the tags from the current user.
--
-- ObjC selector: @- setUserTagNames:@
setUserTagNames :: (IsAVAudioUnitComponent avAudioUnitComponent, IsNSArray value) => avAudioUnitComponent -> value -> IO ()
setUserTagNames avAudioUnitComponent value =
  sendMessage avAudioUnitComponent setUserTagNamesSelector (toNSArray value)

-- | allTagNames
--
-- represent the tags from the current user and the system tags defined by AudioComponent.
--
-- ObjC selector: @- allTagNames@
allTagNames :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Id NSArray)
allTagNames avAudioUnitComponent =
  sendMessage avAudioUnitComponent allTagNamesSelector

-- | audioComponentDescription
--
-- description of the audio component that can be used in AudioComponent APIs.
--
-- ObjC selector: @- audioComponentDescription@
audioComponentDescription :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO AudioComponentDescription
audioComponentDescription avAudioUnitComponent =
  sendMessage avAudioUnitComponent audioComponentDescriptionSelector

-- | iconURL
--
-- A URL that will specify the location of an icon file that can be used when presenting UI for this audio component.
--
-- ObjC selector: @- iconURL@
iconURL :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Id NSURL)
iconURL avAudioUnitComponent =
  sendMessage avAudioUnitComponent iconURLSelector

-- | @- icon@
icon :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO RawId
icon avAudioUnitComponent =
  sendMessage avAudioUnitComponent iconSelector

-- | passesAUVal
--
-- YES if the AudioComponent has passed the AU validation tests, otherwise NO
--
-- ObjC selector: @- passesAUVal@
passesAUVal :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO Bool
passesAUVal avAudioUnitComponent =
  sendMessage avAudioUnitComponent passesAUValSelector

-- | hasCustomView
--
-- YES if the AudioComponent provides custom view, otherwise NO
--
-- ObjC selector: @- hasCustomView@
hasCustomView :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO Bool
hasCustomView avAudioUnitComponent =
  sendMessage avAudioUnitComponent hasCustomViewSelector

-- | configurationDictionary
--
-- A NSDictionary that contains information describing the capabilities of the AudioComponent.	The specific information depends on the type and the keys are defined in AudioUnitProperties.h
--
-- ObjC selector: @- configurationDictionary@
configurationDictionary :: IsAVAudioUnitComponent avAudioUnitComponent => avAudioUnitComponent -> IO (Id NSDictionary)
configurationDictionary avAudioUnitComponent =
  sendMessage avAudioUnitComponent configurationDictionarySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportsNumberInputChannels:outputChannels:@
supportsNumberInputChannels_outputChannelsSelector :: Selector '[CLong, CLong] Bool
supportsNumberInputChannels_outputChannelsSelector = mkSelector "supportsNumberInputChannels:outputChannels:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @typeName@
typeNameSelector :: Selector '[] (Id NSString)
typeNameSelector = mkSelector "typeName"

-- | @Selector@ for @localizedTypeName@
localizedTypeNameSelector :: Selector '[] (Id NSString)
localizedTypeNameSelector = mkSelector "localizedTypeName"

-- | @Selector@ for @manufacturerName@
manufacturerNameSelector :: Selector '[] (Id NSString)
manufacturerNameSelector = mkSelector "manufacturerName"

-- | @Selector@ for @version@
versionSelector :: Selector '[] CULong
versionSelector = mkSelector "version"

-- | @Selector@ for @versionString@
versionStringSelector :: Selector '[] (Id NSString)
versionStringSelector = mkSelector "versionString"

-- | @Selector@ for @componentURL@
componentURLSelector :: Selector '[] RawId
componentURLSelector = mkSelector "componentURL"

-- | @Selector@ for @availableArchitectures@
availableArchitecturesSelector :: Selector '[] (Id NSArray)
availableArchitecturesSelector = mkSelector "availableArchitectures"

-- | @Selector@ for @sandboxSafe@
sandboxSafeSelector :: Selector '[] Bool
sandboxSafeSelector = mkSelector "sandboxSafe"

-- | @Selector@ for @hasMIDIInput@
hasMIDIInputSelector :: Selector '[] Bool
hasMIDIInputSelector = mkSelector "hasMIDIInput"

-- | @Selector@ for @hasMIDIOutput@
hasMIDIOutputSelector :: Selector '[] Bool
hasMIDIOutputSelector = mkSelector "hasMIDIOutput"

-- | @Selector@ for @audioComponent@
audioComponentSelector :: Selector '[] (Ptr ())
audioComponentSelector = mkSelector "audioComponent"

-- | @Selector@ for @userTagNames@
userTagNamesSelector :: Selector '[] (Id NSArray)
userTagNamesSelector = mkSelector "userTagNames"

-- | @Selector@ for @setUserTagNames:@
setUserTagNamesSelector :: Selector '[Id NSArray] ()
setUserTagNamesSelector = mkSelector "setUserTagNames:"

-- | @Selector@ for @allTagNames@
allTagNamesSelector :: Selector '[] (Id NSArray)
allTagNamesSelector = mkSelector "allTagNames"

-- | @Selector@ for @audioComponentDescription@
audioComponentDescriptionSelector :: Selector '[] AudioComponentDescription
audioComponentDescriptionSelector = mkSelector "audioComponentDescription"

-- | @Selector@ for @iconURL@
iconURLSelector :: Selector '[] (Id NSURL)
iconURLSelector = mkSelector "iconURL"

-- | @Selector@ for @icon@
iconSelector :: Selector '[] RawId
iconSelector = mkSelector "icon"

-- | @Selector@ for @passesAUVal@
passesAUValSelector :: Selector '[] Bool
passesAUValSelector = mkSelector "passesAUVal"

-- | @Selector@ for @hasCustomView@
hasCustomViewSelector :: Selector '[] Bool
hasCustomViewSelector = mkSelector "hasCustomView"

-- | @Selector@ for @configurationDictionary@
configurationDictionarySelector :: Selector '[] (Id NSDictionary)
configurationDictionarySelector = mkSelector "configurationDictionary"

