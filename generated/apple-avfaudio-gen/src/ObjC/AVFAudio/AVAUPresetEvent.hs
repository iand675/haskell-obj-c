{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAUPresetEvent
--
-- The event class representing a preset load and change on the AVMusicTrack's destinationAudioUnit.
--
-- AVAUPresetEvents make it possible to schedule and/or automate preset changes on the audio unit		that has been configured as the destination for the AVMusicTrack containing this event.
--
-- Generated bindings for @AVAUPresetEvent@.
module ObjC.AVFAudio.AVAUPresetEvent
  ( AVAUPresetEvent
  , IsAVAUPresetEvent(..)
  , initWithScope_element_dictionary
  , scope
  , setScope
  , element
  , setElement
  , presetDictionary
  , elementSelector
  , initWithScope_element_dictionarySelector
  , presetDictionarySelector
  , scopeSelector
  , setElementSelector
  , setScopeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithScope:element:dictionary
--
-- Initialize the event with the scope, element, and dictionary for the preset.
--
-- @scope@ — The audio unit scope for the parameter (see AudioUnitScope).  This should always be set to Global.
--
-- @element@ — The element index within the scope (see AudioUnitElement).  This should usually be set to 0.
--
-- @presetDictionary@ — An NSDictionary containing the preset.  The audio unit will expect this to be a dictionary		structured as an appropriate audio unit preset.
--
-- The dictionary passed to this initializer will be copied and is not editable once the event is		created.
--
-- ObjC selector: @- initWithScope:element:dictionary:@
initWithScope_element_dictionary :: (IsAVAUPresetEvent avauPresetEvent, IsNSDictionary presetDictionary) => avauPresetEvent -> CUInt -> CUInt -> presetDictionary -> IO (Id AVAUPresetEvent)
initWithScope_element_dictionary avauPresetEvent scope element presetDictionary =
  sendOwnedMessage avauPresetEvent initWithScope_element_dictionarySelector scope element (toNSDictionary presetDictionary)

-- | scope
--
-- The audio unit scope for the parameter (see AudioUnitScope).  This should always be set to Global.
--
-- ObjC selector: @- scope@
scope :: IsAVAUPresetEvent avauPresetEvent => avauPresetEvent -> IO CUInt
scope avauPresetEvent =
  sendMessage avauPresetEvent scopeSelector

-- | scope
--
-- The audio unit scope for the parameter (see AudioUnitScope).  This should always be set to Global.
--
-- ObjC selector: @- setScope:@
setScope :: IsAVAUPresetEvent avauPresetEvent => avauPresetEvent -> CUInt -> IO ()
setScope avauPresetEvent value =
  sendMessage avauPresetEvent setScopeSelector value

-- | element
--
-- The element index within the scope (see AudioUnitElement).  This should usually be set to 0.
--
-- ObjC selector: @- element@
element :: IsAVAUPresetEvent avauPresetEvent => avauPresetEvent -> IO CUInt
element avauPresetEvent =
  sendMessage avauPresetEvent elementSelector

-- | element
--
-- The element index within the scope (see AudioUnitElement).  This should usually be set to 0.
--
-- ObjC selector: @- setElement:@
setElement :: IsAVAUPresetEvent avauPresetEvent => avauPresetEvent -> CUInt -> IO ()
setElement avauPresetEvent value =
  sendMessage avauPresetEvent setElementSelector value

-- | presetDictionary
--
-- An NSDictionary containing the preset.
--
-- ObjC selector: @- presetDictionary@
presetDictionary :: IsAVAUPresetEvent avauPresetEvent => avauPresetEvent -> IO (Id NSDictionary)
presetDictionary avauPresetEvent =
  sendMessage avauPresetEvent presetDictionarySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithScope:element:dictionary:@
initWithScope_element_dictionarySelector :: Selector '[CUInt, CUInt, Id NSDictionary] (Id AVAUPresetEvent)
initWithScope_element_dictionarySelector = mkSelector "initWithScope:element:dictionary:"

-- | @Selector@ for @scope@
scopeSelector :: Selector '[] CUInt
scopeSelector = mkSelector "scope"

-- | @Selector@ for @setScope:@
setScopeSelector :: Selector '[CUInt] ()
setScopeSelector = mkSelector "setScope:"

-- | @Selector@ for @element@
elementSelector :: Selector '[] CUInt
elementSelector = mkSelector "element"

-- | @Selector@ for @setElement:@
setElementSelector :: Selector '[CUInt] ()
setElementSelector = mkSelector "setElement:"

-- | @Selector@ for @presetDictionary@
presetDictionarySelector :: Selector '[] (Id NSDictionary)
presetDictionarySelector = mkSelector "presetDictionary"

