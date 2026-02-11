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
  , initWithScope_element_dictionarySelector
  , scopeSelector
  , setScopeSelector
  , elementSelector
  , setElementSelector
  , presetDictionarySelector


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
initWithScope_element_dictionary avauPresetEvent  scope element presetDictionary =
withObjCPtr presetDictionary $ \raw_presetDictionary ->
    sendMsg avauPresetEvent (mkSelector "initWithScope:element:dictionary:") (retPtr retVoid) [argCUInt (fromIntegral scope), argCUInt (fromIntegral element), argPtr (castPtr raw_presetDictionary :: Ptr ())] >>= ownedObject . castPtr

-- | scope
--
-- The audio unit scope for the parameter (see AudioUnitScope).  This should always be set to Global.
--
-- ObjC selector: @- scope@
scope :: IsAVAUPresetEvent avauPresetEvent => avauPresetEvent -> IO CUInt
scope avauPresetEvent  =
  sendMsg avauPresetEvent (mkSelector "scope") retCUInt []

-- | scope
--
-- The audio unit scope for the parameter (see AudioUnitScope).  This should always be set to Global.
--
-- ObjC selector: @- setScope:@
setScope :: IsAVAUPresetEvent avauPresetEvent => avauPresetEvent -> CUInt -> IO ()
setScope avauPresetEvent  value =
  sendMsg avauPresetEvent (mkSelector "setScope:") retVoid [argCUInt (fromIntegral value)]

-- | element
--
-- The element index within the scope (see AudioUnitElement).  This should usually be set to 0.
--
-- ObjC selector: @- element@
element :: IsAVAUPresetEvent avauPresetEvent => avauPresetEvent -> IO CUInt
element avauPresetEvent  =
  sendMsg avauPresetEvent (mkSelector "element") retCUInt []

-- | element
--
-- The element index within the scope (see AudioUnitElement).  This should usually be set to 0.
--
-- ObjC selector: @- setElement:@
setElement :: IsAVAUPresetEvent avauPresetEvent => avauPresetEvent -> CUInt -> IO ()
setElement avauPresetEvent  value =
  sendMsg avauPresetEvent (mkSelector "setElement:") retVoid [argCUInt (fromIntegral value)]

-- | presetDictionary
--
-- An NSDictionary containing the preset.
--
-- ObjC selector: @- presetDictionary@
presetDictionary :: IsAVAUPresetEvent avauPresetEvent => avauPresetEvent -> IO (Id NSDictionary)
presetDictionary avauPresetEvent  =
  sendMsg avauPresetEvent (mkSelector "presetDictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithScope:element:dictionary:@
initWithScope_element_dictionarySelector :: Selector
initWithScope_element_dictionarySelector = mkSelector "initWithScope:element:dictionary:"

-- | @Selector@ for @scope@
scopeSelector :: Selector
scopeSelector = mkSelector "scope"

-- | @Selector@ for @setScope:@
setScopeSelector :: Selector
setScopeSelector = mkSelector "setScope:"

-- | @Selector@ for @element@
elementSelector :: Selector
elementSelector = mkSelector "element"

-- | @Selector@ for @setElement:@
setElementSelector :: Selector
setElementSelector = mkSelector "setElement:"

-- | @Selector@ for @presetDictionary@
presetDictionarySelector :: Selector
presetDictionarySelector = mkSelector "presetDictionary"

