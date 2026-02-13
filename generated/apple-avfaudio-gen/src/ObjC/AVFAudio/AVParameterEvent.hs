{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVParameterEvent
--
-- The event class representing a parameter set/change event on the AVMusicTrack's destinationAudioUnit.
--
-- AVParameterEvents make it possible to schedule and/or automate parameter changes on the audio unit		that has been configured as the destination for the AVMusicTrack containing this event.
--
-- When the track is played as part of a sequence, the destination audio unit will receive set-parameter		messages whose values change smoothly along a linear ramp between each event's beat location.
--
-- If an AVParameterEvent is added to an empty, non-automation track, the track becomes an automation track.
--
-- Generated bindings for @AVParameterEvent@.
module ObjC.AVFAudio.AVParameterEvent
  ( AVParameterEvent
  , IsAVParameterEvent(..)
  , initWithParameterID_scope_element_value
  , parameterID
  , setParameterID
  , scope
  , setScope
  , element
  , setElement
  , value
  , setValue
  , elementSelector
  , initWithParameterID_scope_element_valueSelector
  , parameterIDSelector
  , scopeSelector
  , setElementSelector
  , setParameterIDSelector
  , setScopeSelector
  , setValueSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithParameterID:scope:element:value
--
-- Initialize the event with the parameter ID, scope, element, and value for the parameter to be set.
--
-- @parameterID@ — The ID of the parameter (see AudioUnitParameterID).
--
-- @scope@ — The audio unit scope for the parameter (see AudioUnitScope).
--
-- @element@ — The element index within the scope (see AudioUnitElement).
--
-- @value@ — The value of the parameter to be set.  Range:  Dependent on parameter.
--
-- ObjC selector: @- initWithParameterID:scope:element:value:@
initWithParameterID_scope_element_value :: IsAVParameterEvent avParameterEvent => avParameterEvent -> CUInt -> CUInt -> CUInt -> CFloat -> IO (Id AVParameterEvent)
initWithParameterID_scope_element_value avParameterEvent parameterID scope element value =
  sendOwnedMessage avParameterEvent initWithParameterID_scope_element_valueSelector parameterID scope element value

-- | parameterID
--
-- The ID of the parameter (see AudioUnitParameterID).
--
-- ObjC selector: @- parameterID@
parameterID :: IsAVParameterEvent avParameterEvent => avParameterEvent -> IO CUInt
parameterID avParameterEvent =
  sendMessage avParameterEvent parameterIDSelector

-- | parameterID
--
-- The ID of the parameter (see AudioUnitParameterID).
--
-- ObjC selector: @- setParameterID:@
setParameterID :: IsAVParameterEvent avParameterEvent => avParameterEvent -> CUInt -> IO ()
setParameterID avParameterEvent value =
  sendMessage avParameterEvent setParameterIDSelector value

-- | scope
--
-- The audio unit scope for the parameter (see AudioUnitScope).
--
-- ObjC selector: @- scope@
scope :: IsAVParameterEvent avParameterEvent => avParameterEvent -> IO CUInt
scope avParameterEvent =
  sendMessage avParameterEvent scopeSelector

-- | scope
--
-- The audio unit scope for the parameter (see AudioUnitScope).
--
-- ObjC selector: @- setScope:@
setScope :: IsAVParameterEvent avParameterEvent => avParameterEvent -> CUInt -> IO ()
setScope avParameterEvent value =
  sendMessage avParameterEvent setScopeSelector value

-- | element
--
-- The element index within the scope (see AudioUnitElement).
--
-- ObjC selector: @- element@
element :: IsAVParameterEvent avParameterEvent => avParameterEvent -> IO CUInt
element avParameterEvent =
  sendMessage avParameterEvent elementSelector

-- | element
--
-- The element index within the scope (see AudioUnitElement).
--
-- ObjC selector: @- setElement:@
setElement :: IsAVParameterEvent avParameterEvent => avParameterEvent -> CUInt -> IO ()
setElement avParameterEvent value =
  sendMessage avParameterEvent setElementSelector value

-- | value
--
-- The value of the parameter to be set.  Range:  Dependent on parameter.
--
-- ObjC selector: @- value@
value :: IsAVParameterEvent avParameterEvent => avParameterEvent -> IO CFloat
value avParameterEvent =
  sendMessage avParameterEvent valueSelector

-- | value
--
-- The value of the parameter to be set.  Range:  Dependent on parameter.
--
-- ObjC selector: @- setValue:@
setValue :: IsAVParameterEvent avParameterEvent => avParameterEvent -> CFloat -> IO ()
setValue avParameterEvent value =
  sendMessage avParameterEvent setValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithParameterID:scope:element:value:@
initWithParameterID_scope_element_valueSelector :: Selector '[CUInt, CUInt, CUInt, CFloat] (Id AVParameterEvent)
initWithParameterID_scope_element_valueSelector = mkSelector "initWithParameterID:scope:element:value:"

-- | @Selector@ for @parameterID@
parameterIDSelector :: Selector '[] CUInt
parameterIDSelector = mkSelector "parameterID"

-- | @Selector@ for @setParameterID:@
setParameterIDSelector :: Selector '[CUInt] ()
setParameterIDSelector = mkSelector "setParameterID:"

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

-- | @Selector@ for @value@
valueSelector :: Selector '[] CFloat
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[CFloat] ()
setValueSelector = mkSelector "setValue:"

