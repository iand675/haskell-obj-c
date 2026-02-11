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
  , initWithParameterID_scope_element_valueSelector
  , parameterIDSelector
  , setParameterIDSelector
  , scopeSelector
  , setScopeSelector
  , elementSelector
  , setElementSelector
  , valueSelector
  , setValueSelector


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
initWithParameterID_scope_element_value avParameterEvent  parameterID scope element value =
  sendMsg avParameterEvent (mkSelector "initWithParameterID:scope:element:value:") (retPtr retVoid) [argCUInt (fromIntegral parameterID), argCUInt (fromIntegral scope), argCUInt (fromIntegral element), argCFloat (fromIntegral value)] >>= ownedObject . castPtr

-- | parameterID
--
-- The ID of the parameter (see AudioUnitParameterID).
--
-- ObjC selector: @- parameterID@
parameterID :: IsAVParameterEvent avParameterEvent => avParameterEvent -> IO CUInt
parameterID avParameterEvent  =
  sendMsg avParameterEvent (mkSelector "parameterID") retCUInt []

-- | parameterID
--
-- The ID of the parameter (see AudioUnitParameterID).
--
-- ObjC selector: @- setParameterID:@
setParameterID :: IsAVParameterEvent avParameterEvent => avParameterEvent -> CUInt -> IO ()
setParameterID avParameterEvent  value =
  sendMsg avParameterEvent (mkSelector "setParameterID:") retVoid [argCUInt (fromIntegral value)]

-- | scope
--
-- The audio unit scope for the parameter (see AudioUnitScope).
--
-- ObjC selector: @- scope@
scope :: IsAVParameterEvent avParameterEvent => avParameterEvent -> IO CUInt
scope avParameterEvent  =
  sendMsg avParameterEvent (mkSelector "scope") retCUInt []

-- | scope
--
-- The audio unit scope for the parameter (see AudioUnitScope).
--
-- ObjC selector: @- setScope:@
setScope :: IsAVParameterEvent avParameterEvent => avParameterEvent -> CUInt -> IO ()
setScope avParameterEvent  value =
  sendMsg avParameterEvent (mkSelector "setScope:") retVoid [argCUInt (fromIntegral value)]

-- | element
--
-- The element index within the scope (see AudioUnitElement).
--
-- ObjC selector: @- element@
element :: IsAVParameterEvent avParameterEvent => avParameterEvent -> IO CUInt
element avParameterEvent  =
  sendMsg avParameterEvent (mkSelector "element") retCUInt []

-- | element
--
-- The element index within the scope (see AudioUnitElement).
--
-- ObjC selector: @- setElement:@
setElement :: IsAVParameterEvent avParameterEvent => avParameterEvent -> CUInt -> IO ()
setElement avParameterEvent  value =
  sendMsg avParameterEvent (mkSelector "setElement:") retVoid [argCUInt (fromIntegral value)]

-- | value
--
-- The value of the parameter to be set.  Range:  Dependent on parameter.
--
-- ObjC selector: @- value@
value :: IsAVParameterEvent avParameterEvent => avParameterEvent -> IO CFloat
value avParameterEvent  =
  sendMsg avParameterEvent (mkSelector "value") retCFloat []

-- | value
--
-- The value of the parameter to be set.  Range:  Dependent on parameter.
--
-- ObjC selector: @- setValue:@
setValue :: IsAVParameterEvent avParameterEvent => avParameterEvent -> CFloat -> IO ()
setValue avParameterEvent  value =
  sendMsg avParameterEvent (mkSelector "setValue:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithParameterID:scope:element:value:@
initWithParameterID_scope_element_valueSelector :: Selector
initWithParameterID_scope_element_valueSelector = mkSelector "initWithParameterID:scope:element:value:"

-- | @Selector@ for @parameterID@
parameterIDSelector :: Selector
parameterIDSelector = mkSelector "parameterID"

-- | @Selector@ for @setParameterID:@
setParameterIDSelector :: Selector
setParameterIDSelector = mkSelector "setParameterID:"

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

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

