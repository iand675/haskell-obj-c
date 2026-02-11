{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CHHapticDynamicParameter
--
-- A CHHapticDynamicParameter contains a CHHapticDynamicParameterID/value pair which will modify (modulate) the ongoing character		of a haptic or audio event.
--
-- CHHapticDynamicParameters have a relative time property to allow specifying the time relationship between parameters in a pattern.
--
-- Generated bindings for @CHHapticDynamicParameter@.
module ObjC.CoreHaptics.CHHapticDynamicParameter
  ( CHHapticDynamicParameter
  , IsCHHapticDynamicParameter(..)
  , init_
  , initWithParameterID_value_relativeTime
  , parameterID
  , value
  , setValue
  , relativeTime
  , setRelativeTime
  , initSelector
  , initWithParameterID_value_relativeTimeSelector
  , parameterIDSelector
  , valueSelector
  , setValueSelector
  , relativeTimeSelector
  , setRelativeTimeSelector


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

import ObjC.CoreHaptics.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCHHapticDynamicParameter chHapticDynamicParameter => chHapticDynamicParameter -> IO (Id CHHapticDynamicParameter)
init_ chHapticDynamicParameter  =
  sendMsg chHapticDynamicParameter (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithParameterID:value:relativeTime
--
-- Initialize a CHHapticDynamicParameter with a parameter ID, value, and relative time.
--
-- @parameterID@ — The CHHapticDynamicParameterID for the desired parameter change.
--
-- @value@ — The value for that parameter.
--
-- @time@ — The time at which this parameter should be applied, relative to the start time of the pattern.
--
-- ObjC selector: @- initWithParameterID:value:relativeTime:@
initWithParameterID_value_relativeTime :: (IsCHHapticDynamicParameter chHapticDynamicParameter, IsNSString parameterID) => chHapticDynamicParameter -> parameterID -> CFloat -> CDouble -> IO (Id CHHapticDynamicParameter)
initWithParameterID_value_relativeTime chHapticDynamicParameter  parameterID value time =
withObjCPtr parameterID $ \raw_parameterID ->
    sendMsg chHapticDynamicParameter (mkSelector "initWithParameterID:value:relativeTime:") (retPtr retVoid) [argPtr (castPtr raw_parameterID :: Ptr ()), argCFloat (fromIntegral value), argCDouble (fromIntegral time)] >>= ownedObject . castPtr

-- | parameterID
--
-- The ID of the dynamic parameter to use.
--
-- ObjC selector: @- parameterID@
parameterID :: IsCHHapticDynamicParameter chHapticDynamicParameter => chHapticDynamicParameter -> IO (Id NSString)
parameterID chHapticDynamicParameter  =
  sendMsg chHapticDynamicParameter (mkSelector "parameterID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | value
--
-- The value of the parameter.
--
-- ObjC selector: @- value@
value :: IsCHHapticDynamicParameter chHapticDynamicParameter => chHapticDynamicParameter -> IO CFloat
value chHapticDynamicParameter  =
  sendMsg chHapticDynamicParameter (mkSelector "value") retCFloat []

-- | value
--
-- The value of the parameter.
--
-- ObjC selector: @- setValue:@
setValue :: IsCHHapticDynamicParameter chHapticDynamicParameter => chHapticDynamicParameter -> CFloat -> IO ()
setValue chHapticDynamicParameter  value =
  sendMsg chHapticDynamicParameter (mkSelector "setValue:") retVoid [argCFloat (fromIntegral value)]

-- | relativeTime
--
-- The time at which the parameter should be applied, relative to the start time for the pattern.
--
-- ObjC selector: @- relativeTime@
relativeTime :: IsCHHapticDynamicParameter chHapticDynamicParameter => chHapticDynamicParameter -> IO CDouble
relativeTime chHapticDynamicParameter  =
  sendMsg chHapticDynamicParameter (mkSelector "relativeTime") retCDouble []

-- | relativeTime
--
-- The time at which the parameter should be applied, relative to the start time for the pattern.
--
-- ObjC selector: @- setRelativeTime:@
setRelativeTime :: IsCHHapticDynamicParameter chHapticDynamicParameter => chHapticDynamicParameter -> CDouble -> IO ()
setRelativeTime chHapticDynamicParameter  value =
  sendMsg chHapticDynamicParameter (mkSelector "setRelativeTime:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithParameterID:value:relativeTime:@
initWithParameterID_value_relativeTimeSelector :: Selector
initWithParameterID_value_relativeTimeSelector = mkSelector "initWithParameterID:value:relativeTime:"

-- | @Selector@ for @parameterID@
parameterIDSelector :: Selector
parameterIDSelector = mkSelector "parameterID"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @relativeTime@
relativeTimeSelector :: Selector
relativeTimeSelector = mkSelector "relativeTime"

-- | @Selector@ for @setRelativeTime:@
setRelativeTimeSelector :: Selector
setRelativeTimeSelector = mkSelector "setRelativeTime:"

