{-# LANGUAGE DataKinds #-}
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
  , relativeTimeSelector
  , setRelativeTimeSelector
  , setValueSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreHaptics.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCHHapticDynamicParameter chHapticDynamicParameter => chHapticDynamicParameter -> IO (Id CHHapticDynamicParameter)
init_ chHapticDynamicParameter =
  sendOwnedMessage chHapticDynamicParameter initSelector

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
initWithParameterID_value_relativeTime chHapticDynamicParameter parameterID value time =
  sendOwnedMessage chHapticDynamicParameter initWithParameterID_value_relativeTimeSelector (toNSString parameterID) value time

-- | parameterID
--
-- The ID of the dynamic parameter to use.
--
-- ObjC selector: @- parameterID@
parameterID :: IsCHHapticDynamicParameter chHapticDynamicParameter => chHapticDynamicParameter -> IO (Id NSString)
parameterID chHapticDynamicParameter =
  sendMessage chHapticDynamicParameter parameterIDSelector

-- | value
--
-- The value of the parameter.
--
-- ObjC selector: @- value@
value :: IsCHHapticDynamicParameter chHapticDynamicParameter => chHapticDynamicParameter -> IO CFloat
value chHapticDynamicParameter =
  sendMessage chHapticDynamicParameter valueSelector

-- | value
--
-- The value of the parameter.
--
-- ObjC selector: @- setValue:@
setValue :: IsCHHapticDynamicParameter chHapticDynamicParameter => chHapticDynamicParameter -> CFloat -> IO ()
setValue chHapticDynamicParameter value =
  sendMessage chHapticDynamicParameter setValueSelector value

-- | relativeTime
--
-- The time at which the parameter should be applied, relative to the start time for the pattern.
--
-- ObjC selector: @- relativeTime@
relativeTime :: IsCHHapticDynamicParameter chHapticDynamicParameter => chHapticDynamicParameter -> IO CDouble
relativeTime chHapticDynamicParameter =
  sendMessage chHapticDynamicParameter relativeTimeSelector

-- | relativeTime
--
-- The time at which the parameter should be applied, relative to the start time for the pattern.
--
-- ObjC selector: @- setRelativeTime:@
setRelativeTime :: IsCHHapticDynamicParameter chHapticDynamicParameter => chHapticDynamicParameter -> CDouble -> IO ()
setRelativeTime chHapticDynamicParameter value =
  sendMessage chHapticDynamicParameter setRelativeTimeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CHHapticDynamicParameter)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithParameterID:value:relativeTime:@
initWithParameterID_value_relativeTimeSelector :: Selector '[Id NSString, CFloat, CDouble] (Id CHHapticDynamicParameter)
initWithParameterID_value_relativeTimeSelector = mkSelector "initWithParameterID:value:relativeTime:"

-- | @Selector@ for @parameterID@
parameterIDSelector :: Selector '[] (Id NSString)
parameterIDSelector = mkSelector "parameterID"

-- | @Selector@ for @value@
valueSelector :: Selector '[] CFloat
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[CFloat] ()
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @relativeTime@
relativeTimeSelector :: Selector '[] CDouble
relativeTimeSelector = mkSelector "relativeTime"

-- | @Selector@ for @setRelativeTime:@
setRelativeTimeSelector :: Selector '[CDouble] ()
setRelativeTimeSelector = mkSelector "setRelativeTime:"

