{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CHHapticEventParameter
--
-- A CHHapticEventParameter contains a CHHapticEventParameterID/value pair which helps determine the initial character		of a haptic or audio event.
--
-- Generated bindings for @CHHapticEventParameter@.
module ObjC.CoreHaptics.CHHapticEventParameter
  ( CHHapticEventParameter
  , IsCHHapticEventParameter(..)
  , init_
  , initWithParameterID_value
  , parameterID
  , value
  , setValue
  , initSelector
  , initWithParameterID_valueSelector
  , parameterIDSelector
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
init_ :: IsCHHapticEventParameter chHapticEventParameter => chHapticEventParameter -> IO (Id CHHapticEventParameter)
init_ chHapticEventParameter =
  sendOwnedMessage chHapticEventParameter initSelector

-- | @- initWithParameterID:value:@
initWithParameterID_value :: (IsCHHapticEventParameter chHapticEventParameter, IsNSString parameterID) => chHapticEventParameter -> parameterID -> CFloat -> IO (Id CHHapticEventParameter)
initWithParameterID_value chHapticEventParameter parameterID value =
  sendOwnedMessage chHapticEventParameter initWithParameterID_valueSelector (toNSString parameterID) value

-- | parameterID
--
-- The ID of the event parameter to use.
--
-- ObjC selector: @- parameterID@
parameterID :: IsCHHapticEventParameter chHapticEventParameter => chHapticEventParameter -> IO (Id NSString)
parameterID chHapticEventParameter =
  sendMessage chHapticEventParameter parameterIDSelector

-- | value
--
-- The value of the event parameter.
--
-- ObjC selector: @- value@
value :: IsCHHapticEventParameter chHapticEventParameter => chHapticEventParameter -> IO CFloat
value chHapticEventParameter =
  sendMessage chHapticEventParameter valueSelector

-- | value
--
-- The value of the event parameter.
--
-- ObjC selector: @- setValue:@
setValue :: IsCHHapticEventParameter chHapticEventParameter => chHapticEventParameter -> CFloat -> IO ()
setValue chHapticEventParameter value =
  sendMessage chHapticEventParameter setValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CHHapticEventParameter)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithParameterID:value:@
initWithParameterID_valueSelector :: Selector '[Id NSString, CFloat] (Id CHHapticEventParameter)
initWithParameterID_valueSelector = mkSelector "initWithParameterID:value:"

-- | @Selector@ for @parameterID@
parameterIDSelector :: Selector '[] (Id NSString)
parameterIDSelector = mkSelector "parameterID"

-- | @Selector@ for @value@
valueSelector :: Selector '[] CFloat
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[CFloat] ()
setValueSelector = mkSelector "setValue:"

