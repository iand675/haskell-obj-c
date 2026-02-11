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

import ObjC.CoreHaptics.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCHHapticEventParameter chHapticEventParameter => chHapticEventParameter -> IO (Id CHHapticEventParameter)
init_ chHapticEventParameter  =
  sendMsg chHapticEventParameter (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithParameterID:value:@
initWithParameterID_value :: (IsCHHapticEventParameter chHapticEventParameter, IsNSString parameterID) => chHapticEventParameter -> parameterID -> CFloat -> IO (Id CHHapticEventParameter)
initWithParameterID_value chHapticEventParameter  parameterID value =
withObjCPtr parameterID $ \raw_parameterID ->
    sendMsg chHapticEventParameter (mkSelector "initWithParameterID:value:") (retPtr retVoid) [argPtr (castPtr raw_parameterID :: Ptr ()), argCFloat (fromIntegral value)] >>= ownedObject . castPtr

-- | parameterID
--
-- The ID of the event parameter to use.
--
-- ObjC selector: @- parameterID@
parameterID :: IsCHHapticEventParameter chHapticEventParameter => chHapticEventParameter -> IO (Id NSString)
parameterID chHapticEventParameter  =
  sendMsg chHapticEventParameter (mkSelector "parameterID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | value
--
-- The value of the event parameter.
--
-- ObjC selector: @- value@
value :: IsCHHapticEventParameter chHapticEventParameter => chHapticEventParameter -> IO CFloat
value chHapticEventParameter  =
  sendMsg chHapticEventParameter (mkSelector "value") retCFloat []

-- | value
--
-- The value of the event parameter.
--
-- ObjC selector: @- setValue:@
setValue :: IsCHHapticEventParameter chHapticEventParameter => chHapticEventParameter -> CFloat -> IO ()
setValue chHapticEventParameter  value =
  sendMsg chHapticEventParameter (mkSelector "setValue:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithParameterID:value:@
initWithParameterID_valueSelector :: Selector
initWithParameterID_valueSelector = mkSelector "initWithParameterID:value:"

-- | @Selector@ for @parameterID@
parameterIDSelector :: Selector
parameterIDSelector = mkSelector "parameterID"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

