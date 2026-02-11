{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSStepperCell@.
module ObjC.AppKit.NSStepperCell
  ( NSStepperCell
  , IsNSStepperCell(..)
  , minValue
  , setMinValue
  , maxValue
  , setMaxValue
  , increment
  , setIncrement
  , valueWraps
  , setValueWraps
  , autorepeat
  , setAutorepeat
  , minValueSelector
  , setMinValueSelector
  , maxValueSelector
  , setMaxValueSelector
  , incrementSelector
  , setIncrementSelector
  , valueWrapsSelector
  , setValueWrapsSelector
  , autorepeatSelector
  , setAutorepeatSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- minValue@
minValue :: IsNSStepperCell nsStepperCell => nsStepperCell -> IO CDouble
minValue nsStepperCell  =
  sendMsg nsStepperCell (mkSelector "minValue") retCDouble []

-- | @- setMinValue:@
setMinValue :: IsNSStepperCell nsStepperCell => nsStepperCell -> CDouble -> IO ()
setMinValue nsStepperCell  value =
  sendMsg nsStepperCell (mkSelector "setMinValue:") retVoid [argCDouble (fromIntegral value)]

-- | @- maxValue@
maxValue :: IsNSStepperCell nsStepperCell => nsStepperCell -> IO CDouble
maxValue nsStepperCell  =
  sendMsg nsStepperCell (mkSelector "maxValue") retCDouble []

-- | @- setMaxValue:@
setMaxValue :: IsNSStepperCell nsStepperCell => nsStepperCell -> CDouble -> IO ()
setMaxValue nsStepperCell  value =
  sendMsg nsStepperCell (mkSelector "setMaxValue:") retVoid [argCDouble (fromIntegral value)]

-- | @- increment@
increment :: IsNSStepperCell nsStepperCell => nsStepperCell -> IO CDouble
increment nsStepperCell  =
  sendMsg nsStepperCell (mkSelector "increment") retCDouble []

-- | @- setIncrement:@
setIncrement :: IsNSStepperCell nsStepperCell => nsStepperCell -> CDouble -> IO ()
setIncrement nsStepperCell  value =
  sendMsg nsStepperCell (mkSelector "setIncrement:") retVoid [argCDouble (fromIntegral value)]

-- | @- valueWraps@
valueWraps :: IsNSStepperCell nsStepperCell => nsStepperCell -> IO Bool
valueWraps nsStepperCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsStepperCell (mkSelector "valueWraps") retCULong []

-- | @- setValueWraps:@
setValueWraps :: IsNSStepperCell nsStepperCell => nsStepperCell -> Bool -> IO ()
setValueWraps nsStepperCell  value =
  sendMsg nsStepperCell (mkSelector "setValueWraps:") retVoid [argCULong (if value then 1 else 0)]

-- | @- autorepeat@
autorepeat :: IsNSStepperCell nsStepperCell => nsStepperCell -> IO Bool
autorepeat nsStepperCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsStepperCell (mkSelector "autorepeat") retCULong []

-- | @- setAutorepeat:@
setAutorepeat :: IsNSStepperCell nsStepperCell => nsStepperCell -> Bool -> IO ()
setAutorepeat nsStepperCell  value =
  sendMsg nsStepperCell (mkSelector "setAutorepeat:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @minValue@
minValueSelector :: Selector
minValueSelector = mkSelector "minValue"

-- | @Selector@ for @setMinValue:@
setMinValueSelector :: Selector
setMinValueSelector = mkSelector "setMinValue:"

-- | @Selector@ for @maxValue@
maxValueSelector :: Selector
maxValueSelector = mkSelector "maxValue"

-- | @Selector@ for @setMaxValue:@
setMaxValueSelector :: Selector
setMaxValueSelector = mkSelector "setMaxValue:"

-- | @Selector@ for @increment@
incrementSelector :: Selector
incrementSelector = mkSelector "increment"

-- | @Selector@ for @setIncrement:@
setIncrementSelector :: Selector
setIncrementSelector = mkSelector "setIncrement:"

-- | @Selector@ for @valueWraps@
valueWrapsSelector :: Selector
valueWrapsSelector = mkSelector "valueWraps"

-- | @Selector@ for @setValueWraps:@
setValueWrapsSelector :: Selector
setValueWrapsSelector = mkSelector "setValueWraps:"

-- | @Selector@ for @autorepeat@
autorepeatSelector :: Selector
autorepeatSelector = mkSelector "autorepeat"

-- | @Selector@ for @setAutorepeat:@
setAutorepeatSelector :: Selector
setAutorepeatSelector = mkSelector "setAutorepeat:"

