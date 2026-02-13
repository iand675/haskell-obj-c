{-# LANGUAGE DataKinds #-}
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
  , autorepeatSelector
  , incrementSelector
  , maxValueSelector
  , minValueSelector
  , setAutorepeatSelector
  , setIncrementSelector
  , setMaxValueSelector
  , setMinValueSelector
  , setValueWrapsSelector
  , valueWrapsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- minValue@
minValue :: IsNSStepperCell nsStepperCell => nsStepperCell -> IO CDouble
minValue nsStepperCell =
  sendMessage nsStepperCell minValueSelector

-- | @- setMinValue:@
setMinValue :: IsNSStepperCell nsStepperCell => nsStepperCell -> CDouble -> IO ()
setMinValue nsStepperCell value =
  sendMessage nsStepperCell setMinValueSelector value

-- | @- maxValue@
maxValue :: IsNSStepperCell nsStepperCell => nsStepperCell -> IO CDouble
maxValue nsStepperCell =
  sendMessage nsStepperCell maxValueSelector

-- | @- setMaxValue:@
setMaxValue :: IsNSStepperCell nsStepperCell => nsStepperCell -> CDouble -> IO ()
setMaxValue nsStepperCell value =
  sendMessage nsStepperCell setMaxValueSelector value

-- | @- increment@
increment :: IsNSStepperCell nsStepperCell => nsStepperCell -> IO CDouble
increment nsStepperCell =
  sendMessage nsStepperCell incrementSelector

-- | @- setIncrement:@
setIncrement :: IsNSStepperCell nsStepperCell => nsStepperCell -> CDouble -> IO ()
setIncrement nsStepperCell value =
  sendMessage nsStepperCell setIncrementSelector value

-- | @- valueWraps@
valueWraps :: IsNSStepperCell nsStepperCell => nsStepperCell -> IO Bool
valueWraps nsStepperCell =
  sendMessage nsStepperCell valueWrapsSelector

-- | @- setValueWraps:@
setValueWraps :: IsNSStepperCell nsStepperCell => nsStepperCell -> Bool -> IO ()
setValueWraps nsStepperCell value =
  sendMessage nsStepperCell setValueWrapsSelector value

-- | @- autorepeat@
autorepeat :: IsNSStepperCell nsStepperCell => nsStepperCell -> IO Bool
autorepeat nsStepperCell =
  sendMessage nsStepperCell autorepeatSelector

-- | @- setAutorepeat:@
setAutorepeat :: IsNSStepperCell nsStepperCell => nsStepperCell -> Bool -> IO ()
setAutorepeat nsStepperCell value =
  sendMessage nsStepperCell setAutorepeatSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @minValue@
minValueSelector :: Selector '[] CDouble
minValueSelector = mkSelector "minValue"

-- | @Selector@ for @setMinValue:@
setMinValueSelector :: Selector '[CDouble] ()
setMinValueSelector = mkSelector "setMinValue:"

-- | @Selector@ for @maxValue@
maxValueSelector :: Selector '[] CDouble
maxValueSelector = mkSelector "maxValue"

-- | @Selector@ for @setMaxValue:@
setMaxValueSelector :: Selector '[CDouble] ()
setMaxValueSelector = mkSelector "setMaxValue:"

-- | @Selector@ for @increment@
incrementSelector :: Selector '[] CDouble
incrementSelector = mkSelector "increment"

-- | @Selector@ for @setIncrement:@
setIncrementSelector :: Selector '[CDouble] ()
setIncrementSelector = mkSelector "setIncrement:"

-- | @Selector@ for @valueWraps@
valueWrapsSelector :: Selector '[] Bool
valueWrapsSelector = mkSelector "valueWraps"

-- | @Selector@ for @setValueWraps:@
setValueWrapsSelector :: Selector '[Bool] ()
setValueWrapsSelector = mkSelector "setValueWraps:"

-- | @Selector@ for @autorepeat@
autorepeatSelector :: Selector '[] Bool
autorepeatSelector = mkSelector "autorepeat"

-- | @Selector@ for @setAutorepeat:@
setAutorepeatSelector :: Selector '[Bool] ()
setAutorepeatSelector = mkSelector "setAutorepeat:"

