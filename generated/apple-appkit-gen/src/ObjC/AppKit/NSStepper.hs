{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSStepper@.
module ObjC.AppKit.NSStepper
  ( NSStepper
  , IsNSStepper(..)
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
minValue :: IsNSStepper nsStepper => nsStepper -> IO CDouble
minValue nsStepper =
  sendMessage nsStepper minValueSelector

-- | @- setMinValue:@
setMinValue :: IsNSStepper nsStepper => nsStepper -> CDouble -> IO ()
setMinValue nsStepper value =
  sendMessage nsStepper setMinValueSelector value

-- | @- maxValue@
maxValue :: IsNSStepper nsStepper => nsStepper -> IO CDouble
maxValue nsStepper =
  sendMessage nsStepper maxValueSelector

-- | @- setMaxValue:@
setMaxValue :: IsNSStepper nsStepper => nsStepper -> CDouble -> IO ()
setMaxValue nsStepper value =
  sendMessage nsStepper setMaxValueSelector value

-- | @- increment@
increment :: IsNSStepper nsStepper => nsStepper -> IO CDouble
increment nsStepper =
  sendMessage nsStepper incrementSelector

-- | @- setIncrement:@
setIncrement :: IsNSStepper nsStepper => nsStepper -> CDouble -> IO ()
setIncrement nsStepper value =
  sendMessage nsStepper setIncrementSelector value

-- | @- valueWraps@
valueWraps :: IsNSStepper nsStepper => nsStepper -> IO Bool
valueWraps nsStepper =
  sendMessage nsStepper valueWrapsSelector

-- | @- setValueWraps:@
setValueWraps :: IsNSStepper nsStepper => nsStepper -> Bool -> IO ()
setValueWraps nsStepper value =
  sendMessage nsStepper setValueWrapsSelector value

-- | @- autorepeat@
autorepeat :: IsNSStepper nsStepper => nsStepper -> IO Bool
autorepeat nsStepper =
  sendMessage nsStepper autorepeatSelector

-- | @- setAutorepeat:@
setAutorepeat :: IsNSStepper nsStepper => nsStepper -> Bool -> IO ()
setAutorepeat nsStepper value =
  sendMessage nsStepper setAutorepeatSelector value

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

