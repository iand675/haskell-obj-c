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
minValue :: IsNSStepper nsStepper => nsStepper -> IO CDouble
minValue nsStepper  =
  sendMsg nsStepper (mkSelector "minValue") retCDouble []

-- | @- setMinValue:@
setMinValue :: IsNSStepper nsStepper => nsStepper -> CDouble -> IO ()
setMinValue nsStepper  value =
  sendMsg nsStepper (mkSelector "setMinValue:") retVoid [argCDouble (fromIntegral value)]

-- | @- maxValue@
maxValue :: IsNSStepper nsStepper => nsStepper -> IO CDouble
maxValue nsStepper  =
  sendMsg nsStepper (mkSelector "maxValue") retCDouble []

-- | @- setMaxValue:@
setMaxValue :: IsNSStepper nsStepper => nsStepper -> CDouble -> IO ()
setMaxValue nsStepper  value =
  sendMsg nsStepper (mkSelector "setMaxValue:") retVoid [argCDouble (fromIntegral value)]

-- | @- increment@
increment :: IsNSStepper nsStepper => nsStepper -> IO CDouble
increment nsStepper  =
  sendMsg nsStepper (mkSelector "increment") retCDouble []

-- | @- setIncrement:@
setIncrement :: IsNSStepper nsStepper => nsStepper -> CDouble -> IO ()
setIncrement nsStepper  value =
  sendMsg nsStepper (mkSelector "setIncrement:") retVoid [argCDouble (fromIntegral value)]

-- | @- valueWraps@
valueWraps :: IsNSStepper nsStepper => nsStepper -> IO Bool
valueWraps nsStepper  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsStepper (mkSelector "valueWraps") retCULong []

-- | @- setValueWraps:@
setValueWraps :: IsNSStepper nsStepper => nsStepper -> Bool -> IO ()
setValueWraps nsStepper  value =
  sendMsg nsStepper (mkSelector "setValueWraps:") retVoid [argCULong (if value then 1 else 0)]

-- | @- autorepeat@
autorepeat :: IsNSStepper nsStepper => nsStepper -> IO Bool
autorepeat nsStepper  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsStepper (mkSelector "autorepeat") retCULong []

-- | @- setAutorepeat:@
setAutorepeat :: IsNSStepper nsStepper => nsStepper -> Bool -> IO ()
setAutorepeat nsStepper  value =
  sendMsg nsStepper (mkSelector "setAutorepeat:") retVoid [argCULong (if value then 1 else 0)]

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

