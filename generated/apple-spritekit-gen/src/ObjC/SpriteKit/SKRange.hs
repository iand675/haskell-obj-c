{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SKRange object used to define a range of allowable values
--
-- Generated bindings for @SKRange@.
module ObjC.SpriteKit.SKRange
  ( SKRange
  , IsSKRange(..)
  , initWithLowerLimit_upperLimit
  , rangeWithLowerLimit_upperLimit
  , rangeWithLowerLimit
  , rangeWithUpperLimit
  , rangeWithConstantValue
  , rangeWithValue_variance
  , rangeWithNoLimits
  , lowerLimit
  , setLowerLimit
  , upperLimit
  , setUpperLimit
  , initWithLowerLimit_upperLimitSelector
  , lowerLimitSelector
  , rangeWithConstantValueSelector
  , rangeWithLowerLimitSelector
  , rangeWithLowerLimit_upperLimitSelector
  , rangeWithNoLimitsSelector
  , rangeWithUpperLimitSelector
  , rangeWithValue_varianceSelector
  , setLowerLimitSelector
  , setUpperLimitSelector
  , upperLimitSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithLowerLimit:upperLimit:@
initWithLowerLimit_upperLimit :: IsSKRange skRange => skRange -> CDouble -> CDouble -> IO (Id SKRange)
initWithLowerLimit_upperLimit skRange lower upper =
  sendOwnedMessage skRange initWithLowerLimit_upperLimitSelector lower upper

-- | @+ rangeWithLowerLimit:upperLimit:@
rangeWithLowerLimit_upperLimit :: CDouble -> CDouble -> IO (Id SKRange)
rangeWithLowerLimit_upperLimit lower upper =
  do
    cls' <- getRequiredClass "SKRange"
    sendClassMessage cls' rangeWithLowerLimit_upperLimitSelector lower upper

-- | @+ rangeWithLowerLimit:@
rangeWithLowerLimit :: CDouble -> IO (Id SKRange)
rangeWithLowerLimit lower =
  do
    cls' <- getRequiredClass "SKRange"
    sendClassMessage cls' rangeWithLowerLimitSelector lower

-- | @+ rangeWithUpperLimit:@
rangeWithUpperLimit :: CDouble -> IO (Id SKRange)
rangeWithUpperLimit upper =
  do
    cls' <- getRequiredClass "SKRange"
    sendClassMessage cls' rangeWithUpperLimitSelector upper

-- | @+ rangeWithConstantValue:@
rangeWithConstantValue :: CDouble -> IO (Id SKRange)
rangeWithConstantValue value =
  do
    cls' <- getRequiredClass "SKRange"
    sendClassMessage cls' rangeWithConstantValueSelector value

-- | @+ rangeWithValue:variance:@
rangeWithValue_variance :: CDouble -> CDouble -> IO (Id SKRange)
rangeWithValue_variance value variance =
  do
    cls' <- getRequiredClass "SKRange"
    sendClassMessage cls' rangeWithValue_varianceSelector value variance

-- | @+ rangeWithNoLimits@
rangeWithNoLimits :: IO (Id SKRange)
rangeWithNoLimits  =
  do
    cls' <- getRequiredClass "SKRange"
    sendClassMessage cls' rangeWithNoLimitsSelector

-- | @- lowerLimit@
lowerLimit :: IsSKRange skRange => skRange -> IO CDouble
lowerLimit skRange =
  sendMessage skRange lowerLimitSelector

-- | @- setLowerLimit:@
setLowerLimit :: IsSKRange skRange => skRange -> CDouble -> IO ()
setLowerLimit skRange value =
  sendMessage skRange setLowerLimitSelector value

-- | @- upperLimit@
upperLimit :: IsSKRange skRange => skRange -> IO CDouble
upperLimit skRange =
  sendMessage skRange upperLimitSelector

-- | @- setUpperLimit:@
setUpperLimit :: IsSKRange skRange => skRange -> CDouble -> IO ()
setUpperLimit skRange value =
  sendMessage skRange setUpperLimitSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLowerLimit:upperLimit:@
initWithLowerLimit_upperLimitSelector :: Selector '[CDouble, CDouble] (Id SKRange)
initWithLowerLimit_upperLimitSelector = mkSelector "initWithLowerLimit:upperLimit:"

-- | @Selector@ for @rangeWithLowerLimit:upperLimit:@
rangeWithLowerLimit_upperLimitSelector :: Selector '[CDouble, CDouble] (Id SKRange)
rangeWithLowerLimit_upperLimitSelector = mkSelector "rangeWithLowerLimit:upperLimit:"

-- | @Selector@ for @rangeWithLowerLimit:@
rangeWithLowerLimitSelector :: Selector '[CDouble] (Id SKRange)
rangeWithLowerLimitSelector = mkSelector "rangeWithLowerLimit:"

-- | @Selector@ for @rangeWithUpperLimit:@
rangeWithUpperLimitSelector :: Selector '[CDouble] (Id SKRange)
rangeWithUpperLimitSelector = mkSelector "rangeWithUpperLimit:"

-- | @Selector@ for @rangeWithConstantValue:@
rangeWithConstantValueSelector :: Selector '[CDouble] (Id SKRange)
rangeWithConstantValueSelector = mkSelector "rangeWithConstantValue:"

-- | @Selector@ for @rangeWithValue:variance:@
rangeWithValue_varianceSelector :: Selector '[CDouble, CDouble] (Id SKRange)
rangeWithValue_varianceSelector = mkSelector "rangeWithValue:variance:"

-- | @Selector@ for @rangeWithNoLimits@
rangeWithNoLimitsSelector :: Selector '[] (Id SKRange)
rangeWithNoLimitsSelector = mkSelector "rangeWithNoLimits"

-- | @Selector@ for @lowerLimit@
lowerLimitSelector :: Selector '[] CDouble
lowerLimitSelector = mkSelector "lowerLimit"

-- | @Selector@ for @setLowerLimit:@
setLowerLimitSelector :: Selector '[CDouble] ()
setLowerLimitSelector = mkSelector "setLowerLimit:"

-- | @Selector@ for @upperLimit@
upperLimitSelector :: Selector '[] CDouble
upperLimitSelector = mkSelector "upperLimit"

-- | @Selector@ for @setUpperLimit:@
setUpperLimitSelector :: Selector '[CDouble] ()
setUpperLimitSelector = mkSelector "setUpperLimit:"

