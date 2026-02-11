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
  , rangeWithLowerLimit_upperLimitSelector
  , rangeWithLowerLimitSelector
  , rangeWithUpperLimitSelector
  , rangeWithConstantValueSelector
  , rangeWithValue_varianceSelector
  , rangeWithNoLimitsSelector
  , lowerLimitSelector
  , setLowerLimitSelector
  , upperLimitSelector
  , setUpperLimitSelector


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

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithLowerLimit:upperLimit:@
initWithLowerLimit_upperLimit :: IsSKRange skRange => skRange -> CDouble -> CDouble -> IO (Id SKRange)
initWithLowerLimit_upperLimit skRange  lower upper =
  sendMsg skRange (mkSelector "initWithLowerLimit:upperLimit:") (retPtr retVoid) [argCDouble (fromIntegral lower), argCDouble (fromIntegral upper)] >>= ownedObject . castPtr

-- | @+ rangeWithLowerLimit:upperLimit:@
rangeWithLowerLimit_upperLimit :: CDouble -> CDouble -> IO (Id SKRange)
rangeWithLowerLimit_upperLimit lower upper =
  do
    cls' <- getRequiredClass "SKRange"
    sendClassMsg cls' (mkSelector "rangeWithLowerLimit:upperLimit:") (retPtr retVoid) [argCDouble (fromIntegral lower), argCDouble (fromIntegral upper)] >>= retainedObject . castPtr

-- | @+ rangeWithLowerLimit:@
rangeWithLowerLimit :: CDouble -> IO (Id SKRange)
rangeWithLowerLimit lower =
  do
    cls' <- getRequiredClass "SKRange"
    sendClassMsg cls' (mkSelector "rangeWithLowerLimit:") (retPtr retVoid) [argCDouble (fromIntegral lower)] >>= retainedObject . castPtr

-- | @+ rangeWithUpperLimit:@
rangeWithUpperLimit :: CDouble -> IO (Id SKRange)
rangeWithUpperLimit upper =
  do
    cls' <- getRequiredClass "SKRange"
    sendClassMsg cls' (mkSelector "rangeWithUpperLimit:") (retPtr retVoid) [argCDouble (fromIntegral upper)] >>= retainedObject . castPtr

-- | @+ rangeWithConstantValue:@
rangeWithConstantValue :: CDouble -> IO (Id SKRange)
rangeWithConstantValue value =
  do
    cls' <- getRequiredClass "SKRange"
    sendClassMsg cls' (mkSelector "rangeWithConstantValue:") (retPtr retVoid) [argCDouble (fromIntegral value)] >>= retainedObject . castPtr

-- | @+ rangeWithValue:variance:@
rangeWithValue_variance :: CDouble -> CDouble -> IO (Id SKRange)
rangeWithValue_variance value variance =
  do
    cls' <- getRequiredClass "SKRange"
    sendClassMsg cls' (mkSelector "rangeWithValue:variance:") (retPtr retVoid) [argCDouble (fromIntegral value), argCDouble (fromIntegral variance)] >>= retainedObject . castPtr

-- | @+ rangeWithNoLimits@
rangeWithNoLimits :: IO (Id SKRange)
rangeWithNoLimits  =
  do
    cls' <- getRequiredClass "SKRange"
    sendClassMsg cls' (mkSelector "rangeWithNoLimits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- lowerLimit@
lowerLimit :: IsSKRange skRange => skRange -> IO CDouble
lowerLimit skRange  =
  sendMsg skRange (mkSelector "lowerLimit") retCDouble []

-- | @- setLowerLimit:@
setLowerLimit :: IsSKRange skRange => skRange -> CDouble -> IO ()
setLowerLimit skRange  value =
  sendMsg skRange (mkSelector "setLowerLimit:") retVoid [argCDouble (fromIntegral value)]

-- | @- upperLimit@
upperLimit :: IsSKRange skRange => skRange -> IO CDouble
upperLimit skRange  =
  sendMsg skRange (mkSelector "upperLimit") retCDouble []

-- | @- setUpperLimit:@
setUpperLimit :: IsSKRange skRange => skRange -> CDouble -> IO ()
setUpperLimit skRange  value =
  sendMsg skRange (mkSelector "setUpperLimit:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLowerLimit:upperLimit:@
initWithLowerLimit_upperLimitSelector :: Selector
initWithLowerLimit_upperLimitSelector = mkSelector "initWithLowerLimit:upperLimit:"

-- | @Selector@ for @rangeWithLowerLimit:upperLimit:@
rangeWithLowerLimit_upperLimitSelector :: Selector
rangeWithLowerLimit_upperLimitSelector = mkSelector "rangeWithLowerLimit:upperLimit:"

-- | @Selector@ for @rangeWithLowerLimit:@
rangeWithLowerLimitSelector :: Selector
rangeWithLowerLimitSelector = mkSelector "rangeWithLowerLimit:"

-- | @Selector@ for @rangeWithUpperLimit:@
rangeWithUpperLimitSelector :: Selector
rangeWithUpperLimitSelector = mkSelector "rangeWithUpperLimit:"

-- | @Selector@ for @rangeWithConstantValue:@
rangeWithConstantValueSelector :: Selector
rangeWithConstantValueSelector = mkSelector "rangeWithConstantValue:"

-- | @Selector@ for @rangeWithValue:variance:@
rangeWithValue_varianceSelector :: Selector
rangeWithValue_varianceSelector = mkSelector "rangeWithValue:variance:"

-- | @Selector@ for @rangeWithNoLimits@
rangeWithNoLimitsSelector :: Selector
rangeWithNoLimitsSelector = mkSelector "rangeWithNoLimits"

-- | @Selector@ for @lowerLimit@
lowerLimitSelector :: Selector
lowerLimitSelector = mkSelector "lowerLimit"

-- | @Selector@ for @setLowerLimit:@
setLowerLimitSelector :: Selector
setLowerLimitSelector = mkSelector "setLowerLimit:"

-- | @Selector@ for @upperLimit@
upperLimitSelector :: Selector
upperLimitSelector = mkSelector "upperLimit"

-- | @Selector@ for @setUpperLimit:@
setUpperLimitSelector :: Selector
setUpperLimitSelector = mkSelector "setUpperLimit:"

