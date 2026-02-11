{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSLevelIndicatorCell@.
module ObjC.AppKit.NSLevelIndicatorCell
  ( NSLevelIndicatorCell
  , IsNSLevelIndicatorCell(..)
  , initWithLevelIndicatorStyle
  , rectOfTickMarkAtIndex
  , tickMarkValueAtIndex
  , levelIndicatorStyle
  , setLevelIndicatorStyle
  , minValue
  , setMinValue
  , maxValue
  , setMaxValue
  , warningValue
  , setWarningValue
  , criticalValue
  , setCriticalValue
  , tickMarkPosition
  , setTickMarkPosition
  , numberOfTickMarks
  , setNumberOfTickMarks
  , numberOfMajorTickMarks
  , setNumberOfMajorTickMarks
  , initWithLevelIndicatorStyleSelector
  , rectOfTickMarkAtIndexSelector
  , tickMarkValueAtIndexSelector
  , levelIndicatorStyleSelector
  , setLevelIndicatorStyleSelector
  , minValueSelector
  , setMinValueSelector
  , maxValueSelector
  , setMaxValueSelector
  , warningValueSelector
  , setWarningValueSelector
  , criticalValueSelector
  , setCriticalValueSelector
  , tickMarkPositionSelector
  , setTickMarkPositionSelector
  , numberOfTickMarksSelector
  , setNumberOfTickMarksSelector
  , numberOfMajorTickMarksSelector
  , setNumberOfMajorTickMarksSelector

  -- * Enum types
  , NSLevelIndicatorStyle(NSLevelIndicatorStyle)
  , pattern NSLevelIndicatorStyleRelevancy
  , pattern NSLevelIndicatorStyleContinuousCapacity
  , pattern NSLevelIndicatorStyleDiscreteCapacity
  , pattern NSLevelIndicatorStyleRating
  , NSTickMarkPosition(NSTickMarkPosition)
  , pattern NSTickMarkPositionBelow
  , pattern NSTickMarkPositionAbove
  , pattern NSTickMarkPositionLeading
  , pattern NSTickMarkPositionTrailing

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithLevelIndicatorStyle:@
initWithLevelIndicatorStyle :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> NSLevelIndicatorStyle -> IO (Id NSLevelIndicatorCell)
initWithLevelIndicatorStyle nsLevelIndicatorCell  levelIndicatorStyle =
  sendMsg nsLevelIndicatorCell (mkSelector "initWithLevelIndicatorStyle:") (retPtr retVoid) [argCULong (coerce levelIndicatorStyle)] >>= ownedObject . castPtr

-- | @- rectOfTickMarkAtIndex:@
rectOfTickMarkAtIndex :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> CLong -> IO NSRect
rectOfTickMarkAtIndex nsLevelIndicatorCell  index =
  sendMsgStret nsLevelIndicatorCell (mkSelector "rectOfTickMarkAtIndex:") retNSRect [argCLong (fromIntegral index)]

-- | @- tickMarkValueAtIndex:@
tickMarkValueAtIndex :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> CLong -> IO CDouble
tickMarkValueAtIndex nsLevelIndicatorCell  index =
  sendMsg nsLevelIndicatorCell (mkSelector "tickMarkValueAtIndex:") retCDouble [argCLong (fromIntegral index)]

-- | @- levelIndicatorStyle@
levelIndicatorStyle :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> IO NSLevelIndicatorStyle
levelIndicatorStyle nsLevelIndicatorCell  =
  fmap (coerce :: CULong -> NSLevelIndicatorStyle) $ sendMsg nsLevelIndicatorCell (mkSelector "levelIndicatorStyle") retCULong []

-- | @- setLevelIndicatorStyle:@
setLevelIndicatorStyle :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> NSLevelIndicatorStyle -> IO ()
setLevelIndicatorStyle nsLevelIndicatorCell  value =
  sendMsg nsLevelIndicatorCell (mkSelector "setLevelIndicatorStyle:") retVoid [argCULong (coerce value)]

-- | @- minValue@
minValue :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> IO CDouble
minValue nsLevelIndicatorCell  =
  sendMsg nsLevelIndicatorCell (mkSelector "minValue") retCDouble []

-- | @- setMinValue:@
setMinValue :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> CDouble -> IO ()
setMinValue nsLevelIndicatorCell  value =
  sendMsg nsLevelIndicatorCell (mkSelector "setMinValue:") retVoid [argCDouble (fromIntegral value)]

-- | @- maxValue@
maxValue :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> IO CDouble
maxValue nsLevelIndicatorCell  =
  sendMsg nsLevelIndicatorCell (mkSelector "maxValue") retCDouble []

-- | @- setMaxValue:@
setMaxValue :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> CDouble -> IO ()
setMaxValue nsLevelIndicatorCell  value =
  sendMsg nsLevelIndicatorCell (mkSelector "setMaxValue:") retVoid [argCDouble (fromIntegral value)]

-- | @- warningValue@
warningValue :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> IO CDouble
warningValue nsLevelIndicatorCell  =
  sendMsg nsLevelIndicatorCell (mkSelector "warningValue") retCDouble []

-- | @- setWarningValue:@
setWarningValue :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> CDouble -> IO ()
setWarningValue nsLevelIndicatorCell  value =
  sendMsg nsLevelIndicatorCell (mkSelector "setWarningValue:") retVoid [argCDouble (fromIntegral value)]

-- | @- criticalValue@
criticalValue :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> IO CDouble
criticalValue nsLevelIndicatorCell  =
  sendMsg nsLevelIndicatorCell (mkSelector "criticalValue") retCDouble []

-- | @- setCriticalValue:@
setCriticalValue :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> CDouble -> IO ()
setCriticalValue nsLevelIndicatorCell  value =
  sendMsg nsLevelIndicatorCell (mkSelector "setCriticalValue:") retVoid [argCDouble (fromIntegral value)]

-- | @- tickMarkPosition@
tickMarkPosition :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> IO NSTickMarkPosition
tickMarkPosition nsLevelIndicatorCell  =
  fmap (coerce :: CULong -> NSTickMarkPosition) $ sendMsg nsLevelIndicatorCell (mkSelector "tickMarkPosition") retCULong []

-- | @- setTickMarkPosition:@
setTickMarkPosition :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> NSTickMarkPosition -> IO ()
setTickMarkPosition nsLevelIndicatorCell  value =
  sendMsg nsLevelIndicatorCell (mkSelector "setTickMarkPosition:") retVoid [argCULong (coerce value)]

-- | @- numberOfTickMarks@
numberOfTickMarks :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> IO CLong
numberOfTickMarks nsLevelIndicatorCell  =
  sendMsg nsLevelIndicatorCell (mkSelector "numberOfTickMarks") retCLong []

-- | @- setNumberOfTickMarks:@
setNumberOfTickMarks :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> CLong -> IO ()
setNumberOfTickMarks nsLevelIndicatorCell  value =
  sendMsg nsLevelIndicatorCell (mkSelector "setNumberOfTickMarks:") retVoid [argCLong (fromIntegral value)]

-- | @- numberOfMajorTickMarks@
numberOfMajorTickMarks :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> IO CLong
numberOfMajorTickMarks nsLevelIndicatorCell  =
  sendMsg nsLevelIndicatorCell (mkSelector "numberOfMajorTickMarks") retCLong []

-- | @- setNumberOfMajorTickMarks:@
setNumberOfMajorTickMarks :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> CLong -> IO ()
setNumberOfMajorTickMarks nsLevelIndicatorCell  value =
  sendMsg nsLevelIndicatorCell (mkSelector "setNumberOfMajorTickMarks:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLevelIndicatorStyle:@
initWithLevelIndicatorStyleSelector :: Selector
initWithLevelIndicatorStyleSelector = mkSelector "initWithLevelIndicatorStyle:"

-- | @Selector@ for @rectOfTickMarkAtIndex:@
rectOfTickMarkAtIndexSelector :: Selector
rectOfTickMarkAtIndexSelector = mkSelector "rectOfTickMarkAtIndex:"

-- | @Selector@ for @tickMarkValueAtIndex:@
tickMarkValueAtIndexSelector :: Selector
tickMarkValueAtIndexSelector = mkSelector "tickMarkValueAtIndex:"

-- | @Selector@ for @levelIndicatorStyle@
levelIndicatorStyleSelector :: Selector
levelIndicatorStyleSelector = mkSelector "levelIndicatorStyle"

-- | @Selector@ for @setLevelIndicatorStyle:@
setLevelIndicatorStyleSelector :: Selector
setLevelIndicatorStyleSelector = mkSelector "setLevelIndicatorStyle:"

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

-- | @Selector@ for @warningValue@
warningValueSelector :: Selector
warningValueSelector = mkSelector "warningValue"

-- | @Selector@ for @setWarningValue:@
setWarningValueSelector :: Selector
setWarningValueSelector = mkSelector "setWarningValue:"

-- | @Selector@ for @criticalValue@
criticalValueSelector :: Selector
criticalValueSelector = mkSelector "criticalValue"

-- | @Selector@ for @setCriticalValue:@
setCriticalValueSelector :: Selector
setCriticalValueSelector = mkSelector "setCriticalValue:"

-- | @Selector@ for @tickMarkPosition@
tickMarkPositionSelector :: Selector
tickMarkPositionSelector = mkSelector "tickMarkPosition"

-- | @Selector@ for @setTickMarkPosition:@
setTickMarkPositionSelector :: Selector
setTickMarkPositionSelector = mkSelector "setTickMarkPosition:"

-- | @Selector@ for @numberOfTickMarks@
numberOfTickMarksSelector :: Selector
numberOfTickMarksSelector = mkSelector "numberOfTickMarks"

-- | @Selector@ for @setNumberOfTickMarks:@
setNumberOfTickMarksSelector :: Selector
setNumberOfTickMarksSelector = mkSelector "setNumberOfTickMarks:"

-- | @Selector@ for @numberOfMajorTickMarks@
numberOfMajorTickMarksSelector :: Selector
numberOfMajorTickMarksSelector = mkSelector "numberOfMajorTickMarks"

-- | @Selector@ for @setNumberOfMajorTickMarks:@
setNumberOfMajorTickMarksSelector :: Selector
setNumberOfMajorTickMarksSelector = mkSelector "setNumberOfMajorTickMarks:"

