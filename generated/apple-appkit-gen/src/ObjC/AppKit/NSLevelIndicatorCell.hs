{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , criticalValueSelector
  , initWithLevelIndicatorStyleSelector
  , levelIndicatorStyleSelector
  , maxValueSelector
  , minValueSelector
  , numberOfMajorTickMarksSelector
  , numberOfTickMarksSelector
  , rectOfTickMarkAtIndexSelector
  , setCriticalValueSelector
  , setLevelIndicatorStyleSelector
  , setMaxValueSelector
  , setMinValueSelector
  , setNumberOfMajorTickMarksSelector
  , setNumberOfTickMarksSelector
  , setTickMarkPositionSelector
  , setWarningValueSelector
  , tickMarkPositionSelector
  , tickMarkValueAtIndexSelector
  , warningValueSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithLevelIndicatorStyle:@
initWithLevelIndicatorStyle :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> NSLevelIndicatorStyle -> IO (Id NSLevelIndicatorCell)
initWithLevelIndicatorStyle nsLevelIndicatorCell levelIndicatorStyle =
  sendOwnedMessage nsLevelIndicatorCell initWithLevelIndicatorStyleSelector levelIndicatorStyle

-- | @- rectOfTickMarkAtIndex:@
rectOfTickMarkAtIndex :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> CLong -> IO NSRect
rectOfTickMarkAtIndex nsLevelIndicatorCell index =
  sendMessage nsLevelIndicatorCell rectOfTickMarkAtIndexSelector index

-- | @- tickMarkValueAtIndex:@
tickMarkValueAtIndex :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> CLong -> IO CDouble
tickMarkValueAtIndex nsLevelIndicatorCell index =
  sendMessage nsLevelIndicatorCell tickMarkValueAtIndexSelector index

-- | @- levelIndicatorStyle@
levelIndicatorStyle :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> IO NSLevelIndicatorStyle
levelIndicatorStyle nsLevelIndicatorCell =
  sendMessage nsLevelIndicatorCell levelIndicatorStyleSelector

-- | @- setLevelIndicatorStyle:@
setLevelIndicatorStyle :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> NSLevelIndicatorStyle -> IO ()
setLevelIndicatorStyle nsLevelIndicatorCell value =
  sendMessage nsLevelIndicatorCell setLevelIndicatorStyleSelector value

-- | @- minValue@
minValue :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> IO CDouble
minValue nsLevelIndicatorCell =
  sendMessage nsLevelIndicatorCell minValueSelector

-- | @- setMinValue:@
setMinValue :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> CDouble -> IO ()
setMinValue nsLevelIndicatorCell value =
  sendMessage nsLevelIndicatorCell setMinValueSelector value

-- | @- maxValue@
maxValue :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> IO CDouble
maxValue nsLevelIndicatorCell =
  sendMessage nsLevelIndicatorCell maxValueSelector

-- | @- setMaxValue:@
setMaxValue :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> CDouble -> IO ()
setMaxValue nsLevelIndicatorCell value =
  sendMessage nsLevelIndicatorCell setMaxValueSelector value

-- | @- warningValue@
warningValue :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> IO CDouble
warningValue nsLevelIndicatorCell =
  sendMessage nsLevelIndicatorCell warningValueSelector

-- | @- setWarningValue:@
setWarningValue :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> CDouble -> IO ()
setWarningValue nsLevelIndicatorCell value =
  sendMessage nsLevelIndicatorCell setWarningValueSelector value

-- | @- criticalValue@
criticalValue :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> IO CDouble
criticalValue nsLevelIndicatorCell =
  sendMessage nsLevelIndicatorCell criticalValueSelector

-- | @- setCriticalValue:@
setCriticalValue :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> CDouble -> IO ()
setCriticalValue nsLevelIndicatorCell value =
  sendMessage nsLevelIndicatorCell setCriticalValueSelector value

-- | @- tickMarkPosition@
tickMarkPosition :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> IO NSTickMarkPosition
tickMarkPosition nsLevelIndicatorCell =
  sendMessage nsLevelIndicatorCell tickMarkPositionSelector

-- | @- setTickMarkPosition:@
setTickMarkPosition :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> NSTickMarkPosition -> IO ()
setTickMarkPosition nsLevelIndicatorCell value =
  sendMessage nsLevelIndicatorCell setTickMarkPositionSelector value

-- | @- numberOfTickMarks@
numberOfTickMarks :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> IO CLong
numberOfTickMarks nsLevelIndicatorCell =
  sendMessage nsLevelIndicatorCell numberOfTickMarksSelector

-- | @- setNumberOfTickMarks:@
setNumberOfTickMarks :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> CLong -> IO ()
setNumberOfTickMarks nsLevelIndicatorCell value =
  sendMessage nsLevelIndicatorCell setNumberOfTickMarksSelector value

-- | @- numberOfMajorTickMarks@
numberOfMajorTickMarks :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> IO CLong
numberOfMajorTickMarks nsLevelIndicatorCell =
  sendMessage nsLevelIndicatorCell numberOfMajorTickMarksSelector

-- | @- setNumberOfMajorTickMarks:@
setNumberOfMajorTickMarks :: IsNSLevelIndicatorCell nsLevelIndicatorCell => nsLevelIndicatorCell -> CLong -> IO ()
setNumberOfMajorTickMarks nsLevelIndicatorCell value =
  sendMessage nsLevelIndicatorCell setNumberOfMajorTickMarksSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLevelIndicatorStyle:@
initWithLevelIndicatorStyleSelector :: Selector '[NSLevelIndicatorStyle] (Id NSLevelIndicatorCell)
initWithLevelIndicatorStyleSelector = mkSelector "initWithLevelIndicatorStyle:"

-- | @Selector@ for @rectOfTickMarkAtIndex:@
rectOfTickMarkAtIndexSelector :: Selector '[CLong] NSRect
rectOfTickMarkAtIndexSelector = mkSelector "rectOfTickMarkAtIndex:"

-- | @Selector@ for @tickMarkValueAtIndex:@
tickMarkValueAtIndexSelector :: Selector '[CLong] CDouble
tickMarkValueAtIndexSelector = mkSelector "tickMarkValueAtIndex:"

-- | @Selector@ for @levelIndicatorStyle@
levelIndicatorStyleSelector :: Selector '[] NSLevelIndicatorStyle
levelIndicatorStyleSelector = mkSelector "levelIndicatorStyle"

-- | @Selector@ for @setLevelIndicatorStyle:@
setLevelIndicatorStyleSelector :: Selector '[NSLevelIndicatorStyle] ()
setLevelIndicatorStyleSelector = mkSelector "setLevelIndicatorStyle:"

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

-- | @Selector@ for @warningValue@
warningValueSelector :: Selector '[] CDouble
warningValueSelector = mkSelector "warningValue"

-- | @Selector@ for @setWarningValue:@
setWarningValueSelector :: Selector '[CDouble] ()
setWarningValueSelector = mkSelector "setWarningValue:"

-- | @Selector@ for @criticalValue@
criticalValueSelector :: Selector '[] CDouble
criticalValueSelector = mkSelector "criticalValue"

-- | @Selector@ for @setCriticalValue:@
setCriticalValueSelector :: Selector '[CDouble] ()
setCriticalValueSelector = mkSelector "setCriticalValue:"

-- | @Selector@ for @tickMarkPosition@
tickMarkPositionSelector :: Selector '[] NSTickMarkPosition
tickMarkPositionSelector = mkSelector "tickMarkPosition"

-- | @Selector@ for @setTickMarkPosition:@
setTickMarkPositionSelector :: Selector '[NSTickMarkPosition] ()
setTickMarkPositionSelector = mkSelector "setTickMarkPosition:"

-- | @Selector@ for @numberOfTickMarks@
numberOfTickMarksSelector :: Selector '[] CLong
numberOfTickMarksSelector = mkSelector "numberOfTickMarks"

-- | @Selector@ for @setNumberOfTickMarks:@
setNumberOfTickMarksSelector :: Selector '[CLong] ()
setNumberOfTickMarksSelector = mkSelector "setNumberOfTickMarks:"

-- | @Selector@ for @numberOfMajorTickMarks@
numberOfMajorTickMarksSelector :: Selector '[] CLong
numberOfMajorTickMarksSelector = mkSelector "numberOfMajorTickMarks"

-- | @Selector@ for @setNumberOfMajorTickMarks:@
setNumberOfMajorTickMarksSelector :: Selector '[CLong] ()
setNumberOfMajorTickMarksSelector = mkSelector "setNumberOfMajorTickMarks:"

