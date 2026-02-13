{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Provides axis values for a single data point within a series.
--
-- Generated bindings for @AXDataPoint@.
module ObjC.Accessibility.AXDataPoint
  ( AXDataPoint
  , IsAXDataPoint(..)
  , initWithX_y
  , initWithX_y_additionalValues
  , initWithX_y_additionalValues_label
  , init_
  , new
  , xValue
  , setXValue
  , yValue
  , setYValue
  , additionalValues
  , setAdditionalValues
  , label
  , setLabel
  , attributedLabel
  , setAttributedLabel
  , additionalValuesSelector
  , attributedLabelSelector
  , initSelector
  , initWithX_ySelector
  , initWithX_y_additionalValuesSelector
  , initWithX_y_additionalValues_labelSelector
  , labelSelector
  , newSelector
  , setAdditionalValuesSelector
  , setAttributedLabelSelector
  , setLabelSelector
  , setXValueSelector
  , setYValueSelector
  , xValueSelector
  , yValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithX:y:@
initWithX_y :: (IsAXDataPoint axDataPoint, IsAXDataPointValue xValue, IsAXDataPointValue yValue) => axDataPoint -> xValue -> yValue -> IO (Id AXDataPoint)
initWithX_y axDataPoint xValue yValue =
  sendOwnedMessage axDataPoint initWithX_ySelector (toAXDataPointValue xValue) (toAXDataPointValue yValue)

-- | @- initWithX:y:additionalValues:@
initWithX_y_additionalValues :: (IsAXDataPoint axDataPoint, IsAXDataPointValue xValue, IsAXDataPointValue yValue, IsNSArray additionalValues) => axDataPoint -> xValue -> yValue -> additionalValues -> IO (Id AXDataPoint)
initWithX_y_additionalValues axDataPoint xValue yValue additionalValues =
  sendOwnedMessage axDataPoint initWithX_y_additionalValuesSelector (toAXDataPointValue xValue) (toAXDataPointValue yValue) (toNSArray additionalValues)

-- | @- initWithX:y:additionalValues:label:@
initWithX_y_additionalValues_label :: (IsAXDataPoint axDataPoint, IsAXDataPointValue xValue, IsAXDataPointValue yValue, IsNSArray additionalValues, IsNSString label) => axDataPoint -> xValue -> yValue -> additionalValues -> label -> IO (Id AXDataPoint)
initWithX_y_additionalValues_label axDataPoint xValue yValue additionalValues label =
  sendOwnedMessage axDataPoint initWithX_y_additionalValues_labelSelector (toAXDataPointValue xValue) (toAXDataPointValue yValue) (toNSArray additionalValues) (toNSString label)

-- | @- init@
init_ :: IsAXDataPoint axDataPoint => axDataPoint -> IO (Id AXDataPoint)
init_ axDataPoint =
  sendOwnedMessage axDataPoint initSelector

-- | @+ new@
new :: IO (Id AXDataPoint)
new  =
  do
    cls' <- getRequiredClass "AXDataPoint"
    sendOwnedClassMessage cls' newSelector

-- | The x-axis value for this data point. Should be a Double for a numeric x-axis or a String for a categorical x-axis.
--
-- ObjC selector: @- xValue@
xValue :: IsAXDataPoint axDataPoint => axDataPoint -> IO (Id AXDataPointValue)
xValue axDataPoint =
  sendMessage axDataPoint xValueSelector

-- | The x-axis value for this data point. Should be a Double for a numeric x-axis or a String for a categorical x-axis.
--
-- ObjC selector: @- setXValue:@
setXValue :: (IsAXDataPoint axDataPoint, IsAXDataPointValue value) => axDataPoint -> value -> IO ()
setXValue axDataPoint value =
  sendMessage axDataPoint setXValueSelector (toAXDataPointValue value)

-- | The y-axis value for this data point.
--
-- ObjC selector: @- yValue@
yValue :: IsAXDataPoint axDataPoint => axDataPoint -> IO (Id AXDataPointValue)
yValue axDataPoint =
  sendMessage axDataPoint yValueSelector

-- | The y-axis value for this data point.
--
-- ObjC selector: @- setYValue:@
setYValue :: (IsAXDataPoint axDataPoint, IsAXDataPointValue value) => axDataPoint -> value -> IO ()
setYValue axDataPoint value =
  sendMessage axDataPoint setYValueSelector (toAXDataPointValue value)

-- | Any additional values for additional axes for this data point. These should be provided in the same order as their corresponding @AXDataAxisDescriptor@ objects in @AXChartDescriptor.additionalAxes@.
--
-- ObjC selector: @- additionalValues@
additionalValues :: IsAXDataPoint axDataPoint => axDataPoint -> IO (Id NSArray)
additionalValues axDataPoint =
  sendMessage axDataPoint additionalValuesSelector

-- | Any additional values for additional axes for this data point. These should be provided in the same order as their corresponding @AXDataAxisDescriptor@ objects in @AXChartDescriptor.additionalAxes@.
--
-- ObjC selector: @- setAdditionalValues:@
setAdditionalValues :: (IsAXDataPoint axDataPoint, IsNSArray value) => axDataPoint -> value -> IO ()
setAdditionalValues axDataPoint value =
  sendMessage axDataPoint setAdditionalValuesSelector (toNSArray value)

-- | A name or label for this data point.
--
-- ObjC selector: @- label@
label :: IsAXDataPoint axDataPoint => axDataPoint -> IO (Id NSString)
label axDataPoint =
  sendMessage axDataPoint labelSelector

-- | A name or label for this data point.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsAXDataPoint axDataPoint, IsNSString value) => axDataPoint -> value -> IO ()
setLabel axDataPoint value =
  sendMessage axDataPoint setLabelSelector (toNSString value)

-- | An attributed version of the name or label for this data point.
--
-- ObjC selector: @- attributedLabel@
attributedLabel :: IsAXDataPoint axDataPoint => axDataPoint -> IO (Id NSAttributedString)
attributedLabel axDataPoint =
  sendMessage axDataPoint attributedLabelSelector

-- | An attributed version of the name or label for this data point.
--
-- ObjC selector: @- setAttributedLabel:@
setAttributedLabel :: (IsAXDataPoint axDataPoint, IsNSAttributedString value) => axDataPoint -> value -> IO ()
setAttributedLabel axDataPoint value =
  sendMessage axDataPoint setAttributedLabelSelector (toNSAttributedString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithX:y:@
initWithX_ySelector :: Selector '[Id AXDataPointValue, Id AXDataPointValue] (Id AXDataPoint)
initWithX_ySelector = mkSelector "initWithX:y:"

-- | @Selector@ for @initWithX:y:additionalValues:@
initWithX_y_additionalValuesSelector :: Selector '[Id AXDataPointValue, Id AXDataPointValue, Id NSArray] (Id AXDataPoint)
initWithX_y_additionalValuesSelector = mkSelector "initWithX:y:additionalValues:"

-- | @Selector@ for @initWithX:y:additionalValues:label:@
initWithX_y_additionalValues_labelSelector :: Selector '[Id AXDataPointValue, Id AXDataPointValue, Id NSArray, Id NSString] (Id AXDataPoint)
initWithX_y_additionalValues_labelSelector = mkSelector "initWithX:y:additionalValues:label:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AXDataPoint)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AXDataPoint)
newSelector = mkSelector "new"

-- | @Selector@ for @xValue@
xValueSelector :: Selector '[] (Id AXDataPointValue)
xValueSelector = mkSelector "xValue"

-- | @Selector@ for @setXValue:@
setXValueSelector :: Selector '[Id AXDataPointValue] ()
setXValueSelector = mkSelector "setXValue:"

-- | @Selector@ for @yValue@
yValueSelector :: Selector '[] (Id AXDataPointValue)
yValueSelector = mkSelector "yValue"

-- | @Selector@ for @setYValue:@
setYValueSelector :: Selector '[Id AXDataPointValue] ()
setYValueSelector = mkSelector "setYValue:"

-- | @Selector@ for @additionalValues@
additionalValuesSelector :: Selector '[] (Id NSArray)
additionalValuesSelector = mkSelector "additionalValues"

-- | @Selector@ for @setAdditionalValues:@
setAdditionalValuesSelector :: Selector '[Id NSArray] ()
setAdditionalValuesSelector = mkSelector "setAdditionalValues:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @attributedLabel@
attributedLabelSelector :: Selector '[] (Id NSAttributedString)
attributedLabelSelector = mkSelector "attributedLabel"

-- | @Selector@ for @setAttributedLabel:@
setAttributedLabelSelector :: Selector '[Id NSAttributedString] ()
setAttributedLabelSelector = mkSelector "setAttributedLabel:"

