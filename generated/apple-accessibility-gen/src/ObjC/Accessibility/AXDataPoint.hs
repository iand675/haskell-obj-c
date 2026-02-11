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
  , initWithX_ySelector
  , initWithX_y_additionalValuesSelector
  , initWithX_y_additionalValues_labelSelector
  , initSelector
  , newSelector
  , xValueSelector
  , setXValueSelector
  , yValueSelector
  , setYValueSelector
  , additionalValuesSelector
  , setAdditionalValuesSelector
  , labelSelector
  , setLabelSelector
  , attributedLabelSelector
  , setAttributedLabelSelector


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

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithX:y:@
initWithX_y :: (IsAXDataPoint axDataPoint, IsAXDataPointValue xValue, IsAXDataPointValue yValue) => axDataPoint -> xValue -> yValue -> IO (Id AXDataPoint)
initWithX_y axDataPoint  xValue yValue =
  withObjCPtr xValue $ \raw_xValue ->
    withObjCPtr yValue $ \raw_yValue ->
        sendMsg axDataPoint (mkSelector "initWithX:y:") (retPtr retVoid) [argPtr (castPtr raw_xValue :: Ptr ()), argPtr (castPtr raw_yValue :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithX:y:additionalValues:@
initWithX_y_additionalValues :: (IsAXDataPoint axDataPoint, IsAXDataPointValue xValue, IsAXDataPointValue yValue, IsNSArray additionalValues) => axDataPoint -> xValue -> yValue -> additionalValues -> IO (Id AXDataPoint)
initWithX_y_additionalValues axDataPoint  xValue yValue additionalValues =
  withObjCPtr xValue $ \raw_xValue ->
    withObjCPtr yValue $ \raw_yValue ->
      withObjCPtr additionalValues $ \raw_additionalValues ->
          sendMsg axDataPoint (mkSelector "initWithX:y:additionalValues:") (retPtr retVoid) [argPtr (castPtr raw_xValue :: Ptr ()), argPtr (castPtr raw_yValue :: Ptr ()), argPtr (castPtr raw_additionalValues :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithX:y:additionalValues:label:@
initWithX_y_additionalValues_label :: (IsAXDataPoint axDataPoint, IsAXDataPointValue xValue, IsAXDataPointValue yValue, IsNSArray additionalValues, IsNSString label) => axDataPoint -> xValue -> yValue -> additionalValues -> label -> IO (Id AXDataPoint)
initWithX_y_additionalValues_label axDataPoint  xValue yValue additionalValues label =
  withObjCPtr xValue $ \raw_xValue ->
    withObjCPtr yValue $ \raw_yValue ->
      withObjCPtr additionalValues $ \raw_additionalValues ->
        withObjCPtr label $ \raw_label ->
            sendMsg axDataPoint (mkSelector "initWithX:y:additionalValues:label:") (retPtr retVoid) [argPtr (castPtr raw_xValue :: Ptr ()), argPtr (castPtr raw_yValue :: Ptr ()), argPtr (castPtr raw_additionalValues :: Ptr ()), argPtr (castPtr raw_label :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsAXDataPoint axDataPoint => axDataPoint -> IO (Id AXDataPoint)
init_ axDataPoint  =
    sendMsg axDataPoint (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AXDataPoint)
new  =
  do
    cls' <- getRequiredClass "AXDataPoint"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The x-axis value for this data point. Should be a Double for a numeric x-axis or a String for a categorical x-axis.
--
-- ObjC selector: @- xValue@
xValue :: IsAXDataPoint axDataPoint => axDataPoint -> IO (Id AXDataPointValue)
xValue axDataPoint  =
    sendMsg axDataPoint (mkSelector "xValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The x-axis value for this data point. Should be a Double for a numeric x-axis or a String for a categorical x-axis.
--
-- ObjC selector: @- setXValue:@
setXValue :: (IsAXDataPoint axDataPoint, IsAXDataPointValue value) => axDataPoint -> value -> IO ()
setXValue axDataPoint  value =
  withObjCPtr value $ \raw_value ->
      sendMsg axDataPoint (mkSelector "setXValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The y-axis value for this data point.
--
-- ObjC selector: @- yValue@
yValue :: IsAXDataPoint axDataPoint => axDataPoint -> IO (Id AXDataPointValue)
yValue axDataPoint  =
    sendMsg axDataPoint (mkSelector "yValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The y-axis value for this data point.
--
-- ObjC selector: @- setYValue:@
setYValue :: (IsAXDataPoint axDataPoint, IsAXDataPointValue value) => axDataPoint -> value -> IO ()
setYValue axDataPoint  value =
  withObjCPtr value $ \raw_value ->
      sendMsg axDataPoint (mkSelector "setYValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Any additional values for additional axes for this data point. These should be provided in the same order as their corresponding @AXDataAxisDescriptor@ objects in @AXChartDescriptor.additionalAxes@.
--
-- ObjC selector: @- additionalValues@
additionalValues :: IsAXDataPoint axDataPoint => axDataPoint -> IO (Id NSArray)
additionalValues axDataPoint  =
    sendMsg axDataPoint (mkSelector "additionalValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Any additional values for additional axes for this data point. These should be provided in the same order as their corresponding @AXDataAxisDescriptor@ objects in @AXChartDescriptor.additionalAxes@.
--
-- ObjC selector: @- setAdditionalValues:@
setAdditionalValues :: (IsAXDataPoint axDataPoint, IsNSArray value) => axDataPoint -> value -> IO ()
setAdditionalValues axDataPoint  value =
  withObjCPtr value $ \raw_value ->
      sendMsg axDataPoint (mkSelector "setAdditionalValues:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A name or label for this data point.
--
-- ObjC selector: @- label@
label :: IsAXDataPoint axDataPoint => axDataPoint -> IO (Id NSString)
label axDataPoint  =
    sendMsg axDataPoint (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A name or label for this data point.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsAXDataPoint axDataPoint, IsNSString value) => axDataPoint -> value -> IO ()
setLabel axDataPoint  value =
  withObjCPtr value $ \raw_value ->
      sendMsg axDataPoint (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | An attributed version of the name or label for this data point.
--
-- ObjC selector: @- attributedLabel@
attributedLabel :: IsAXDataPoint axDataPoint => axDataPoint -> IO (Id NSAttributedString)
attributedLabel axDataPoint  =
    sendMsg axDataPoint (mkSelector "attributedLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An attributed version of the name or label for this data point.
--
-- ObjC selector: @- setAttributedLabel:@
setAttributedLabel :: (IsAXDataPoint axDataPoint, IsNSAttributedString value) => axDataPoint -> value -> IO ()
setAttributedLabel axDataPoint  value =
  withObjCPtr value $ \raw_value ->
      sendMsg axDataPoint (mkSelector "setAttributedLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithX:y:@
initWithX_ySelector :: Selector
initWithX_ySelector = mkSelector "initWithX:y:"

-- | @Selector@ for @initWithX:y:additionalValues:@
initWithX_y_additionalValuesSelector :: Selector
initWithX_y_additionalValuesSelector = mkSelector "initWithX:y:additionalValues:"

-- | @Selector@ for @initWithX:y:additionalValues:label:@
initWithX_y_additionalValues_labelSelector :: Selector
initWithX_y_additionalValues_labelSelector = mkSelector "initWithX:y:additionalValues:label:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @xValue@
xValueSelector :: Selector
xValueSelector = mkSelector "xValue"

-- | @Selector@ for @setXValue:@
setXValueSelector :: Selector
setXValueSelector = mkSelector "setXValue:"

-- | @Selector@ for @yValue@
yValueSelector :: Selector
yValueSelector = mkSelector "yValue"

-- | @Selector@ for @setYValue:@
setYValueSelector :: Selector
setYValueSelector = mkSelector "setYValue:"

-- | @Selector@ for @additionalValues@
additionalValuesSelector :: Selector
additionalValuesSelector = mkSelector "additionalValues"

-- | @Selector@ for @setAdditionalValues:@
setAdditionalValuesSelector :: Selector
setAdditionalValuesSelector = mkSelector "setAdditionalValues:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @attributedLabel@
attributedLabelSelector :: Selector
attributedLabelSelector = mkSelector "attributedLabel"

-- | @Selector@ for @setAttributedLabel:@
setAttributedLabelSelector :: Selector
setAttributedLabelSelector = mkSelector "setAttributedLabel:"

