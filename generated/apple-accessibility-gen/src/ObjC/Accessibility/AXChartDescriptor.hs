{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The top-level descriptor object for an accessible chart.
--
-- Generated bindings for @AXChartDescriptor@.
module ObjC.Accessibility.AXChartDescriptor
  ( AXChartDescriptor
  , IsAXChartDescriptor(..)
  , initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_series
  , initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_series
  , initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_series
  , initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_series
  , init_
  , new
  , title
  , setTitle
  , attributedTitle
  , setAttributedTitle
  , summary
  , setSummary
  , contentDirection
  , setContentDirection
  , series
  , setSeries
  , xAxis
  , setXAxis
  , yAxis
  , setYAxis
  , additionalAxes
  , setAdditionalAxes
  , initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_seriesSelector
  , initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_seriesSelector
  , initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_seriesSelector
  , initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_seriesSelector
  , initSelector
  , newSelector
  , titleSelector
  , setTitleSelector
  , attributedTitleSelector
  , setAttributedTitleSelector
  , summarySelector
  , setSummarySelector
  , contentDirectionSelector
  , setContentDirectionSelector
  , seriesSelector
  , setSeriesSelector
  , xAxisSelector
  , setXAxisSelector
  , yAxisSelector
  , setYAxisSelector
  , additionalAxesSelector
  , setAdditionalAxesSelector

  -- * Enum types
  , AXChartDescriptorContentDirection(AXChartDescriptorContentDirection)
  , pattern AXChartContentDirectionLeftToRight
  , pattern AXChartContentDirectionRightToLeft
  , pattern AXChartContentDirectionTopToBottom
  , pattern AXChartContentDirectionBottomToTop
  , pattern AXChartContentDirectionRadialClockwise
  , pattern AXChartContentDirectionRadialCounterClockwise

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

import ObjC.Accessibility.Internal.Classes
import ObjC.Accessibility.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithTitle:summary:xAxisDescriptor:yAxisDescriptor:series:@
initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_series :: (IsAXChartDescriptor axChartDescriptor, IsNSString title, IsNSString summary, IsAXNumericDataAxisDescriptor yAxis, IsNSArray series) => axChartDescriptor -> title -> summary -> RawId -> yAxis -> series -> IO (Id AXChartDescriptor)
initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_series axChartDescriptor  title summary xAxis yAxis series =
  withObjCPtr title $ \raw_title ->
    withObjCPtr summary $ \raw_summary ->
      withObjCPtr yAxis $ \raw_yAxis ->
        withObjCPtr series $ \raw_series ->
            sendMsg axChartDescriptor (mkSelector "initWithTitle:summary:xAxisDescriptor:yAxisDescriptor:series:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_summary :: Ptr ()), argPtr (castPtr (unRawId xAxis) :: Ptr ()), argPtr (castPtr raw_yAxis :: Ptr ()), argPtr (castPtr raw_series :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithAttributedTitle:summary:xAxisDescriptor:yAxisDescriptor:series:@
initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_series :: (IsAXChartDescriptor axChartDescriptor, IsNSAttributedString attributedTitle, IsNSString summary, IsAXNumericDataAxisDescriptor yAxis, IsNSArray series) => axChartDescriptor -> attributedTitle -> summary -> RawId -> yAxis -> series -> IO (Id AXChartDescriptor)
initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_series axChartDescriptor  attributedTitle summary xAxis yAxis series =
  withObjCPtr attributedTitle $ \raw_attributedTitle ->
    withObjCPtr summary $ \raw_summary ->
      withObjCPtr yAxis $ \raw_yAxis ->
        withObjCPtr series $ \raw_series ->
            sendMsg axChartDescriptor (mkSelector "initWithAttributedTitle:summary:xAxisDescriptor:yAxisDescriptor:series:") (retPtr retVoid) [argPtr (castPtr raw_attributedTitle :: Ptr ()), argPtr (castPtr raw_summary :: Ptr ()), argPtr (castPtr (unRawId xAxis) :: Ptr ()), argPtr (castPtr raw_yAxis :: Ptr ()), argPtr (castPtr raw_series :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithTitle:summary:xAxisDescriptor:yAxisDescriptor:additionalAxes:series:@
initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_series :: (IsAXChartDescriptor axChartDescriptor, IsNSString title, IsNSString summary, IsAXNumericDataAxisDescriptor yAxis, IsNSArray additionalAxes, IsNSArray series) => axChartDescriptor -> title -> summary -> RawId -> yAxis -> additionalAxes -> series -> IO (Id AXChartDescriptor)
initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_series axChartDescriptor  title summary xAxis yAxis additionalAxes series =
  withObjCPtr title $ \raw_title ->
    withObjCPtr summary $ \raw_summary ->
      withObjCPtr yAxis $ \raw_yAxis ->
        withObjCPtr additionalAxes $ \raw_additionalAxes ->
          withObjCPtr series $ \raw_series ->
              sendMsg axChartDescriptor (mkSelector "initWithTitle:summary:xAxisDescriptor:yAxisDescriptor:additionalAxes:series:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_summary :: Ptr ()), argPtr (castPtr (unRawId xAxis) :: Ptr ()), argPtr (castPtr raw_yAxis :: Ptr ()), argPtr (castPtr raw_additionalAxes :: Ptr ()), argPtr (castPtr raw_series :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithAttributedTitle:summary:xAxisDescriptor:yAxisDescriptor:additionalAxes:series:@
initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_series :: (IsAXChartDescriptor axChartDescriptor, IsNSAttributedString attributedTitle, IsNSString summary, IsAXNumericDataAxisDescriptor yAxis, IsNSArray additionalAxes, IsNSArray series) => axChartDescriptor -> attributedTitle -> summary -> RawId -> yAxis -> additionalAxes -> series -> IO (Id AXChartDescriptor)
initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_series axChartDescriptor  attributedTitle summary xAxis yAxis additionalAxes series =
  withObjCPtr attributedTitle $ \raw_attributedTitle ->
    withObjCPtr summary $ \raw_summary ->
      withObjCPtr yAxis $ \raw_yAxis ->
        withObjCPtr additionalAxes $ \raw_additionalAxes ->
          withObjCPtr series $ \raw_series ->
              sendMsg axChartDescriptor (mkSelector "initWithAttributedTitle:summary:xAxisDescriptor:yAxisDescriptor:additionalAxes:series:") (retPtr retVoid) [argPtr (castPtr raw_attributedTitle :: Ptr ()), argPtr (castPtr raw_summary :: Ptr ()), argPtr (castPtr (unRawId xAxis) :: Ptr ()), argPtr (castPtr raw_yAxis :: Ptr ()), argPtr (castPtr raw_additionalAxes :: Ptr ()), argPtr (castPtr raw_series :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> IO (Id AXChartDescriptor)
init_ axChartDescriptor  =
    sendMsg axChartDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AXChartDescriptor)
new  =
  do
    cls' <- getRequiredClass "AXChartDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The title of the chart.
--
-- ObjC selector: @- title@
title :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> IO (Id NSString)
title axChartDescriptor  =
    sendMsg axChartDescriptor (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The title of the chart.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsAXChartDescriptor axChartDescriptor, IsNSString value) => axChartDescriptor -> value -> IO ()
setTitle axChartDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg axChartDescriptor (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | An attributed version of the title of the chart. When set, this will be used instead of @title@.
--
-- ObjC selector: @- attributedTitle@
attributedTitle :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> IO (Id NSAttributedString)
attributedTitle axChartDescriptor  =
    sendMsg axChartDescriptor (mkSelector "attributedTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An attributed version of the title of the chart. When set, this will be used instead of @title@.
--
-- ObjC selector: @- setAttributedTitle:@
setAttributedTitle :: (IsAXChartDescriptor axChartDescriptor, IsNSAttributedString value) => axChartDescriptor -> value -> IO ()
setAttributedTitle axChartDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg axChartDescriptor (mkSelector "setAttributedTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A natural language summary of the key message or features of the chart. e.g. "The chart shows that fuel efficiency decreases as vehicle weight increases."
--
-- ObjC selector: @- summary@
summary :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> IO (Id NSString)
summary axChartDescriptor  =
    sendMsg axChartDescriptor (mkSelector "summary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A natural language summary of the key message or features of the chart. e.g. "The chart shows that fuel efficiency decreases as vehicle weight increases."
--
-- ObjC selector: @- setSummary:@
setSummary :: (IsAXChartDescriptor axChartDescriptor, IsNSString value) => axChartDescriptor -> value -> IO ()
setSummary axChartDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg axChartDescriptor (mkSelector "setSummary:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The direction of the chart's X axis.
--
-- ObjC selector: @- contentDirection@
contentDirection :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> IO AXChartDescriptorContentDirection
contentDirection axChartDescriptor  =
    fmap (coerce :: CLong -> AXChartDescriptorContentDirection) $ sendMsg axChartDescriptor (mkSelector "contentDirection") retCLong []

-- | The direction of the chart's X axis.
--
-- ObjC selector: @- setContentDirection:@
setContentDirection :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> AXChartDescriptorContentDirection -> IO ()
setContentDirection axChartDescriptor  value =
    sendMsg axChartDescriptor (mkSelector "setContentDirection:") retVoid [argCLong (coerce value)]

-- | A set of data series descriptors describing each series in the chart.
--
-- ObjC selector: @- series@
series :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> IO (Id NSArray)
series axChartDescriptor  =
    sendMsg axChartDescriptor (mkSelector "series") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A set of data series descriptors describing each series in the chart.
--
-- ObjC selector: @- setSeries:@
setSeries :: (IsAXChartDescriptor axChartDescriptor, IsNSArray value) => axChartDescriptor -> value -> IO ()
setSeries axChartDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg axChartDescriptor (mkSelector "setSeries:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The axis descriptor for the chart's X axis.
--
-- ObjC selector: @- xAxis@
xAxis :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> IO RawId
xAxis axChartDescriptor  =
    fmap (RawId . castPtr) $ sendMsg axChartDescriptor (mkSelector "xAxis") (retPtr retVoid) []

-- | The axis descriptor for the chart's X axis.
--
-- ObjC selector: @- setXAxis:@
setXAxis :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> RawId -> IO ()
setXAxis axChartDescriptor  value =
    sendMsg axChartDescriptor (mkSelector "setXAxis:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The axis descriptor for the chart's Y axis.
--
-- ObjC selector: @- yAxis@
yAxis :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> IO (Id AXNumericDataAxisDescriptor)
yAxis axChartDescriptor  =
    sendMsg axChartDescriptor (mkSelector "yAxis") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The axis descriptor for the chart's Y axis.
--
-- ObjC selector: @- setYAxis:@
setYAxis :: (IsAXChartDescriptor axChartDescriptor, IsAXNumericDataAxisDescriptor value) => axChartDescriptor -> value -> IO ()
setYAxis axChartDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg axChartDescriptor (mkSelector "setYAxis:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Descriptors for additional categorical or numerical axes beyond x and y. For example, in a visual chart, these values might be represented by the size or color of data points.
--
-- ObjC selector: @- additionalAxes@
additionalAxes :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> IO (Id NSArray)
additionalAxes axChartDescriptor  =
    sendMsg axChartDescriptor (mkSelector "additionalAxes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Descriptors for additional categorical or numerical axes beyond x and y. For example, in a visual chart, these values might be represented by the size or color of data points.
--
-- ObjC selector: @- setAdditionalAxes:@
setAdditionalAxes :: (IsAXChartDescriptor axChartDescriptor, IsNSArray value) => axChartDescriptor -> value -> IO ()
setAdditionalAxes axChartDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg axChartDescriptor (mkSelector "setAdditionalAxes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:summary:xAxisDescriptor:yAxisDescriptor:series:@
initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_seriesSelector :: Selector
initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_seriesSelector = mkSelector "initWithTitle:summary:xAxisDescriptor:yAxisDescriptor:series:"

-- | @Selector@ for @initWithAttributedTitle:summary:xAxisDescriptor:yAxisDescriptor:series:@
initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_seriesSelector :: Selector
initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_seriesSelector = mkSelector "initWithAttributedTitle:summary:xAxisDescriptor:yAxisDescriptor:series:"

-- | @Selector@ for @initWithTitle:summary:xAxisDescriptor:yAxisDescriptor:additionalAxes:series:@
initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_seriesSelector :: Selector
initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_seriesSelector = mkSelector "initWithTitle:summary:xAxisDescriptor:yAxisDescriptor:additionalAxes:series:"

-- | @Selector@ for @initWithAttributedTitle:summary:xAxisDescriptor:yAxisDescriptor:additionalAxes:series:@
initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_seriesSelector :: Selector
initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_seriesSelector = mkSelector "initWithAttributedTitle:summary:xAxisDescriptor:yAxisDescriptor:additionalAxes:series:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @attributedTitle@
attributedTitleSelector :: Selector
attributedTitleSelector = mkSelector "attributedTitle"

-- | @Selector@ for @setAttributedTitle:@
setAttributedTitleSelector :: Selector
setAttributedTitleSelector = mkSelector "setAttributedTitle:"

-- | @Selector@ for @summary@
summarySelector :: Selector
summarySelector = mkSelector "summary"

-- | @Selector@ for @setSummary:@
setSummarySelector :: Selector
setSummarySelector = mkSelector "setSummary:"

-- | @Selector@ for @contentDirection@
contentDirectionSelector :: Selector
contentDirectionSelector = mkSelector "contentDirection"

-- | @Selector@ for @setContentDirection:@
setContentDirectionSelector :: Selector
setContentDirectionSelector = mkSelector "setContentDirection:"

-- | @Selector@ for @series@
seriesSelector :: Selector
seriesSelector = mkSelector "series"

-- | @Selector@ for @setSeries:@
setSeriesSelector :: Selector
setSeriesSelector = mkSelector "setSeries:"

-- | @Selector@ for @xAxis@
xAxisSelector :: Selector
xAxisSelector = mkSelector "xAxis"

-- | @Selector@ for @setXAxis:@
setXAxisSelector :: Selector
setXAxisSelector = mkSelector "setXAxis:"

-- | @Selector@ for @yAxis@
yAxisSelector :: Selector
yAxisSelector = mkSelector "yAxis"

-- | @Selector@ for @setYAxis:@
setYAxisSelector :: Selector
setYAxisSelector = mkSelector "setYAxis:"

-- | @Selector@ for @additionalAxes@
additionalAxesSelector :: Selector
additionalAxesSelector = mkSelector "additionalAxes"

-- | @Selector@ for @setAdditionalAxes:@
setAdditionalAxesSelector :: Selector
setAdditionalAxesSelector = mkSelector "setAdditionalAxes:"

