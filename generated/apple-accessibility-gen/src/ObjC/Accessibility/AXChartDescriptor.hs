{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , additionalAxesSelector
  , attributedTitleSelector
  , contentDirectionSelector
  , initSelector
  , initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_seriesSelector
  , initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_seriesSelector
  , initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_seriesSelector
  , initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_seriesSelector
  , newSelector
  , seriesSelector
  , setAdditionalAxesSelector
  , setAttributedTitleSelector
  , setContentDirectionSelector
  , setSeriesSelector
  , setSummarySelector
  , setTitleSelector
  , setXAxisSelector
  , setYAxisSelector
  , summarySelector
  , titleSelector
  , xAxisSelector
  , yAxisSelector

  -- * Enum types
  , AXChartDescriptorContentDirection(AXChartDescriptorContentDirection)
  , pattern AXChartContentDirectionLeftToRight
  , pattern AXChartContentDirectionRightToLeft
  , pattern AXChartContentDirectionTopToBottom
  , pattern AXChartContentDirectionBottomToTop
  , pattern AXChartContentDirectionRadialClockwise
  , pattern AXChartContentDirectionRadialCounterClockwise

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Accessibility.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithTitle:summary:xAxisDescriptor:yAxisDescriptor:series:@
initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_series :: (IsAXChartDescriptor axChartDescriptor, IsNSString title, IsNSString summary, IsAXNumericDataAxisDescriptor yAxis, IsNSArray series) => axChartDescriptor -> title -> summary -> RawId -> yAxis -> series -> IO (Id AXChartDescriptor)
initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_series axChartDescriptor title summary xAxis yAxis series =
  sendOwnedMessage axChartDescriptor initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_seriesSelector (toNSString title) (toNSString summary) xAxis (toAXNumericDataAxisDescriptor yAxis) (toNSArray series)

-- | @- initWithAttributedTitle:summary:xAxisDescriptor:yAxisDescriptor:series:@
initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_series :: (IsAXChartDescriptor axChartDescriptor, IsNSAttributedString attributedTitle, IsNSString summary, IsAXNumericDataAxisDescriptor yAxis, IsNSArray series) => axChartDescriptor -> attributedTitle -> summary -> RawId -> yAxis -> series -> IO (Id AXChartDescriptor)
initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_series axChartDescriptor attributedTitle summary xAxis yAxis series =
  sendOwnedMessage axChartDescriptor initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_seriesSelector (toNSAttributedString attributedTitle) (toNSString summary) xAxis (toAXNumericDataAxisDescriptor yAxis) (toNSArray series)

-- | @- initWithTitle:summary:xAxisDescriptor:yAxisDescriptor:additionalAxes:series:@
initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_series :: (IsAXChartDescriptor axChartDescriptor, IsNSString title, IsNSString summary, IsAXNumericDataAxisDescriptor yAxis, IsNSArray additionalAxes, IsNSArray series) => axChartDescriptor -> title -> summary -> RawId -> yAxis -> additionalAxes -> series -> IO (Id AXChartDescriptor)
initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_series axChartDescriptor title summary xAxis yAxis additionalAxes series =
  sendOwnedMessage axChartDescriptor initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_seriesSelector (toNSString title) (toNSString summary) xAxis (toAXNumericDataAxisDescriptor yAxis) (toNSArray additionalAxes) (toNSArray series)

-- | @- initWithAttributedTitle:summary:xAxisDescriptor:yAxisDescriptor:additionalAxes:series:@
initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_series :: (IsAXChartDescriptor axChartDescriptor, IsNSAttributedString attributedTitle, IsNSString summary, IsAXNumericDataAxisDescriptor yAxis, IsNSArray additionalAxes, IsNSArray series) => axChartDescriptor -> attributedTitle -> summary -> RawId -> yAxis -> additionalAxes -> series -> IO (Id AXChartDescriptor)
initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_series axChartDescriptor attributedTitle summary xAxis yAxis additionalAxes series =
  sendOwnedMessage axChartDescriptor initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_seriesSelector (toNSAttributedString attributedTitle) (toNSString summary) xAxis (toAXNumericDataAxisDescriptor yAxis) (toNSArray additionalAxes) (toNSArray series)

-- | @- init@
init_ :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> IO (Id AXChartDescriptor)
init_ axChartDescriptor =
  sendOwnedMessage axChartDescriptor initSelector

-- | @+ new@
new :: IO (Id AXChartDescriptor)
new  =
  do
    cls' <- getRequiredClass "AXChartDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | The title of the chart.
--
-- ObjC selector: @- title@
title :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> IO (Id NSString)
title axChartDescriptor =
  sendMessage axChartDescriptor titleSelector

-- | The title of the chart.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsAXChartDescriptor axChartDescriptor, IsNSString value) => axChartDescriptor -> value -> IO ()
setTitle axChartDescriptor value =
  sendMessage axChartDescriptor setTitleSelector (toNSString value)

-- | An attributed version of the title of the chart. When set, this will be used instead of @title@.
--
-- ObjC selector: @- attributedTitle@
attributedTitle :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> IO (Id NSAttributedString)
attributedTitle axChartDescriptor =
  sendMessage axChartDescriptor attributedTitleSelector

-- | An attributed version of the title of the chart. When set, this will be used instead of @title@.
--
-- ObjC selector: @- setAttributedTitle:@
setAttributedTitle :: (IsAXChartDescriptor axChartDescriptor, IsNSAttributedString value) => axChartDescriptor -> value -> IO ()
setAttributedTitle axChartDescriptor value =
  sendMessage axChartDescriptor setAttributedTitleSelector (toNSAttributedString value)

-- | A natural language summary of the key message or features of the chart. e.g. "The chart shows that fuel efficiency decreases as vehicle weight increases."
--
-- ObjC selector: @- summary@
summary :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> IO (Id NSString)
summary axChartDescriptor =
  sendMessage axChartDescriptor summarySelector

-- | A natural language summary of the key message or features of the chart. e.g. "The chart shows that fuel efficiency decreases as vehicle weight increases."
--
-- ObjC selector: @- setSummary:@
setSummary :: (IsAXChartDescriptor axChartDescriptor, IsNSString value) => axChartDescriptor -> value -> IO ()
setSummary axChartDescriptor value =
  sendMessage axChartDescriptor setSummarySelector (toNSString value)

-- | The direction of the chart's X axis.
--
-- ObjC selector: @- contentDirection@
contentDirection :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> IO AXChartDescriptorContentDirection
contentDirection axChartDescriptor =
  sendMessage axChartDescriptor contentDirectionSelector

-- | The direction of the chart's X axis.
--
-- ObjC selector: @- setContentDirection:@
setContentDirection :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> AXChartDescriptorContentDirection -> IO ()
setContentDirection axChartDescriptor value =
  sendMessage axChartDescriptor setContentDirectionSelector value

-- | A set of data series descriptors describing each series in the chart.
--
-- ObjC selector: @- series@
series :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> IO (Id NSArray)
series axChartDescriptor =
  sendMessage axChartDescriptor seriesSelector

-- | A set of data series descriptors describing each series in the chart.
--
-- ObjC selector: @- setSeries:@
setSeries :: (IsAXChartDescriptor axChartDescriptor, IsNSArray value) => axChartDescriptor -> value -> IO ()
setSeries axChartDescriptor value =
  sendMessage axChartDescriptor setSeriesSelector (toNSArray value)

-- | The axis descriptor for the chart's X axis.
--
-- ObjC selector: @- xAxis@
xAxis :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> IO RawId
xAxis axChartDescriptor =
  sendMessage axChartDescriptor xAxisSelector

-- | The axis descriptor for the chart's X axis.
--
-- ObjC selector: @- setXAxis:@
setXAxis :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> RawId -> IO ()
setXAxis axChartDescriptor value =
  sendMessage axChartDescriptor setXAxisSelector value

-- | The axis descriptor for the chart's Y axis.
--
-- ObjC selector: @- yAxis@
yAxis :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> IO (Id AXNumericDataAxisDescriptor)
yAxis axChartDescriptor =
  sendMessage axChartDescriptor yAxisSelector

-- | The axis descriptor for the chart's Y axis.
--
-- ObjC selector: @- setYAxis:@
setYAxis :: (IsAXChartDescriptor axChartDescriptor, IsAXNumericDataAxisDescriptor value) => axChartDescriptor -> value -> IO ()
setYAxis axChartDescriptor value =
  sendMessage axChartDescriptor setYAxisSelector (toAXNumericDataAxisDescriptor value)

-- | Descriptors for additional categorical or numerical axes beyond x and y. For example, in a visual chart, these values might be represented by the size or color of data points.
--
-- ObjC selector: @- additionalAxes@
additionalAxes :: IsAXChartDescriptor axChartDescriptor => axChartDescriptor -> IO (Id NSArray)
additionalAxes axChartDescriptor =
  sendMessage axChartDescriptor additionalAxesSelector

-- | Descriptors for additional categorical or numerical axes beyond x and y. For example, in a visual chart, these values might be represented by the size or color of data points.
--
-- ObjC selector: @- setAdditionalAxes:@
setAdditionalAxes :: (IsAXChartDescriptor axChartDescriptor, IsNSArray value) => axChartDescriptor -> value -> IO ()
setAdditionalAxes axChartDescriptor value =
  sendMessage axChartDescriptor setAdditionalAxesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:summary:xAxisDescriptor:yAxisDescriptor:series:@
initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_seriesSelector :: Selector '[Id NSString, Id NSString, RawId, Id AXNumericDataAxisDescriptor, Id NSArray] (Id AXChartDescriptor)
initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_seriesSelector = mkSelector "initWithTitle:summary:xAxisDescriptor:yAxisDescriptor:series:"

-- | @Selector@ for @initWithAttributedTitle:summary:xAxisDescriptor:yAxisDescriptor:series:@
initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_seriesSelector :: Selector '[Id NSAttributedString, Id NSString, RawId, Id AXNumericDataAxisDescriptor, Id NSArray] (Id AXChartDescriptor)
initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_seriesSelector = mkSelector "initWithAttributedTitle:summary:xAxisDescriptor:yAxisDescriptor:series:"

-- | @Selector@ for @initWithTitle:summary:xAxisDescriptor:yAxisDescriptor:additionalAxes:series:@
initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_seriesSelector :: Selector '[Id NSString, Id NSString, RawId, Id AXNumericDataAxisDescriptor, Id NSArray, Id NSArray] (Id AXChartDescriptor)
initWithTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_seriesSelector = mkSelector "initWithTitle:summary:xAxisDescriptor:yAxisDescriptor:additionalAxes:series:"

-- | @Selector@ for @initWithAttributedTitle:summary:xAxisDescriptor:yAxisDescriptor:additionalAxes:series:@
initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_seriesSelector :: Selector '[Id NSAttributedString, Id NSString, RawId, Id AXNumericDataAxisDescriptor, Id NSArray, Id NSArray] (Id AXChartDescriptor)
initWithAttributedTitle_summary_xAxisDescriptor_yAxisDescriptor_additionalAxes_seriesSelector = mkSelector "initWithAttributedTitle:summary:xAxisDescriptor:yAxisDescriptor:additionalAxes:series:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AXChartDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AXChartDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @attributedTitle@
attributedTitleSelector :: Selector '[] (Id NSAttributedString)
attributedTitleSelector = mkSelector "attributedTitle"

-- | @Selector@ for @setAttributedTitle:@
setAttributedTitleSelector :: Selector '[Id NSAttributedString] ()
setAttributedTitleSelector = mkSelector "setAttributedTitle:"

-- | @Selector@ for @summary@
summarySelector :: Selector '[] (Id NSString)
summarySelector = mkSelector "summary"

-- | @Selector@ for @setSummary:@
setSummarySelector :: Selector '[Id NSString] ()
setSummarySelector = mkSelector "setSummary:"

-- | @Selector@ for @contentDirection@
contentDirectionSelector :: Selector '[] AXChartDescriptorContentDirection
contentDirectionSelector = mkSelector "contentDirection"

-- | @Selector@ for @setContentDirection:@
setContentDirectionSelector :: Selector '[AXChartDescriptorContentDirection] ()
setContentDirectionSelector = mkSelector "setContentDirection:"

-- | @Selector@ for @series@
seriesSelector :: Selector '[] (Id NSArray)
seriesSelector = mkSelector "series"

-- | @Selector@ for @setSeries:@
setSeriesSelector :: Selector '[Id NSArray] ()
setSeriesSelector = mkSelector "setSeries:"

-- | @Selector@ for @xAxis@
xAxisSelector :: Selector '[] RawId
xAxisSelector = mkSelector "xAxis"

-- | @Selector@ for @setXAxis:@
setXAxisSelector :: Selector '[RawId] ()
setXAxisSelector = mkSelector "setXAxis:"

-- | @Selector@ for @yAxis@
yAxisSelector :: Selector '[] (Id AXNumericDataAxisDescriptor)
yAxisSelector = mkSelector "yAxis"

-- | @Selector@ for @setYAxis:@
setYAxisSelector :: Selector '[Id AXNumericDataAxisDescriptor] ()
setYAxisSelector = mkSelector "setYAxis:"

-- | @Selector@ for @additionalAxes@
additionalAxesSelector :: Selector '[] (Id NSArray)
additionalAxesSelector = mkSelector "additionalAxes"

-- | @Selector@ for @setAdditionalAxes:@
setAdditionalAxesSelector :: Selector '[Id NSArray] ()
setAdditionalAxesSelector = mkSelector "setAdditionalAxes:"

