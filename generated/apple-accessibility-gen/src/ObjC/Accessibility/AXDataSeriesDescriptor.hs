{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Provides information about a data series. A chart may have one or many data series.
--
-- Generated bindings for @AXDataSeriesDescriptor@.
module ObjC.Accessibility.AXDataSeriesDescriptor
  ( AXDataSeriesDescriptor
  , IsAXDataSeriesDescriptor(..)
  , initWithName_isContinuous_dataPoints
  , initWithAttributedName_isContinuous_dataPoints
  , init_
  , new
  , name
  , setName
  , attributedName
  , setAttributedName
  , isContinuous
  , setIsContinuous
  , dataPoints
  , setDataPoints
  , attributedNameSelector
  , dataPointsSelector
  , initSelector
  , initWithAttributedName_isContinuous_dataPointsSelector
  , initWithName_isContinuous_dataPointsSelector
  , isContinuousSelector
  , nameSelector
  , newSelector
  , setAttributedNameSelector
  , setDataPointsSelector
  , setIsContinuousSelector
  , setNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithName:isContinuous:dataPoints:@
initWithName_isContinuous_dataPoints :: (IsAXDataSeriesDescriptor axDataSeriesDescriptor, IsNSString name, IsNSArray dataPoints) => axDataSeriesDescriptor -> name -> Bool -> dataPoints -> IO (Id AXDataSeriesDescriptor)
initWithName_isContinuous_dataPoints axDataSeriesDescriptor name isContinuous dataPoints =
  sendOwnedMessage axDataSeriesDescriptor initWithName_isContinuous_dataPointsSelector (toNSString name) isContinuous (toNSArray dataPoints)

-- | @- initWithAttributedName:isContinuous:dataPoints:@
initWithAttributedName_isContinuous_dataPoints :: (IsAXDataSeriesDescriptor axDataSeriesDescriptor, IsNSAttributedString attributedName, IsNSArray dataPoints) => axDataSeriesDescriptor -> attributedName -> Bool -> dataPoints -> IO (Id AXDataSeriesDescriptor)
initWithAttributedName_isContinuous_dataPoints axDataSeriesDescriptor attributedName isContinuous dataPoints =
  sendOwnedMessage axDataSeriesDescriptor initWithAttributedName_isContinuous_dataPointsSelector (toNSAttributedString attributedName) isContinuous (toNSArray dataPoints)

-- | @- init@
init_ :: IsAXDataSeriesDescriptor axDataSeriesDescriptor => axDataSeriesDescriptor -> IO (Id AXDataSeriesDescriptor)
init_ axDataSeriesDescriptor =
  sendOwnedMessage axDataSeriesDescriptor initSelector

-- | @+ new@
new :: IO (Id AXDataSeriesDescriptor)
new  =
  do
    cls' <- getRequiredClass "AXDataSeriesDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | The name or title of this data series.
--
-- ObjC selector: @- name@
name :: IsAXDataSeriesDescriptor axDataSeriesDescriptor => axDataSeriesDescriptor -> IO (Id NSString)
name axDataSeriesDescriptor =
  sendMessage axDataSeriesDescriptor nameSelector

-- | The name or title of this data series.
--
-- ObjC selector: @- setName:@
setName :: (IsAXDataSeriesDescriptor axDataSeriesDescriptor, IsNSString value) => axDataSeriesDescriptor -> value -> IO ()
setName axDataSeriesDescriptor value =
  sendMessage axDataSeriesDescriptor setNameSelector (toNSString value)

-- | An attributed version of the name of this data series. When set, this will be used instead of @name@.
--
-- ObjC selector: @- attributedName@
attributedName :: IsAXDataSeriesDescriptor axDataSeriesDescriptor => axDataSeriesDescriptor -> IO (Id NSAttributedString)
attributedName axDataSeriesDescriptor =
  sendMessage axDataSeriesDescriptor attributedNameSelector

-- | An attributed version of the name of this data series. When set, this will be used instead of @name@.
--
-- ObjC selector: @- setAttributedName:@
setAttributedName :: (IsAXDataSeriesDescriptor axDataSeriesDescriptor, IsNSAttributedString value) => axDataSeriesDescriptor -> value -> IO ()
setAttributedName axDataSeriesDescriptor value =
  sendMessage axDataSeriesDescriptor setAttributedNameSelector (toNSAttributedString value)

-- | Whether or not this data series should be treated as continuous.
--
-- ObjC selector: @- isContinuous@
isContinuous :: IsAXDataSeriesDescriptor axDataSeriesDescriptor => axDataSeriesDescriptor -> IO Bool
isContinuous axDataSeriesDescriptor =
  sendMessage axDataSeriesDescriptor isContinuousSelector

-- | Whether or not this data series should be treated as continuous.
--
-- ObjC selector: @- setIsContinuous:@
setIsContinuous :: IsAXDataSeriesDescriptor axDataSeriesDescriptor => axDataSeriesDescriptor -> Bool -> IO ()
setIsContinuous axDataSeriesDescriptor value =
  sendMessage axDataSeriesDescriptor setIsContinuousSelector value

-- | The data points that make up the series.
--
-- ObjC selector: @- dataPoints@
dataPoints :: IsAXDataSeriesDescriptor axDataSeriesDescriptor => axDataSeriesDescriptor -> IO (Id NSArray)
dataPoints axDataSeriesDescriptor =
  sendMessage axDataSeriesDescriptor dataPointsSelector

-- | The data points that make up the series.
--
-- ObjC selector: @- setDataPoints:@
setDataPoints :: (IsAXDataSeriesDescriptor axDataSeriesDescriptor, IsNSArray value) => axDataSeriesDescriptor -> value -> IO ()
setDataPoints axDataSeriesDescriptor value =
  sendMessage axDataSeriesDescriptor setDataPointsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:isContinuous:dataPoints:@
initWithName_isContinuous_dataPointsSelector :: Selector '[Id NSString, Bool, Id NSArray] (Id AXDataSeriesDescriptor)
initWithName_isContinuous_dataPointsSelector = mkSelector "initWithName:isContinuous:dataPoints:"

-- | @Selector@ for @initWithAttributedName:isContinuous:dataPoints:@
initWithAttributedName_isContinuous_dataPointsSelector :: Selector '[Id NSAttributedString, Bool, Id NSArray] (Id AXDataSeriesDescriptor)
initWithAttributedName_isContinuous_dataPointsSelector = mkSelector "initWithAttributedName:isContinuous:dataPoints:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AXDataSeriesDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AXDataSeriesDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @attributedName@
attributedNameSelector :: Selector '[] (Id NSAttributedString)
attributedNameSelector = mkSelector "attributedName"

-- | @Selector@ for @setAttributedName:@
setAttributedNameSelector :: Selector '[Id NSAttributedString] ()
setAttributedNameSelector = mkSelector "setAttributedName:"

-- | @Selector@ for @isContinuous@
isContinuousSelector :: Selector '[] Bool
isContinuousSelector = mkSelector "isContinuous"

-- | @Selector@ for @setIsContinuous:@
setIsContinuousSelector :: Selector '[Bool] ()
setIsContinuousSelector = mkSelector "setIsContinuous:"

-- | @Selector@ for @dataPoints@
dataPointsSelector :: Selector '[] (Id NSArray)
dataPointsSelector = mkSelector "dataPoints"

-- | @Selector@ for @setDataPoints:@
setDataPointsSelector :: Selector '[Id NSArray] ()
setDataPointsSelector = mkSelector "setDataPoints:"

