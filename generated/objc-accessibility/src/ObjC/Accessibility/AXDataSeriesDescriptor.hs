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
  , initWithName_isContinuous_dataPointsSelector
  , initWithAttributedName_isContinuous_dataPointsSelector
  , initSelector
  , newSelector
  , nameSelector
  , setNameSelector
  , attributedNameSelector
  , setAttributedNameSelector
  , isContinuousSelector
  , setIsContinuousSelector
  , dataPointsSelector
  , setDataPointsSelector


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

-- | @- initWithName:isContinuous:dataPoints:@
initWithName_isContinuous_dataPoints :: (IsAXDataSeriesDescriptor axDataSeriesDescriptor, IsNSString name, IsNSArray dataPoints) => axDataSeriesDescriptor -> name -> Bool -> dataPoints -> IO (Id AXDataSeriesDescriptor)
initWithName_isContinuous_dataPoints axDataSeriesDescriptor  name isContinuous dataPoints =
withObjCPtr name $ \raw_name ->
  withObjCPtr dataPoints $ \raw_dataPoints ->
      sendMsg axDataSeriesDescriptor (mkSelector "initWithName:isContinuous:dataPoints:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCULong (if isContinuous then 1 else 0), argPtr (castPtr raw_dataPoints :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithAttributedName:isContinuous:dataPoints:@
initWithAttributedName_isContinuous_dataPoints :: (IsAXDataSeriesDescriptor axDataSeriesDescriptor, IsNSAttributedString attributedName, IsNSArray dataPoints) => axDataSeriesDescriptor -> attributedName -> Bool -> dataPoints -> IO (Id AXDataSeriesDescriptor)
initWithAttributedName_isContinuous_dataPoints axDataSeriesDescriptor  attributedName isContinuous dataPoints =
withObjCPtr attributedName $ \raw_attributedName ->
  withObjCPtr dataPoints $ \raw_dataPoints ->
      sendMsg axDataSeriesDescriptor (mkSelector "initWithAttributedName:isContinuous:dataPoints:") (retPtr retVoid) [argPtr (castPtr raw_attributedName :: Ptr ()), argCULong (if isContinuous then 1 else 0), argPtr (castPtr raw_dataPoints :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsAXDataSeriesDescriptor axDataSeriesDescriptor => axDataSeriesDescriptor -> IO (Id AXDataSeriesDescriptor)
init_ axDataSeriesDescriptor  =
  sendMsg axDataSeriesDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AXDataSeriesDescriptor)
new  =
  do
    cls' <- getRequiredClass "AXDataSeriesDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The name or title of this data series.
--
-- ObjC selector: @- name@
name :: IsAXDataSeriesDescriptor axDataSeriesDescriptor => axDataSeriesDescriptor -> IO (Id NSString)
name axDataSeriesDescriptor  =
  sendMsg axDataSeriesDescriptor (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name or title of this data series.
--
-- ObjC selector: @- setName:@
setName :: (IsAXDataSeriesDescriptor axDataSeriesDescriptor, IsNSString value) => axDataSeriesDescriptor -> value -> IO ()
setName axDataSeriesDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg axDataSeriesDescriptor (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | An attributed version of the name of this data series. When set, this will be used instead of @name@.
--
-- ObjC selector: @- attributedName@
attributedName :: IsAXDataSeriesDescriptor axDataSeriesDescriptor => axDataSeriesDescriptor -> IO (Id NSAttributedString)
attributedName axDataSeriesDescriptor  =
  sendMsg axDataSeriesDescriptor (mkSelector "attributedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An attributed version of the name of this data series. When set, this will be used instead of @name@.
--
-- ObjC selector: @- setAttributedName:@
setAttributedName :: (IsAXDataSeriesDescriptor axDataSeriesDescriptor, IsNSAttributedString value) => axDataSeriesDescriptor -> value -> IO ()
setAttributedName axDataSeriesDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg axDataSeriesDescriptor (mkSelector "setAttributedName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Whether or not this data series should be treated as continuous.
--
-- ObjC selector: @- isContinuous@
isContinuous :: IsAXDataSeriesDescriptor axDataSeriesDescriptor => axDataSeriesDescriptor -> IO Bool
isContinuous axDataSeriesDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg axDataSeriesDescriptor (mkSelector "isContinuous") retCULong []

-- | Whether or not this data series should be treated as continuous.
--
-- ObjC selector: @- setIsContinuous:@
setIsContinuous :: IsAXDataSeriesDescriptor axDataSeriesDescriptor => axDataSeriesDescriptor -> Bool -> IO ()
setIsContinuous axDataSeriesDescriptor  value =
  sendMsg axDataSeriesDescriptor (mkSelector "setIsContinuous:") retVoid [argCULong (if value then 1 else 0)]

-- | The data points that make up the series.
--
-- ObjC selector: @- dataPoints@
dataPoints :: IsAXDataSeriesDescriptor axDataSeriesDescriptor => axDataSeriesDescriptor -> IO (Id NSArray)
dataPoints axDataSeriesDescriptor  =
  sendMsg axDataSeriesDescriptor (mkSelector "dataPoints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The data points that make up the series.
--
-- ObjC selector: @- setDataPoints:@
setDataPoints :: (IsAXDataSeriesDescriptor axDataSeriesDescriptor, IsNSArray value) => axDataSeriesDescriptor -> value -> IO ()
setDataPoints axDataSeriesDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg axDataSeriesDescriptor (mkSelector "setDataPoints:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:isContinuous:dataPoints:@
initWithName_isContinuous_dataPointsSelector :: Selector
initWithName_isContinuous_dataPointsSelector = mkSelector "initWithName:isContinuous:dataPoints:"

-- | @Selector@ for @initWithAttributedName:isContinuous:dataPoints:@
initWithAttributedName_isContinuous_dataPointsSelector :: Selector
initWithAttributedName_isContinuous_dataPointsSelector = mkSelector "initWithAttributedName:isContinuous:dataPoints:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @attributedName@
attributedNameSelector :: Selector
attributedNameSelector = mkSelector "attributedName"

-- | @Selector@ for @setAttributedName:@
setAttributedNameSelector :: Selector
setAttributedNameSelector = mkSelector "setAttributedName:"

-- | @Selector@ for @isContinuous@
isContinuousSelector :: Selector
isContinuousSelector = mkSelector "isContinuous"

-- | @Selector@ for @setIsContinuous:@
setIsContinuousSelector :: Selector
setIsContinuousSelector = mkSelector "setIsContinuous:"

-- | @Selector@ for @dataPoints@
dataPointsSelector :: Selector
dataPointsSelector = mkSelector "dataPoints"

-- | @Selector@ for @setDataPoints:@
setDataPointsSelector :: Selector
setDataPointsSelector = mkSelector "setDataPoints:"

