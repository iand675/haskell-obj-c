{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXNumericDataAxisDescriptor@.
module ObjC.Accessibility.AXNumericDataAxisDescriptor
  ( AXNumericDataAxisDescriptor
  , IsAXNumericDataAxisDescriptor(..)
  , initWithTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProvider
  , initWithAttributedTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProvider
  , init_
  , new
  , scaleType
  , setScaleType
  , lowerBound
  , setLowerBound
  , upperBound
  , setUpperBound
  , valueDescriptionProvider
  , setValueDescriptionProvider
  , gridlinePositions
  , setGridlinePositions
  , initWithTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProviderSelector
  , initWithAttributedTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProviderSelector
  , initSelector
  , newSelector
  , scaleTypeSelector
  , setScaleTypeSelector
  , lowerBoundSelector
  , setLowerBoundSelector
  , upperBoundSelector
  , setUpperBoundSelector
  , valueDescriptionProviderSelector
  , setValueDescriptionProviderSelector
  , gridlinePositionsSelector
  , setGridlinePositionsSelector

  -- * Enum types
  , AXNumericDataAxisDescriptorScale(AXNumericDataAxisDescriptorScale)
  , pattern AXScaleTypeLinear
  , pattern AXScaleTypeLog10
  , pattern AXScaleTypeLn

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
import ObjC.Accessibility.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithTitle:lowerBound:upperBound:gridlinePositions:valueDescriptionProvider:@
initWithTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProvider :: (IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor, IsNSString title, IsNSArray gridlinePositions) => axNumericDataAxisDescriptor -> title -> CDouble -> CDouble -> gridlinePositions -> Ptr () -> IO (Id AXNumericDataAxisDescriptor)
initWithTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProvider axNumericDataAxisDescriptor  title lowerbound upperBound gridlinePositions valueDescriptionProvider =
  withObjCPtr title $ \raw_title ->
    withObjCPtr gridlinePositions $ \raw_gridlinePositions ->
        sendMsg axNumericDataAxisDescriptor (mkSelector "initWithTitle:lowerBound:upperBound:gridlinePositions:valueDescriptionProvider:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argCDouble lowerbound, argCDouble upperBound, argPtr (castPtr raw_gridlinePositions :: Ptr ()), argPtr (castPtr valueDescriptionProvider :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithAttributedTitle:lowerBound:upperBound:gridlinePositions:valueDescriptionProvider:@
initWithAttributedTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProvider :: (IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor, IsNSAttributedString attributedTitle, IsNSArray gridlinePositions) => axNumericDataAxisDescriptor -> attributedTitle -> CDouble -> CDouble -> gridlinePositions -> Ptr () -> IO (Id AXNumericDataAxisDescriptor)
initWithAttributedTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProvider axNumericDataAxisDescriptor  attributedTitle lowerbound upperBound gridlinePositions valueDescriptionProvider =
  withObjCPtr attributedTitle $ \raw_attributedTitle ->
    withObjCPtr gridlinePositions $ \raw_gridlinePositions ->
        sendMsg axNumericDataAxisDescriptor (mkSelector "initWithAttributedTitle:lowerBound:upperBound:gridlinePositions:valueDescriptionProvider:") (retPtr retVoid) [argPtr (castPtr raw_attributedTitle :: Ptr ()), argCDouble lowerbound, argCDouble upperBound, argPtr (castPtr raw_gridlinePositions :: Ptr ()), argPtr (castPtr valueDescriptionProvider :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor => axNumericDataAxisDescriptor -> IO (Id AXNumericDataAxisDescriptor)
init_ axNumericDataAxisDescriptor  =
    sendMsg axNumericDataAxisDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AXNumericDataAxisDescriptor)
new  =
  do
    cls' <- getRequiredClass "AXNumericDataAxisDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The scale to use for this axis. This should match the visual representation in the chart. If not set explicitly, this will default to @linear@.
--
-- ObjC selector: @- scaleType@
scaleType :: IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor => axNumericDataAxisDescriptor -> IO AXNumericDataAxisDescriptorScale
scaleType axNumericDataAxisDescriptor  =
    fmap (coerce :: CLong -> AXNumericDataAxisDescriptorScale) $ sendMsg axNumericDataAxisDescriptor (mkSelector "scaleType") retCLong []

-- | The scale to use for this axis. This should match the visual representation in the chart. If not set explicitly, this will default to @linear@.
--
-- ObjC selector: @- setScaleType:@
setScaleType :: IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor => axNumericDataAxisDescriptor -> AXNumericDataAxisDescriptorScale -> IO ()
setScaleType axNumericDataAxisDescriptor  value =
    sendMsg axNumericDataAxisDescriptor (mkSelector "setScaleType:") retVoid [argCLong (coerce value)]

-- | The minimum displayable value for the axis.
--
-- ObjC selector: @- lowerBound@
lowerBound :: IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor => axNumericDataAxisDescriptor -> IO CDouble
lowerBound axNumericDataAxisDescriptor  =
    sendMsg axNumericDataAxisDescriptor (mkSelector "lowerBound") retCDouble []

-- | The minimum displayable value for the axis.
--
-- ObjC selector: @- setLowerBound:@
setLowerBound :: IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor => axNumericDataAxisDescriptor -> CDouble -> IO ()
setLowerBound axNumericDataAxisDescriptor  value =
    sendMsg axNumericDataAxisDescriptor (mkSelector "setLowerBound:") retVoid [argCDouble value]

-- | The maximum displayable value for the axis.
--
-- ObjC selector: @- upperBound@
upperBound :: IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor => axNumericDataAxisDescriptor -> IO CDouble
upperBound axNumericDataAxisDescriptor  =
    sendMsg axNumericDataAxisDescriptor (mkSelector "upperBound") retCDouble []

-- | The maximum displayable value for the axis.
--
-- ObjC selector: @- setUpperBound:@
setUpperBound :: IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor => axNumericDataAxisDescriptor -> CDouble -> IO ()
setUpperBound axNumericDataAxisDescriptor  value =
    sendMsg axNumericDataAxisDescriptor (mkSelector "setUpperBound:") retVoid [argCDouble value]

-- | Provides a value description to be spoken for a particular data value on this axis. Use this to format data values to string representations that include units, dates, times, etc.
--
-- ObjC selector: @- valueDescriptionProvider@
valueDescriptionProvider :: IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor => axNumericDataAxisDescriptor -> IO (Ptr ())
valueDescriptionProvider axNumericDataAxisDescriptor  =
    fmap castPtr $ sendMsg axNumericDataAxisDescriptor (mkSelector "valueDescriptionProvider") (retPtr retVoid) []

-- | Provides a value description to be spoken for a particular data value on this axis. Use this to format data values to string representations that include units, dates, times, etc.
--
-- ObjC selector: @- setValueDescriptionProvider:@
setValueDescriptionProvider :: IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor => axNumericDataAxisDescriptor -> Ptr () -> IO ()
setValueDescriptionProvider axNumericDataAxisDescriptor  value =
    sendMsg axNumericDataAxisDescriptor (mkSelector "setValueDescriptionProvider:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | The positions of any gridlines along this axis.
--
-- ObjC selector: @- gridlinePositions@
gridlinePositions :: IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor => axNumericDataAxisDescriptor -> IO (Id NSArray)
gridlinePositions axNumericDataAxisDescriptor  =
    sendMsg axNumericDataAxisDescriptor (mkSelector "gridlinePositions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The positions of any gridlines along this axis.
--
-- ObjC selector: @- setGridlinePositions:@
setGridlinePositions :: (IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor, IsNSArray value) => axNumericDataAxisDescriptor -> value -> IO ()
setGridlinePositions axNumericDataAxisDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg axNumericDataAxisDescriptor (mkSelector "setGridlinePositions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:lowerBound:upperBound:gridlinePositions:valueDescriptionProvider:@
initWithTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProviderSelector :: Selector
initWithTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProviderSelector = mkSelector "initWithTitle:lowerBound:upperBound:gridlinePositions:valueDescriptionProvider:"

-- | @Selector@ for @initWithAttributedTitle:lowerBound:upperBound:gridlinePositions:valueDescriptionProvider:@
initWithAttributedTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProviderSelector :: Selector
initWithAttributedTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProviderSelector = mkSelector "initWithAttributedTitle:lowerBound:upperBound:gridlinePositions:valueDescriptionProvider:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @scaleType@
scaleTypeSelector :: Selector
scaleTypeSelector = mkSelector "scaleType"

-- | @Selector@ for @setScaleType:@
setScaleTypeSelector :: Selector
setScaleTypeSelector = mkSelector "setScaleType:"

-- | @Selector@ for @lowerBound@
lowerBoundSelector :: Selector
lowerBoundSelector = mkSelector "lowerBound"

-- | @Selector@ for @setLowerBound:@
setLowerBoundSelector :: Selector
setLowerBoundSelector = mkSelector "setLowerBound:"

-- | @Selector@ for @upperBound@
upperBoundSelector :: Selector
upperBoundSelector = mkSelector "upperBound"

-- | @Selector@ for @setUpperBound:@
setUpperBoundSelector :: Selector
setUpperBoundSelector = mkSelector "setUpperBound:"

-- | @Selector@ for @valueDescriptionProvider@
valueDescriptionProviderSelector :: Selector
valueDescriptionProviderSelector = mkSelector "valueDescriptionProvider"

-- | @Selector@ for @setValueDescriptionProvider:@
setValueDescriptionProviderSelector :: Selector
setValueDescriptionProviderSelector = mkSelector "setValueDescriptionProvider:"

-- | @Selector@ for @gridlinePositions@
gridlinePositionsSelector :: Selector
gridlinePositionsSelector = mkSelector "gridlinePositions"

-- | @Selector@ for @setGridlinePositions:@
setGridlinePositionsSelector :: Selector
setGridlinePositionsSelector = mkSelector "setGridlinePositions:"

