{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , gridlinePositionsSelector
  , initSelector
  , initWithAttributedTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProviderSelector
  , initWithTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProviderSelector
  , lowerBoundSelector
  , newSelector
  , scaleTypeSelector
  , setGridlinePositionsSelector
  , setLowerBoundSelector
  , setScaleTypeSelector
  , setUpperBoundSelector
  , setValueDescriptionProviderSelector
  , upperBoundSelector
  , valueDescriptionProviderSelector

  -- * Enum types
  , AXNumericDataAxisDescriptorScale(AXNumericDataAxisDescriptorScale)
  , pattern AXScaleTypeLinear
  , pattern AXScaleTypeLog10
  , pattern AXScaleTypeLn

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

-- | @- initWithTitle:lowerBound:upperBound:gridlinePositions:valueDescriptionProvider:@
initWithTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProvider :: (IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor, IsNSString title, IsNSArray gridlinePositions) => axNumericDataAxisDescriptor -> title -> CDouble -> CDouble -> gridlinePositions -> Ptr () -> IO (Id AXNumericDataAxisDescriptor)
initWithTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProvider axNumericDataAxisDescriptor title lowerbound upperBound gridlinePositions valueDescriptionProvider =
  sendOwnedMessage axNumericDataAxisDescriptor initWithTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProviderSelector (toNSString title) lowerbound upperBound (toNSArray gridlinePositions) valueDescriptionProvider

-- | @- initWithAttributedTitle:lowerBound:upperBound:gridlinePositions:valueDescriptionProvider:@
initWithAttributedTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProvider :: (IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor, IsNSAttributedString attributedTitle, IsNSArray gridlinePositions) => axNumericDataAxisDescriptor -> attributedTitle -> CDouble -> CDouble -> gridlinePositions -> Ptr () -> IO (Id AXNumericDataAxisDescriptor)
initWithAttributedTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProvider axNumericDataAxisDescriptor attributedTitle lowerbound upperBound gridlinePositions valueDescriptionProvider =
  sendOwnedMessage axNumericDataAxisDescriptor initWithAttributedTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProviderSelector (toNSAttributedString attributedTitle) lowerbound upperBound (toNSArray gridlinePositions) valueDescriptionProvider

-- | @- init@
init_ :: IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor => axNumericDataAxisDescriptor -> IO (Id AXNumericDataAxisDescriptor)
init_ axNumericDataAxisDescriptor =
  sendOwnedMessage axNumericDataAxisDescriptor initSelector

-- | @+ new@
new :: IO (Id AXNumericDataAxisDescriptor)
new  =
  do
    cls' <- getRequiredClass "AXNumericDataAxisDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | The scale to use for this axis. This should match the visual representation in the chart. If not set explicitly, this will default to @linear@.
--
-- ObjC selector: @- scaleType@
scaleType :: IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor => axNumericDataAxisDescriptor -> IO AXNumericDataAxisDescriptorScale
scaleType axNumericDataAxisDescriptor =
  sendMessage axNumericDataAxisDescriptor scaleTypeSelector

-- | The scale to use for this axis. This should match the visual representation in the chart. If not set explicitly, this will default to @linear@.
--
-- ObjC selector: @- setScaleType:@
setScaleType :: IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor => axNumericDataAxisDescriptor -> AXNumericDataAxisDescriptorScale -> IO ()
setScaleType axNumericDataAxisDescriptor value =
  sendMessage axNumericDataAxisDescriptor setScaleTypeSelector value

-- | The minimum displayable value for the axis.
--
-- ObjC selector: @- lowerBound@
lowerBound :: IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor => axNumericDataAxisDescriptor -> IO CDouble
lowerBound axNumericDataAxisDescriptor =
  sendMessage axNumericDataAxisDescriptor lowerBoundSelector

-- | The minimum displayable value for the axis.
--
-- ObjC selector: @- setLowerBound:@
setLowerBound :: IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor => axNumericDataAxisDescriptor -> CDouble -> IO ()
setLowerBound axNumericDataAxisDescriptor value =
  sendMessage axNumericDataAxisDescriptor setLowerBoundSelector value

-- | The maximum displayable value for the axis.
--
-- ObjC selector: @- upperBound@
upperBound :: IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor => axNumericDataAxisDescriptor -> IO CDouble
upperBound axNumericDataAxisDescriptor =
  sendMessage axNumericDataAxisDescriptor upperBoundSelector

-- | The maximum displayable value for the axis.
--
-- ObjC selector: @- setUpperBound:@
setUpperBound :: IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor => axNumericDataAxisDescriptor -> CDouble -> IO ()
setUpperBound axNumericDataAxisDescriptor value =
  sendMessage axNumericDataAxisDescriptor setUpperBoundSelector value

-- | Provides a value description to be spoken for a particular data value on this axis. Use this to format data values to string representations that include units, dates, times, etc.
--
-- ObjC selector: @- valueDescriptionProvider@
valueDescriptionProvider :: IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor => axNumericDataAxisDescriptor -> IO (Ptr ())
valueDescriptionProvider axNumericDataAxisDescriptor =
  sendMessage axNumericDataAxisDescriptor valueDescriptionProviderSelector

-- | Provides a value description to be spoken for a particular data value on this axis. Use this to format data values to string representations that include units, dates, times, etc.
--
-- ObjC selector: @- setValueDescriptionProvider:@
setValueDescriptionProvider :: IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor => axNumericDataAxisDescriptor -> Ptr () -> IO ()
setValueDescriptionProvider axNumericDataAxisDescriptor value =
  sendMessage axNumericDataAxisDescriptor setValueDescriptionProviderSelector value

-- | The positions of any gridlines along this axis.
--
-- ObjC selector: @- gridlinePositions@
gridlinePositions :: IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor => axNumericDataAxisDescriptor -> IO (Id NSArray)
gridlinePositions axNumericDataAxisDescriptor =
  sendMessage axNumericDataAxisDescriptor gridlinePositionsSelector

-- | The positions of any gridlines along this axis.
--
-- ObjC selector: @- setGridlinePositions:@
setGridlinePositions :: (IsAXNumericDataAxisDescriptor axNumericDataAxisDescriptor, IsNSArray value) => axNumericDataAxisDescriptor -> value -> IO ()
setGridlinePositions axNumericDataAxisDescriptor value =
  sendMessage axNumericDataAxisDescriptor setGridlinePositionsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:lowerBound:upperBound:gridlinePositions:valueDescriptionProvider:@
initWithTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProviderSelector :: Selector '[Id NSString, CDouble, CDouble, Id NSArray, Ptr ()] (Id AXNumericDataAxisDescriptor)
initWithTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProviderSelector = mkSelector "initWithTitle:lowerBound:upperBound:gridlinePositions:valueDescriptionProvider:"

-- | @Selector@ for @initWithAttributedTitle:lowerBound:upperBound:gridlinePositions:valueDescriptionProvider:@
initWithAttributedTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProviderSelector :: Selector '[Id NSAttributedString, CDouble, CDouble, Id NSArray, Ptr ()] (Id AXNumericDataAxisDescriptor)
initWithAttributedTitle_lowerBound_upperBound_gridlinePositions_valueDescriptionProviderSelector = mkSelector "initWithAttributedTitle:lowerBound:upperBound:gridlinePositions:valueDescriptionProvider:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AXNumericDataAxisDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AXNumericDataAxisDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @scaleType@
scaleTypeSelector :: Selector '[] AXNumericDataAxisDescriptorScale
scaleTypeSelector = mkSelector "scaleType"

-- | @Selector@ for @setScaleType:@
setScaleTypeSelector :: Selector '[AXNumericDataAxisDescriptorScale] ()
setScaleTypeSelector = mkSelector "setScaleType:"

-- | @Selector@ for @lowerBound@
lowerBoundSelector :: Selector '[] CDouble
lowerBoundSelector = mkSelector "lowerBound"

-- | @Selector@ for @setLowerBound:@
setLowerBoundSelector :: Selector '[CDouble] ()
setLowerBoundSelector = mkSelector "setLowerBound:"

-- | @Selector@ for @upperBound@
upperBoundSelector :: Selector '[] CDouble
upperBoundSelector = mkSelector "upperBound"

-- | @Selector@ for @setUpperBound:@
setUpperBoundSelector :: Selector '[CDouble] ()
setUpperBoundSelector = mkSelector "setUpperBound:"

-- | @Selector@ for @valueDescriptionProvider@
valueDescriptionProviderSelector :: Selector '[] (Ptr ())
valueDescriptionProviderSelector = mkSelector "valueDescriptionProvider"

-- | @Selector@ for @setValueDescriptionProvider:@
setValueDescriptionProviderSelector :: Selector '[Ptr ()] ()
setValueDescriptionProviderSelector = mkSelector "setValueDescriptionProvider:"

-- | @Selector@ for @gridlinePositions@
gridlinePositionsSelector :: Selector '[] (Id NSArray)
gridlinePositionsSelector = mkSelector "gridlinePositions"

-- | @Selector@ for @setGridlinePositions:@
setGridlinePositionsSelector :: Selector '[Id NSArray] ()
setGridlinePositionsSelector = mkSelector "setGridlinePositions:"

