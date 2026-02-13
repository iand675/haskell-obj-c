{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @QCCompositionPickerView@.
module ObjC.Quartz.QCCompositionPickerView
  ( QCCompositionPickerView
  , IsQCCompositionPickerView(..)
  , setCompositionsFromRepositoryWithProtocol_andAttributes
  , compositions
  , setDelegate
  , delegate
  , setShowsCompositionNames
  , showsCompositionNames
  , setAllowsEmptySelection
  , allowsEmptySelection
  , setCompositionAspectRatio
  , compositionAspectRatio
  , setDefaultValue_forInputKey
  , resetDefaultInputValues
  , setSelectedComposition
  , selectedComposition
  , startAnimation
  , stopAnimation
  , isAnimating
  , setMaxAnimationFrameRate
  , maxAnimationFrameRate
  , setBackgroundColor
  , backgroundColor
  , setDrawsBackground
  , drawsBackground
  , numberOfColumns
  , setNumberOfColumns
  , numberOfRows
  , setNumberOfRows
  , allowsEmptySelectionSelector
  , backgroundColorSelector
  , compositionAspectRatioSelector
  , compositionsSelector
  , delegateSelector
  , drawsBackgroundSelector
  , isAnimatingSelector
  , maxAnimationFrameRateSelector
  , numberOfColumnsSelector
  , numberOfRowsSelector
  , resetDefaultInputValuesSelector
  , selectedCompositionSelector
  , setAllowsEmptySelectionSelector
  , setBackgroundColorSelector
  , setCompositionAspectRatioSelector
  , setCompositionsFromRepositoryWithProtocol_andAttributesSelector
  , setDefaultValue_forInputKeySelector
  , setDelegateSelector
  , setDrawsBackgroundSelector
  , setMaxAnimationFrameRateSelector
  , setNumberOfColumnsSelector
  , setNumberOfRowsSelector
  , setSelectedCompositionSelector
  , setShowsCompositionNamesSelector
  , showsCompositionNamesSelector
  , startAnimationSelector
  , stopAnimationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Quartz.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- setCompositionsFromRepositoryWithProtocol:andAttributes:@
setCompositionsFromRepositoryWithProtocol_andAttributes :: (IsQCCompositionPickerView qcCompositionPickerView, IsNSString protocol, IsNSDictionary attributes) => qcCompositionPickerView -> protocol -> attributes -> IO ()
setCompositionsFromRepositoryWithProtocol_andAttributes qcCompositionPickerView protocol attributes =
  sendMessage qcCompositionPickerView setCompositionsFromRepositoryWithProtocol_andAttributesSelector (toNSString protocol) (toNSDictionary attributes)

-- | @- compositions@
compositions :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO (Id NSArray)
compositions qcCompositionPickerView =
  sendMessage qcCompositionPickerView compositionsSelector

-- | @- setDelegate:@
setDelegate :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> RawId -> IO ()
setDelegate qcCompositionPickerView delegate =
  sendMessage qcCompositionPickerView setDelegateSelector delegate

-- | @- delegate@
delegate :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO RawId
delegate qcCompositionPickerView =
  sendMessage qcCompositionPickerView delegateSelector

-- | @- setShowsCompositionNames:@
setShowsCompositionNames :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> Bool -> IO ()
setShowsCompositionNames qcCompositionPickerView flag =
  sendMessage qcCompositionPickerView setShowsCompositionNamesSelector flag

-- | @- showsCompositionNames@
showsCompositionNames :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO Bool
showsCompositionNames qcCompositionPickerView =
  sendMessage qcCompositionPickerView showsCompositionNamesSelector

-- | @- setAllowsEmptySelection:@
setAllowsEmptySelection :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> Bool -> IO ()
setAllowsEmptySelection qcCompositionPickerView flag =
  sendMessage qcCompositionPickerView setAllowsEmptySelectionSelector flag

-- | @- allowsEmptySelection@
allowsEmptySelection :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO Bool
allowsEmptySelection qcCompositionPickerView =
  sendMessage qcCompositionPickerView allowsEmptySelectionSelector

-- | @- setCompositionAspectRatio:@
setCompositionAspectRatio :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> NSSize -> IO ()
setCompositionAspectRatio qcCompositionPickerView ratio =
  sendMessage qcCompositionPickerView setCompositionAspectRatioSelector ratio

-- | @- compositionAspectRatio@
compositionAspectRatio :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO NSSize
compositionAspectRatio qcCompositionPickerView =
  sendMessage qcCompositionPickerView compositionAspectRatioSelector

-- | @- setDefaultValue:forInputKey:@
setDefaultValue_forInputKey :: (IsQCCompositionPickerView qcCompositionPickerView, IsNSString key) => qcCompositionPickerView -> RawId -> key -> IO ()
setDefaultValue_forInputKey qcCompositionPickerView value key =
  sendMessage qcCompositionPickerView setDefaultValue_forInputKeySelector value (toNSString key)

-- | @- resetDefaultInputValues@
resetDefaultInputValues :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO ()
resetDefaultInputValues qcCompositionPickerView =
  sendMessage qcCompositionPickerView resetDefaultInputValuesSelector

-- | @- setSelectedComposition:@
setSelectedComposition :: (IsQCCompositionPickerView qcCompositionPickerView, IsQCComposition composition) => qcCompositionPickerView -> composition -> IO ()
setSelectedComposition qcCompositionPickerView composition =
  sendMessage qcCompositionPickerView setSelectedCompositionSelector (toQCComposition composition)

-- | @- selectedComposition@
selectedComposition :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO (Id QCComposition)
selectedComposition qcCompositionPickerView =
  sendMessage qcCompositionPickerView selectedCompositionSelector

-- | @- startAnimation:@
startAnimation :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> RawId -> IO ()
startAnimation qcCompositionPickerView sender =
  sendMessage qcCompositionPickerView startAnimationSelector sender

-- | @- stopAnimation:@
stopAnimation :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> RawId -> IO ()
stopAnimation qcCompositionPickerView sender =
  sendMessage qcCompositionPickerView stopAnimationSelector sender

-- | @- isAnimating@
isAnimating :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO Bool
isAnimating qcCompositionPickerView =
  sendMessage qcCompositionPickerView isAnimatingSelector

-- | @- setMaxAnimationFrameRate:@
setMaxAnimationFrameRate :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> CFloat -> IO ()
setMaxAnimationFrameRate qcCompositionPickerView maxFPS =
  sendMessage qcCompositionPickerView setMaxAnimationFrameRateSelector maxFPS

-- | @- maxAnimationFrameRate@
maxAnimationFrameRate :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO CFloat
maxAnimationFrameRate qcCompositionPickerView =
  sendMessage qcCompositionPickerView maxAnimationFrameRateSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsQCCompositionPickerView qcCompositionPickerView, IsNSColor color) => qcCompositionPickerView -> color -> IO ()
setBackgroundColor qcCompositionPickerView color =
  sendMessage qcCompositionPickerView setBackgroundColorSelector (toNSColor color)

-- | @- backgroundColor@
backgroundColor :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO (Id NSColor)
backgroundColor qcCompositionPickerView =
  sendMessage qcCompositionPickerView backgroundColorSelector

-- | @- setDrawsBackground:@
setDrawsBackground :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> Bool -> IO ()
setDrawsBackground qcCompositionPickerView flag =
  sendMessage qcCompositionPickerView setDrawsBackgroundSelector flag

-- | @- drawsBackground@
drawsBackground :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO Bool
drawsBackground qcCompositionPickerView =
  sendMessage qcCompositionPickerView drawsBackgroundSelector

-- | @- numberOfColumns@
numberOfColumns :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO CULong
numberOfColumns qcCompositionPickerView =
  sendMessage qcCompositionPickerView numberOfColumnsSelector

-- | @- setNumberOfColumns:@
setNumberOfColumns :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> CULong -> IO ()
setNumberOfColumns qcCompositionPickerView columns =
  sendMessage qcCompositionPickerView setNumberOfColumnsSelector columns

-- | @- numberOfRows@
numberOfRows :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO CULong
numberOfRows qcCompositionPickerView =
  sendMessage qcCompositionPickerView numberOfRowsSelector

-- | @- setNumberOfRows:@
setNumberOfRows :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> CULong -> IO ()
setNumberOfRows qcCompositionPickerView rows =
  sendMessage qcCompositionPickerView setNumberOfRowsSelector rows

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setCompositionsFromRepositoryWithProtocol:andAttributes:@
setCompositionsFromRepositoryWithProtocol_andAttributesSelector :: Selector '[Id NSString, Id NSDictionary] ()
setCompositionsFromRepositoryWithProtocol_andAttributesSelector = mkSelector "setCompositionsFromRepositoryWithProtocol:andAttributes:"

-- | @Selector@ for @compositions@
compositionsSelector :: Selector '[] (Id NSArray)
compositionsSelector = mkSelector "compositions"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setShowsCompositionNames:@
setShowsCompositionNamesSelector :: Selector '[Bool] ()
setShowsCompositionNamesSelector = mkSelector "setShowsCompositionNames:"

-- | @Selector@ for @showsCompositionNames@
showsCompositionNamesSelector :: Selector '[] Bool
showsCompositionNamesSelector = mkSelector "showsCompositionNames"

-- | @Selector@ for @setAllowsEmptySelection:@
setAllowsEmptySelectionSelector :: Selector '[Bool] ()
setAllowsEmptySelectionSelector = mkSelector "setAllowsEmptySelection:"

-- | @Selector@ for @allowsEmptySelection@
allowsEmptySelectionSelector :: Selector '[] Bool
allowsEmptySelectionSelector = mkSelector "allowsEmptySelection"

-- | @Selector@ for @setCompositionAspectRatio:@
setCompositionAspectRatioSelector :: Selector '[NSSize] ()
setCompositionAspectRatioSelector = mkSelector "setCompositionAspectRatio:"

-- | @Selector@ for @compositionAspectRatio@
compositionAspectRatioSelector :: Selector '[] NSSize
compositionAspectRatioSelector = mkSelector "compositionAspectRatio"

-- | @Selector@ for @setDefaultValue:forInputKey:@
setDefaultValue_forInputKeySelector :: Selector '[RawId, Id NSString] ()
setDefaultValue_forInputKeySelector = mkSelector "setDefaultValue:forInputKey:"

-- | @Selector@ for @resetDefaultInputValues@
resetDefaultInputValuesSelector :: Selector '[] ()
resetDefaultInputValuesSelector = mkSelector "resetDefaultInputValues"

-- | @Selector@ for @setSelectedComposition:@
setSelectedCompositionSelector :: Selector '[Id QCComposition] ()
setSelectedCompositionSelector = mkSelector "setSelectedComposition:"

-- | @Selector@ for @selectedComposition@
selectedCompositionSelector :: Selector '[] (Id QCComposition)
selectedCompositionSelector = mkSelector "selectedComposition"

-- | @Selector@ for @startAnimation:@
startAnimationSelector :: Selector '[RawId] ()
startAnimationSelector = mkSelector "startAnimation:"

-- | @Selector@ for @stopAnimation:@
stopAnimationSelector :: Selector '[RawId] ()
stopAnimationSelector = mkSelector "stopAnimation:"

-- | @Selector@ for @isAnimating@
isAnimatingSelector :: Selector '[] Bool
isAnimatingSelector = mkSelector "isAnimating"

-- | @Selector@ for @setMaxAnimationFrameRate:@
setMaxAnimationFrameRateSelector :: Selector '[CFloat] ()
setMaxAnimationFrameRateSelector = mkSelector "setMaxAnimationFrameRate:"

-- | @Selector@ for @maxAnimationFrameRate@
maxAnimationFrameRateSelector :: Selector '[] CFloat
maxAnimationFrameRateSelector = mkSelector "maxAnimationFrameRate"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setDrawsBackground:@
setDrawsBackgroundSelector :: Selector '[Bool] ()
setDrawsBackgroundSelector = mkSelector "setDrawsBackground:"

-- | @Selector@ for @drawsBackground@
drawsBackgroundSelector :: Selector '[] Bool
drawsBackgroundSelector = mkSelector "drawsBackground"

-- | @Selector@ for @numberOfColumns@
numberOfColumnsSelector :: Selector '[] CULong
numberOfColumnsSelector = mkSelector "numberOfColumns"

-- | @Selector@ for @setNumberOfColumns:@
setNumberOfColumnsSelector :: Selector '[CULong] ()
setNumberOfColumnsSelector = mkSelector "setNumberOfColumns:"

-- | @Selector@ for @numberOfRows@
numberOfRowsSelector :: Selector '[] CULong
numberOfRowsSelector = mkSelector "numberOfRows"

-- | @Selector@ for @setNumberOfRows:@
setNumberOfRowsSelector :: Selector '[CULong] ()
setNumberOfRowsSelector = mkSelector "setNumberOfRows:"

