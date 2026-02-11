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
  , setCompositionsFromRepositoryWithProtocol_andAttributesSelector
  , compositionsSelector
  , setDelegateSelector
  , delegateSelector
  , setShowsCompositionNamesSelector
  , showsCompositionNamesSelector
  , setAllowsEmptySelectionSelector
  , allowsEmptySelectionSelector
  , setCompositionAspectRatioSelector
  , compositionAspectRatioSelector
  , setDefaultValue_forInputKeySelector
  , resetDefaultInputValuesSelector
  , setSelectedCompositionSelector
  , selectedCompositionSelector
  , startAnimationSelector
  , stopAnimationSelector
  , isAnimatingSelector
  , setMaxAnimationFrameRateSelector
  , maxAnimationFrameRateSelector
  , setBackgroundColorSelector
  , backgroundColorSelector
  , setDrawsBackgroundSelector
  , drawsBackgroundSelector
  , numberOfColumnsSelector
  , setNumberOfColumnsSelector
  , numberOfRowsSelector
  , setNumberOfRowsSelector


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

import ObjC.Quartz.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- setCompositionsFromRepositoryWithProtocol:andAttributes:@
setCompositionsFromRepositoryWithProtocol_andAttributes :: (IsQCCompositionPickerView qcCompositionPickerView, IsNSString protocol, IsNSDictionary attributes) => qcCompositionPickerView -> protocol -> attributes -> IO ()
setCompositionsFromRepositoryWithProtocol_andAttributes qcCompositionPickerView  protocol attributes =
withObjCPtr protocol $ \raw_protocol ->
  withObjCPtr attributes $ \raw_attributes ->
      sendMsg qcCompositionPickerView (mkSelector "setCompositionsFromRepositoryWithProtocol:andAttributes:") retVoid [argPtr (castPtr raw_protocol :: Ptr ()), argPtr (castPtr raw_attributes :: Ptr ())]

-- | @- compositions@
compositions :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO (Id NSArray)
compositions qcCompositionPickerView  =
  sendMsg qcCompositionPickerView (mkSelector "compositions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDelegate:@
setDelegate :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> RawId -> IO ()
setDelegate qcCompositionPickerView  delegate =
  sendMsg qcCompositionPickerView (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ())]

-- | @- delegate@
delegate :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO RawId
delegate qcCompositionPickerView  =
  fmap (RawId . castPtr) $ sendMsg qcCompositionPickerView (mkSelector "delegate") (retPtr retVoid) []

-- | @- setShowsCompositionNames:@
setShowsCompositionNames :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> Bool -> IO ()
setShowsCompositionNames qcCompositionPickerView  flag =
  sendMsg qcCompositionPickerView (mkSelector "setShowsCompositionNames:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- showsCompositionNames@
showsCompositionNames :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO Bool
showsCompositionNames qcCompositionPickerView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg qcCompositionPickerView (mkSelector "showsCompositionNames") retCULong []

-- | @- setAllowsEmptySelection:@
setAllowsEmptySelection :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> Bool -> IO ()
setAllowsEmptySelection qcCompositionPickerView  flag =
  sendMsg qcCompositionPickerView (mkSelector "setAllowsEmptySelection:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- allowsEmptySelection@
allowsEmptySelection :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO Bool
allowsEmptySelection qcCompositionPickerView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg qcCompositionPickerView (mkSelector "allowsEmptySelection") retCULong []

-- | @- setCompositionAspectRatio:@
setCompositionAspectRatio :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> NSSize -> IO ()
setCompositionAspectRatio qcCompositionPickerView  ratio =
  sendMsg qcCompositionPickerView (mkSelector "setCompositionAspectRatio:") retVoid [argNSSize ratio]

-- | @- compositionAspectRatio@
compositionAspectRatio :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO NSSize
compositionAspectRatio qcCompositionPickerView  =
  sendMsgStret qcCompositionPickerView (mkSelector "compositionAspectRatio") retNSSize []

-- | @- setDefaultValue:forInputKey:@
setDefaultValue_forInputKey :: (IsQCCompositionPickerView qcCompositionPickerView, IsNSString key) => qcCompositionPickerView -> RawId -> key -> IO ()
setDefaultValue_forInputKey qcCompositionPickerView  value key =
withObjCPtr key $ \raw_key ->
    sendMsg qcCompositionPickerView (mkSelector "setDefaultValue:forInputKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- resetDefaultInputValues@
resetDefaultInputValues :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO ()
resetDefaultInputValues qcCompositionPickerView  =
  sendMsg qcCompositionPickerView (mkSelector "resetDefaultInputValues") retVoid []

-- | @- setSelectedComposition:@
setSelectedComposition :: (IsQCCompositionPickerView qcCompositionPickerView, IsQCComposition composition) => qcCompositionPickerView -> composition -> IO ()
setSelectedComposition qcCompositionPickerView  composition =
withObjCPtr composition $ \raw_composition ->
    sendMsg qcCompositionPickerView (mkSelector "setSelectedComposition:") retVoid [argPtr (castPtr raw_composition :: Ptr ())]

-- | @- selectedComposition@
selectedComposition :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO (Id QCComposition)
selectedComposition qcCompositionPickerView  =
  sendMsg qcCompositionPickerView (mkSelector "selectedComposition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- startAnimation:@
startAnimation :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> RawId -> IO ()
startAnimation qcCompositionPickerView  sender =
  sendMsg qcCompositionPickerView (mkSelector "startAnimation:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- stopAnimation:@
stopAnimation :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> RawId -> IO ()
stopAnimation qcCompositionPickerView  sender =
  sendMsg qcCompositionPickerView (mkSelector "stopAnimation:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- isAnimating@
isAnimating :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO Bool
isAnimating qcCompositionPickerView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg qcCompositionPickerView (mkSelector "isAnimating") retCULong []

-- | @- setMaxAnimationFrameRate:@
setMaxAnimationFrameRate :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> CFloat -> IO ()
setMaxAnimationFrameRate qcCompositionPickerView  maxFPS =
  sendMsg qcCompositionPickerView (mkSelector "setMaxAnimationFrameRate:") retVoid [argCFloat (fromIntegral maxFPS)]

-- | @- maxAnimationFrameRate@
maxAnimationFrameRate :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO CFloat
maxAnimationFrameRate qcCompositionPickerView  =
  sendMsg qcCompositionPickerView (mkSelector "maxAnimationFrameRate") retCFloat []

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsQCCompositionPickerView qcCompositionPickerView, IsNSColor color) => qcCompositionPickerView -> color -> IO ()
setBackgroundColor qcCompositionPickerView  color =
withObjCPtr color $ \raw_color ->
    sendMsg qcCompositionPickerView (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_color :: Ptr ())]

-- | @- backgroundColor@
backgroundColor :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO (Id NSColor)
backgroundColor qcCompositionPickerView  =
  sendMsg qcCompositionPickerView (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDrawsBackground:@
setDrawsBackground :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> Bool -> IO ()
setDrawsBackground qcCompositionPickerView  flag =
  sendMsg qcCompositionPickerView (mkSelector "setDrawsBackground:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- drawsBackground@
drawsBackground :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO Bool
drawsBackground qcCompositionPickerView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg qcCompositionPickerView (mkSelector "drawsBackground") retCULong []

-- | @- numberOfColumns@
numberOfColumns :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO CULong
numberOfColumns qcCompositionPickerView  =
  sendMsg qcCompositionPickerView (mkSelector "numberOfColumns") retCULong []

-- | @- setNumberOfColumns:@
setNumberOfColumns :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> CULong -> IO ()
setNumberOfColumns qcCompositionPickerView  columns =
  sendMsg qcCompositionPickerView (mkSelector "setNumberOfColumns:") retVoid [argCULong (fromIntegral columns)]

-- | @- numberOfRows@
numberOfRows :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> IO CULong
numberOfRows qcCompositionPickerView  =
  sendMsg qcCompositionPickerView (mkSelector "numberOfRows") retCULong []

-- | @- setNumberOfRows:@
setNumberOfRows :: IsQCCompositionPickerView qcCompositionPickerView => qcCompositionPickerView -> CULong -> IO ()
setNumberOfRows qcCompositionPickerView  rows =
  sendMsg qcCompositionPickerView (mkSelector "setNumberOfRows:") retVoid [argCULong (fromIntegral rows)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setCompositionsFromRepositoryWithProtocol:andAttributes:@
setCompositionsFromRepositoryWithProtocol_andAttributesSelector :: Selector
setCompositionsFromRepositoryWithProtocol_andAttributesSelector = mkSelector "setCompositionsFromRepositoryWithProtocol:andAttributes:"

-- | @Selector@ for @compositions@
compositionsSelector :: Selector
compositionsSelector = mkSelector "compositions"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setShowsCompositionNames:@
setShowsCompositionNamesSelector :: Selector
setShowsCompositionNamesSelector = mkSelector "setShowsCompositionNames:"

-- | @Selector@ for @showsCompositionNames@
showsCompositionNamesSelector :: Selector
showsCompositionNamesSelector = mkSelector "showsCompositionNames"

-- | @Selector@ for @setAllowsEmptySelection:@
setAllowsEmptySelectionSelector :: Selector
setAllowsEmptySelectionSelector = mkSelector "setAllowsEmptySelection:"

-- | @Selector@ for @allowsEmptySelection@
allowsEmptySelectionSelector :: Selector
allowsEmptySelectionSelector = mkSelector "allowsEmptySelection"

-- | @Selector@ for @setCompositionAspectRatio:@
setCompositionAspectRatioSelector :: Selector
setCompositionAspectRatioSelector = mkSelector "setCompositionAspectRatio:"

-- | @Selector@ for @compositionAspectRatio@
compositionAspectRatioSelector :: Selector
compositionAspectRatioSelector = mkSelector "compositionAspectRatio"

-- | @Selector@ for @setDefaultValue:forInputKey:@
setDefaultValue_forInputKeySelector :: Selector
setDefaultValue_forInputKeySelector = mkSelector "setDefaultValue:forInputKey:"

-- | @Selector@ for @resetDefaultInputValues@
resetDefaultInputValuesSelector :: Selector
resetDefaultInputValuesSelector = mkSelector "resetDefaultInputValues"

-- | @Selector@ for @setSelectedComposition:@
setSelectedCompositionSelector :: Selector
setSelectedCompositionSelector = mkSelector "setSelectedComposition:"

-- | @Selector@ for @selectedComposition@
selectedCompositionSelector :: Selector
selectedCompositionSelector = mkSelector "selectedComposition"

-- | @Selector@ for @startAnimation:@
startAnimationSelector :: Selector
startAnimationSelector = mkSelector "startAnimation:"

-- | @Selector@ for @stopAnimation:@
stopAnimationSelector :: Selector
stopAnimationSelector = mkSelector "stopAnimation:"

-- | @Selector@ for @isAnimating@
isAnimatingSelector :: Selector
isAnimatingSelector = mkSelector "isAnimating"

-- | @Selector@ for @setMaxAnimationFrameRate:@
setMaxAnimationFrameRateSelector :: Selector
setMaxAnimationFrameRateSelector = mkSelector "setMaxAnimationFrameRate:"

-- | @Selector@ for @maxAnimationFrameRate@
maxAnimationFrameRateSelector :: Selector
maxAnimationFrameRateSelector = mkSelector "maxAnimationFrameRate"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setDrawsBackground:@
setDrawsBackgroundSelector :: Selector
setDrawsBackgroundSelector = mkSelector "setDrawsBackground:"

-- | @Selector@ for @drawsBackground@
drawsBackgroundSelector :: Selector
drawsBackgroundSelector = mkSelector "drawsBackground"

-- | @Selector@ for @numberOfColumns@
numberOfColumnsSelector :: Selector
numberOfColumnsSelector = mkSelector "numberOfColumns"

-- | @Selector@ for @setNumberOfColumns:@
setNumberOfColumnsSelector :: Selector
setNumberOfColumnsSelector = mkSelector "setNumberOfColumns:"

-- | @Selector@ for @numberOfRows@
numberOfRowsSelector :: Selector
numberOfRowsSelector = mkSelector "numberOfRows"

-- | @Selector@ for @setNumberOfRows:@
setNumberOfRowsSelector :: Selector
setNumberOfRowsSelector = mkSelector "setNumberOfRows:"

