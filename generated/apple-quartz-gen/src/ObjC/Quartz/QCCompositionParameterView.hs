{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @QCCompositionParameterView@.
module ObjC.Quartz.QCCompositionParameterView
  ( QCCompositionParameterView
  , IsQCCompositionParameterView(..)
  , setCompositionRenderer
  , compositionRenderer
  , hasParameters
  , setBackgroundColor
  , backgroundColor
  , setDrawsBackground
  , drawsBackground
  , setDelegate
  , delegate
  , backgroundColorSelector
  , compositionRendererSelector
  , delegateSelector
  , drawsBackgroundSelector
  , hasParametersSelector
  , setBackgroundColorSelector
  , setCompositionRendererSelector
  , setDelegateSelector
  , setDrawsBackgroundSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Quartz.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- setCompositionRenderer:@
setCompositionRenderer :: IsQCCompositionParameterView qcCompositionParameterView => qcCompositionParameterView -> RawId -> IO ()
setCompositionRenderer qcCompositionParameterView renderer =
  sendMessage qcCompositionParameterView setCompositionRendererSelector renderer

-- | @- compositionRenderer@
compositionRenderer :: IsQCCompositionParameterView qcCompositionParameterView => qcCompositionParameterView -> IO RawId
compositionRenderer qcCompositionParameterView =
  sendMessage qcCompositionParameterView compositionRendererSelector

-- | @- hasParameters@
hasParameters :: IsQCCompositionParameterView qcCompositionParameterView => qcCompositionParameterView -> IO Bool
hasParameters qcCompositionParameterView =
  sendMessage qcCompositionParameterView hasParametersSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsQCCompositionParameterView qcCompositionParameterView, IsNSColor color) => qcCompositionParameterView -> color -> IO ()
setBackgroundColor qcCompositionParameterView color =
  sendMessage qcCompositionParameterView setBackgroundColorSelector (toNSColor color)

-- | @- backgroundColor@
backgroundColor :: IsQCCompositionParameterView qcCompositionParameterView => qcCompositionParameterView -> IO (Id NSColor)
backgroundColor qcCompositionParameterView =
  sendMessage qcCompositionParameterView backgroundColorSelector

-- | @- setDrawsBackground:@
setDrawsBackground :: IsQCCompositionParameterView qcCompositionParameterView => qcCompositionParameterView -> Bool -> IO ()
setDrawsBackground qcCompositionParameterView flag =
  sendMessage qcCompositionParameterView setDrawsBackgroundSelector flag

-- | @- drawsBackground@
drawsBackground :: IsQCCompositionParameterView qcCompositionParameterView => qcCompositionParameterView -> IO Bool
drawsBackground qcCompositionParameterView =
  sendMessage qcCompositionParameterView drawsBackgroundSelector

-- | @- setDelegate:@
setDelegate :: IsQCCompositionParameterView qcCompositionParameterView => qcCompositionParameterView -> RawId -> IO ()
setDelegate qcCompositionParameterView delegate =
  sendMessage qcCompositionParameterView setDelegateSelector delegate

-- | @- delegate@
delegate :: IsQCCompositionParameterView qcCompositionParameterView => qcCompositionParameterView -> IO RawId
delegate qcCompositionParameterView =
  sendMessage qcCompositionParameterView delegateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setCompositionRenderer:@
setCompositionRendererSelector :: Selector '[RawId] ()
setCompositionRendererSelector = mkSelector "setCompositionRenderer:"

-- | @Selector@ for @compositionRenderer@
compositionRendererSelector :: Selector '[] RawId
compositionRendererSelector = mkSelector "compositionRenderer"

-- | @Selector@ for @hasParameters@
hasParametersSelector :: Selector '[] Bool
hasParametersSelector = mkSelector "hasParameters"

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

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

