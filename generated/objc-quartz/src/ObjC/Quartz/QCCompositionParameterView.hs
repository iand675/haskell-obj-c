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
  , setCompositionRendererSelector
  , compositionRendererSelector
  , hasParametersSelector
  , setBackgroundColorSelector
  , backgroundColorSelector
  , setDrawsBackgroundSelector
  , drawsBackgroundSelector
  , setDelegateSelector
  , delegateSelector


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

import ObjC.Quartz.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- setCompositionRenderer:@
setCompositionRenderer :: IsQCCompositionParameterView qcCompositionParameterView => qcCompositionParameterView -> RawId -> IO ()
setCompositionRenderer qcCompositionParameterView  renderer =
  sendMsg qcCompositionParameterView (mkSelector "setCompositionRenderer:") retVoid [argPtr (castPtr (unRawId renderer) :: Ptr ())]

-- | @- compositionRenderer@
compositionRenderer :: IsQCCompositionParameterView qcCompositionParameterView => qcCompositionParameterView -> IO RawId
compositionRenderer qcCompositionParameterView  =
  fmap (RawId . castPtr) $ sendMsg qcCompositionParameterView (mkSelector "compositionRenderer") (retPtr retVoid) []

-- | @- hasParameters@
hasParameters :: IsQCCompositionParameterView qcCompositionParameterView => qcCompositionParameterView -> IO Bool
hasParameters qcCompositionParameterView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg qcCompositionParameterView (mkSelector "hasParameters") retCULong []

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsQCCompositionParameterView qcCompositionParameterView, IsNSColor color) => qcCompositionParameterView -> color -> IO ()
setBackgroundColor qcCompositionParameterView  color =
withObjCPtr color $ \raw_color ->
    sendMsg qcCompositionParameterView (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_color :: Ptr ())]

-- | @- backgroundColor@
backgroundColor :: IsQCCompositionParameterView qcCompositionParameterView => qcCompositionParameterView -> IO (Id NSColor)
backgroundColor qcCompositionParameterView  =
  sendMsg qcCompositionParameterView (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDrawsBackground:@
setDrawsBackground :: IsQCCompositionParameterView qcCompositionParameterView => qcCompositionParameterView -> Bool -> IO ()
setDrawsBackground qcCompositionParameterView  flag =
  sendMsg qcCompositionParameterView (mkSelector "setDrawsBackground:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- drawsBackground@
drawsBackground :: IsQCCompositionParameterView qcCompositionParameterView => qcCompositionParameterView -> IO Bool
drawsBackground qcCompositionParameterView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg qcCompositionParameterView (mkSelector "drawsBackground") retCULong []

-- | @- setDelegate:@
setDelegate :: IsQCCompositionParameterView qcCompositionParameterView => qcCompositionParameterView -> RawId -> IO ()
setDelegate qcCompositionParameterView  delegate =
  sendMsg qcCompositionParameterView (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ())]

-- | @- delegate@
delegate :: IsQCCompositionParameterView qcCompositionParameterView => qcCompositionParameterView -> IO RawId
delegate qcCompositionParameterView  =
  fmap (RawId . castPtr) $ sendMsg qcCompositionParameterView (mkSelector "delegate") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setCompositionRenderer:@
setCompositionRendererSelector :: Selector
setCompositionRendererSelector = mkSelector "setCompositionRenderer:"

-- | @Selector@ for @compositionRenderer@
compositionRendererSelector :: Selector
compositionRendererSelector = mkSelector "compositionRenderer"

-- | @Selector@ for @hasParameters@
hasParametersSelector :: Selector
hasParametersSelector = mkSelector "hasParameters"

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

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

