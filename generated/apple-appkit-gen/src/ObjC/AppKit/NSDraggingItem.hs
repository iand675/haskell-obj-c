{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDraggingItem@.
module ObjC.AppKit.NSDraggingItem
  ( NSDraggingItem
  , IsNSDraggingItem(..)
  , initWithPasteboardWriter
  , init_
  , setDraggingFrame_contents
  , item
  , draggingFrame
  , setDraggingFrame
  , imageComponentsProvider
  , setImageComponentsProvider
  , imageComponents
  , draggingFrameSelector
  , imageComponentsProviderSelector
  , imageComponentsSelector
  , initSelector
  , initWithPasteboardWriterSelector
  , itemSelector
  , setDraggingFrameSelector
  , setDraggingFrame_contentsSelector
  , setImageComponentsProviderSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- initWithPasteboardWriter:@
initWithPasteboardWriter :: IsNSDraggingItem nsDraggingItem => nsDraggingItem -> RawId -> IO (Id NSDraggingItem)
initWithPasteboardWriter nsDraggingItem pasteboardWriter =
  sendOwnedMessage nsDraggingItem initWithPasteboardWriterSelector pasteboardWriter

-- | @- init@
init_ :: IsNSDraggingItem nsDraggingItem => nsDraggingItem -> IO (Id NSDraggingItem)
init_ nsDraggingItem =
  sendOwnedMessage nsDraggingItem initSelector

-- | @- setDraggingFrame:contents:@
setDraggingFrame_contents :: IsNSDraggingItem nsDraggingItem => nsDraggingItem -> NSRect -> RawId -> IO ()
setDraggingFrame_contents nsDraggingItem frame contents =
  sendMessage nsDraggingItem setDraggingFrame_contentsSelector frame contents

-- | @- item@
item :: IsNSDraggingItem nsDraggingItem => nsDraggingItem -> IO RawId
item nsDraggingItem =
  sendMessage nsDraggingItem itemSelector

-- | @- draggingFrame@
draggingFrame :: IsNSDraggingItem nsDraggingItem => nsDraggingItem -> IO NSRect
draggingFrame nsDraggingItem =
  sendMessage nsDraggingItem draggingFrameSelector

-- | @- setDraggingFrame:@
setDraggingFrame :: IsNSDraggingItem nsDraggingItem => nsDraggingItem -> NSRect -> IO ()
setDraggingFrame nsDraggingItem value =
  sendMessage nsDraggingItem setDraggingFrameSelector value

-- | @- imageComponentsProvider@
imageComponentsProvider :: IsNSDraggingItem nsDraggingItem => nsDraggingItem -> IO (Id NSArray)
imageComponentsProvider nsDraggingItem =
  sendMessage nsDraggingItem imageComponentsProviderSelector

-- | @- setImageComponentsProvider:@
setImageComponentsProvider :: (IsNSDraggingItem nsDraggingItem, IsNSArray value) => nsDraggingItem -> value -> IO ()
setImageComponentsProvider nsDraggingItem value =
  sendMessage nsDraggingItem setImageComponentsProviderSelector (toNSArray value)

-- | @- imageComponents@
imageComponents :: IsNSDraggingItem nsDraggingItem => nsDraggingItem -> IO (Id NSArray)
imageComponents nsDraggingItem =
  sendMessage nsDraggingItem imageComponentsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPasteboardWriter:@
initWithPasteboardWriterSelector :: Selector '[RawId] (Id NSDraggingItem)
initWithPasteboardWriterSelector = mkSelector "initWithPasteboardWriter:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSDraggingItem)
initSelector = mkSelector "init"

-- | @Selector@ for @setDraggingFrame:contents:@
setDraggingFrame_contentsSelector :: Selector '[NSRect, RawId] ()
setDraggingFrame_contentsSelector = mkSelector "setDraggingFrame:contents:"

-- | @Selector@ for @item@
itemSelector :: Selector '[] RawId
itemSelector = mkSelector "item"

-- | @Selector@ for @draggingFrame@
draggingFrameSelector :: Selector '[] NSRect
draggingFrameSelector = mkSelector "draggingFrame"

-- | @Selector@ for @setDraggingFrame:@
setDraggingFrameSelector :: Selector '[NSRect] ()
setDraggingFrameSelector = mkSelector "setDraggingFrame:"

-- | @Selector@ for @imageComponentsProvider@
imageComponentsProviderSelector :: Selector '[] (Id NSArray)
imageComponentsProviderSelector = mkSelector "imageComponentsProvider"

-- | @Selector@ for @setImageComponentsProvider:@
setImageComponentsProviderSelector :: Selector '[Id NSArray] ()
setImageComponentsProviderSelector = mkSelector "setImageComponentsProvider:"

-- | @Selector@ for @imageComponents@
imageComponentsSelector :: Selector '[] (Id NSArray)
imageComponentsSelector = mkSelector "imageComponents"

