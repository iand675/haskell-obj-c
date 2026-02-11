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
  , initWithPasteboardWriterSelector
  , initSelector
  , setDraggingFrame_contentsSelector
  , itemSelector
  , draggingFrameSelector
  , setDraggingFrameSelector
  , imageComponentsProviderSelector
  , setImageComponentsProviderSelector
  , imageComponentsSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- initWithPasteboardWriter:@
initWithPasteboardWriter :: IsNSDraggingItem nsDraggingItem => nsDraggingItem -> RawId -> IO (Id NSDraggingItem)
initWithPasteboardWriter nsDraggingItem  pasteboardWriter =
  sendMsg nsDraggingItem (mkSelector "initWithPasteboardWriter:") (retPtr retVoid) [argPtr (castPtr (unRawId pasteboardWriter) :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSDraggingItem nsDraggingItem => nsDraggingItem -> IO (Id NSDraggingItem)
init_ nsDraggingItem  =
  sendMsg nsDraggingItem (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setDraggingFrame:contents:@
setDraggingFrame_contents :: IsNSDraggingItem nsDraggingItem => nsDraggingItem -> NSRect -> RawId -> IO ()
setDraggingFrame_contents nsDraggingItem  frame contents =
  sendMsg nsDraggingItem (mkSelector "setDraggingFrame:contents:") retVoid [argNSRect frame, argPtr (castPtr (unRawId contents) :: Ptr ())]

-- | @- item@
item :: IsNSDraggingItem nsDraggingItem => nsDraggingItem -> IO RawId
item nsDraggingItem  =
  fmap (RawId . castPtr) $ sendMsg nsDraggingItem (mkSelector "item") (retPtr retVoid) []

-- | @- draggingFrame@
draggingFrame :: IsNSDraggingItem nsDraggingItem => nsDraggingItem -> IO NSRect
draggingFrame nsDraggingItem  =
  sendMsgStret nsDraggingItem (mkSelector "draggingFrame") retNSRect []

-- | @- setDraggingFrame:@
setDraggingFrame :: IsNSDraggingItem nsDraggingItem => nsDraggingItem -> NSRect -> IO ()
setDraggingFrame nsDraggingItem  value =
  sendMsg nsDraggingItem (mkSelector "setDraggingFrame:") retVoid [argNSRect value]

-- | @- imageComponentsProvider@
imageComponentsProvider :: IsNSDraggingItem nsDraggingItem => nsDraggingItem -> IO (Id NSArray)
imageComponentsProvider nsDraggingItem  =
  sendMsg nsDraggingItem (mkSelector "imageComponentsProvider") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImageComponentsProvider:@
setImageComponentsProvider :: (IsNSDraggingItem nsDraggingItem, IsNSArray value) => nsDraggingItem -> value -> IO ()
setImageComponentsProvider nsDraggingItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDraggingItem (mkSelector "setImageComponentsProvider:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- imageComponents@
imageComponents :: IsNSDraggingItem nsDraggingItem => nsDraggingItem -> IO (Id NSArray)
imageComponents nsDraggingItem  =
  sendMsg nsDraggingItem (mkSelector "imageComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPasteboardWriter:@
initWithPasteboardWriterSelector :: Selector
initWithPasteboardWriterSelector = mkSelector "initWithPasteboardWriter:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @setDraggingFrame:contents:@
setDraggingFrame_contentsSelector :: Selector
setDraggingFrame_contentsSelector = mkSelector "setDraggingFrame:contents:"

-- | @Selector@ for @item@
itemSelector :: Selector
itemSelector = mkSelector "item"

-- | @Selector@ for @draggingFrame@
draggingFrameSelector :: Selector
draggingFrameSelector = mkSelector "draggingFrame"

-- | @Selector@ for @setDraggingFrame:@
setDraggingFrameSelector :: Selector
setDraggingFrameSelector = mkSelector "setDraggingFrame:"

-- | @Selector@ for @imageComponentsProvider@
imageComponentsProviderSelector :: Selector
imageComponentsProviderSelector = mkSelector "imageComponentsProvider"

-- | @Selector@ for @setImageComponentsProvider:@
setImageComponentsProviderSelector :: Selector
setImageComponentsProviderSelector = mkSelector "setImageComponentsProvider:"

-- | @Selector@ for @imageComponents@
imageComponentsSelector :: Selector
imageComponentsSelector = mkSelector "imageComponents"

