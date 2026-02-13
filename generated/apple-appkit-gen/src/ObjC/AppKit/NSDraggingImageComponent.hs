{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDraggingImageComponent@.
module ObjC.AppKit.NSDraggingImageComponent
  ( NSDraggingImageComponent
  , IsNSDraggingImageComponent(..)
  , draggingImageComponentWithKey
  , initWithKey
  , init_
  , key
  , setKey
  , contents
  , setContents
  , frame
  , setFrame
  , contentsSelector
  , draggingImageComponentWithKeySelector
  , frameSelector
  , initSelector
  , initWithKeySelector
  , keySelector
  , setContentsSelector
  , setFrameSelector
  , setKeySelector


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

-- | @+ draggingImageComponentWithKey:@
draggingImageComponentWithKey :: IsNSString key => key -> IO (Id NSDraggingImageComponent)
draggingImageComponentWithKey key =
  do
    cls' <- getRequiredClass "NSDraggingImageComponent"
    sendClassMessage cls' draggingImageComponentWithKeySelector (toNSString key)

-- | @- initWithKey:@
initWithKey :: (IsNSDraggingImageComponent nsDraggingImageComponent, IsNSString key) => nsDraggingImageComponent -> key -> IO (Id NSDraggingImageComponent)
initWithKey nsDraggingImageComponent key =
  sendOwnedMessage nsDraggingImageComponent initWithKeySelector (toNSString key)

-- | @- init@
init_ :: IsNSDraggingImageComponent nsDraggingImageComponent => nsDraggingImageComponent -> IO (Id NSDraggingImageComponent)
init_ nsDraggingImageComponent =
  sendOwnedMessage nsDraggingImageComponent initSelector

-- | @- key@
key :: IsNSDraggingImageComponent nsDraggingImageComponent => nsDraggingImageComponent -> IO (Id NSString)
key nsDraggingImageComponent =
  sendMessage nsDraggingImageComponent keySelector

-- | @- setKey:@
setKey :: (IsNSDraggingImageComponent nsDraggingImageComponent, IsNSString value) => nsDraggingImageComponent -> value -> IO ()
setKey nsDraggingImageComponent value =
  sendMessage nsDraggingImageComponent setKeySelector (toNSString value)

-- | @- contents@
contents :: IsNSDraggingImageComponent nsDraggingImageComponent => nsDraggingImageComponent -> IO RawId
contents nsDraggingImageComponent =
  sendMessage nsDraggingImageComponent contentsSelector

-- | @- setContents:@
setContents :: IsNSDraggingImageComponent nsDraggingImageComponent => nsDraggingImageComponent -> RawId -> IO ()
setContents nsDraggingImageComponent value =
  sendMessage nsDraggingImageComponent setContentsSelector value

-- | @- frame@
frame :: IsNSDraggingImageComponent nsDraggingImageComponent => nsDraggingImageComponent -> IO NSRect
frame nsDraggingImageComponent =
  sendMessage nsDraggingImageComponent frameSelector

-- | @- setFrame:@
setFrame :: IsNSDraggingImageComponent nsDraggingImageComponent => nsDraggingImageComponent -> NSRect -> IO ()
setFrame nsDraggingImageComponent value =
  sendMessage nsDraggingImageComponent setFrameSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @draggingImageComponentWithKey:@
draggingImageComponentWithKeySelector :: Selector '[Id NSString] (Id NSDraggingImageComponent)
draggingImageComponentWithKeySelector = mkSelector "draggingImageComponentWithKey:"

-- | @Selector@ for @initWithKey:@
initWithKeySelector :: Selector '[Id NSString] (Id NSDraggingImageComponent)
initWithKeySelector = mkSelector "initWithKey:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSDraggingImageComponent)
initSelector = mkSelector "init"

-- | @Selector@ for @key@
keySelector :: Selector '[] (Id NSString)
keySelector = mkSelector "key"

-- | @Selector@ for @setKey:@
setKeySelector :: Selector '[Id NSString] ()
setKeySelector = mkSelector "setKey:"

-- | @Selector@ for @contents@
contentsSelector :: Selector '[] RawId
contentsSelector = mkSelector "contents"

-- | @Selector@ for @setContents:@
setContentsSelector :: Selector '[RawId] ()
setContentsSelector = mkSelector "setContents:"

-- | @Selector@ for @frame@
frameSelector :: Selector '[] NSRect
frameSelector = mkSelector "frame"

-- | @Selector@ for @setFrame:@
setFrameSelector :: Selector '[NSRect] ()
setFrameSelector = mkSelector "setFrame:"

