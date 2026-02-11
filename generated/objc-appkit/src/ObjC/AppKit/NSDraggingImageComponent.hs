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
  , draggingImageComponentWithKeySelector
  , initWithKeySelector
  , initSelector
  , keySelector
  , setKeySelector
  , contentsSelector
  , setContentsSelector
  , frameSelector
  , setFrameSelector


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

-- | @+ draggingImageComponentWithKey:@
draggingImageComponentWithKey :: IsNSString key => key -> IO (Id NSDraggingImageComponent)
draggingImageComponentWithKey key =
  do
    cls' <- getRequiredClass "NSDraggingImageComponent"
    withObjCPtr key $ \raw_key ->
      sendClassMsg cls' (mkSelector "draggingImageComponentWithKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithKey:@
initWithKey :: (IsNSDraggingImageComponent nsDraggingImageComponent, IsNSString key) => nsDraggingImageComponent -> key -> IO (Id NSDraggingImageComponent)
initWithKey nsDraggingImageComponent  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsDraggingImageComponent (mkSelector "initWithKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSDraggingImageComponent nsDraggingImageComponent => nsDraggingImageComponent -> IO (Id NSDraggingImageComponent)
init_ nsDraggingImageComponent  =
  sendMsg nsDraggingImageComponent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- key@
key :: IsNSDraggingImageComponent nsDraggingImageComponent => nsDraggingImageComponent -> IO (Id NSString)
key nsDraggingImageComponent  =
  sendMsg nsDraggingImageComponent (mkSelector "key") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKey:@
setKey :: (IsNSDraggingImageComponent nsDraggingImageComponent, IsNSString value) => nsDraggingImageComponent -> value -> IO ()
setKey nsDraggingImageComponent  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDraggingImageComponent (mkSelector "setKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contents@
contents :: IsNSDraggingImageComponent nsDraggingImageComponent => nsDraggingImageComponent -> IO RawId
contents nsDraggingImageComponent  =
  fmap (RawId . castPtr) $ sendMsg nsDraggingImageComponent (mkSelector "contents") (retPtr retVoid) []

-- | @- setContents:@
setContents :: IsNSDraggingImageComponent nsDraggingImageComponent => nsDraggingImageComponent -> RawId -> IO ()
setContents nsDraggingImageComponent  value =
  sendMsg nsDraggingImageComponent (mkSelector "setContents:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- frame@
frame :: IsNSDraggingImageComponent nsDraggingImageComponent => nsDraggingImageComponent -> IO NSRect
frame nsDraggingImageComponent  =
  sendMsgStret nsDraggingImageComponent (mkSelector "frame") retNSRect []

-- | @- setFrame:@
setFrame :: IsNSDraggingImageComponent nsDraggingImageComponent => nsDraggingImageComponent -> NSRect -> IO ()
setFrame nsDraggingImageComponent  value =
  sendMsg nsDraggingImageComponent (mkSelector "setFrame:") retVoid [argNSRect value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @draggingImageComponentWithKey:@
draggingImageComponentWithKeySelector :: Selector
draggingImageComponentWithKeySelector = mkSelector "draggingImageComponentWithKey:"

-- | @Selector@ for @initWithKey:@
initWithKeySelector :: Selector
initWithKeySelector = mkSelector "initWithKey:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @key@
keySelector :: Selector
keySelector = mkSelector "key"

-- | @Selector@ for @setKey:@
setKeySelector :: Selector
setKeySelector = mkSelector "setKey:"

-- | @Selector@ for @contents@
contentsSelector :: Selector
contentsSelector = mkSelector "contents"

-- | @Selector@ for @setContents:@
setContentsSelector :: Selector
setContentsSelector = mkSelector "setContents:"

-- | @Selector@ for @frame@
frameSelector :: Selector
frameSelector = mkSelector "frame"

-- | @Selector@ for @setFrame:@
setFrameSelector :: Selector
setFrameSelector = mkSelector "setFrame:"

