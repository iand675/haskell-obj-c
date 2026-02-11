{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | PGDisplayDescriptor:
--
-- Descriptor to facilitate creation of PGDisplay.
--
-- See [PGDevice newDisplayWithDescriptor:port:serialNum]
--
-- Generated bindings for @PGDisplayDescriptor@.
module ObjC.ParavirtualizedGraphics.PGDisplayDescriptor
  ( PGDisplayDescriptor
  , IsPGDisplayDescriptor(..)
  , name
  , setName
  , sizeInMillimeters
  , setSizeInMillimeters
  , queue
  , setQueue
  , modeChangeHandler
  , setModeChangeHandler
  , newFrameEventHandler
  , setNewFrameEventHandler
  , cursorGlyphHandler
  , setCursorGlyphHandler
  , cursorShowHandler
  , setCursorShowHandler
  , cursorMoveHandler
  , setCursorMoveHandler
  , nameSelector
  , setNameSelector
  , sizeInMillimetersSelector
  , setSizeInMillimetersSelector
  , queueSelector
  , setQueueSelector
  , modeChangeHandlerSelector
  , setModeChangeHandlerSelector
  , newFrameEventHandlerSelector
  , setNewFrameEventHandlerSelector
  , cursorGlyphHandlerSelector
  , setCursorGlyphHandlerSelector
  , cursorShowHandlerSelector
  , setCursorShowHandlerSelector
  , cursorMoveHandlerSelector
  , setCursorMoveHandlerSelector


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

import ObjC.ParavirtualizedGraphics.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | name
--
-- Client supplied name of display, as seen by guest.
--
-- Truncates to 13 characters.  Defaults to "Apple Virtual".  Value provided here may be made visible via guest UI.
--
-- ObjC selector: @- name@
name :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> IO (Id NSString)
name pgDisplayDescriptor  =
    sendMsg pgDisplayDescriptor (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- Client supplied name of display, as seen by guest.
--
-- Truncates to 13 characters.  Defaults to "Apple Virtual".  Value provided here may be made visible via guest UI.
--
-- ObjC selector: @- setName:@
setName :: (IsPGDisplayDescriptor pgDisplayDescriptor, IsNSString value) => pgDisplayDescriptor -> value -> IO ()
setName pgDisplayDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pgDisplayDescriptor (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | sizeInMillimeters
--
-- Client supplied display size conveyed to guest compositor.
--
-- Conveyed size contributes to guest compositor layout, but host-side VM app can scale to UI of its own choosing.
--
-- ObjC selector: @- sizeInMillimeters@
sizeInMillimeters :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> IO NSSize
sizeInMillimeters pgDisplayDescriptor  =
    sendMsgStret pgDisplayDescriptor (mkSelector "sizeInMillimeters") retNSSize []

-- | sizeInMillimeters
--
-- Client supplied display size conveyed to guest compositor.
--
-- Conveyed size contributes to guest compositor layout, but host-side VM app can scale to UI of its own choosing.
--
-- ObjC selector: @- setSizeInMillimeters:@
setSizeInMillimeters :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> NSSize -> IO ()
setSizeInMillimeters pgDisplayDescriptor  value =
    sendMsg pgDisplayDescriptor (mkSelector "setSizeInMillimeters:") retVoid [argNSSize value]

-- | queue
--
-- Client supplied dispatch_queue on which to invoke client supplied blocks.
--
-- Typical client provides serial queue, and redispatches if beneficial to process out of order.
--
-- ObjC selector: @- queue@
queue :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> IO (Id NSObject)
queue pgDisplayDescriptor  =
    sendMsg pgDisplayDescriptor (mkSelector "queue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | queue
--
-- Client supplied dispatch_queue on which to invoke client supplied blocks.
--
-- Typical client provides serial queue, and redispatches if beneficial to process out of order.
--
-- ObjC selector: @- setQueue:@
setQueue :: (IsPGDisplayDescriptor pgDisplayDescriptor, IsNSObject value) => pgDisplayDescriptor -> value -> IO ()
setQueue pgDisplayDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pgDisplayDescriptor (mkSelector "setQueue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | modeChangeHandler
--
-- The block to invoke to handle display mode change.
--
-- Handler invocation indicative of display mode change.
--
-- ObjC selector: @- modeChangeHandler@
modeChangeHandler :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> IO (Ptr ())
modeChangeHandler pgDisplayDescriptor  =
    fmap castPtr $ sendMsg pgDisplayDescriptor (mkSelector "modeChangeHandler") (retPtr retVoid) []

-- | modeChangeHandler
--
-- The block to invoke to handle display mode change.
--
-- Handler invocation indicative of display mode change.
--
-- ObjC selector: @- setModeChangeHandler:@
setModeChangeHandler :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> Ptr () -> IO ()
setModeChangeHandler pgDisplayDescriptor  value =
    sendMsg pgDisplayDescriptor (mkSelector "setModeChangeHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | newFrameEventHandler
--
-- The block to invoke to handle notification of the presence of a new Guest compositor frame.
--
-- Handler invocation indicates presence of new frame to be processed for display.  Only one of newFrameEventHandler or presentHandler may be non-nil.
--
-- ObjC selector: @- newFrameEventHandler@
newFrameEventHandler :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> IO (Ptr ())
newFrameEventHandler pgDisplayDescriptor  =
    fmap castPtr $ sendMsg pgDisplayDescriptor (mkSelector "newFrameEventHandler") (retPtr retVoid) []

-- | newFrameEventHandler
--
-- The block to invoke to handle notification of the presence of a new Guest compositor frame.
--
-- Handler invocation indicates presence of new frame to be processed for display.  Only one of newFrameEventHandler or presentHandler may be non-nil.
--
-- ObjC selector: @- setNewFrameEventHandler:@
setNewFrameEventHandler :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> Ptr () -> IO ()
setNewFrameEventHandler pgDisplayDescriptor  value =
    sendMsg pgDisplayDescriptor (mkSelector "setNewFrameEventHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | cursorGlyphHandler
--
-- The block to invoke to handle cursor glyph updates.
--
-- Handler invocation indicative of new cursor image for display.  If this block is not set, cursor will be precomposited in presented image.
--
-- ObjC selector: @- cursorGlyphHandler@
cursorGlyphHandler :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> IO (Ptr ())
cursorGlyphHandler pgDisplayDescriptor  =
    fmap castPtr $ sendMsg pgDisplayDescriptor (mkSelector "cursorGlyphHandler") (retPtr retVoid) []

-- | cursorGlyphHandler
--
-- The block to invoke to handle cursor glyph updates.
--
-- Handler invocation indicative of new cursor image for display.  If this block is not set, cursor will be precomposited in presented image.
--
-- ObjC selector: @- setCursorGlyphHandler:@
setCursorGlyphHandler :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> Ptr () -> IO ()
setCursorGlyphHandler pgDisplayDescriptor  value =
    sendMsg pgDisplayDescriptor (mkSelector "setCursorGlyphHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | cursorShowHandler
--
-- The block to invoke to handle cursor show/hide updates.
--
-- Handler invocation indicative of hide/show of cursor glyph.  If this block is not set, cursor will be precomposited in presented image.
--
-- ObjC selector: @- cursorShowHandler@
cursorShowHandler :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> IO (Ptr ())
cursorShowHandler pgDisplayDescriptor  =
    fmap castPtr $ sendMsg pgDisplayDescriptor (mkSelector "cursorShowHandler") (retPtr retVoid) []

-- | cursorShowHandler
--
-- The block to invoke to handle cursor show/hide updates.
--
-- Handler invocation indicative of hide/show of cursor glyph.  If this block is not set, cursor will be precomposited in presented image.
--
-- ObjC selector: @- setCursorShowHandler:@
setCursorShowHandler :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> Ptr () -> IO ()
setCursorShowHandler pgDisplayDescriptor  value =
    sendMsg pgDisplayDescriptor (mkSelector "setCursorShowHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | cursorMoveHandler
--
-- The block to invoke to handle cursor movement.
--
-- Handler invocation indicative of movement.  Handler should resampling via PGDisplay::cursorPosition.
--
-- ObjC selector: @- cursorMoveHandler@
cursorMoveHandler :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> IO (Ptr ())
cursorMoveHandler pgDisplayDescriptor  =
    fmap castPtr $ sendMsg pgDisplayDescriptor (mkSelector "cursorMoveHandler") (retPtr retVoid) []

-- | cursorMoveHandler
--
-- The block to invoke to handle cursor movement.
--
-- Handler invocation indicative of movement.  Handler should resampling via PGDisplay::cursorPosition.
--
-- ObjC selector: @- setCursorMoveHandler:@
setCursorMoveHandler :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> Ptr () -> IO ()
setCursorMoveHandler pgDisplayDescriptor  value =
    sendMsg pgDisplayDescriptor (mkSelector "setCursorMoveHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @sizeInMillimeters@
sizeInMillimetersSelector :: Selector
sizeInMillimetersSelector = mkSelector "sizeInMillimeters"

-- | @Selector@ for @setSizeInMillimeters:@
setSizeInMillimetersSelector :: Selector
setSizeInMillimetersSelector = mkSelector "setSizeInMillimeters:"

-- | @Selector@ for @queue@
queueSelector :: Selector
queueSelector = mkSelector "queue"

-- | @Selector@ for @setQueue:@
setQueueSelector :: Selector
setQueueSelector = mkSelector "setQueue:"

-- | @Selector@ for @modeChangeHandler@
modeChangeHandlerSelector :: Selector
modeChangeHandlerSelector = mkSelector "modeChangeHandler"

-- | @Selector@ for @setModeChangeHandler:@
setModeChangeHandlerSelector :: Selector
setModeChangeHandlerSelector = mkSelector "setModeChangeHandler:"

-- | @Selector@ for @newFrameEventHandler@
newFrameEventHandlerSelector :: Selector
newFrameEventHandlerSelector = mkSelector "newFrameEventHandler"

-- | @Selector@ for @setNewFrameEventHandler:@
setNewFrameEventHandlerSelector :: Selector
setNewFrameEventHandlerSelector = mkSelector "setNewFrameEventHandler:"

-- | @Selector@ for @cursorGlyphHandler@
cursorGlyphHandlerSelector :: Selector
cursorGlyphHandlerSelector = mkSelector "cursorGlyphHandler"

-- | @Selector@ for @setCursorGlyphHandler:@
setCursorGlyphHandlerSelector :: Selector
setCursorGlyphHandlerSelector = mkSelector "setCursorGlyphHandler:"

-- | @Selector@ for @cursorShowHandler@
cursorShowHandlerSelector :: Selector
cursorShowHandlerSelector = mkSelector "cursorShowHandler"

-- | @Selector@ for @setCursorShowHandler:@
setCursorShowHandlerSelector :: Selector
setCursorShowHandlerSelector = mkSelector "setCursorShowHandler:"

-- | @Selector@ for @cursorMoveHandler@
cursorMoveHandlerSelector :: Selector
cursorMoveHandlerSelector = mkSelector "cursorMoveHandler"

-- | @Selector@ for @setCursorMoveHandler:@
setCursorMoveHandlerSelector :: Selector
setCursorMoveHandlerSelector = mkSelector "setCursorMoveHandler:"

