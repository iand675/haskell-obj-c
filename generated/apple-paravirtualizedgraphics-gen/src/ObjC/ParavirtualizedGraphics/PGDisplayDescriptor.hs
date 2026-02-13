{-# LANGUAGE DataKinds #-}
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
  , cursorGlyphHandlerSelector
  , cursorMoveHandlerSelector
  , cursorShowHandlerSelector
  , modeChangeHandlerSelector
  , nameSelector
  , newFrameEventHandlerSelector
  , queueSelector
  , setCursorGlyphHandlerSelector
  , setCursorMoveHandlerSelector
  , setCursorShowHandlerSelector
  , setModeChangeHandlerSelector
  , setNameSelector
  , setNewFrameEventHandlerSelector
  , setQueueSelector
  , setSizeInMillimetersSelector
  , sizeInMillimetersSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
name pgDisplayDescriptor =
  sendMessage pgDisplayDescriptor nameSelector

-- | name
--
-- Client supplied name of display, as seen by guest.
--
-- Truncates to 13 characters.  Defaults to "Apple Virtual".  Value provided here may be made visible via guest UI.
--
-- ObjC selector: @- setName:@
setName :: (IsPGDisplayDescriptor pgDisplayDescriptor, IsNSString value) => pgDisplayDescriptor -> value -> IO ()
setName pgDisplayDescriptor value =
  sendMessage pgDisplayDescriptor setNameSelector (toNSString value)

-- | sizeInMillimeters
--
-- Client supplied display size conveyed to guest compositor.
--
-- Conveyed size contributes to guest compositor layout, but host-side VM app can scale to UI of its own choosing.
--
-- ObjC selector: @- sizeInMillimeters@
sizeInMillimeters :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> IO NSSize
sizeInMillimeters pgDisplayDescriptor =
  sendMessage pgDisplayDescriptor sizeInMillimetersSelector

-- | sizeInMillimeters
--
-- Client supplied display size conveyed to guest compositor.
--
-- Conveyed size contributes to guest compositor layout, but host-side VM app can scale to UI of its own choosing.
--
-- ObjC selector: @- setSizeInMillimeters:@
setSizeInMillimeters :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> NSSize -> IO ()
setSizeInMillimeters pgDisplayDescriptor value =
  sendMessage pgDisplayDescriptor setSizeInMillimetersSelector value

-- | queue
--
-- Client supplied dispatch_queue on which to invoke client supplied blocks.
--
-- Typical client provides serial queue, and redispatches if beneficial to process out of order.
--
-- ObjC selector: @- queue@
queue :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> IO (Id NSObject)
queue pgDisplayDescriptor =
  sendMessage pgDisplayDescriptor queueSelector

-- | queue
--
-- Client supplied dispatch_queue on which to invoke client supplied blocks.
--
-- Typical client provides serial queue, and redispatches if beneficial to process out of order.
--
-- ObjC selector: @- setQueue:@
setQueue :: (IsPGDisplayDescriptor pgDisplayDescriptor, IsNSObject value) => pgDisplayDescriptor -> value -> IO ()
setQueue pgDisplayDescriptor value =
  sendMessage pgDisplayDescriptor setQueueSelector (toNSObject value)

-- | modeChangeHandler
--
-- The block to invoke to handle display mode change.
--
-- Handler invocation indicative of display mode change.
--
-- ObjC selector: @- modeChangeHandler@
modeChangeHandler :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> IO (Ptr ())
modeChangeHandler pgDisplayDescriptor =
  sendMessage pgDisplayDescriptor modeChangeHandlerSelector

-- | modeChangeHandler
--
-- The block to invoke to handle display mode change.
--
-- Handler invocation indicative of display mode change.
--
-- ObjC selector: @- setModeChangeHandler:@
setModeChangeHandler :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> Ptr () -> IO ()
setModeChangeHandler pgDisplayDescriptor value =
  sendMessage pgDisplayDescriptor setModeChangeHandlerSelector value

-- | newFrameEventHandler
--
-- The block to invoke to handle notification of the presence of a new Guest compositor frame.
--
-- Handler invocation indicates presence of new frame to be processed for display.  Only one of newFrameEventHandler or presentHandler may be non-nil.
--
-- ObjC selector: @- newFrameEventHandler@
newFrameEventHandler :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> IO (Ptr ())
newFrameEventHandler pgDisplayDescriptor =
  sendOwnedMessage pgDisplayDescriptor newFrameEventHandlerSelector

-- | newFrameEventHandler
--
-- The block to invoke to handle notification of the presence of a new Guest compositor frame.
--
-- Handler invocation indicates presence of new frame to be processed for display.  Only one of newFrameEventHandler or presentHandler may be non-nil.
--
-- ObjC selector: @- setNewFrameEventHandler:@
setNewFrameEventHandler :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> Ptr () -> IO ()
setNewFrameEventHandler pgDisplayDescriptor value =
  sendMessage pgDisplayDescriptor setNewFrameEventHandlerSelector value

-- | cursorGlyphHandler
--
-- The block to invoke to handle cursor glyph updates.
--
-- Handler invocation indicative of new cursor image for display.  If this block is not set, cursor will be precomposited in presented image.
--
-- ObjC selector: @- cursorGlyphHandler@
cursorGlyphHandler :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> IO (Ptr ())
cursorGlyphHandler pgDisplayDescriptor =
  sendMessage pgDisplayDescriptor cursorGlyphHandlerSelector

-- | cursorGlyphHandler
--
-- The block to invoke to handle cursor glyph updates.
--
-- Handler invocation indicative of new cursor image for display.  If this block is not set, cursor will be precomposited in presented image.
--
-- ObjC selector: @- setCursorGlyphHandler:@
setCursorGlyphHandler :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> Ptr () -> IO ()
setCursorGlyphHandler pgDisplayDescriptor value =
  sendMessage pgDisplayDescriptor setCursorGlyphHandlerSelector value

-- | cursorShowHandler
--
-- The block to invoke to handle cursor show/hide updates.
--
-- Handler invocation indicative of hide/show of cursor glyph.  If this block is not set, cursor will be precomposited in presented image.
--
-- ObjC selector: @- cursorShowHandler@
cursorShowHandler :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> IO (Ptr ())
cursorShowHandler pgDisplayDescriptor =
  sendMessage pgDisplayDescriptor cursorShowHandlerSelector

-- | cursorShowHandler
--
-- The block to invoke to handle cursor show/hide updates.
--
-- Handler invocation indicative of hide/show of cursor glyph.  If this block is not set, cursor will be precomposited in presented image.
--
-- ObjC selector: @- setCursorShowHandler:@
setCursorShowHandler :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> Ptr () -> IO ()
setCursorShowHandler pgDisplayDescriptor value =
  sendMessage pgDisplayDescriptor setCursorShowHandlerSelector value

-- | cursorMoveHandler
--
-- The block to invoke to handle cursor movement.
--
-- Handler invocation indicative of movement.  Handler should resampling via PGDisplay::cursorPosition.
--
-- ObjC selector: @- cursorMoveHandler@
cursorMoveHandler :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> IO (Ptr ())
cursorMoveHandler pgDisplayDescriptor =
  sendMessage pgDisplayDescriptor cursorMoveHandlerSelector

-- | cursorMoveHandler
--
-- The block to invoke to handle cursor movement.
--
-- Handler invocation indicative of movement.  Handler should resampling via PGDisplay::cursorPosition.
--
-- ObjC selector: @- setCursorMoveHandler:@
setCursorMoveHandler :: IsPGDisplayDescriptor pgDisplayDescriptor => pgDisplayDescriptor -> Ptr () -> IO ()
setCursorMoveHandler pgDisplayDescriptor value =
  sendMessage pgDisplayDescriptor setCursorMoveHandlerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @sizeInMillimeters@
sizeInMillimetersSelector :: Selector '[] NSSize
sizeInMillimetersSelector = mkSelector "sizeInMillimeters"

-- | @Selector@ for @setSizeInMillimeters:@
setSizeInMillimetersSelector :: Selector '[NSSize] ()
setSizeInMillimetersSelector = mkSelector "setSizeInMillimeters:"

-- | @Selector@ for @queue@
queueSelector :: Selector '[] (Id NSObject)
queueSelector = mkSelector "queue"

-- | @Selector@ for @setQueue:@
setQueueSelector :: Selector '[Id NSObject] ()
setQueueSelector = mkSelector "setQueue:"

-- | @Selector@ for @modeChangeHandler@
modeChangeHandlerSelector :: Selector '[] (Ptr ())
modeChangeHandlerSelector = mkSelector "modeChangeHandler"

-- | @Selector@ for @setModeChangeHandler:@
setModeChangeHandlerSelector :: Selector '[Ptr ()] ()
setModeChangeHandlerSelector = mkSelector "setModeChangeHandler:"

-- | @Selector@ for @newFrameEventHandler@
newFrameEventHandlerSelector :: Selector '[] (Ptr ())
newFrameEventHandlerSelector = mkSelector "newFrameEventHandler"

-- | @Selector@ for @setNewFrameEventHandler:@
setNewFrameEventHandlerSelector :: Selector '[Ptr ()] ()
setNewFrameEventHandlerSelector = mkSelector "setNewFrameEventHandler:"

-- | @Selector@ for @cursorGlyphHandler@
cursorGlyphHandlerSelector :: Selector '[] (Ptr ())
cursorGlyphHandlerSelector = mkSelector "cursorGlyphHandler"

-- | @Selector@ for @setCursorGlyphHandler:@
setCursorGlyphHandlerSelector :: Selector '[Ptr ()] ()
setCursorGlyphHandlerSelector = mkSelector "setCursorGlyphHandler:"

-- | @Selector@ for @cursorShowHandler@
cursorShowHandlerSelector :: Selector '[] (Ptr ())
cursorShowHandlerSelector = mkSelector "cursorShowHandler"

-- | @Selector@ for @setCursorShowHandler:@
setCursorShowHandlerSelector :: Selector '[Ptr ()] ()
setCursorShowHandlerSelector = mkSelector "setCursorShowHandler:"

-- | @Selector@ for @cursorMoveHandler@
cursorMoveHandlerSelector :: Selector '[] (Ptr ())
cursorMoveHandlerSelector = mkSelector "cursorMoveHandler"

-- | @Selector@ for @setCursorMoveHandler:@
setCursorMoveHandlerSelector :: Selector '[Ptr ()] ()
setCursorMoveHandlerSelector = mkSelector "setCursorMoveHandler:"

