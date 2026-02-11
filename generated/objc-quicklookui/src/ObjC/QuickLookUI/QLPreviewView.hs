{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A Quick Look preview of an item that you can embed into your view hierarchy.
--
-- Generated bindings for @QLPreviewView@.
module ObjC.QuickLookUI.QLPreviewView
  ( QLPreviewView
  , IsQLPreviewView(..)
  , initWithFrame_style
  , initWithFrame
  , refreshPreviewItem
  , close
  , displayState
  , setDisplayState
  , shouldCloseWithWindow
  , setShouldCloseWithWindow
  , autostarts
  , setAutostarts
  , initWithFrame_styleSelector
  , initWithFrameSelector
  , refreshPreviewItemSelector
  , closeSelector
  , displayStateSelector
  , setDisplayStateSelector
  , shouldCloseWithWindowSelector
  , setShouldCloseWithWindowSelector
  , autostartsSelector
  , setAutostartsSelector

  -- * Enum types
  , QLPreviewViewStyle(QLPreviewViewStyle)
  , pattern QLPreviewViewStyleNormal
  , pattern QLPreviewViewStyleCompact

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

import ObjC.QuickLookUI.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.QuickLookUI.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a preview view with the provided frame and style.
--
-- This is the designated initializer for the @QLPreviewView@ class.
--
-- - Parameters:   - frame: The frame rectangle for the initialized @QLPreviewView@ object.   - style: The desired style for the @QLPreviewView@ object. For a list of possible styles, see ``QuickLookUI/QLPreviewViewStyle``.
--
-- - Returns: Returns a @QLPreviewView@ object with the designated frame and style.
--
-- ObjC selector: @- initWithFrame:style:@
initWithFrame_style :: IsQLPreviewView qlPreviewView => qlPreviewView -> NSRect -> QLPreviewViewStyle -> IO RawId
initWithFrame_style qlPreviewView  frame style =
  fmap (RawId . castPtr) $ sendMsg qlPreviewView (mkSelector "initWithFrame:style:") (retPtr retVoid) [argNSRect frame, argCULong (coerce style)]

-- | Creates a preview view with the provided frame.
--
-- Calling this method is equivalent to calling ``QuickLookUI/QLPreviewView/initWithFrame:style:`` with the @style@ parameter being ``QuickLookUI/QLPreviewViewStyle/QLPreviewViewStyleNormal``.
--
-- - Parameters:   - frame: The frame rectangle for the initialized @QLPreviewView@ object.
--
-- - Returns: Returns a @QLPreviewView@ object with the designated frame and the default style.
--
-- ObjC selector: @- initWithFrame:@
initWithFrame :: IsQLPreviewView qlPreviewView => qlPreviewView -> NSRect -> IO RawId
initWithFrame qlPreviewView  frame =
  fmap (RawId . castPtr) $ sendMsg qlPreviewView (mkSelector "initWithFrame:") (retPtr retVoid) [argNSRect frame]

-- | Updates the preview to display the currently previewed item.
--
-- When you modify the object that the ``QuickLookUI/QLPreviewView/previewItem`` property points to, call this method to generate and display the new preview.
--
-- ObjC selector: @- refreshPreviewItem@
refreshPreviewItem :: IsQLPreviewView qlPreviewView => qlPreviewView -> IO ()
refreshPreviewItem qlPreviewView  =
  sendMsg qlPreviewView (mkSelector "refreshPreviewItem") retVoid []

-- | Closes the view, releasing the current preview item.
--
-- Once a ``QuickLookUI/QLPreviewView`` is closed, it won’t accept any more preview items. You only need to call this method if ``QuickLookUI/QLPreviewView/shouldCloseWithWindow`` is set to <doc://com.apple.documentation/documentation/objectivec/no>. If you don’t close a ``QuickLookUI/QLPreviewView`` when you are done using it, your app will leak memory.
--
-- ObjC selector: @- close@
close :: IsQLPreviewView qlPreviewView => qlPreviewView -> IO ()
close qlPreviewView  =
  sendMsg qlPreviewView (mkSelector "close") retVoid []

-- | The current display state of the <doc://com.apple.documentation/documentation/quicklookui/qlpreviewview/1504747-previewitem>.
--
-- This property is an opaque object that Quick Look uses to get and set the current display state of the preview. The display state could be, for example, the currently displayed page, the zoom factor on an image, or the position in a movie.
--
-- You can use this property to get and save the current display state of the preview before switching to another. This saving allows you to restore a preview later on when the user switches back to it.
--
-- ObjC selector: @- displayState@
displayState :: IsQLPreviewView qlPreviewView => qlPreviewView -> IO RawId
displayState qlPreviewView  =
  fmap (RawId . castPtr) $ sendMsg qlPreviewView (mkSelector "displayState") (retPtr retVoid) []

-- | The current display state of the <doc://com.apple.documentation/documentation/quicklookui/qlpreviewview/1504747-previewitem>.
--
-- This property is an opaque object that Quick Look uses to get and set the current display state of the preview. The display state could be, for example, the currently displayed page, the zoom factor on an image, or the position in a movie.
--
-- You can use this property to get and save the current display state of the preview before switching to another. This saving allows you to restore a preview later on when the user switches back to it.
--
-- ObjC selector: @- setDisplayState:@
setDisplayState :: IsQLPreviewView qlPreviewView => qlPreviewView -> RawId -> IO ()
setDisplayState qlPreviewView  value =
  sendMsg qlPreviewView (mkSelector "setDisplayState:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | A Boolean value that determines whether the preview should close when its window closes.
--
-- The default value of this property is <doc://com.apple.documentation/documentation/objectivec/yes>, which means that the preview automatically closes when its window closes. If you set this property to <doc://com.apple.documentation/documentation/objectivec/no>, close the preview by calling the ``QuickLookUI/QLPreviewView/close`` method when finished with it. Once you close a ``QuickLookUI/QLPreviewView``, it won’t accept any more preview items.
--
-- ObjC selector: @- shouldCloseWithWindow@
shouldCloseWithWindow :: IsQLPreviewView qlPreviewView => qlPreviewView -> IO Bool
shouldCloseWithWindow qlPreviewView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg qlPreviewView (mkSelector "shouldCloseWithWindow") retCULong []

-- | A Boolean value that determines whether the preview should close when its window closes.
--
-- The default value of this property is <doc://com.apple.documentation/documentation/objectivec/yes>, which means that the preview automatically closes when its window closes. If you set this property to <doc://com.apple.documentation/documentation/objectivec/no>, close the preview by calling the ``QuickLookUI/QLPreviewView/close`` method when finished with it. Once you close a ``QuickLookUI/QLPreviewView``, it won’t accept any more preview items.
--
-- ObjC selector: @- setShouldCloseWithWindow:@
setShouldCloseWithWindow :: IsQLPreviewView qlPreviewView => qlPreviewView -> Bool -> IO ()
setShouldCloseWithWindow qlPreviewView  value =
  sendMsg qlPreviewView (mkSelector "setShouldCloseWithWindow:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value that determines whether the preview starts automatically.
--
-- Set this property to allow previews of movie files to start playback automatically when displayed.
--
-- ObjC selector: @- autostarts@
autostarts :: IsQLPreviewView qlPreviewView => qlPreviewView -> IO Bool
autostarts qlPreviewView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg qlPreviewView (mkSelector "autostarts") retCULong []

-- | A Boolean value that determines whether the preview starts automatically.
--
-- Set this property to allow previews of movie files to start playback automatically when displayed.
--
-- ObjC selector: @- setAutostarts:@
setAutostarts :: IsQLPreviewView qlPreviewView => qlPreviewView -> Bool -> IO ()
setAutostarts qlPreviewView  value =
  sendMsg qlPreviewView (mkSelector "setAutostarts:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:style:@
initWithFrame_styleSelector :: Selector
initWithFrame_styleSelector = mkSelector "initWithFrame:style:"

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @refreshPreviewItem@
refreshPreviewItemSelector :: Selector
refreshPreviewItemSelector = mkSelector "refreshPreviewItem"

-- | @Selector@ for @close@
closeSelector :: Selector
closeSelector = mkSelector "close"

-- | @Selector@ for @displayState@
displayStateSelector :: Selector
displayStateSelector = mkSelector "displayState"

-- | @Selector@ for @setDisplayState:@
setDisplayStateSelector :: Selector
setDisplayStateSelector = mkSelector "setDisplayState:"

-- | @Selector@ for @shouldCloseWithWindow@
shouldCloseWithWindowSelector :: Selector
shouldCloseWithWindowSelector = mkSelector "shouldCloseWithWindow"

-- | @Selector@ for @setShouldCloseWithWindow:@
setShouldCloseWithWindowSelector :: Selector
setShouldCloseWithWindowSelector = mkSelector "setShouldCloseWithWindow:"

-- | @Selector@ for @autostarts@
autostartsSelector :: Selector
autostartsSelector = mkSelector "autostarts"

-- | @Selector@ for @setAutostarts:@
setAutostartsSelector :: Selector
setAutostartsSelector = mkSelector "setAutostarts:"

