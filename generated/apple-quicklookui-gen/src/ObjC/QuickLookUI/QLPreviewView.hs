{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , previewItem
  , setPreviewItem
  , displayState
  , setDisplayState
  , shouldCloseWithWindow
  , setShouldCloseWithWindow
  , autostarts
  , setAutostarts
  , autostartsSelector
  , closeSelector
  , displayStateSelector
  , initWithFrameSelector
  , initWithFrame_styleSelector
  , previewItemSelector
  , refreshPreviewItemSelector
  , setAutostartsSelector
  , setDisplayStateSelector
  , setPreviewItemSelector
  , setShouldCloseWithWindowSelector
  , shouldCloseWithWindowSelector

  -- * Enum types
  , QLPreviewViewStyle(QLPreviewViewStyle)
  , pattern QLPreviewViewStyleNormal
  , pattern QLPreviewViewStyleCompact

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithFrame_style qlPreviewView frame style =
  sendOwnedMessage qlPreviewView initWithFrame_styleSelector frame style

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
initWithFrame qlPreviewView frame =
  sendOwnedMessage qlPreviewView initWithFrameSelector frame

-- | Updates the preview to display the currently previewed item.
--
-- When you modify the object that the ``QuickLookUI/QLPreviewView/previewItem`` property points to, call this method to generate and display the new preview.
--
-- ObjC selector: @- refreshPreviewItem@
refreshPreviewItem :: IsQLPreviewView qlPreviewView => qlPreviewView -> IO ()
refreshPreviewItem qlPreviewView =
  sendMessage qlPreviewView refreshPreviewItemSelector

-- | Closes the view, releasing the current preview item.
--
-- Once a ``QuickLookUI/QLPreviewView`` is closed, it won’t accept any more preview items. You only need to call this method if ``QuickLookUI/QLPreviewView/shouldCloseWithWindow`` is set to <doc://com.apple.documentation/documentation/objectivec/no>. If you don’t close a ``QuickLookUI/QLPreviewView`` when you are done using it, your app will leak memory.
--
-- ObjC selector: @- close@
close :: IsQLPreviewView qlPreviewView => qlPreviewView -> IO ()
close qlPreviewView =
  sendMessage qlPreviewView closeSelector

-- | The item to preview.
--
-- Quick Look requires Items you wish to conform to the <doc://com.apple.documentation/documentation/quicklook/qlpreviewitem> protocol. When you set this property, the ``QuickLookUI/QLPreviewView`` loads the preview asynchronously. Due to this asynchronous behavior, don’t assume that the preview is ready immediately after assigning it to this property.
--
-- ObjC selector: @- previewItem@
previewItem :: IsQLPreviewView qlPreviewView => qlPreviewView -> IO RawId
previewItem qlPreviewView =
  sendMessage qlPreviewView previewItemSelector

-- | The item to preview.
--
-- Quick Look requires Items you wish to conform to the <doc://com.apple.documentation/documentation/quicklook/qlpreviewitem> protocol. When you set this property, the ``QuickLookUI/QLPreviewView`` loads the preview asynchronously. Due to this asynchronous behavior, don’t assume that the preview is ready immediately after assigning it to this property.
--
-- ObjC selector: @- setPreviewItem:@
setPreviewItem :: IsQLPreviewView qlPreviewView => qlPreviewView -> RawId -> IO ()
setPreviewItem qlPreviewView value =
  sendMessage qlPreviewView setPreviewItemSelector value

-- | The current display state of the <doc://com.apple.documentation/documentation/quicklookui/qlpreviewview/1504747-previewitem>.
--
-- This property is an opaque object that Quick Look uses to get and set the current display state of the preview. The display state could be, for example, the currently displayed page, the zoom factor on an image, or the position in a movie.
--
-- You can use this property to get and save the current display state of the preview before switching to another. This saving allows you to restore a preview later on when the user switches back to it.
--
-- ObjC selector: @- displayState@
displayState :: IsQLPreviewView qlPreviewView => qlPreviewView -> IO RawId
displayState qlPreviewView =
  sendMessage qlPreviewView displayStateSelector

-- | The current display state of the <doc://com.apple.documentation/documentation/quicklookui/qlpreviewview/1504747-previewitem>.
--
-- This property is an opaque object that Quick Look uses to get and set the current display state of the preview. The display state could be, for example, the currently displayed page, the zoom factor on an image, or the position in a movie.
--
-- You can use this property to get and save the current display state of the preview before switching to another. This saving allows you to restore a preview later on when the user switches back to it.
--
-- ObjC selector: @- setDisplayState:@
setDisplayState :: IsQLPreviewView qlPreviewView => qlPreviewView -> RawId -> IO ()
setDisplayState qlPreviewView value =
  sendMessage qlPreviewView setDisplayStateSelector value

-- | A Boolean value that determines whether the preview should close when its window closes.
--
-- The default value of this property is <doc://com.apple.documentation/documentation/objectivec/yes>, which means that the preview automatically closes when its window closes. If you set this property to <doc://com.apple.documentation/documentation/objectivec/no>, close the preview by calling the ``QuickLookUI/QLPreviewView/close`` method when finished with it. Once you close a ``QuickLookUI/QLPreviewView``, it won’t accept any more preview items.
--
-- ObjC selector: @- shouldCloseWithWindow@
shouldCloseWithWindow :: IsQLPreviewView qlPreviewView => qlPreviewView -> IO Bool
shouldCloseWithWindow qlPreviewView =
  sendMessage qlPreviewView shouldCloseWithWindowSelector

-- | A Boolean value that determines whether the preview should close when its window closes.
--
-- The default value of this property is <doc://com.apple.documentation/documentation/objectivec/yes>, which means that the preview automatically closes when its window closes. If you set this property to <doc://com.apple.documentation/documentation/objectivec/no>, close the preview by calling the ``QuickLookUI/QLPreviewView/close`` method when finished with it. Once you close a ``QuickLookUI/QLPreviewView``, it won’t accept any more preview items.
--
-- ObjC selector: @- setShouldCloseWithWindow:@
setShouldCloseWithWindow :: IsQLPreviewView qlPreviewView => qlPreviewView -> Bool -> IO ()
setShouldCloseWithWindow qlPreviewView value =
  sendMessage qlPreviewView setShouldCloseWithWindowSelector value

-- | A Boolean value that determines whether the preview starts automatically.
--
-- Set this property to allow previews of movie files to start playback automatically when displayed.
--
-- ObjC selector: @- autostarts@
autostarts :: IsQLPreviewView qlPreviewView => qlPreviewView -> IO Bool
autostarts qlPreviewView =
  sendMessage qlPreviewView autostartsSelector

-- | A Boolean value that determines whether the preview starts automatically.
--
-- Set this property to allow previews of movie files to start playback automatically when displayed.
--
-- ObjC selector: @- setAutostarts:@
setAutostarts :: IsQLPreviewView qlPreviewView => qlPreviewView -> Bool -> IO ()
setAutostarts qlPreviewView value =
  sendMessage qlPreviewView setAutostartsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:style:@
initWithFrame_styleSelector :: Selector '[NSRect, QLPreviewViewStyle] RawId
initWithFrame_styleSelector = mkSelector "initWithFrame:style:"

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector '[NSRect] RawId
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @refreshPreviewItem@
refreshPreviewItemSelector :: Selector '[] ()
refreshPreviewItemSelector = mkSelector "refreshPreviewItem"

-- | @Selector@ for @close@
closeSelector :: Selector '[] ()
closeSelector = mkSelector "close"

-- | @Selector@ for @previewItem@
previewItemSelector :: Selector '[] RawId
previewItemSelector = mkSelector "previewItem"

-- | @Selector@ for @setPreviewItem:@
setPreviewItemSelector :: Selector '[RawId] ()
setPreviewItemSelector = mkSelector "setPreviewItem:"

-- | @Selector@ for @displayState@
displayStateSelector :: Selector '[] RawId
displayStateSelector = mkSelector "displayState"

-- | @Selector@ for @setDisplayState:@
setDisplayStateSelector :: Selector '[RawId] ()
setDisplayStateSelector = mkSelector "setDisplayState:"

-- | @Selector@ for @shouldCloseWithWindow@
shouldCloseWithWindowSelector :: Selector '[] Bool
shouldCloseWithWindowSelector = mkSelector "shouldCloseWithWindow"

-- | @Selector@ for @setShouldCloseWithWindow:@
setShouldCloseWithWindowSelector :: Selector '[Bool] ()
setShouldCloseWithWindowSelector = mkSelector "setShouldCloseWithWindow:"

-- | @Selector@ for @autostarts@
autostartsSelector :: Selector '[] Bool
autostartsSelector = mkSelector "autostarts"

-- | @Selector@ for @setAutostarts:@
setAutostartsSelector :: Selector '[Bool] ()
setAutostartsSelector = mkSelector "setAutostarts:"

