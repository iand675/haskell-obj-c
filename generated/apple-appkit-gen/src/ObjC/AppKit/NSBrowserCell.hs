{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSBrowserCell@.
module ObjC.AppKit.NSBrowserCell
  ( NSBrowserCell
  , IsNSBrowserCell(..)
  , initTextCell
  , initImageCell
  , initWithCoder
  , highlightColorInView
  , reset
  , set
  , branchImage
  , highlightedBranchImage
  , leaf
  , setLeaf
  , loaded
  , setLoaded
  , image
  , setImage
  , alternateImage
  , setAlternateImage
  , alternateImageSelector
  , branchImageSelector
  , highlightColorInViewSelector
  , highlightedBranchImageSelector
  , imageSelector
  , initImageCellSelector
  , initTextCellSelector
  , initWithCoderSelector
  , leafSelector
  , loadedSelector
  , resetSelector
  , setAlternateImageSelector
  , setImageSelector
  , setLeafSelector
  , setLoadedSelector
  , setSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initTextCell:@
initTextCell :: (IsNSBrowserCell nsBrowserCell, IsNSString string) => nsBrowserCell -> string -> IO (Id NSBrowserCell)
initTextCell nsBrowserCell string =
  sendOwnedMessage nsBrowserCell initTextCellSelector (toNSString string)

-- | @- initImageCell:@
initImageCell :: (IsNSBrowserCell nsBrowserCell, IsNSImage image) => nsBrowserCell -> image -> IO (Id NSBrowserCell)
initImageCell nsBrowserCell image =
  sendOwnedMessage nsBrowserCell initImageCellSelector (toNSImage image)

-- | @- initWithCoder:@
initWithCoder :: (IsNSBrowserCell nsBrowserCell, IsNSCoder coder) => nsBrowserCell -> coder -> IO (Id NSBrowserCell)
initWithCoder nsBrowserCell coder =
  sendOwnedMessage nsBrowserCell initWithCoderSelector (toNSCoder coder)

-- | @- highlightColorInView:@
highlightColorInView :: (IsNSBrowserCell nsBrowserCell, IsNSView controlView) => nsBrowserCell -> controlView -> IO (Id NSColor)
highlightColorInView nsBrowserCell controlView =
  sendMessage nsBrowserCell highlightColorInViewSelector (toNSView controlView)

-- | @- reset@
reset :: IsNSBrowserCell nsBrowserCell => nsBrowserCell -> IO ()
reset nsBrowserCell =
  sendMessage nsBrowserCell resetSelector

-- | @- set@
set :: IsNSBrowserCell nsBrowserCell => nsBrowserCell -> IO ()
set nsBrowserCell =
  sendMessage nsBrowserCell setSelector

-- | @+ branchImage@
branchImage :: IO (Id NSImage)
branchImage  =
  do
    cls' <- getRequiredClass "NSBrowserCell"
    sendClassMessage cls' branchImageSelector

-- | @+ highlightedBranchImage@
highlightedBranchImage :: IO (Id NSImage)
highlightedBranchImage  =
  do
    cls' <- getRequiredClass "NSBrowserCell"
    sendClassMessage cls' highlightedBranchImageSelector

-- | @- leaf@
leaf :: IsNSBrowserCell nsBrowserCell => nsBrowserCell -> IO Bool
leaf nsBrowserCell =
  sendMessage nsBrowserCell leafSelector

-- | @- setLeaf:@
setLeaf :: IsNSBrowserCell nsBrowserCell => nsBrowserCell -> Bool -> IO ()
setLeaf nsBrowserCell value =
  sendMessage nsBrowserCell setLeafSelector value

-- | @- loaded@
loaded :: IsNSBrowserCell nsBrowserCell => nsBrowserCell -> IO Bool
loaded nsBrowserCell =
  sendMessage nsBrowserCell loadedSelector

-- | @- setLoaded:@
setLoaded :: IsNSBrowserCell nsBrowserCell => nsBrowserCell -> Bool -> IO ()
setLoaded nsBrowserCell value =
  sendMessage nsBrowserCell setLoadedSelector value

-- | @- image@
image :: IsNSBrowserCell nsBrowserCell => nsBrowserCell -> IO (Id NSImage)
image nsBrowserCell =
  sendMessage nsBrowserCell imageSelector

-- | @- setImage:@
setImage :: (IsNSBrowserCell nsBrowserCell, IsNSImage value) => nsBrowserCell -> value -> IO ()
setImage nsBrowserCell value =
  sendMessage nsBrowserCell setImageSelector (toNSImage value)

-- | @- alternateImage@
alternateImage :: IsNSBrowserCell nsBrowserCell => nsBrowserCell -> IO (Id NSImage)
alternateImage nsBrowserCell =
  sendMessage nsBrowserCell alternateImageSelector

-- | @- setAlternateImage:@
setAlternateImage :: (IsNSBrowserCell nsBrowserCell, IsNSImage value) => nsBrowserCell -> value -> IO ()
setAlternateImage nsBrowserCell value =
  sendMessage nsBrowserCell setAlternateImageSelector (toNSImage value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initTextCell:@
initTextCellSelector :: Selector '[Id NSString] (Id NSBrowserCell)
initTextCellSelector = mkSelector "initTextCell:"

-- | @Selector@ for @initImageCell:@
initImageCellSelector :: Selector '[Id NSImage] (Id NSBrowserCell)
initImageCellSelector = mkSelector "initImageCell:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSBrowserCell)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @highlightColorInView:@
highlightColorInViewSelector :: Selector '[Id NSView] (Id NSColor)
highlightColorInViewSelector = mkSelector "highlightColorInView:"

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @set@
setSelector :: Selector '[] ()
setSelector = mkSelector "set"

-- | @Selector@ for @branchImage@
branchImageSelector :: Selector '[] (Id NSImage)
branchImageSelector = mkSelector "branchImage"

-- | @Selector@ for @highlightedBranchImage@
highlightedBranchImageSelector :: Selector '[] (Id NSImage)
highlightedBranchImageSelector = mkSelector "highlightedBranchImage"

-- | @Selector@ for @leaf@
leafSelector :: Selector '[] Bool
leafSelector = mkSelector "leaf"

-- | @Selector@ for @setLeaf:@
setLeafSelector :: Selector '[Bool] ()
setLeafSelector = mkSelector "setLeaf:"

-- | @Selector@ for @loaded@
loadedSelector :: Selector '[] Bool
loadedSelector = mkSelector "loaded"

-- | @Selector@ for @setLoaded:@
setLoadedSelector :: Selector '[Bool] ()
setLoadedSelector = mkSelector "setLoaded:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id NSImage] ()
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @alternateImage@
alternateImageSelector :: Selector '[] (Id NSImage)
alternateImageSelector = mkSelector "alternateImage"

-- | @Selector@ for @setAlternateImage:@
setAlternateImageSelector :: Selector '[Id NSImage] ()
setAlternateImageSelector = mkSelector "setAlternateImage:"

