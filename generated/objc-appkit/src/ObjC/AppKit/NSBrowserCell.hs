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
  , initTextCellSelector
  , initImageCellSelector
  , initWithCoderSelector
  , highlightColorInViewSelector
  , resetSelector
  , setSelector
  , branchImageSelector
  , highlightedBranchImageSelector
  , leafSelector
  , setLeafSelector
  , loadedSelector
  , setLoadedSelector
  , imageSelector
  , setImageSelector
  , alternateImageSelector
  , setAlternateImageSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initTextCell:@
initTextCell :: (IsNSBrowserCell nsBrowserCell, IsNSString string) => nsBrowserCell -> string -> IO (Id NSBrowserCell)
initTextCell nsBrowserCell  string =
withObjCPtr string $ \raw_string ->
    sendMsg nsBrowserCell (mkSelector "initTextCell:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | @- initImageCell:@
initImageCell :: (IsNSBrowserCell nsBrowserCell, IsNSImage image) => nsBrowserCell -> image -> IO (Id NSBrowserCell)
initImageCell nsBrowserCell  image =
withObjCPtr image $ \raw_image ->
    sendMsg nsBrowserCell (mkSelector "initImageCell:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSBrowserCell nsBrowserCell, IsNSCoder coder) => nsBrowserCell -> coder -> IO (Id NSBrowserCell)
initWithCoder nsBrowserCell  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsBrowserCell (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- highlightColorInView:@
highlightColorInView :: (IsNSBrowserCell nsBrowserCell, IsNSView controlView) => nsBrowserCell -> controlView -> IO (Id NSColor)
highlightColorInView nsBrowserCell  controlView =
withObjCPtr controlView $ \raw_controlView ->
    sendMsg nsBrowserCell (mkSelector "highlightColorInView:") (retPtr retVoid) [argPtr (castPtr raw_controlView :: Ptr ())] >>= retainedObject . castPtr

-- | @- reset@
reset :: IsNSBrowserCell nsBrowserCell => nsBrowserCell -> IO ()
reset nsBrowserCell  =
  sendMsg nsBrowserCell (mkSelector "reset") retVoid []

-- | @- set@
set :: IsNSBrowserCell nsBrowserCell => nsBrowserCell -> IO ()
set nsBrowserCell  =
  sendMsg nsBrowserCell (mkSelector "set") retVoid []

-- | @+ branchImage@
branchImage :: IO (Id NSImage)
branchImage  =
  do
    cls' <- getRequiredClass "NSBrowserCell"
    sendClassMsg cls' (mkSelector "branchImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ highlightedBranchImage@
highlightedBranchImage :: IO (Id NSImage)
highlightedBranchImage  =
  do
    cls' <- getRequiredClass "NSBrowserCell"
    sendClassMsg cls' (mkSelector "highlightedBranchImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- leaf@
leaf :: IsNSBrowserCell nsBrowserCell => nsBrowserCell -> IO Bool
leaf nsBrowserCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowserCell (mkSelector "leaf") retCULong []

-- | @- setLeaf:@
setLeaf :: IsNSBrowserCell nsBrowserCell => nsBrowserCell -> Bool -> IO ()
setLeaf nsBrowserCell  value =
  sendMsg nsBrowserCell (mkSelector "setLeaf:") retVoid [argCULong (if value then 1 else 0)]

-- | @- loaded@
loaded :: IsNSBrowserCell nsBrowserCell => nsBrowserCell -> IO Bool
loaded nsBrowserCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBrowserCell (mkSelector "loaded") retCULong []

-- | @- setLoaded:@
setLoaded :: IsNSBrowserCell nsBrowserCell => nsBrowserCell -> Bool -> IO ()
setLoaded nsBrowserCell  value =
  sendMsg nsBrowserCell (mkSelector "setLoaded:") retVoid [argCULong (if value then 1 else 0)]

-- | @- image@
image :: IsNSBrowserCell nsBrowserCell => nsBrowserCell -> IO (Id NSImage)
image nsBrowserCell  =
  sendMsg nsBrowserCell (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImage:@
setImage :: (IsNSBrowserCell nsBrowserCell, IsNSImage value) => nsBrowserCell -> value -> IO ()
setImage nsBrowserCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsBrowserCell (mkSelector "setImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- alternateImage@
alternateImage :: IsNSBrowserCell nsBrowserCell => nsBrowserCell -> IO (Id NSImage)
alternateImage nsBrowserCell  =
  sendMsg nsBrowserCell (mkSelector "alternateImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlternateImage:@
setAlternateImage :: (IsNSBrowserCell nsBrowserCell, IsNSImage value) => nsBrowserCell -> value -> IO ()
setAlternateImage nsBrowserCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsBrowserCell (mkSelector "setAlternateImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initTextCell:@
initTextCellSelector :: Selector
initTextCellSelector = mkSelector "initTextCell:"

-- | @Selector@ for @initImageCell:@
initImageCellSelector :: Selector
initImageCellSelector = mkSelector "initImageCell:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @highlightColorInView:@
highlightColorInViewSelector :: Selector
highlightColorInViewSelector = mkSelector "highlightColorInView:"

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @set@
setSelector :: Selector
setSelector = mkSelector "set"

-- | @Selector@ for @branchImage@
branchImageSelector :: Selector
branchImageSelector = mkSelector "branchImage"

-- | @Selector@ for @highlightedBranchImage@
highlightedBranchImageSelector :: Selector
highlightedBranchImageSelector = mkSelector "highlightedBranchImage"

-- | @Selector@ for @leaf@
leafSelector :: Selector
leafSelector = mkSelector "leaf"

-- | @Selector@ for @setLeaf:@
setLeafSelector :: Selector
setLeafSelector = mkSelector "setLeaf:"

-- | @Selector@ for @loaded@
loadedSelector :: Selector
loadedSelector = mkSelector "loaded"

-- | @Selector@ for @setLoaded:@
setLoadedSelector :: Selector
setLoadedSelector = mkSelector "setLoaded:"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @alternateImage@
alternateImageSelector :: Selector
alternateImageSelector = mkSelector "alternateImage"

-- | @Selector@ for @setAlternateImage:@
setAlternateImageSelector :: Selector
setAlternateImageSelector = mkSelector "setAlternateImage:"

