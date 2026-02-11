{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPopoverTouchBarItem@.
module ObjC.AppKit.NSPopoverTouchBarItem
  ( NSPopoverTouchBarItem
  , IsNSPopoverTouchBarItem(..)
  , showPopover
  , dismissPopover
  , makeStandardActivatePopoverGestureRecognizer
  , popoverTouchBar
  , setPopoverTouchBar
  , customizationLabel
  , setCustomizationLabel
  , collapsedRepresentation
  , setCollapsedRepresentation
  , collapsedRepresentationImage
  , setCollapsedRepresentationImage
  , collapsedRepresentationLabel
  , setCollapsedRepresentationLabel
  , pressAndHoldTouchBar
  , setPressAndHoldTouchBar
  , showsCloseButton
  , setShowsCloseButton
  , showPopoverSelector
  , dismissPopoverSelector
  , makeStandardActivatePopoverGestureRecognizerSelector
  , popoverTouchBarSelector
  , setPopoverTouchBarSelector
  , customizationLabelSelector
  , setCustomizationLabelSelector
  , collapsedRepresentationSelector
  , setCollapsedRepresentationSelector
  , collapsedRepresentationImageSelector
  , setCollapsedRepresentationImageSelector
  , collapsedRepresentationLabelSelector
  , setCollapsedRepresentationLabelSelector
  , pressAndHoldTouchBarSelector
  , setPressAndHoldTouchBarSelector
  , showsCloseButtonSelector
  , setShowsCloseButtonSelector


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

-- | @- showPopover:@
showPopover :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> RawId -> IO ()
showPopover nsPopoverTouchBarItem  sender =
    sendMsg nsPopoverTouchBarItem (mkSelector "showPopover:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- dismissPopover:@
dismissPopover :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> RawId -> IO ()
dismissPopover nsPopoverTouchBarItem  sender =
    sendMsg nsPopoverTouchBarItem (mkSelector "dismissPopover:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- makeStandardActivatePopoverGestureRecognizer@
makeStandardActivatePopoverGestureRecognizer :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> IO (Id NSGestureRecognizer)
makeStandardActivatePopoverGestureRecognizer nsPopoverTouchBarItem  =
    sendMsg nsPopoverTouchBarItem (mkSelector "makeStandardActivatePopoverGestureRecognizer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- popoverTouchBar@
popoverTouchBar :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> IO (Id NSTouchBar)
popoverTouchBar nsPopoverTouchBarItem  =
    sendMsg nsPopoverTouchBarItem (mkSelector "popoverTouchBar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPopoverTouchBar:@
setPopoverTouchBar :: (IsNSPopoverTouchBarItem nsPopoverTouchBarItem, IsNSTouchBar value) => nsPopoverTouchBarItem -> value -> IO ()
setPopoverTouchBar nsPopoverTouchBarItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsPopoverTouchBarItem (mkSelector "setPopoverTouchBar:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- customizationLabel@
customizationLabel :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> IO (Id NSString)
customizationLabel nsPopoverTouchBarItem  =
    sendMsg nsPopoverTouchBarItem (mkSelector "customizationLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCustomizationLabel:@
setCustomizationLabel :: (IsNSPopoverTouchBarItem nsPopoverTouchBarItem, IsNSString value) => nsPopoverTouchBarItem -> value -> IO ()
setCustomizationLabel nsPopoverTouchBarItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsPopoverTouchBarItem (mkSelector "setCustomizationLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- collapsedRepresentation@
collapsedRepresentation :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> IO (Id NSView)
collapsedRepresentation nsPopoverTouchBarItem  =
    sendMsg nsPopoverTouchBarItem (mkSelector "collapsedRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCollapsedRepresentation:@
setCollapsedRepresentation :: (IsNSPopoverTouchBarItem nsPopoverTouchBarItem, IsNSView value) => nsPopoverTouchBarItem -> value -> IO ()
setCollapsedRepresentation nsPopoverTouchBarItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsPopoverTouchBarItem (mkSelector "setCollapsedRepresentation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- collapsedRepresentationImage@
collapsedRepresentationImage :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> IO (Id NSImage)
collapsedRepresentationImage nsPopoverTouchBarItem  =
    sendMsg nsPopoverTouchBarItem (mkSelector "collapsedRepresentationImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCollapsedRepresentationImage:@
setCollapsedRepresentationImage :: (IsNSPopoverTouchBarItem nsPopoverTouchBarItem, IsNSImage value) => nsPopoverTouchBarItem -> value -> IO ()
setCollapsedRepresentationImage nsPopoverTouchBarItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsPopoverTouchBarItem (mkSelector "setCollapsedRepresentationImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- collapsedRepresentationLabel@
collapsedRepresentationLabel :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> IO (Id NSString)
collapsedRepresentationLabel nsPopoverTouchBarItem  =
    sendMsg nsPopoverTouchBarItem (mkSelector "collapsedRepresentationLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCollapsedRepresentationLabel:@
setCollapsedRepresentationLabel :: (IsNSPopoverTouchBarItem nsPopoverTouchBarItem, IsNSString value) => nsPopoverTouchBarItem -> value -> IO ()
setCollapsedRepresentationLabel nsPopoverTouchBarItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsPopoverTouchBarItem (mkSelector "setCollapsedRepresentationLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pressAndHoldTouchBar@
pressAndHoldTouchBar :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> IO (Id NSTouchBar)
pressAndHoldTouchBar nsPopoverTouchBarItem  =
    sendMsg nsPopoverTouchBarItem (mkSelector "pressAndHoldTouchBar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPressAndHoldTouchBar:@
setPressAndHoldTouchBar :: (IsNSPopoverTouchBarItem nsPopoverTouchBarItem, IsNSTouchBar value) => nsPopoverTouchBarItem -> value -> IO ()
setPressAndHoldTouchBar nsPopoverTouchBarItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsPopoverTouchBarItem (mkSelector "setPressAndHoldTouchBar:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- showsCloseButton@
showsCloseButton :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> IO Bool
showsCloseButton nsPopoverTouchBarItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPopoverTouchBarItem (mkSelector "showsCloseButton") retCULong []

-- | @- setShowsCloseButton:@
setShowsCloseButton :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> Bool -> IO ()
setShowsCloseButton nsPopoverTouchBarItem  value =
    sendMsg nsPopoverTouchBarItem (mkSelector "setShowsCloseButton:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @showPopover:@
showPopoverSelector :: Selector
showPopoverSelector = mkSelector "showPopover:"

-- | @Selector@ for @dismissPopover:@
dismissPopoverSelector :: Selector
dismissPopoverSelector = mkSelector "dismissPopover:"

-- | @Selector@ for @makeStandardActivatePopoverGestureRecognizer@
makeStandardActivatePopoverGestureRecognizerSelector :: Selector
makeStandardActivatePopoverGestureRecognizerSelector = mkSelector "makeStandardActivatePopoverGestureRecognizer"

-- | @Selector@ for @popoverTouchBar@
popoverTouchBarSelector :: Selector
popoverTouchBarSelector = mkSelector "popoverTouchBar"

-- | @Selector@ for @setPopoverTouchBar:@
setPopoverTouchBarSelector :: Selector
setPopoverTouchBarSelector = mkSelector "setPopoverTouchBar:"

-- | @Selector@ for @customizationLabel@
customizationLabelSelector :: Selector
customizationLabelSelector = mkSelector "customizationLabel"

-- | @Selector@ for @setCustomizationLabel:@
setCustomizationLabelSelector :: Selector
setCustomizationLabelSelector = mkSelector "setCustomizationLabel:"

-- | @Selector@ for @collapsedRepresentation@
collapsedRepresentationSelector :: Selector
collapsedRepresentationSelector = mkSelector "collapsedRepresentation"

-- | @Selector@ for @setCollapsedRepresentation:@
setCollapsedRepresentationSelector :: Selector
setCollapsedRepresentationSelector = mkSelector "setCollapsedRepresentation:"

-- | @Selector@ for @collapsedRepresentationImage@
collapsedRepresentationImageSelector :: Selector
collapsedRepresentationImageSelector = mkSelector "collapsedRepresentationImage"

-- | @Selector@ for @setCollapsedRepresentationImage:@
setCollapsedRepresentationImageSelector :: Selector
setCollapsedRepresentationImageSelector = mkSelector "setCollapsedRepresentationImage:"

-- | @Selector@ for @collapsedRepresentationLabel@
collapsedRepresentationLabelSelector :: Selector
collapsedRepresentationLabelSelector = mkSelector "collapsedRepresentationLabel"

-- | @Selector@ for @setCollapsedRepresentationLabel:@
setCollapsedRepresentationLabelSelector :: Selector
setCollapsedRepresentationLabelSelector = mkSelector "setCollapsedRepresentationLabel:"

-- | @Selector@ for @pressAndHoldTouchBar@
pressAndHoldTouchBarSelector :: Selector
pressAndHoldTouchBarSelector = mkSelector "pressAndHoldTouchBar"

-- | @Selector@ for @setPressAndHoldTouchBar:@
setPressAndHoldTouchBarSelector :: Selector
setPressAndHoldTouchBarSelector = mkSelector "setPressAndHoldTouchBar:"

-- | @Selector@ for @showsCloseButton@
showsCloseButtonSelector :: Selector
showsCloseButtonSelector = mkSelector "showsCloseButton"

-- | @Selector@ for @setShowsCloseButton:@
setShowsCloseButtonSelector :: Selector
setShowsCloseButtonSelector = mkSelector "setShowsCloseButton:"

