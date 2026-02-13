{-# LANGUAGE DataKinds #-}
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
  , collapsedRepresentationImageSelector
  , collapsedRepresentationLabelSelector
  , collapsedRepresentationSelector
  , customizationLabelSelector
  , dismissPopoverSelector
  , makeStandardActivatePopoverGestureRecognizerSelector
  , popoverTouchBarSelector
  , pressAndHoldTouchBarSelector
  , setCollapsedRepresentationImageSelector
  , setCollapsedRepresentationLabelSelector
  , setCollapsedRepresentationSelector
  , setCustomizationLabelSelector
  , setPopoverTouchBarSelector
  , setPressAndHoldTouchBarSelector
  , setShowsCloseButtonSelector
  , showPopoverSelector
  , showsCloseButtonSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- showPopover:@
showPopover :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> RawId -> IO ()
showPopover nsPopoverTouchBarItem sender =
  sendMessage nsPopoverTouchBarItem showPopoverSelector sender

-- | @- dismissPopover:@
dismissPopover :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> RawId -> IO ()
dismissPopover nsPopoverTouchBarItem sender =
  sendMessage nsPopoverTouchBarItem dismissPopoverSelector sender

-- | @- makeStandardActivatePopoverGestureRecognizer@
makeStandardActivatePopoverGestureRecognizer :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> IO (Id NSGestureRecognizer)
makeStandardActivatePopoverGestureRecognizer nsPopoverTouchBarItem =
  sendMessage nsPopoverTouchBarItem makeStandardActivatePopoverGestureRecognizerSelector

-- | @- popoverTouchBar@
popoverTouchBar :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> IO (Id NSTouchBar)
popoverTouchBar nsPopoverTouchBarItem =
  sendMessage nsPopoverTouchBarItem popoverTouchBarSelector

-- | @- setPopoverTouchBar:@
setPopoverTouchBar :: (IsNSPopoverTouchBarItem nsPopoverTouchBarItem, IsNSTouchBar value) => nsPopoverTouchBarItem -> value -> IO ()
setPopoverTouchBar nsPopoverTouchBarItem value =
  sendMessage nsPopoverTouchBarItem setPopoverTouchBarSelector (toNSTouchBar value)

-- | @- customizationLabel@
customizationLabel :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> IO (Id NSString)
customizationLabel nsPopoverTouchBarItem =
  sendMessage nsPopoverTouchBarItem customizationLabelSelector

-- | @- setCustomizationLabel:@
setCustomizationLabel :: (IsNSPopoverTouchBarItem nsPopoverTouchBarItem, IsNSString value) => nsPopoverTouchBarItem -> value -> IO ()
setCustomizationLabel nsPopoverTouchBarItem value =
  sendMessage nsPopoverTouchBarItem setCustomizationLabelSelector (toNSString value)

-- | @- collapsedRepresentation@
collapsedRepresentation :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> IO (Id NSView)
collapsedRepresentation nsPopoverTouchBarItem =
  sendMessage nsPopoverTouchBarItem collapsedRepresentationSelector

-- | @- setCollapsedRepresentation:@
setCollapsedRepresentation :: (IsNSPopoverTouchBarItem nsPopoverTouchBarItem, IsNSView value) => nsPopoverTouchBarItem -> value -> IO ()
setCollapsedRepresentation nsPopoverTouchBarItem value =
  sendMessage nsPopoverTouchBarItem setCollapsedRepresentationSelector (toNSView value)

-- | @- collapsedRepresentationImage@
collapsedRepresentationImage :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> IO (Id NSImage)
collapsedRepresentationImage nsPopoverTouchBarItem =
  sendMessage nsPopoverTouchBarItem collapsedRepresentationImageSelector

-- | @- setCollapsedRepresentationImage:@
setCollapsedRepresentationImage :: (IsNSPopoverTouchBarItem nsPopoverTouchBarItem, IsNSImage value) => nsPopoverTouchBarItem -> value -> IO ()
setCollapsedRepresentationImage nsPopoverTouchBarItem value =
  sendMessage nsPopoverTouchBarItem setCollapsedRepresentationImageSelector (toNSImage value)

-- | @- collapsedRepresentationLabel@
collapsedRepresentationLabel :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> IO (Id NSString)
collapsedRepresentationLabel nsPopoverTouchBarItem =
  sendMessage nsPopoverTouchBarItem collapsedRepresentationLabelSelector

-- | @- setCollapsedRepresentationLabel:@
setCollapsedRepresentationLabel :: (IsNSPopoverTouchBarItem nsPopoverTouchBarItem, IsNSString value) => nsPopoverTouchBarItem -> value -> IO ()
setCollapsedRepresentationLabel nsPopoverTouchBarItem value =
  sendMessage nsPopoverTouchBarItem setCollapsedRepresentationLabelSelector (toNSString value)

-- | @- pressAndHoldTouchBar@
pressAndHoldTouchBar :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> IO (Id NSTouchBar)
pressAndHoldTouchBar nsPopoverTouchBarItem =
  sendMessage nsPopoverTouchBarItem pressAndHoldTouchBarSelector

-- | @- setPressAndHoldTouchBar:@
setPressAndHoldTouchBar :: (IsNSPopoverTouchBarItem nsPopoverTouchBarItem, IsNSTouchBar value) => nsPopoverTouchBarItem -> value -> IO ()
setPressAndHoldTouchBar nsPopoverTouchBarItem value =
  sendMessage nsPopoverTouchBarItem setPressAndHoldTouchBarSelector (toNSTouchBar value)

-- | @- showsCloseButton@
showsCloseButton :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> IO Bool
showsCloseButton nsPopoverTouchBarItem =
  sendMessage nsPopoverTouchBarItem showsCloseButtonSelector

-- | @- setShowsCloseButton:@
setShowsCloseButton :: IsNSPopoverTouchBarItem nsPopoverTouchBarItem => nsPopoverTouchBarItem -> Bool -> IO ()
setShowsCloseButton nsPopoverTouchBarItem value =
  sendMessage nsPopoverTouchBarItem setShowsCloseButtonSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @showPopover:@
showPopoverSelector :: Selector '[RawId] ()
showPopoverSelector = mkSelector "showPopover:"

-- | @Selector@ for @dismissPopover:@
dismissPopoverSelector :: Selector '[RawId] ()
dismissPopoverSelector = mkSelector "dismissPopover:"

-- | @Selector@ for @makeStandardActivatePopoverGestureRecognizer@
makeStandardActivatePopoverGestureRecognizerSelector :: Selector '[] (Id NSGestureRecognizer)
makeStandardActivatePopoverGestureRecognizerSelector = mkSelector "makeStandardActivatePopoverGestureRecognizer"

-- | @Selector@ for @popoverTouchBar@
popoverTouchBarSelector :: Selector '[] (Id NSTouchBar)
popoverTouchBarSelector = mkSelector "popoverTouchBar"

-- | @Selector@ for @setPopoverTouchBar:@
setPopoverTouchBarSelector :: Selector '[Id NSTouchBar] ()
setPopoverTouchBarSelector = mkSelector "setPopoverTouchBar:"

-- | @Selector@ for @customizationLabel@
customizationLabelSelector :: Selector '[] (Id NSString)
customizationLabelSelector = mkSelector "customizationLabel"

-- | @Selector@ for @setCustomizationLabel:@
setCustomizationLabelSelector :: Selector '[Id NSString] ()
setCustomizationLabelSelector = mkSelector "setCustomizationLabel:"

-- | @Selector@ for @collapsedRepresentation@
collapsedRepresentationSelector :: Selector '[] (Id NSView)
collapsedRepresentationSelector = mkSelector "collapsedRepresentation"

-- | @Selector@ for @setCollapsedRepresentation:@
setCollapsedRepresentationSelector :: Selector '[Id NSView] ()
setCollapsedRepresentationSelector = mkSelector "setCollapsedRepresentation:"

-- | @Selector@ for @collapsedRepresentationImage@
collapsedRepresentationImageSelector :: Selector '[] (Id NSImage)
collapsedRepresentationImageSelector = mkSelector "collapsedRepresentationImage"

-- | @Selector@ for @setCollapsedRepresentationImage:@
setCollapsedRepresentationImageSelector :: Selector '[Id NSImage] ()
setCollapsedRepresentationImageSelector = mkSelector "setCollapsedRepresentationImage:"

-- | @Selector@ for @collapsedRepresentationLabel@
collapsedRepresentationLabelSelector :: Selector '[] (Id NSString)
collapsedRepresentationLabelSelector = mkSelector "collapsedRepresentationLabel"

-- | @Selector@ for @setCollapsedRepresentationLabel:@
setCollapsedRepresentationLabelSelector :: Selector '[Id NSString] ()
setCollapsedRepresentationLabelSelector = mkSelector "setCollapsedRepresentationLabel:"

-- | @Selector@ for @pressAndHoldTouchBar@
pressAndHoldTouchBarSelector :: Selector '[] (Id NSTouchBar)
pressAndHoldTouchBarSelector = mkSelector "pressAndHoldTouchBar"

-- | @Selector@ for @setPressAndHoldTouchBar:@
setPressAndHoldTouchBarSelector :: Selector '[Id NSTouchBar] ()
setPressAndHoldTouchBarSelector = mkSelector "setPressAndHoldTouchBar:"

-- | @Selector@ for @showsCloseButton@
showsCloseButtonSelector :: Selector '[] Bool
showsCloseButtonSelector = mkSelector "showsCloseButton"

-- | @Selector@ for @setShowsCloseButton:@
setShowsCloseButtonSelector :: Selector '[Bool] ()
setShowsCloseButtonSelector = mkSelector "setShowsCloseButton:"

