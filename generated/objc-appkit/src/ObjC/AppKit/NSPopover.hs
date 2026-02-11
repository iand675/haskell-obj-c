{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPopover@.
module ObjC.AppKit.NSPopover
  ( NSPopover
  , IsNSPopover(..)
  , init_
  , initWithCoder
  , showRelativeToRect_ofView_preferredEdge
  , showRelativeToToolbarItem
  , performClose
  , close
  , behavior
  , setBehavior
  , animates
  , setAnimates
  , contentViewController
  , setContentViewController
  , contentSize
  , setContentSize
  , shown
  , detached
  , positioningRect
  , setPositioningRect
  , hasFullSizeContent
  , setHasFullSizeContent
  , initSelector
  , initWithCoderSelector
  , showRelativeToRect_ofView_preferredEdgeSelector
  , showRelativeToToolbarItemSelector
  , performCloseSelector
  , closeSelector
  , behaviorSelector
  , setBehaviorSelector
  , animatesSelector
  , setAnimatesSelector
  , contentViewControllerSelector
  , setContentViewControllerSelector
  , contentSizeSelector
  , setContentSizeSelector
  , shownSelector
  , detachedSelector
  , positioningRectSelector
  , setPositioningRectSelector
  , hasFullSizeContentSelector
  , setHasFullSizeContentSelector

  -- * Enum types
  , NSPopoverBehavior(NSPopoverBehavior)
  , pattern NSPopoverBehaviorApplicationDefined
  , pattern NSPopoverBehaviorTransient
  , pattern NSPopoverBehaviorSemitransient
  , NSRectEdge(NSRectEdge)
  , pattern NSRectEdgeMinX
  , pattern NSRectEdgeMinY
  , pattern NSRectEdgeMaxX
  , pattern NSRectEdgeMaxY
  , pattern NSMinXEdge
  , pattern NSMinYEdge
  , pattern NSMaxXEdge
  , pattern NSMaxYEdge

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSPopover nsPopover => nsPopover -> IO (Id NSPopover)
init_ nsPopover  =
  sendMsg nsPopover (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSPopover nsPopover, IsNSCoder coder) => nsPopover -> coder -> IO (Id NSPopover)
initWithCoder nsPopover  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsPopover (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | Shows the popover anchored to the specified view.
--
-- The popover will animate onscreen and eventually animate offscreen when it is closed (unless the property @animates@ is set to @NO@).
--
-- - Parameters:   - positioningRect: The rectangle within @positioningView@ relative to which the popover should be positioned. Normally set to the bounds of @positioningView@. May be an empty rectangle, which will default to the bounds of @positioningView@.   - positioningView: The view relative to which the popover should be positioned. Causes the method to raise @NSInvalidArgumentException@ if @nil@.   - preferredEdge: The edge of @positioningView@ the popover should prefer to be anchored to (respects to the @-isFlipped@ state of @positioningView@). The current (but not guaranteed) behavior is that AppKit will place the anchor towards the @preferredEdge@ of the @positioningRect@ unless such a placement would cause the popover not to fit on the screen of @positioningView@. If the anchor cannot be placed towards the @preferredEdge@, AppKit will (in the current implementation) attempt to place the anchor on the opposite side of the @positioningRect@. If that cannot be done, AppKit will attempt to place the anchor on a remaining side of the popover, and failing that will center the popover on the screen, causing it to (at least temporarily) lose its anchor.
--
-- - Note: This method will throw a @NSInvalidArgumentException@ if view is @nil@ or if @view@ is not in a window, or if the popover’s behavior is @NSPopoverBehaviorSemitransient@ and the popover’s @positioningView@ is in a popover or child window. It will throw a @NSInternalInconsistencyException@ if the popover’s  content view controller (or the view controller’s view) is @nil@. If the popover is already being shown, this method will update to be associated with the new @view@ and @positioningRect@ passed.
--
-- - Note: If the positioning view isn’t visible (its window isn’t visible, or the positioning rect is outside of its visible rect), this method does nothing.
--
-- ObjC selector: @- showRelativeToRect:ofView:preferredEdge:@
showRelativeToRect_ofView_preferredEdge :: (IsNSPopover nsPopover, IsNSView positioningView) => nsPopover -> NSRect -> positioningView -> NSRectEdge -> IO ()
showRelativeToRect_ofView_preferredEdge nsPopover  positioningRect positioningView preferredEdge =
withObjCPtr positioningView $ \raw_positioningView ->
    sendMsg nsPopover (mkSelector "showRelativeToRect:ofView:preferredEdge:") retVoid [argNSRect positioningRect, argPtr (castPtr raw_positioningView :: Ptr ()), argCULong (coerce preferredEdge)]

-- | Shows the popover positioned relative to @toolbarItem@ . When the item is in the overflow menu, the popover will be presented from another appropriate affordance in the window. See the comments in @-showRelativeToRect:ofView:preferredEdge:@ for the popover behavior.
--
-- This method will throw an @NSInvalidArgumentException@ if it cannot locate the toolbar item. This could happen because the item is not in a toolbar, or because the toolbar is not in a window.
--
-- ObjC selector: @- showRelativeToToolbarItem:@
showRelativeToToolbarItem :: (IsNSPopover nsPopover, IsNSToolbarItem toolbarItem) => nsPopover -> toolbarItem -> IO ()
showRelativeToToolbarItem nsPopover  toolbarItem =
withObjCPtr toolbarItem $ \raw_toolbarItem ->
    sendMsg nsPopover (mkSelector "showRelativeToToolbarItem:") retVoid [argPtr (castPtr raw_toolbarItem :: Ptr ())]

-- | @- performClose:@
performClose :: IsNSPopover nsPopover => nsPopover -> RawId -> IO ()
performClose nsPopover  sender =
  sendMsg nsPopover (mkSelector "performClose:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- close@
close :: IsNSPopover nsPopover => nsPopover -> IO ()
close nsPopover  =
  sendMsg nsPopover (mkSelector "close") retVoid []

-- | @- behavior@
behavior :: IsNSPopover nsPopover => nsPopover -> IO NSPopoverBehavior
behavior nsPopover  =
  fmap (coerce :: CLong -> NSPopoverBehavior) $ sendMsg nsPopover (mkSelector "behavior") retCLong []

-- | @- setBehavior:@
setBehavior :: IsNSPopover nsPopover => nsPopover -> NSPopoverBehavior -> IO ()
setBehavior nsPopover  value =
  sendMsg nsPopover (mkSelector "setBehavior:") retVoid [argCLong (coerce value)]

-- | @- animates@
animates :: IsNSPopover nsPopover => nsPopover -> IO Bool
animates nsPopover  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPopover (mkSelector "animates") retCULong []

-- | @- setAnimates:@
setAnimates :: IsNSPopover nsPopover => nsPopover -> Bool -> IO ()
setAnimates nsPopover  value =
  sendMsg nsPopover (mkSelector "setAnimates:") retVoid [argCULong (if value then 1 else 0)]

-- | @- contentViewController@
contentViewController :: IsNSPopover nsPopover => nsPopover -> IO (Id NSViewController)
contentViewController nsPopover  =
  sendMsg nsPopover (mkSelector "contentViewController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentViewController:@
setContentViewController :: (IsNSPopover nsPopover, IsNSViewController value) => nsPopover -> value -> IO ()
setContentViewController nsPopover  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPopover (mkSelector "setContentViewController:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contentSize@
contentSize :: IsNSPopover nsPopover => nsPopover -> IO NSSize
contentSize nsPopover  =
  sendMsgStret nsPopover (mkSelector "contentSize") retNSSize []

-- | @- setContentSize:@
setContentSize :: IsNSPopover nsPopover => nsPopover -> NSSize -> IO ()
setContentSize nsPopover  value =
  sendMsg nsPopover (mkSelector "setContentSize:") retVoid [argNSSize value]

-- | @- shown@
shown :: IsNSPopover nsPopover => nsPopover -> IO Bool
shown nsPopover  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPopover (mkSelector "shown") retCULong []

-- | Returns @YES@ if the window is detached to an implicitly created detached window, @NO@ otherwise. This method does not apply when the popover is detached to a window returned with @-detachableWindowForPopover:.@
--
-- ObjC selector: @- detached@
detached :: IsNSPopover nsPopover => nsPopover -> IO Bool
detached nsPopover  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPopover (mkSelector "detached") retCULong []

-- | @- positioningRect@
positioningRect :: IsNSPopover nsPopover => nsPopover -> IO NSRect
positioningRect nsPopover  =
  sendMsgStret nsPopover (mkSelector "positioningRect") retNSRect []

-- | @- setPositioningRect:@
setPositioningRect :: IsNSPopover nsPopover => nsPopover -> NSRect -> IO ()
setPositioningRect nsPopover  value =
  sendMsg nsPopover (mkSelector "setPositioningRect:") retVoid [argNSRect value]

-- | @- hasFullSizeContent@
hasFullSizeContent :: IsNSPopover nsPopover => nsPopover -> IO Bool
hasFullSizeContent nsPopover  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPopover (mkSelector "hasFullSizeContent") retCULong []

-- | @- setHasFullSizeContent:@
setHasFullSizeContent :: IsNSPopover nsPopover => nsPopover -> Bool -> IO ()
setHasFullSizeContent nsPopover  value =
  sendMsg nsPopover (mkSelector "setHasFullSizeContent:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @showRelativeToRect:ofView:preferredEdge:@
showRelativeToRect_ofView_preferredEdgeSelector :: Selector
showRelativeToRect_ofView_preferredEdgeSelector = mkSelector "showRelativeToRect:ofView:preferredEdge:"

-- | @Selector@ for @showRelativeToToolbarItem:@
showRelativeToToolbarItemSelector :: Selector
showRelativeToToolbarItemSelector = mkSelector "showRelativeToToolbarItem:"

-- | @Selector@ for @performClose:@
performCloseSelector :: Selector
performCloseSelector = mkSelector "performClose:"

-- | @Selector@ for @close@
closeSelector :: Selector
closeSelector = mkSelector "close"

-- | @Selector@ for @behavior@
behaviorSelector :: Selector
behaviorSelector = mkSelector "behavior"

-- | @Selector@ for @setBehavior:@
setBehaviorSelector :: Selector
setBehaviorSelector = mkSelector "setBehavior:"

-- | @Selector@ for @animates@
animatesSelector :: Selector
animatesSelector = mkSelector "animates"

-- | @Selector@ for @setAnimates:@
setAnimatesSelector :: Selector
setAnimatesSelector = mkSelector "setAnimates:"

-- | @Selector@ for @contentViewController@
contentViewControllerSelector :: Selector
contentViewControllerSelector = mkSelector "contentViewController"

-- | @Selector@ for @setContentViewController:@
setContentViewControllerSelector :: Selector
setContentViewControllerSelector = mkSelector "setContentViewController:"

-- | @Selector@ for @contentSize@
contentSizeSelector :: Selector
contentSizeSelector = mkSelector "contentSize"

-- | @Selector@ for @setContentSize:@
setContentSizeSelector :: Selector
setContentSizeSelector = mkSelector "setContentSize:"

-- | @Selector@ for @shown@
shownSelector :: Selector
shownSelector = mkSelector "shown"

-- | @Selector@ for @detached@
detachedSelector :: Selector
detachedSelector = mkSelector "detached"

-- | @Selector@ for @positioningRect@
positioningRectSelector :: Selector
positioningRectSelector = mkSelector "positioningRect"

-- | @Selector@ for @setPositioningRect:@
setPositioningRectSelector :: Selector
setPositioningRectSelector = mkSelector "setPositioningRect:"

-- | @Selector@ for @hasFullSizeContent@
hasFullSizeContentSelector :: Selector
hasFullSizeContentSelector = mkSelector "hasFullSizeContent"

-- | @Selector@ for @setHasFullSizeContent:@
setHasFullSizeContentSelector :: Selector
setHasFullSizeContentSelector = mkSelector "setHasFullSizeContent:"

