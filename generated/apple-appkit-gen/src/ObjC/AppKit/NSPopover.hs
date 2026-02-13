{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , appearance
  , setAppearance
  , effectiveAppearance
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
  , animatesSelector
  , appearanceSelector
  , behaviorSelector
  , closeSelector
  , contentSizeSelector
  , contentViewControllerSelector
  , delegateSelector
  , detachedSelector
  , effectiveAppearanceSelector
  , hasFullSizeContentSelector
  , initSelector
  , initWithCoderSelector
  , performCloseSelector
  , positioningRectSelector
  , setAnimatesSelector
  , setAppearanceSelector
  , setBehaviorSelector
  , setContentSizeSelector
  , setContentViewControllerSelector
  , setDelegateSelector
  , setHasFullSizeContentSelector
  , setPositioningRectSelector
  , showRelativeToRect_ofView_preferredEdgeSelector
  , showRelativeToToolbarItemSelector
  , shownSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSPopover nsPopover => nsPopover -> IO (Id NSPopover)
init_ nsPopover =
  sendOwnedMessage nsPopover initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsNSPopover nsPopover, IsNSCoder coder) => nsPopover -> coder -> IO (Id NSPopover)
initWithCoder nsPopover coder =
  sendOwnedMessage nsPopover initWithCoderSelector (toNSCoder coder)

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
showRelativeToRect_ofView_preferredEdge nsPopover positioningRect positioningView preferredEdge =
  sendMessage nsPopover showRelativeToRect_ofView_preferredEdgeSelector positioningRect (toNSView positioningView) preferredEdge

-- | Shows the popover positioned relative to @toolbarItem@ . When the item is in the overflow menu, the popover will be presented from another appropriate affordance in the window. See the comments in @-showRelativeToRect:ofView:preferredEdge:@ for the popover behavior.
--
-- This method will throw an @NSInvalidArgumentException@ if it cannot locate the toolbar item. This could happen because the item is not in a toolbar, or because the toolbar is not in a window.
--
-- ObjC selector: @- showRelativeToToolbarItem:@
showRelativeToToolbarItem :: (IsNSPopover nsPopover, IsNSToolbarItem toolbarItem) => nsPopover -> toolbarItem -> IO ()
showRelativeToToolbarItem nsPopover toolbarItem =
  sendMessage nsPopover showRelativeToToolbarItemSelector (toNSToolbarItem toolbarItem)

-- | @- performClose:@
performClose :: IsNSPopover nsPopover => nsPopover -> RawId -> IO ()
performClose nsPopover sender =
  sendMessage nsPopover performCloseSelector sender

-- | @- close@
close :: IsNSPopover nsPopover => nsPopover -> IO ()
close nsPopover =
  sendMessage nsPopover closeSelector

-- | @- delegate@
delegate :: IsNSPopover nsPopover => nsPopover -> IO RawId
delegate nsPopover =
  sendMessage nsPopover delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSPopover nsPopover => nsPopover -> RawId -> IO ()
setDelegate nsPopover value =
  sendMessage nsPopover setDelegateSelector value

-- | The appearance of the popover. The popover's contentView will inherit this appearance. The default effective appearance is the NSAppearanceNameVibrantLight appearance. If nil is set, nil will be returned, and the effective appearance will return to the default. To prevent conflicts with the previous appearance property, this is only available for apps that target 10.10 and higher.
--
-- ObjC selector: @- appearance@
appearance :: IsNSPopover nsPopover => nsPopover -> IO (Id NSAppearance)
appearance nsPopover =
  sendMessage nsPopover appearanceSelector

-- | The appearance of the popover. The popover's contentView will inherit this appearance. The default effective appearance is the NSAppearanceNameVibrantLight appearance. If nil is set, nil will be returned, and the effective appearance will return to the default. To prevent conflicts with the previous appearance property, this is only available for apps that target 10.10 and higher.
--
-- ObjC selector: @- setAppearance:@
setAppearance :: (IsNSPopover nsPopover, IsNSAppearance value) => nsPopover -> value -> IO ()
setAppearance nsPopover value =
  sendMessage nsPopover setAppearanceSelector (toNSAppearance value)

-- | @- effectiveAppearance@
effectiveAppearance :: IsNSPopover nsPopover => nsPopover -> IO (Id NSAppearance)
effectiveAppearance nsPopover =
  sendMessage nsPopover effectiveAppearanceSelector

-- | @- behavior@
behavior :: IsNSPopover nsPopover => nsPopover -> IO NSPopoverBehavior
behavior nsPopover =
  sendMessage nsPopover behaviorSelector

-- | @- setBehavior:@
setBehavior :: IsNSPopover nsPopover => nsPopover -> NSPopoverBehavior -> IO ()
setBehavior nsPopover value =
  sendMessage nsPopover setBehaviorSelector value

-- | @- animates@
animates :: IsNSPopover nsPopover => nsPopover -> IO Bool
animates nsPopover =
  sendMessage nsPopover animatesSelector

-- | @- setAnimates:@
setAnimates :: IsNSPopover nsPopover => nsPopover -> Bool -> IO ()
setAnimates nsPopover value =
  sendMessage nsPopover setAnimatesSelector value

-- | @- contentViewController@
contentViewController :: IsNSPopover nsPopover => nsPopover -> IO (Id NSViewController)
contentViewController nsPopover =
  sendMessage nsPopover contentViewControllerSelector

-- | @- setContentViewController:@
setContentViewController :: (IsNSPopover nsPopover, IsNSViewController value) => nsPopover -> value -> IO ()
setContentViewController nsPopover value =
  sendMessage nsPopover setContentViewControllerSelector (toNSViewController value)

-- | @- contentSize@
contentSize :: IsNSPopover nsPopover => nsPopover -> IO NSSize
contentSize nsPopover =
  sendMessage nsPopover contentSizeSelector

-- | @- setContentSize:@
setContentSize :: IsNSPopover nsPopover => nsPopover -> NSSize -> IO ()
setContentSize nsPopover value =
  sendMessage nsPopover setContentSizeSelector value

-- | @- shown@
shown :: IsNSPopover nsPopover => nsPopover -> IO Bool
shown nsPopover =
  sendMessage nsPopover shownSelector

-- | Returns @YES@ if the window is detached to an implicitly created detached window, @NO@ otherwise. This method does not apply when the popover is detached to a window returned with @-detachableWindowForPopover:.@
--
-- ObjC selector: @- detached@
detached :: IsNSPopover nsPopover => nsPopover -> IO Bool
detached nsPopover =
  sendMessage nsPopover detachedSelector

-- | @- positioningRect@
positioningRect :: IsNSPopover nsPopover => nsPopover -> IO NSRect
positioningRect nsPopover =
  sendMessage nsPopover positioningRectSelector

-- | @- setPositioningRect:@
setPositioningRect :: IsNSPopover nsPopover => nsPopover -> NSRect -> IO ()
setPositioningRect nsPopover value =
  sendMessage nsPopover setPositioningRectSelector value

-- | @- hasFullSizeContent@
hasFullSizeContent :: IsNSPopover nsPopover => nsPopover -> IO Bool
hasFullSizeContent nsPopover =
  sendMessage nsPopover hasFullSizeContentSelector

-- | @- setHasFullSizeContent:@
setHasFullSizeContent :: IsNSPopover nsPopover => nsPopover -> Bool -> IO ()
setHasFullSizeContent nsPopover value =
  sendMessage nsPopover setHasFullSizeContentSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSPopover)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSPopover)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @showRelativeToRect:ofView:preferredEdge:@
showRelativeToRect_ofView_preferredEdgeSelector :: Selector '[NSRect, Id NSView, NSRectEdge] ()
showRelativeToRect_ofView_preferredEdgeSelector = mkSelector "showRelativeToRect:ofView:preferredEdge:"

-- | @Selector@ for @showRelativeToToolbarItem:@
showRelativeToToolbarItemSelector :: Selector '[Id NSToolbarItem] ()
showRelativeToToolbarItemSelector = mkSelector "showRelativeToToolbarItem:"

-- | @Selector@ for @performClose:@
performCloseSelector :: Selector '[RawId] ()
performCloseSelector = mkSelector "performClose:"

-- | @Selector@ for @close@
closeSelector :: Selector '[] ()
closeSelector = mkSelector "close"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @appearance@
appearanceSelector :: Selector '[] (Id NSAppearance)
appearanceSelector = mkSelector "appearance"

-- | @Selector@ for @setAppearance:@
setAppearanceSelector :: Selector '[Id NSAppearance] ()
setAppearanceSelector = mkSelector "setAppearance:"

-- | @Selector@ for @effectiveAppearance@
effectiveAppearanceSelector :: Selector '[] (Id NSAppearance)
effectiveAppearanceSelector = mkSelector "effectiveAppearance"

-- | @Selector@ for @behavior@
behaviorSelector :: Selector '[] NSPopoverBehavior
behaviorSelector = mkSelector "behavior"

-- | @Selector@ for @setBehavior:@
setBehaviorSelector :: Selector '[NSPopoverBehavior] ()
setBehaviorSelector = mkSelector "setBehavior:"

-- | @Selector@ for @animates@
animatesSelector :: Selector '[] Bool
animatesSelector = mkSelector "animates"

-- | @Selector@ for @setAnimates:@
setAnimatesSelector :: Selector '[Bool] ()
setAnimatesSelector = mkSelector "setAnimates:"

-- | @Selector@ for @contentViewController@
contentViewControllerSelector :: Selector '[] (Id NSViewController)
contentViewControllerSelector = mkSelector "contentViewController"

-- | @Selector@ for @setContentViewController:@
setContentViewControllerSelector :: Selector '[Id NSViewController] ()
setContentViewControllerSelector = mkSelector "setContentViewController:"

-- | @Selector@ for @contentSize@
contentSizeSelector :: Selector '[] NSSize
contentSizeSelector = mkSelector "contentSize"

-- | @Selector@ for @setContentSize:@
setContentSizeSelector :: Selector '[NSSize] ()
setContentSizeSelector = mkSelector "setContentSize:"

-- | @Selector@ for @shown@
shownSelector :: Selector '[] Bool
shownSelector = mkSelector "shown"

-- | @Selector@ for @detached@
detachedSelector :: Selector '[] Bool
detachedSelector = mkSelector "detached"

-- | @Selector@ for @positioningRect@
positioningRectSelector :: Selector '[] NSRect
positioningRectSelector = mkSelector "positioningRect"

-- | @Selector@ for @setPositioningRect:@
setPositioningRectSelector :: Selector '[NSRect] ()
setPositioningRectSelector = mkSelector "setPositioningRect:"

-- | @Selector@ for @hasFullSizeContent@
hasFullSizeContentSelector :: Selector '[] Bool
hasFullSizeContentSelector = mkSelector "hasFullSizeContent"

-- | @Selector@ for @setHasFullSizeContent:@
setHasFullSizeContentSelector :: Selector '[Bool] ()
setHasFullSizeContentSelector = mkSelector "setHasFullSizeContent:"

