{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSplitView@.
module ObjC.AppKit.NSSplitView
  ( NSSplitView
  , IsNSSplitView(..)
  , drawDividerInRect
  , adjustSubviews
  , isSubviewCollapsed
  , minPossiblePositionOfDividerAtIndex
  , maxPossiblePositionOfDividerAtIndex
  , setPosition_ofDividerAtIndex
  , holdingPriorityForSubviewAtIndex
  , setHoldingPriority_forSubviewAtIndex
  , setIsPaneSplitter
  , isPaneSplitter
  , addArrangedSubview
  , insertArrangedSubview_atIndex
  , removeArrangedSubview
  , vertical
  , setVertical
  , dividerStyle
  , setDividerStyle
  , autosaveName
  , setAutosaveName
  , delegate
  , setDelegate
  , dividerColor
  , dividerThickness
  , arrangesAllSubviews
  , setArrangesAllSubviews
  , arrangedSubviews
  , addArrangedSubviewSelector
  , adjustSubviewsSelector
  , arrangedSubviewsSelector
  , arrangesAllSubviewsSelector
  , autosaveNameSelector
  , delegateSelector
  , dividerColorSelector
  , dividerStyleSelector
  , dividerThicknessSelector
  , drawDividerInRectSelector
  , holdingPriorityForSubviewAtIndexSelector
  , insertArrangedSubview_atIndexSelector
  , isPaneSplitterSelector
  , isSubviewCollapsedSelector
  , maxPossiblePositionOfDividerAtIndexSelector
  , minPossiblePositionOfDividerAtIndexSelector
  , removeArrangedSubviewSelector
  , setArrangesAllSubviewsSelector
  , setAutosaveNameSelector
  , setDelegateSelector
  , setDividerStyleSelector
  , setHoldingPriority_forSubviewAtIndexSelector
  , setIsPaneSplitterSelector
  , setPosition_ofDividerAtIndexSelector
  , setVerticalSelector
  , verticalSelector

  -- * Enum types
  , NSSplitViewDividerStyle(NSSplitViewDividerStyle)
  , pattern NSSplitViewDividerStyleThick
  , pattern NSSplitViewDividerStyleThin
  , pattern NSSplitViewDividerStylePaneSplitter

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
import ObjC.Foundation.Internal.Classes

-- | @- drawDividerInRect:@
drawDividerInRect :: IsNSSplitView nsSplitView => nsSplitView -> NSRect -> IO ()
drawDividerInRect nsSplitView rect =
  sendMessage nsSplitView drawDividerInRectSelector rect

-- | @- adjustSubviews@
adjustSubviews :: IsNSSplitView nsSplitView => nsSplitView -> IO ()
adjustSubviews nsSplitView =
  sendMessage nsSplitView adjustSubviewsSelector

-- | @- isSubviewCollapsed:@
isSubviewCollapsed :: (IsNSSplitView nsSplitView, IsNSView subview) => nsSplitView -> subview -> IO Bool
isSubviewCollapsed nsSplitView subview =
  sendMessage nsSplitView isSubviewCollapsedSelector (toNSView subview)

-- | @- minPossiblePositionOfDividerAtIndex:@
minPossiblePositionOfDividerAtIndex :: IsNSSplitView nsSplitView => nsSplitView -> CLong -> IO CDouble
minPossiblePositionOfDividerAtIndex nsSplitView dividerIndex =
  sendMessage nsSplitView minPossiblePositionOfDividerAtIndexSelector dividerIndex

-- | @- maxPossiblePositionOfDividerAtIndex:@
maxPossiblePositionOfDividerAtIndex :: IsNSSplitView nsSplitView => nsSplitView -> CLong -> IO CDouble
maxPossiblePositionOfDividerAtIndex nsSplitView dividerIndex =
  sendMessage nsSplitView maxPossiblePositionOfDividerAtIndexSelector dividerIndex

-- | @- setPosition:ofDividerAtIndex:@
setPosition_ofDividerAtIndex :: IsNSSplitView nsSplitView => nsSplitView -> CDouble -> CLong -> IO ()
setPosition_ofDividerAtIndex nsSplitView position dividerIndex =
  sendMessage nsSplitView setPosition_ofDividerAtIndexSelector position dividerIndex

-- | @- holdingPriorityForSubviewAtIndex:@
holdingPriorityForSubviewAtIndex :: IsNSSplitView nsSplitView => nsSplitView -> CLong -> IO CFloat
holdingPriorityForSubviewAtIndex nsSplitView subviewIndex =
  sendMessage nsSplitView holdingPriorityForSubviewAtIndexSelector subviewIndex

-- | @- setHoldingPriority:forSubviewAtIndex:@
setHoldingPriority_forSubviewAtIndex :: IsNSSplitView nsSplitView => nsSplitView -> CFloat -> CLong -> IO ()
setHoldingPriority_forSubviewAtIndex nsSplitView priority subviewIndex =
  sendMessage nsSplitView setHoldingPriority_forSubviewAtIndexSelector priority subviewIndex

-- | @- setIsPaneSplitter:@
setIsPaneSplitter :: IsNSSplitView nsSplitView => nsSplitView -> Bool -> IO ()
setIsPaneSplitter nsSplitView flag =
  sendMessage nsSplitView setIsPaneSplitterSelector flag

-- | @- isPaneSplitter@
isPaneSplitter :: IsNSSplitView nsSplitView => nsSplitView -> IO Bool
isPaneSplitter nsSplitView =
  sendMessage nsSplitView isPaneSplitterSelector

-- | Adds a view as arranged split pane. If the view is not a subview of the receiver, it will be added as one.
--
-- ObjC selector: @- addArrangedSubview:@
addArrangedSubview :: (IsNSSplitView nsSplitView, IsNSView view) => nsSplitView -> view -> IO ()
addArrangedSubview nsSplitView view =
  sendMessage nsSplitView addArrangedSubviewSelector (toNSView view)

-- | Adds a view as an arranged split pane list at the specific index. If the view is already an arranged split view, it will move the view the specified index (but not move the subview index). If the view is not a subview of the receiver, it will be added as one (not necessarily at the same index).
--
-- ObjC selector: @- insertArrangedSubview:atIndex:@
insertArrangedSubview_atIndex :: (IsNSSplitView nsSplitView, IsNSView view) => nsSplitView -> view -> CLong -> IO ()
insertArrangedSubview_atIndex nsSplitView view index =
  sendMessage nsSplitView insertArrangedSubview_atIndexSelector (toNSView view) index

-- | Removes a view as arranged split pane. If @-arrangesAllSubviews@ is set to NO, this does not remove the view as a subview. Removing the view as a subview (either by -[view removeFromSuperview] or setting the receiver's subviews) will automatically remove it as an arranged subview.
--
-- ObjC selector: @- removeArrangedSubview:@
removeArrangedSubview :: (IsNSSplitView nsSplitView, IsNSView view) => nsSplitView -> view -> IO ()
removeArrangedSubview nsSplitView view =
  sendMessage nsSplitView removeArrangedSubviewSelector (toNSView view)

-- | @- vertical@
vertical :: IsNSSplitView nsSplitView => nsSplitView -> IO Bool
vertical nsSplitView =
  sendMessage nsSplitView verticalSelector

-- | @- setVertical:@
setVertical :: IsNSSplitView nsSplitView => nsSplitView -> Bool -> IO ()
setVertical nsSplitView value =
  sendMessage nsSplitView setVerticalSelector value

-- | @- dividerStyle@
dividerStyle :: IsNSSplitView nsSplitView => nsSplitView -> IO NSSplitViewDividerStyle
dividerStyle nsSplitView =
  sendMessage nsSplitView dividerStyleSelector

-- | @- setDividerStyle:@
setDividerStyle :: IsNSSplitView nsSplitView => nsSplitView -> NSSplitViewDividerStyle -> IO ()
setDividerStyle nsSplitView value =
  sendMessage nsSplitView setDividerStyleSelector value

-- | @- autosaveName@
autosaveName :: IsNSSplitView nsSplitView => nsSplitView -> IO (Id NSString)
autosaveName nsSplitView =
  sendMessage nsSplitView autosaveNameSelector

-- | @- setAutosaveName:@
setAutosaveName :: (IsNSSplitView nsSplitView, IsNSString value) => nsSplitView -> value -> IO ()
setAutosaveName nsSplitView value =
  sendMessage nsSplitView setAutosaveNameSelector (toNSString value)

-- | @- delegate@
delegate :: IsNSSplitView nsSplitView => nsSplitView -> IO RawId
delegate nsSplitView =
  sendMessage nsSplitView delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSSplitView nsSplitView => nsSplitView -> RawId -> IO ()
setDelegate nsSplitView value =
  sendMessage nsSplitView setDelegateSelector value

-- | @- dividerColor@
dividerColor :: IsNSSplitView nsSplitView => nsSplitView -> IO (Id NSColor)
dividerColor nsSplitView =
  sendMessage nsSplitView dividerColorSelector

-- | @- dividerThickness@
dividerThickness :: IsNSSplitView nsSplitView => nsSplitView -> IO CDouble
dividerThickness nsSplitView =
  sendMessage nsSplitView dividerThicknessSelector

-- | Whether or not all subviews will be added as arranged views. When NO, a subview must be explicitly added as an arrangedSubview if the view should be arranged as a split pane. When YES, @-arrangedSubviews@ always be identical to @-subviews.@ Defaults to YES. Setting this from YES to NO will leave all existing subviews as @-arrangedSubviews.@ Setting this from NO to YES will cause @-arrangedSubviews@ to become the value of @-subviews.@
--
-- ObjC selector: @- arrangesAllSubviews@
arrangesAllSubviews :: IsNSSplitView nsSplitView => nsSplitView -> IO Bool
arrangesAllSubviews nsSplitView =
  sendMessage nsSplitView arrangesAllSubviewsSelector

-- | Whether or not all subviews will be added as arranged views. When NO, a subview must be explicitly added as an arrangedSubview if the view should be arranged as a split pane. When YES, @-arrangedSubviews@ always be identical to @-subviews.@ Defaults to YES. Setting this from YES to NO will leave all existing subviews as @-arrangedSubviews.@ Setting this from NO to YES will cause @-arrangedSubviews@ to become the value of @-subviews.@
--
-- ObjC selector: @- setArrangesAllSubviews:@
setArrangesAllSubviews :: IsNSSplitView nsSplitView => nsSplitView -> Bool -> IO ()
setArrangesAllSubviews nsSplitView value =
  sendMessage nsSplitView setArrangesAllSubviewsSelector value

-- | The list of views that are arranged as split panes in the receiver. They are a subset of @-subviews,@ with potential difference in ordering. If @-arrangesAllSubviews@ is YES, then @-arrangedSubviews@ is identical to @-subviews.@
--
-- ObjC selector: @- arrangedSubviews@
arrangedSubviews :: IsNSSplitView nsSplitView => nsSplitView -> IO (Id NSArray)
arrangedSubviews nsSplitView =
  sendMessage nsSplitView arrangedSubviewsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @drawDividerInRect:@
drawDividerInRectSelector :: Selector '[NSRect] ()
drawDividerInRectSelector = mkSelector "drawDividerInRect:"

-- | @Selector@ for @adjustSubviews@
adjustSubviewsSelector :: Selector '[] ()
adjustSubviewsSelector = mkSelector "adjustSubviews"

-- | @Selector@ for @isSubviewCollapsed:@
isSubviewCollapsedSelector :: Selector '[Id NSView] Bool
isSubviewCollapsedSelector = mkSelector "isSubviewCollapsed:"

-- | @Selector@ for @minPossiblePositionOfDividerAtIndex:@
minPossiblePositionOfDividerAtIndexSelector :: Selector '[CLong] CDouble
minPossiblePositionOfDividerAtIndexSelector = mkSelector "minPossiblePositionOfDividerAtIndex:"

-- | @Selector@ for @maxPossiblePositionOfDividerAtIndex:@
maxPossiblePositionOfDividerAtIndexSelector :: Selector '[CLong] CDouble
maxPossiblePositionOfDividerAtIndexSelector = mkSelector "maxPossiblePositionOfDividerAtIndex:"

-- | @Selector@ for @setPosition:ofDividerAtIndex:@
setPosition_ofDividerAtIndexSelector :: Selector '[CDouble, CLong] ()
setPosition_ofDividerAtIndexSelector = mkSelector "setPosition:ofDividerAtIndex:"

-- | @Selector@ for @holdingPriorityForSubviewAtIndex:@
holdingPriorityForSubviewAtIndexSelector :: Selector '[CLong] CFloat
holdingPriorityForSubviewAtIndexSelector = mkSelector "holdingPriorityForSubviewAtIndex:"

-- | @Selector@ for @setHoldingPriority:forSubviewAtIndex:@
setHoldingPriority_forSubviewAtIndexSelector :: Selector '[CFloat, CLong] ()
setHoldingPriority_forSubviewAtIndexSelector = mkSelector "setHoldingPriority:forSubviewAtIndex:"

-- | @Selector@ for @setIsPaneSplitter:@
setIsPaneSplitterSelector :: Selector '[Bool] ()
setIsPaneSplitterSelector = mkSelector "setIsPaneSplitter:"

-- | @Selector@ for @isPaneSplitter@
isPaneSplitterSelector :: Selector '[] Bool
isPaneSplitterSelector = mkSelector "isPaneSplitter"

-- | @Selector@ for @addArrangedSubview:@
addArrangedSubviewSelector :: Selector '[Id NSView] ()
addArrangedSubviewSelector = mkSelector "addArrangedSubview:"

-- | @Selector@ for @insertArrangedSubview:atIndex:@
insertArrangedSubview_atIndexSelector :: Selector '[Id NSView, CLong] ()
insertArrangedSubview_atIndexSelector = mkSelector "insertArrangedSubview:atIndex:"

-- | @Selector@ for @removeArrangedSubview:@
removeArrangedSubviewSelector :: Selector '[Id NSView] ()
removeArrangedSubviewSelector = mkSelector "removeArrangedSubview:"

-- | @Selector@ for @vertical@
verticalSelector :: Selector '[] Bool
verticalSelector = mkSelector "vertical"

-- | @Selector@ for @setVertical:@
setVerticalSelector :: Selector '[Bool] ()
setVerticalSelector = mkSelector "setVertical:"

-- | @Selector@ for @dividerStyle@
dividerStyleSelector :: Selector '[] NSSplitViewDividerStyle
dividerStyleSelector = mkSelector "dividerStyle"

-- | @Selector@ for @setDividerStyle:@
setDividerStyleSelector :: Selector '[NSSplitViewDividerStyle] ()
setDividerStyleSelector = mkSelector "setDividerStyle:"

-- | @Selector@ for @autosaveName@
autosaveNameSelector :: Selector '[] (Id NSString)
autosaveNameSelector = mkSelector "autosaveName"

-- | @Selector@ for @setAutosaveName:@
setAutosaveNameSelector :: Selector '[Id NSString] ()
setAutosaveNameSelector = mkSelector "setAutosaveName:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @dividerColor@
dividerColorSelector :: Selector '[] (Id NSColor)
dividerColorSelector = mkSelector "dividerColor"

-- | @Selector@ for @dividerThickness@
dividerThicknessSelector :: Selector '[] CDouble
dividerThicknessSelector = mkSelector "dividerThickness"

-- | @Selector@ for @arrangesAllSubviews@
arrangesAllSubviewsSelector :: Selector '[] Bool
arrangesAllSubviewsSelector = mkSelector "arrangesAllSubviews"

-- | @Selector@ for @setArrangesAllSubviews:@
setArrangesAllSubviewsSelector :: Selector '[Bool] ()
setArrangesAllSubviewsSelector = mkSelector "setArrangesAllSubviews:"

-- | @Selector@ for @arrangedSubviews@
arrangedSubviewsSelector :: Selector '[] (Id NSArray)
arrangedSubviewsSelector = mkSelector "arrangedSubviews"

