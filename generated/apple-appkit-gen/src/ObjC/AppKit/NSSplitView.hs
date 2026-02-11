{-# LANGUAGE PatternSynonyms #-}
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
  , drawDividerInRectSelector
  , adjustSubviewsSelector
  , isSubviewCollapsedSelector
  , minPossiblePositionOfDividerAtIndexSelector
  , maxPossiblePositionOfDividerAtIndexSelector
  , setPosition_ofDividerAtIndexSelector
  , holdingPriorityForSubviewAtIndexSelector
  , setHoldingPriority_forSubviewAtIndexSelector
  , setIsPaneSplitterSelector
  , isPaneSplitterSelector
  , addArrangedSubviewSelector
  , insertArrangedSubview_atIndexSelector
  , removeArrangedSubviewSelector
  , verticalSelector
  , setVerticalSelector
  , dividerStyleSelector
  , setDividerStyleSelector
  , autosaveNameSelector
  , setAutosaveNameSelector
  , delegateSelector
  , setDelegateSelector
  , dividerColorSelector
  , dividerThicknessSelector
  , arrangesAllSubviewsSelector
  , setArrangesAllSubviewsSelector
  , arrangedSubviewsSelector

  -- * Enum types
  , NSSplitViewDividerStyle(NSSplitViewDividerStyle)
  , pattern NSSplitViewDividerStyleThick
  , pattern NSSplitViewDividerStyleThin
  , pattern NSSplitViewDividerStylePaneSplitter

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
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- drawDividerInRect:@
drawDividerInRect :: IsNSSplitView nsSplitView => nsSplitView -> NSRect -> IO ()
drawDividerInRect nsSplitView  rect =
    sendMsg nsSplitView (mkSelector "drawDividerInRect:") retVoid [argNSRect rect]

-- | @- adjustSubviews@
adjustSubviews :: IsNSSplitView nsSplitView => nsSplitView -> IO ()
adjustSubviews nsSplitView  =
    sendMsg nsSplitView (mkSelector "adjustSubviews") retVoid []

-- | @- isSubviewCollapsed:@
isSubviewCollapsed :: (IsNSSplitView nsSplitView, IsNSView subview) => nsSplitView -> subview -> IO Bool
isSubviewCollapsed nsSplitView  subview =
  withObjCPtr subview $ \raw_subview ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSplitView (mkSelector "isSubviewCollapsed:") retCULong [argPtr (castPtr raw_subview :: Ptr ())]

-- | @- minPossiblePositionOfDividerAtIndex:@
minPossiblePositionOfDividerAtIndex :: IsNSSplitView nsSplitView => nsSplitView -> CLong -> IO CDouble
minPossiblePositionOfDividerAtIndex nsSplitView  dividerIndex =
    sendMsg nsSplitView (mkSelector "minPossiblePositionOfDividerAtIndex:") retCDouble [argCLong dividerIndex]

-- | @- maxPossiblePositionOfDividerAtIndex:@
maxPossiblePositionOfDividerAtIndex :: IsNSSplitView nsSplitView => nsSplitView -> CLong -> IO CDouble
maxPossiblePositionOfDividerAtIndex nsSplitView  dividerIndex =
    sendMsg nsSplitView (mkSelector "maxPossiblePositionOfDividerAtIndex:") retCDouble [argCLong dividerIndex]

-- | @- setPosition:ofDividerAtIndex:@
setPosition_ofDividerAtIndex :: IsNSSplitView nsSplitView => nsSplitView -> CDouble -> CLong -> IO ()
setPosition_ofDividerAtIndex nsSplitView  position dividerIndex =
    sendMsg nsSplitView (mkSelector "setPosition:ofDividerAtIndex:") retVoid [argCDouble position, argCLong dividerIndex]

-- | @- holdingPriorityForSubviewAtIndex:@
holdingPriorityForSubviewAtIndex :: IsNSSplitView nsSplitView => nsSplitView -> CLong -> IO CFloat
holdingPriorityForSubviewAtIndex nsSplitView  subviewIndex =
    sendMsg nsSplitView (mkSelector "holdingPriorityForSubviewAtIndex:") retCFloat [argCLong subviewIndex]

-- | @- setHoldingPriority:forSubviewAtIndex:@
setHoldingPriority_forSubviewAtIndex :: IsNSSplitView nsSplitView => nsSplitView -> CFloat -> CLong -> IO ()
setHoldingPriority_forSubviewAtIndex nsSplitView  priority subviewIndex =
    sendMsg nsSplitView (mkSelector "setHoldingPriority:forSubviewAtIndex:") retVoid [argCFloat priority, argCLong subviewIndex]

-- | @- setIsPaneSplitter:@
setIsPaneSplitter :: IsNSSplitView nsSplitView => nsSplitView -> Bool -> IO ()
setIsPaneSplitter nsSplitView  flag =
    sendMsg nsSplitView (mkSelector "setIsPaneSplitter:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- isPaneSplitter@
isPaneSplitter :: IsNSSplitView nsSplitView => nsSplitView -> IO Bool
isPaneSplitter nsSplitView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSplitView (mkSelector "isPaneSplitter") retCULong []

-- | Adds a view as arranged split pane. If the view is not a subview of the receiver, it will be added as one.
--
-- ObjC selector: @- addArrangedSubview:@
addArrangedSubview :: (IsNSSplitView nsSplitView, IsNSView view) => nsSplitView -> view -> IO ()
addArrangedSubview nsSplitView  view =
  withObjCPtr view $ \raw_view ->
      sendMsg nsSplitView (mkSelector "addArrangedSubview:") retVoid [argPtr (castPtr raw_view :: Ptr ())]

-- | Adds a view as an arranged split pane list at the specific index. If the view is already an arranged split view, it will move the view the specified index (but not move the subview index). If the view is not a subview of the receiver, it will be added as one (not necessarily at the same index).
--
-- ObjC selector: @- insertArrangedSubview:atIndex:@
insertArrangedSubview_atIndex :: (IsNSSplitView nsSplitView, IsNSView view) => nsSplitView -> view -> CLong -> IO ()
insertArrangedSubview_atIndex nsSplitView  view index =
  withObjCPtr view $ \raw_view ->
      sendMsg nsSplitView (mkSelector "insertArrangedSubview:atIndex:") retVoid [argPtr (castPtr raw_view :: Ptr ()), argCLong index]

-- | Removes a view as arranged split pane. If @-arrangesAllSubviews@ is set to NO, this does not remove the view as a subview. Removing the view as a subview (either by -[view removeFromSuperview] or setting the receiver's subviews) will automatically remove it as an arranged subview.
--
-- ObjC selector: @- removeArrangedSubview:@
removeArrangedSubview :: (IsNSSplitView nsSplitView, IsNSView view) => nsSplitView -> view -> IO ()
removeArrangedSubview nsSplitView  view =
  withObjCPtr view $ \raw_view ->
      sendMsg nsSplitView (mkSelector "removeArrangedSubview:") retVoid [argPtr (castPtr raw_view :: Ptr ())]

-- | @- vertical@
vertical :: IsNSSplitView nsSplitView => nsSplitView -> IO Bool
vertical nsSplitView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSplitView (mkSelector "vertical") retCULong []

-- | @- setVertical:@
setVertical :: IsNSSplitView nsSplitView => nsSplitView -> Bool -> IO ()
setVertical nsSplitView  value =
    sendMsg nsSplitView (mkSelector "setVertical:") retVoid [argCULong (if value then 1 else 0)]

-- | @- dividerStyle@
dividerStyle :: IsNSSplitView nsSplitView => nsSplitView -> IO NSSplitViewDividerStyle
dividerStyle nsSplitView  =
    fmap (coerce :: CLong -> NSSplitViewDividerStyle) $ sendMsg nsSplitView (mkSelector "dividerStyle") retCLong []

-- | @- setDividerStyle:@
setDividerStyle :: IsNSSplitView nsSplitView => nsSplitView -> NSSplitViewDividerStyle -> IO ()
setDividerStyle nsSplitView  value =
    sendMsg nsSplitView (mkSelector "setDividerStyle:") retVoid [argCLong (coerce value)]

-- | @- autosaveName@
autosaveName :: IsNSSplitView nsSplitView => nsSplitView -> IO (Id NSString)
autosaveName nsSplitView  =
    sendMsg nsSplitView (mkSelector "autosaveName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAutosaveName:@
setAutosaveName :: (IsNSSplitView nsSplitView, IsNSString value) => nsSplitView -> value -> IO ()
setAutosaveName nsSplitView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSplitView (mkSelector "setAutosaveName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- delegate@
delegate :: IsNSSplitView nsSplitView => nsSplitView -> IO RawId
delegate nsSplitView  =
    fmap (RawId . castPtr) $ sendMsg nsSplitView (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSSplitView nsSplitView => nsSplitView -> RawId -> IO ()
setDelegate nsSplitView  value =
    sendMsg nsSplitView (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- dividerColor@
dividerColor :: IsNSSplitView nsSplitView => nsSplitView -> IO (Id NSColor)
dividerColor nsSplitView  =
    sendMsg nsSplitView (mkSelector "dividerColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dividerThickness@
dividerThickness :: IsNSSplitView nsSplitView => nsSplitView -> IO CDouble
dividerThickness nsSplitView  =
    sendMsg nsSplitView (mkSelector "dividerThickness") retCDouble []

-- | Whether or not all subviews will be added as arranged views. When NO, a subview must be explicitly added as an arrangedSubview if the view should be arranged as a split pane. When YES, @-arrangedSubviews@ always be identical to @-subviews.@ Defaults to YES. Setting this from YES to NO will leave all existing subviews as @-arrangedSubviews.@ Setting this from NO to YES will cause @-arrangedSubviews@ to become the value of @-subviews.@
--
-- ObjC selector: @- arrangesAllSubviews@
arrangesAllSubviews :: IsNSSplitView nsSplitView => nsSplitView -> IO Bool
arrangesAllSubviews nsSplitView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSplitView (mkSelector "arrangesAllSubviews") retCULong []

-- | Whether or not all subviews will be added as arranged views. When NO, a subview must be explicitly added as an arrangedSubview if the view should be arranged as a split pane. When YES, @-arrangedSubviews@ always be identical to @-subviews.@ Defaults to YES. Setting this from YES to NO will leave all existing subviews as @-arrangedSubviews.@ Setting this from NO to YES will cause @-arrangedSubviews@ to become the value of @-subviews.@
--
-- ObjC selector: @- setArrangesAllSubviews:@
setArrangesAllSubviews :: IsNSSplitView nsSplitView => nsSplitView -> Bool -> IO ()
setArrangesAllSubviews nsSplitView  value =
    sendMsg nsSplitView (mkSelector "setArrangesAllSubviews:") retVoid [argCULong (if value then 1 else 0)]

-- | The list of views that are arranged as split panes in the receiver. They are a subset of @-subviews,@ with potential difference in ordering. If @-arrangesAllSubviews@ is YES, then @-arrangedSubviews@ is identical to @-subviews.@
--
-- ObjC selector: @- arrangedSubviews@
arrangedSubviews :: IsNSSplitView nsSplitView => nsSplitView -> IO (Id NSArray)
arrangedSubviews nsSplitView  =
    sendMsg nsSplitView (mkSelector "arrangedSubviews") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @drawDividerInRect:@
drawDividerInRectSelector :: Selector
drawDividerInRectSelector = mkSelector "drawDividerInRect:"

-- | @Selector@ for @adjustSubviews@
adjustSubviewsSelector :: Selector
adjustSubviewsSelector = mkSelector "adjustSubviews"

-- | @Selector@ for @isSubviewCollapsed:@
isSubviewCollapsedSelector :: Selector
isSubviewCollapsedSelector = mkSelector "isSubviewCollapsed:"

-- | @Selector@ for @minPossiblePositionOfDividerAtIndex:@
minPossiblePositionOfDividerAtIndexSelector :: Selector
minPossiblePositionOfDividerAtIndexSelector = mkSelector "minPossiblePositionOfDividerAtIndex:"

-- | @Selector@ for @maxPossiblePositionOfDividerAtIndex:@
maxPossiblePositionOfDividerAtIndexSelector :: Selector
maxPossiblePositionOfDividerAtIndexSelector = mkSelector "maxPossiblePositionOfDividerAtIndex:"

-- | @Selector@ for @setPosition:ofDividerAtIndex:@
setPosition_ofDividerAtIndexSelector :: Selector
setPosition_ofDividerAtIndexSelector = mkSelector "setPosition:ofDividerAtIndex:"

-- | @Selector@ for @holdingPriorityForSubviewAtIndex:@
holdingPriorityForSubviewAtIndexSelector :: Selector
holdingPriorityForSubviewAtIndexSelector = mkSelector "holdingPriorityForSubviewAtIndex:"

-- | @Selector@ for @setHoldingPriority:forSubviewAtIndex:@
setHoldingPriority_forSubviewAtIndexSelector :: Selector
setHoldingPriority_forSubviewAtIndexSelector = mkSelector "setHoldingPriority:forSubviewAtIndex:"

-- | @Selector@ for @setIsPaneSplitter:@
setIsPaneSplitterSelector :: Selector
setIsPaneSplitterSelector = mkSelector "setIsPaneSplitter:"

-- | @Selector@ for @isPaneSplitter@
isPaneSplitterSelector :: Selector
isPaneSplitterSelector = mkSelector "isPaneSplitter"

-- | @Selector@ for @addArrangedSubview:@
addArrangedSubviewSelector :: Selector
addArrangedSubviewSelector = mkSelector "addArrangedSubview:"

-- | @Selector@ for @insertArrangedSubview:atIndex:@
insertArrangedSubview_atIndexSelector :: Selector
insertArrangedSubview_atIndexSelector = mkSelector "insertArrangedSubview:atIndex:"

-- | @Selector@ for @removeArrangedSubview:@
removeArrangedSubviewSelector :: Selector
removeArrangedSubviewSelector = mkSelector "removeArrangedSubview:"

-- | @Selector@ for @vertical@
verticalSelector :: Selector
verticalSelector = mkSelector "vertical"

-- | @Selector@ for @setVertical:@
setVerticalSelector :: Selector
setVerticalSelector = mkSelector "setVertical:"

-- | @Selector@ for @dividerStyle@
dividerStyleSelector :: Selector
dividerStyleSelector = mkSelector "dividerStyle"

-- | @Selector@ for @setDividerStyle:@
setDividerStyleSelector :: Selector
setDividerStyleSelector = mkSelector "setDividerStyle:"

-- | @Selector@ for @autosaveName@
autosaveNameSelector :: Selector
autosaveNameSelector = mkSelector "autosaveName"

-- | @Selector@ for @setAutosaveName:@
setAutosaveNameSelector :: Selector
setAutosaveNameSelector = mkSelector "setAutosaveName:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @dividerColor@
dividerColorSelector :: Selector
dividerColorSelector = mkSelector "dividerColor"

-- | @Selector@ for @dividerThickness@
dividerThicknessSelector :: Selector
dividerThicknessSelector = mkSelector "dividerThickness"

-- | @Selector@ for @arrangesAllSubviews@
arrangesAllSubviewsSelector :: Selector
arrangesAllSubviewsSelector = mkSelector "arrangesAllSubviews"

-- | @Selector@ for @setArrangesAllSubviews:@
setArrangesAllSubviewsSelector :: Selector
setArrangesAllSubviewsSelector = mkSelector "setArrangesAllSubviews:"

-- | @Selector@ for @arrangedSubviews@
arrangedSubviewsSelector :: Selector
arrangedSubviewsSelector = mkSelector "arrangedSubviews"

