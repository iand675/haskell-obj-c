{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSOutlineView@.
module ObjC.AppKit.NSOutlineView
  ( NSOutlineView
  , IsNSOutlineView(..)
  , isExpandable
  , numberOfChildrenOfItem
  , child_ofItem
  , expandItem_expandChildren
  , expandItem
  , collapseItem_collapseChildren
  , collapseItem
  , reloadItem_reloadChildren
  , reloadItem
  , parentForItem
  , childIndexForItem
  , itemAtRow
  , rowForItem
  , levelForItem
  , levelForRow
  , isItemExpanded
  , frameOfOutlineCellAtRow
  , setDropItem_dropChildIndex
  , shouldCollapseAutoExpandedItemsForDeposited
  , insertItemsAtIndexes_inParent_withAnimation
  , removeItemsAtIndexes_inParent_withAnimation
  , moveItemAtIndex_inParent_toIndex_inParent
  , insertRowsAtIndexes_withAnimation
  , removeRowsAtIndexes_withAnimation
  , moveRowAtIndex_toIndex
  , delegate
  , setDelegate
  , dataSource
  , setDataSource
  , outlineTableColumn
  , setOutlineTableColumn
  , indentationPerLevel
  , setIndentationPerLevel
  , indentationMarkerFollowsCell
  , setIndentationMarkerFollowsCell
  , autoresizesOutlineColumn
  , setAutoresizesOutlineColumn
  , autosaveExpandedItems
  , setAutosaveExpandedItems
  , userInterfaceLayoutDirection
  , setUserInterfaceLayoutDirection
  , stronglyReferencesItems
  , setStronglyReferencesItems
  , isExpandableSelector
  , numberOfChildrenOfItemSelector
  , child_ofItemSelector
  , expandItem_expandChildrenSelector
  , expandItemSelector
  , collapseItem_collapseChildrenSelector
  , collapseItemSelector
  , reloadItem_reloadChildrenSelector
  , reloadItemSelector
  , parentForItemSelector
  , childIndexForItemSelector
  , itemAtRowSelector
  , rowForItemSelector
  , levelForItemSelector
  , levelForRowSelector
  , isItemExpandedSelector
  , frameOfOutlineCellAtRowSelector
  , setDropItem_dropChildIndexSelector
  , shouldCollapseAutoExpandedItemsForDepositedSelector
  , insertItemsAtIndexes_inParent_withAnimationSelector
  , removeItemsAtIndexes_inParent_withAnimationSelector
  , moveItemAtIndex_inParent_toIndex_inParentSelector
  , insertRowsAtIndexes_withAnimationSelector
  , removeRowsAtIndexes_withAnimationSelector
  , moveRowAtIndex_toIndexSelector
  , delegateSelector
  , setDelegateSelector
  , dataSourceSelector
  , setDataSourceSelector
  , outlineTableColumnSelector
  , setOutlineTableColumnSelector
  , indentationPerLevelSelector
  , setIndentationPerLevelSelector
  , indentationMarkerFollowsCellSelector
  , setIndentationMarkerFollowsCellSelector
  , autoresizesOutlineColumnSelector
  , setAutoresizesOutlineColumnSelector
  , autosaveExpandedItemsSelector
  , setAutosaveExpandedItemsSelector
  , userInterfaceLayoutDirectionSelector
  , setUserInterfaceLayoutDirectionSelector
  , stronglyReferencesItemsSelector
  , setStronglyReferencesItemsSelector

  -- * Enum types
  , NSTableViewAnimationOptions(NSTableViewAnimationOptions)
  , pattern NSTableViewAnimationEffectNone
  , pattern NSTableViewAnimationEffectFade
  , pattern NSTableViewAnimationEffectGap
  , pattern NSTableViewAnimationSlideUp
  , pattern NSTableViewAnimationSlideDown
  , pattern NSTableViewAnimationSlideLeft
  , pattern NSTableViewAnimationSlideRight
  , NSUserInterfaceLayoutDirection(NSUserInterfaceLayoutDirection)
  , pattern NSUserInterfaceLayoutDirectionLeftToRight
  , pattern NSUserInterfaceLayoutDirectionRightToLeft

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
import ObjC.Foundation.Internal.Classes

-- | @- isExpandable:@
isExpandable :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO Bool
isExpandable nsOutlineView  item =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOutlineView (mkSelector "isExpandable:") retCULong [argPtr (castPtr (unRawId item) :: Ptr ())]

-- | @- numberOfChildrenOfItem:@
numberOfChildrenOfItem :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO CLong
numberOfChildrenOfItem nsOutlineView  item =
    sendMsg nsOutlineView (mkSelector "numberOfChildrenOfItem:") retCLong [argPtr (castPtr (unRawId item) :: Ptr ())]

-- | @- child:ofItem:@
child_ofItem :: IsNSOutlineView nsOutlineView => nsOutlineView -> CLong -> RawId -> IO RawId
child_ofItem nsOutlineView  index item =
    fmap (RawId . castPtr) $ sendMsg nsOutlineView (mkSelector "child:ofItem:") (retPtr retVoid) [argCLong index, argPtr (castPtr (unRawId item) :: Ptr ())]

-- | @- expandItem:expandChildren:@
expandItem_expandChildren :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> Bool -> IO ()
expandItem_expandChildren nsOutlineView  item expandChildren =
    sendMsg nsOutlineView (mkSelector "expandItem:expandChildren:") retVoid [argPtr (castPtr (unRawId item) :: Ptr ()), argCULong (if expandChildren then 1 else 0)]

-- | @- expandItem:@
expandItem :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO ()
expandItem nsOutlineView  item =
    sendMsg nsOutlineView (mkSelector "expandItem:") retVoid [argPtr (castPtr (unRawId item) :: Ptr ())]

-- | @- collapseItem:collapseChildren:@
collapseItem_collapseChildren :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> Bool -> IO ()
collapseItem_collapseChildren nsOutlineView  item collapseChildren =
    sendMsg nsOutlineView (mkSelector "collapseItem:collapseChildren:") retVoid [argPtr (castPtr (unRawId item) :: Ptr ()), argCULong (if collapseChildren then 1 else 0)]

-- | @- collapseItem:@
collapseItem :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO ()
collapseItem nsOutlineView  item =
    sendMsg nsOutlineView (mkSelector "collapseItem:") retVoid [argPtr (castPtr (unRawId item) :: Ptr ())]

-- | @- reloadItem:reloadChildren:@
reloadItem_reloadChildren :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> Bool -> IO ()
reloadItem_reloadChildren nsOutlineView  item reloadChildren =
    sendMsg nsOutlineView (mkSelector "reloadItem:reloadChildren:") retVoid [argPtr (castPtr (unRawId item) :: Ptr ()), argCULong (if reloadChildren then 1 else 0)]

-- | @- reloadItem:@
reloadItem :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO ()
reloadItem nsOutlineView  item =
    sendMsg nsOutlineView (mkSelector "reloadItem:") retVoid [argPtr (castPtr (unRawId item) :: Ptr ())]

-- | @- parentForItem:@
parentForItem :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO RawId
parentForItem nsOutlineView  item =
    fmap (RawId . castPtr) $ sendMsg nsOutlineView (mkSelector "parentForItem:") (retPtr retVoid) [argPtr (castPtr (unRawId item) :: Ptr ())]

-- | @- childIndexForItem:@
childIndexForItem :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO CLong
childIndexForItem nsOutlineView  item =
    sendMsg nsOutlineView (mkSelector "childIndexForItem:") retCLong [argPtr (castPtr (unRawId item) :: Ptr ())]

-- | @- itemAtRow:@
itemAtRow :: IsNSOutlineView nsOutlineView => nsOutlineView -> CLong -> IO RawId
itemAtRow nsOutlineView  row =
    fmap (RawId . castPtr) $ sendMsg nsOutlineView (mkSelector "itemAtRow:") (retPtr retVoid) [argCLong row]

-- | @- rowForItem:@
rowForItem :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO CLong
rowForItem nsOutlineView  item =
    sendMsg nsOutlineView (mkSelector "rowForItem:") retCLong [argPtr (castPtr (unRawId item) :: Ptr ())]

-- | @- levelForItem:@
levelForItem :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO CLong
levelForItem nsOutlineView  item =
    sendMsg nsOutlineView (mkSelector "levelForItem:") retCLong [argPtr (castPtr (unRawId item) :: Ptr ())]

-- | @- levelForRow:@
levelForRow :: IsNSOutlineView nsOutlineView => nsOutlineView -> CLong -> IO CLong
levelForRow nsOutlineView  row =
    sendMsg nsOutlineView (mkSelector "levelForRow:") retCLong [argCLong row]

-- | @- isItemExpanded:@
isItemExpanded :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO Bool
isItemExpanded nsOutlineView  item =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOutlineView (mkSelector "isItemExpanded:") retCULong [argPtr (castPtr (unRawId item) :: Ptr ())]

-- | @- frameOfOutlineCellAtRow:@
frameOfOutlineCellAtRow :: IsNSOutlineView nsOutlineView => nsOutlineView -> CLong -> IO NSRect
frameOfOutlineCellAtRow nsOutlineView  row =
    sendMsgStret nsOutlineView (mkSelector "frameOfOutlineCellAtRow:") retNSRect [argCLong row]

-- | @- setDropItem:dropChildIndex:@
setDropItem_dropChildIndex :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> CLong -> IO ()
setDropItem_dropChildIndex nsOutlineView  item index =
    sendMsg nsOutlineView (mkSelector "setDropItem:dropChildIndex:") retVoid [argPtr (castPtr (unRawId item) :: Ptr ()), argCLong index]

-- | @- shouldCollapseAutoExpandedItemsForDeposited:@
shouldCollapseAutoExpandedItemsForDeposited :: IsNSOutlineView nsOutlineView => nsOutlineView -> Bool -> IO Bool
shouldCollapseAutoExpandedItemsForDeposited nsOutlineView  deposited =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOutlineView (mkSelector "shouldCollapseAutoExpandedItemsForDeposited:") retCULong [argCULong (if deposited then 1 else 0)]

-- | @- insertItemsAtIndexes:inParent:withAnimation:@
insertItemsAtIndexes_inParent_withAnimation :: (IsNSOutlineView nsOutlineView, IsNSIndexSet indexes) => nsOutlineView -> indexes -> RawId -> NSTableViewAnimationOptions -> IO ()
insertItemsAtIndexes_inParent_withAnimation nsOutlineView  indexes parent animationOptions =
  withObjCPtr indexes $ \raw_indexes ->
      sendMsg nsOutlineView (mkSelector "insertItemsAtIndexes:inParent:withAnimation:") retVoid [argPtr (castPtr raw_indexes :: Ptr ()), argPtr (castPtr (unRawId parent) :: Ptr ()), argCULong (coerce animationOptions)]

-- | @- removeItemsAtIndexes:inParent:withAnimation:@
removeItemsAtIndexes_inParent_withAnimation :: (IsNSOutlineView nsOutlineView, IsNSIndexSet indexes) => nsOutlineView -> indexes -> RawId -> NSTableViewAnimationOptions -> IO ()
removeItemsAtIndexes_inParent_withAnimation nsOutlineView  indexes parent animationOptions =
  withObjCPtr indexes $ \raw_indexes ->
      sendMsg nsOutlineView (mkSelector "removeItemsAtIndexes:inParent:withAnimation:") retVoid [argPtr (castPtr raw_indexes :: Ptr ()), argPtr (castPtr (unRawId parent) :: Ptr ()), argCULong (coerce animationOptions)]

-- | @- moveItemAtIndex:inParent:toIndex:inParent:@
moveItemAtIndex_inParent_toIndex_inParent :: IsNSOutlineView nsOutlineView => nsOutlineView -> CLong -> RawId -> CLong -> RawId -> IO ()
moveItemAtIndex_inParent_toIndex_inParent nsOutlineView  fromIndex oldParent toIndex newParent =
    sendMsg nsOutlineView (mkSelector "moveItemAtIndex:inParent:toIndex:inParent:") retVoid [argCLong fromIndex, argPtr (castPtr (unRawId oldParent) :: Ptr ()), argCLong toIndex, argPtr (castPtr (unRawId newParent) :: Ptr ())]

-- | @- insertRowsAtIndexes:withAnimation:@
insertRowsAtIndexes_withAnimation :: (IsNSOutlineView nsOutlineView, IsNSIndexSet indexes) => nsOutlineView -> indexes -> NSTableViewAnimationOptions -> IO ()
insertRowsAtIndexes_withAnimation nsOutlineView  indexes animationOptions =
  withObjCPtr indexes $ \raw_indexes ->
      sendMsg nsOutlineView (mkSelector "insertRowsAtIndexes:withAnimation:") retVoid [argPtr (castPtr raw_indexes :: Ptr ()), argCULong (coerce animationOptions)]

-- | @- removeRowsAtIndexes:withAnimation:@
removeRowsAtIndexes_withAnimation :: (IsNSOutlineView nsOutlineView, IsNSIndexSet indexes) => nsOutlineView -> indexes -> NSTableViewAnimationOptions -> IO ()
removeRowsAtIndexes_withAnimation nsOutlineView  indexes animationOptions =
  withObjCPtr indexes $ \raw_indexes ->
      sendMsg nsOutlineView (mkSelector "removeRowsAtIndexes:withAnimation:") retVoid [argPtr (castPtr raw_indexes :: Ptr ()), argCULong (coerce animationOptions)]

-- | @- moveRowAtIndex:toIndex:@
moveRowAtIndex_toIndex :: IsNSOutlineView nsOutlineView => nsOutlineView -> CLong -> CLong -> IO ()
moveRowAtIndex_toIndex nsOutlineView  oldIndex newIndex =
    sendMsg nsOutlineView (mkSelector "moveRowAtIndex:toIndex:") retVoid [argCLong oldIndex, argCLong newIndex]

-- | @- delegate@
delegate :: IsNSOutlineView nsOutlineView => nsOutlineView -> IO RawId
delegate nsOutlineView  =
    fmap (RawId . castPtr) $ sendMsg nsOutlineView (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO ()
setDelegate nsOutlineView  value =
    sendMsg nsOutlineView (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- dataSource@
dataSource :: IsNSOutlineView nsOutlineView => nsOutlineView -> IO RawId
dataSource nsOutlineView  =
    fmap (RawId . castPtr) $ sendMsg nsOutlineView (mkSelector "dataSource") (retPtr retVoid) []

-- | @- setDataSource:@
setDataSource :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO ()
setDataSource nsOutlineView  value =
    sendMsg nsOutlineView (mkSelector "setDataSource:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- outlineTableColumn@
outlineTableColumn :: IsNSOutlineView nsOutlineView => nsOutlineView -> IO (Id NSTableColumn)
outlineTableColumn nsOutlineView  =
    sendMsg nsOutlineView (mkSelector "outlineTableColumn") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOutlineTableColumn:@
setOutlineTableColumn :: (IsNSOutlineView nsOutlineView, IsNSTableColumn value) => nsOutlineView -> value -> IO ()
setOutlineTableColumn nsOutlineView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsOutlineView (mkSelector "setOutlineTableColumn:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- indentationPerLevel@
indentationPerLevel :: IsNSOutlineView nsOutlineView => nsOutlineView -> IO CDouble
indentationPerLevel nsOutlineView  =
    sendMsg nsOutlineView (mkSelector "indentationPerLevel") retCDouble []

-- | @- setIndentationPerLevel:@
setIndentationPerLevel :: IsNSOutlineView nsOutlineView => nsOutlineView -> CDouble -> IO ()
setIndentationPerLevel nsOutlineView  value =
    sendMsg nsOutlineView (mkSelector "setIndentationPerLevel:") retVoid [argCDouble value]

-- | @- indentationMarkerFollowsCell@
indentationMarkerFollowsCell :: IsNSOutlineView nsOutlineView => nsOutlineView -> IO Bool
indentationMarkerFollowsCell nsOutlineView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOutlineView (mkSelector "indentationMarkerFollowsCell") retCULong []

-- | @- setIndentationMarkerFollowsCell:@
setIndentationMarkerFollowsCell :: IsNSOutlineView nsOutlineView => nsOutlineView -> Bool -> IO ()
setIndentationMarkerFollowsCell nsOutlineView  value =
    sendMsg nsOutlineView (mkSelector "setIndentationMarkerFollowsCell:") retVoid [argCULong (if value then 1 else 0)]

-- | @- autoresizesOutlineColumn@
autoresizesOutlineColumn :: IsNSOutlineView nsOutlineView => nsOutlineView -> IO Bool
autoresizesOutlineColumn nsOutlineView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOutlineView (mkSelector "autoresizesOutlineColumn") retCULong []

-- | @- setAutoresizesOutlineColumn:@
setAutoresizesOutlineColumn :: IsNSOutlineView nsOutlineView => nsOutlineView -> Bool -> IO ()
setAutoresizesOutlineColumn nsOutlineView  value =
    sendMsg nsOutlineView (mkSelector "setAutoresizesOutlineColumn:") retVoid [argCULong (if value then 1 else 0)]

-- | @- autosaveExpandedItems@
autosaveExpandedItems :: IsNSOutlineView nsOutlineView => nsOutlineView -> IO Bool
autosaveExpandedItems nsOutlineView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOutlineView (mkSelector "autosaveExpandedItems") retCULong []

-- | @- setAutosaveExpandedItems:@
setAutosaveExpandedItems :: IsNSOutlineView nsOutlineView => nsOutlineView -> Bool -> IO ()
setAutosaveExpandedItems nsOutlineView  value =
    sendMsg nsOutlineView (mkSelector "setAutosaveExpandedItems:") retVoid [argCULong (if value then 1 else 0)]

-- | @- userInterfaceLayoutDirection@
userInterfaceLayoutDirection :: IsNSOutlineView nsOutlineView => nsOutlineView -> IO NSUserInterfaceLayoutDirection
userInterfaceLayoutDirection nsOutlineView  =
    fmap (coerce :: CLong -> NSUserInterfaceLayoutDirection) $ sendMsg nsOutlineView (mkSelector "userInterfaceLayoutDirection") retCLong []

-- | @- setUserInterfaceLayoutDirection:@
setUserInterfaceLayoutDirection :: IsNSOutlineView nsOutlineView => nsOutlineView -> NSUserInterfaceLayoutDirection -> IO ()
setUserInterfaceLayoutDirection nsOutlineView  value =
    sendMsg nsOutlineView (mkSelector "setUserInterfaceLayoutDirection:") retVoid [argCLong (coerce value)]

-- | @- stronglyReferencesItems@
stronglyReferencesItems :: IsNSOutlineView nsOutlineView => nsOutlineView -> IO Bool
stronglyReferencesItems nsOutlineView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsOutlineView (mkSelector "stronglyReferencesItems") retCULong []

-- | @- setStronglyReferencesItems:@
setStronglyReferencesItems :: IsNSOutlineView nsOutlineView => nsOutlineView -> Bool -> IO ()
setStronglyReferencesItems nsOutlineView  value =
    sendMsg nsOutlineView (mkSelector "setStronglyReferencesItems:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isExpandable:@
isExpandableSelector :: Selector
isExpandableSelector = mkSelector "isExpandable:"

-- | @Selector@ for @numberOfChildrenOfItem:@
numberOfChildrenOfItemSelector :: Selector
numberOfChildrenOfItemSelector = mkSelector "numberOfChildrenOfItem:"

-- | @Selector@ for @child:ofItem:@
child_ofItemSelector :: Selector
child_ofItemSelector = mkSelector "child:ofItem:"

-- | @Selector@ for @expandItem:expandChildren:@
expandItem_expandChildrenSelector :: Selector
expandItem_expandChildrenSelector = mkSelector "expandItem:expandChildren:"

-- | @Selector@ for @expandItem:@
expandItemSelector :: Selector
expandItemSelector = mkSelector "expandItem:"

-- | @Selector@ for @collapseItem:collapseChildren:@
collapseItem_collapseChildrenSelector :: Selector
collapseItem_collapseChildrenSelector = mkSelector "collapseItem:collapseChildren:"

-- | @Selector@ for @collapseItem:@
collapseItemSelector :: Selector
collapseItemSelector = mkSelector "collapseItem:"

-- | @Selector@ for @reloadItem:reloadChildren:@
reloadItem_reloadChildrenSelector :: Selector
reloadItem_reloadChildrenSelector = mkSelector "reloadItem:reloadChildren:"

-- | @Selector@ for @reloadItem:@
reloadItemSelector :: Selector
reloadItemSelector = mkSelector "reloadItem:"

-- | @Selector@ for @parentForItem:@
parentForItemSelector :: Selector
parentForItemSelector = mkSelector "parentForItem:"

-- | @Selector@ for @childIndexForItem:@
childIndexForItemSelector :: Selector
childIndexForItemSelector = mkSelector "childIndexForItem:"

-- | @Selector@ for @itemAtRow:@
itemAtRowSelector :: Selector
itemAtRowSelector = mkSelector "itemAtRow:"

-- | @Selector@ for @rowForItem:@
rowForItemSelector :: Selector
rowForItemSelector = mkSelector "rowForItem:"

-- | @Selector@ for @levelForItem:@
levelForItemSelector :: Selector
levelForItemSelector = mkSelector "levelForItem:"

-- | @Selector@ for @levelForRow:@
levelForRowSelector :: Selector
levelForRowSelector = mkSelector "levelForRow:"

-- | @Selector@ for @isItemExpanded:@
isItemExpandedSelector :: Selector
isItemExpandedSelector = mkSelector "isItemExpanded:"

-- | @Selector@ for @frameOfOutlineCellAtRow:@
frameOfOutlineCellAtRowSelector :: Selector
frameOfOutlineCellAtRowSelector = mkSelector "frameOfOutlineCellAtRow:"

-- | @Selector@ for @setDropItem:dropChildIndex:@
setDropItem_dropChildIndexSelector :: Selector
setDropItem_dropChildIndexSelector = mkSelector "setDropItem:dropChildIndex:"

-- | @Selector@ for @shouldCollapseAutoExpandedItemsForDeposited:@
shouldCollapseAutoExpandedItemsForDepositedSelector :: Selector
shouldCollapseAutoExpandedItemsForDepositedSelector = mkSelector "shouldCollapseAutoExpandedItemsForDeposited:"

-- | @Selector@ for @insertItemsAtIndexes:inParent:withAnimation:@
insertItemsAtIndexes_inParent_withAnimationSelector :: Selector
insertItemsAtIndexes_inParent_withAnimationSelector = mkSelector "insertItemsAtIndexes:inParent:withAnimation:"

-- | @Selector@ for @removeItemsAtIndexes:inParent:withAnimation:@
removeItemsAtIndexes_inParent_withAnimationSelector :: Selector
removeItemsAtIndexes_inParent_withAnimationSelector = mkSelector "removeItemsAtIndexes:inParent:withAnimation:"

-- | @Selector@ for @moveItemAtIndex:inParent:toIndex:inParent:@
moveItemAtIndex_inParent_toIndex_inParentSelector :: Selector
moveItemAtIndex_inParent_toIndex_inParentSelector = mkSelector "moveItemAtIndex:inParent:toIndex:inParent:"

-- | @Selector@ for @insertRowsAtIndexes:withAnimation:@
insertRowsAtIndexes_withAnimationSelector :: Selector
insertRowsAtIndexes_withAnimationSelector = mkSelector "insertRowsAtIndexes:withAnimation:"

-- | @Selector@ for @removeRowsAtIndexes:withAnimation:@
removeRowsAtIndexes_withAnimationSelector :: Selector
removeRowsAtIndexes_withAnimationSelector = mkSelector "removeRowsAtIndexes:withAnimation:"

-- | @Selector@ for @moveRowAtIndex:toIndex:@
moveRowAtIndex_toIndexSelector :: Selector
moveRowAtIndex_toIndexSelector = mkSelector "moveRowAtIndex:toIndex:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @setDataSource:@
setDataSourceSelector :: Selector
setDataSourceSelector = mkSelector "setDataSource:"

-- | @Selector@ for @outlineTableColumn@
outlineTableColumnSelector :: Selector
outlineTableColumnSelector = mkSelector "outlineTableColumn"

-- | @Selector@ for @setOutlineTableColumn:@
setOutlineTableColumnSelector :: Selector
setOutlineTableColumnSelector = mkSelector "setOutlineTableColumn:"

-- | @Selector@ for @indentationPerLevel@
indentationPerLevelSelector :: Selector
indentationPerLevelSelector = mkSelector "indentationPerLevel"

-- | @Selector@ for @setIndentationPerLevel:@
setIndentationPerLevelSelector :: Selector
setIndentationPerLevelSelector = mkSelector "setIndentationPerLevel:"

-- | @Selector@ for @indentationMarkerFollowsCell@
indentationMarkerFollowsCellSelector :: Selector
indentationMarkerFollowsCellSelector = mkSelector "indentationMarkerFollowsCell"

-- | @Selector@ for @setIndentationMarkerFollowsCell:@
setIndentationMarkerFollowsCellSelector :: Selector
setIndentationMarkerFollowsCellSelector = mkSelector "setIndentationMarkerFollowsCell:"

-- | @Selector@ for @autoresizesOutlineColumn@
autoresizesOutlineColumnSelector :: Selector
autoresizesOutlineColumnSelector = mkSelector "autoresizesOutlineColumn"

-- | @Selector@ for @setAutoresizesOutlineColumn:@
setAutoresizesOutlineColumnSelector :: Selector
setAutoresizesOutlineColumnSelector = mkSelector "setAutoresizesOutlineColumn:"

-- | @Selector@ for @autosaveExpandedItems@
autosaveExpandedItemsSelector :: Selector
autosaveExpandedItemsSelector = mkSelector "autosaveExpandedItems"

-- | @Selector@ for @setAutosaveExpandedItems:@
setAutosaveExpandedItemsSelector :: Selector
setAutosaveExpandedItemsSelector = mkSelector "setAutosaveExpandedItems:"

-- | @Selector@ for @userInterfaceLayoutDirection@
userInterfaceLayoutDirectionSelector :: Selector
userInterfaceLayoutDirectionSelector = mkSelector "userInterfaceLayoutDirection"

-- | @Selector@ for @setUserInterfaceLayoutDirection:@
setUserInterfaceLayoutDirectionSelector :: Selector
setUserInterfaceLayoutDirectionSelector = mkSelector "setUserInterfaceLayoutDirection:"

-- | @Selector@ for @stronglyReferencesItems@
stronglyReferencesItemsSelector :: Selector
stronglyReferencesItemsSelector = mkSelector "stronglyReferencesItems"

-- | @Selector@ for @setStronglyReferencesItems:@
setStronglyReferencesItemsSelector :: Selector
setStronglyReferencesItemsSelector = mkSelector "setStronglyReferencesItems:"

