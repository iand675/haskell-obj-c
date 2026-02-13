{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , autoresizesOutlineColumnSelector
  , autosaveExpandedItemsSelector
  , childIndexForItemSelector
  , child_ofItemSelector
  , collapseItemSelector
  , collapseItem_collapseChildrenSelector
  , dataSourceSelector
  , delegateSelector
  , expandItemSelector
  , expandItem_expandChildrenSelector
  , frameOfOutlineCellAtRowSelector
  , indentationMarkerFollowsCellSelector
  , indentationPerLevelSelector
  , insertItemsAtIndexes_inParent_withAnimationSelector
  , insertRowsAtIndexes_withAnimationSelector
  , isExpandableSelector
  , isItemExpandedSelector
  , itemAtRowSelector
  , levelForItemSelector
  , levelForRowSelector
  , moveItemAtIndex_inParent_toIndex_inParentSelector
  , moveRowAtIndex_toIndexSelector
  , numberOfChildrenOfItemSelector
  , outlineTableColumnSelector
  , parentForItemSelector
  , reloadItemSelector
  , reloadItem_reloadChildrenSelector
  , removeItemsAtIndexes_inParent_withAnimationSelector
  , removeRowsAtIndexes_withAnimationSelector
  , rowForItemSelector
  , setAutoresizesOutlineColumnSelector
  , setAutosaveExpandedItemsSelector
  , setDataSourceSelector
  , setDelegateSelector
  , setDropItem_dropChildIndexSelector
  , setIndentationMarkerFollowsCellSelector
  , setIndentationPerLevelSelector
  , setOutlineTableColumnSelector
  , setStronglyReferencesItemsSelector
  , setUserInterfaceLayoutDirectionSelector
  , shouldCollapseAutoExpandedItemsForDepositedSelector
  , stronglyReferencesItemsSelector
  , userInterfaceLayoutDirectionSelector

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

-- | @- isExpandable:@
isExpandable :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO Bool
isExpandable nsOutlineView item =
  sendMessage nsOutlineView isExpandableSelector item

-- | @- numberOfChildrenOfItem:@
numberOfChildrenOfItem :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO CLong
numberOfChildrenOfItem nsOutlineView item =
  sendMessage nsOutlineView numberOfChildrenOfItemSelector item

-- | @- child:ofItem:@
child_ofItem :: IsNSOutlineView nsOutlineView => nsOutlineView -> CLong -> RawId -> IO RawId
child_ofItem nsOutlineView index item =
  sendMessage nsOutlineView child_ofItemSelector index item

-- | @- expandItem:expandChildren:@
expandItem_expandChildren :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> Bool -> IO ()
expandItem_expandChildren nsOutlineView item expandChildren =
  sendMessage nsOutlineView expandItem_expandChildrenSelector item expandChildren

-- | @- expandItem:@
expandItem :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO ()
expandItem nsOutlineView item =
  sendMessage nsOutlineView expandItemSelector item

-- | @- collapseItem:collapseChildren:@
collapseItem_collapseChildren :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> Bool -> IO ()
collapseItem_collapseChildren nsOutlineView item collapseChildren =
  sendMessage nsOutlineView collapseItem_collapseChildrenSelector item collapseChildren

-- | @- collapseItem:@
collapseItem :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO ()
collapseItem nsOutlineView item =
  sendMessage nsOutlineView collapseItemSelector item

-- | @- reloadItem:reloadChildren:@
reloadItem_reloadChildren :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> Bool -> IO ()
reloadItem_reloadChildren nsOutlineView item reloadChildren =
  sendMessage nsOutlineView reloadItem_reloadChildrenSelector item reloadChildren

-- | @- reloadItem:@
reloadItem :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO ()
reloadItem nsOutlineView item =
  sendMessage nsOutlineView reloadItemSelector item

-- | @- parentForItem:@
parentForItem :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO RawId
parentForItem nsOutlineView item =
  sendMessage nsOutlineView parentForItemSelector item

-- | @- childIndexForItem:@
childIndexForItem :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO CLong
childIndexForItem nsOutlineView item =
  sendMessage nsOutlineView childIndexForItemSelector item

-- | @- itemAtRow:@
itemAtRow :: IsNSOutlineView nsOutlineView => nsOutlineView -> CLong -> IO RawId
itemAtRow nsOutlineView row =
  sendMessage nsOutlineView itemAtRowSelector row

-- | @- rowForItem:@
rowForItem :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO CLong
rowForItem nsOutlineView item =
  sendMessage nsOutlineView rowForItemSelector item

-- | @- levelForItem:@
levelForItem :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO CLong
levelForItem nsOutlineView item =
  sendMessage nsOutlineView levelForItemSelector item

-- | @- levelForRow:@
levelForRow :: IsNSOutlineView nsOutlineView => nsOutlineView -> CLong -> IO CLong
levelForRow nsOutlineView row =
  sendMessage nsOutlineView levelForRowSelector row

-- | @- isItemExpanded:@
isItemExpanded :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO Bool
isItemExpanded nsOutlineView item =
  sendMessage nsOutlineView isItemExpandedSelector item

-- | @- frameOfOutlineCellAtRow:@
frameOfOutlineCellAtRow :: IsNSOutlineView nsOutlineView => nsOutlineView -> CLong -> IO NSRect
frameOfOutlineCellAtRow nsOutlineView row =
  sendMessage nsOutlineView frameOfOutlineCellAtRowSelector row

-- | @- setDropItem:dropChildIndex:@
setDropItem_dropChildIndex :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> CLong -> IO ()
setDropItem_dropChildIndex nsOutlineView item index =
  sendMessage nsOutlineView setDropItem_dropChildIndexSelector item index

-- | @- shouldCollapseAutoExpandedItemsForDeposited:@
shouldCollapseAutoExpandedItemsForDeposited :: IsNSOutlineView nsOutlineView => nsOutlineView -> Bool -> IO Bool
shouldCollapseAutoExpandedItemsForDeposited nsOutlineView deposited =
  sendMessage nsOutlineView shouldCollapseAutoExpandedItemsForDepositedSelector deposited

-- | @- insertItemsAtIndexes:inParent:withAnimation:@
insertItemsAtIndexes_inParent_withAnimation :: (IsNSOutlineView nsOutlineView, IsNSIndexSet indexes) => nsOutlineView -> indexes -> RawId -> NSTableViewAnimationOptions -> IO ()
insertItemsAtIndexes_inParent_withAnimation nsOutlineView indexes parent animationOptions =
  sendMessage nsOutlineView insertItemsAtIndexes_inParent_withAnimationSelector (toNSIndexSet indexes) parent animationOptions

-- | @- removeItemsAtIndexes:inParent:withAnimation:@
removeItemsAtIndexes_inParent_withAnimation :: (IsNSOutlineView nsOutlineView, IsNSIndexSet indexes) => nsOutlineView -> indexes -> RawId -> NSTableViewAnimationOptions -> IO ()
removeItemsAtIndexes_inParent_withAnimation nsOutlineView indexes parent animationOptions =
  sendMessage nsOutlineView removeItemsAtIndexes_inParent_withAnimationSelector (toNSIndexSet indexes) parent animationOptions

-- | @- moveItemAtIndex:inParent:toIndex:inParent:@
moveItemAtIndex_inParent_toIndex_inParent :: IsNSOutlineView nsOutlineView => nsOutlineView -> CLong -> RawId -> CLong -> RawId -> IO ()
moveItemAtIndex_inParent_toIndex_inParent nsOutlineView fromIndex oldParent toIndex newParent =
  sendMessage nsOutlineView moveItemAtIndex_inParent_toIndex_inParentSelector fromIndex oldParent toIndex newParent

-- | @- insertRowsAtIndexes:withAnimation:@
insertRowsAtIndexes_withAnimation :: (IsNSOutlineView nsOutlineView, IsNSIndexSet indexes) => nsOutlineView -> indexes -> NSTableViewAnimationOptions -> IO ()
insertRowsAtIndexes_withAnimation nsOutlineView indexes animationOptions =
  sendMessage nsOutlineView insertRowsAtIndexes_withAnimationSelector (toNSIndexSet indexes) animationOptions

-- | @- removeRowsAtIndexes:withAnimation:@
removeRowsAtIndexes_withAnimation :: (IsNSOutlineView nsOutlineView, IsNSIndexSet indexes) => nsOutlineView -> indexes -> NSTableViewAnimationOptions -> IO ()
removeRowsAtIndexes_withAnimation nsOutlineView indexes animationOptions =
  sendMessage nsOutlineView removeRowsAtIndexes_withAnimationSelector (toNSIndexSet indexes) animationOptions

-- | @- moveRowAtIndex:toIndex:@
moveRowAtIndex_toIndex :: IsNSOutlineView nsOutlineView => nsOutlineView -> CLong -> CLong -> IO ()
moveRowAtIndex_toIndex nsOutlineView oldIndex newIndex =
  sendMessage nsOutlineView moveRowAtIndex_toIndexSelector oldIndex newIndex

-- | @- delegate@
delegate :: IsNSOutlineView nsOutlineView => nsOutlineView -> IO RawId
delegate nsOutlineView =
  sendMessage nsOutlineView delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO ()
setDelegate nsOutlineView value =
  sendMessage nsOutlineView setDelegateSelector value

-- | @- dataSource@
dataSource :: IsNSOutlineView nsOutlineView => nsOutlineView -> IO RawId
dataSource nsOutlineView =
  sendMessage nsOutlineView dataSourceSelector

-- | @- setDataSource:@
setDataSource :: IsNSOutlineView nsOutlineView => nsOutlineView -> RawId -> IO ()
setDataSource nsOutlineView value =
  sendMessage nsOutlineView setDataSourceSelector value

-- | @- outlineTableColumn@
outlineTableColumn :: IsNSOutlineView nsOutlineView => nsOutlineView -> IO (Id NSTableColumn)
outlineTableColumn nsOutlineView =
  sendMessage nsOutlineView outlineTableColumnSelector

-- | @- setOutlineTableColumn:@
setOutlineTableColumn :: (IsNSOutlineView nsOutlineView, IsNSTableColumn value) => nsOutlineView -> value -> IO ()
setOutlineTableColumn nsOutlineView value =
  sendMessage nsOutlineView setOutlineTableColumnSelector (toNSTableColumn value)

-- | @- indentationPerLevel@
indentationPerLevel :: IsNSOutlineView nsOutlineView => nsOutlineView -> IO CDouble
indentationPerLevel nsOutlineView =
  sendMessage nsOutlineView indentationPerLevelSelector

-- | @- setIndentationPerLevel:@
setIndentationPerLevel :: IsNSOutlineView nsOutlineView => nsOutlineView -> CDouble -> IO ()
setIndentationPerLevel nsOutlineView value =
  sendMessage nsOutlineView setIndentationPerLevelSelector value

-- | @- indentationMarkerFollowsCell@
indentationMarkerFollowsCell :: IsNSOutlineView nsOutlineView => nsOutlineView -> IO Bool
indentationMarkerFollowsCell nsOutlineView =
  sendMessage nsOutlineView indentationMarkerFollowsCellSelector

-- | @- setIndentationMarkerFollowsCell:@
setIndentationMarkerFollowsCell :: IsNSOutlineView nsOutlineView => nsOutlineView -> Bool -> IO ()
setIndentationMarkerFollowsCell nsOutlineView value =
  sendMessage nsOutlineView setIndentationMarkerFollowsCellSelector value

-- | @- autoresizesOutlineColumn@
autoresizesOutlineColumn :: IsNSOutlineView nsOutlineView => nsOutlineView -> IO Bool
autoresizesOutlineColumn nsOutlineView =
  sendMessage nsOutlineView autoresizesOutlineColumnSelector

-- | @- setAutoresizesOutlineColumn:@
setAutoresizesOutlineColumn :: IsNSOutlineView nsOutlineView => nsOutlineView -> Bool -> IO ()
setAutoresizesOutlineColumn nsOutlineView value =
  sendMessage nsOutlineView setAutoresizesOutlineColumnSelector value

-- | @- autosaveExpandedItems@
autosaveExpandedItems :: IsNSOutlineView nsOutlineView => nsOutlineView -> IO Bool
autosaveExpandedItems nsOutlineView =
  sendMessage nsOutlineView autosaveExpandedItemsSelector

-- | @- setAutosaveExpandedItems:@
setAutosaveExpandedItems :: IsNSOutlineView nsOutlineView => nsOutlineView -> Bool -> IO ()
setAutosaveExpandedItems nsOutlineView value =
  sendMessage nsOutlineView setAutosaveExpandedItemsSelector value

-- | @- userInterfaceLayoutDirection@
userInterfaceLayoutDirection :: IsNSOutlineView nsOutlineView => nsOutlineView -> IO NSUserInterfaceLayoutDirection
userInterfaceLayoutDirection nsOutlineView =
  sendMessage nsOutlineView userInterfaceLayoutDirectionSelector

-- | @- setUserInterfaceLayoutDirection:@
setUserInterfaceLayoutDirection :: IsNSOutlineView nsOutlineView => nsOutlineView -> NSUserInterfaceLayoutDirection -> IO ()
setUserInterfaceLayoutDirection nsOutlineView value =
  sendMessage nsOutlineView setUserInterfaceLayoutDirectionSelector value

-- | @- stronglyReferencesItems@
stronglyReferencesItems :: IsNSOutlineView nsOutlineView => nsOutlineView -> IO Bool
stronglyReferencesItems nsOutlineView =
  sendMessage nsOutlineView stronglyReferencesItemsSelector

-- | @- setStronglyReferencesItems:@
setStronglyReferencesItems :: IsNSOutlineView nsOutlineView => nsOutlineView -> Bool -> IO ()
setStronglyReferencesItems nsOutlineView value =
  sendMessage nsOutlineView setStronglyReferencesItemsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isExpandable:@
isExpandableSelector :: Selector '[RawId] Bool
isExpandableSelector = mkSelector "isExpandable:"

-- | @Selector@ for @numberOfChildrenOfItem:@
numberOfChildrenOfItemSelector :: Selector '[RawId] CLong
numberOfChildrenOfItemSelector = mkSelector "numberOfChildrenOfItem:"

-- | @Selector@ for @child:ofItem:@
child_ofItemSelector :: Selector '[CLong, RawId] RawId
child_ofItemSelector = mkSelector "child:ofItem:"

-- | @Selector@ for @expandItem:expandChildren:@
expandItem_expandChildrenSelector :: Selector '[RawId, Bool] ()
expandItem_expandChildrenSelector = mkSelector "expandItem:expandChildren:"

-- | @Selector@ for @expandItem:@
expandItemSelector :: Selector '[RawId] ()
expandItemSelector = mkSelector "expandItem:"

-- | @Selector@ for @collapseItem:collapseChildren:@
collapseItem_collapseChildrenSelector :: Selector '[RawId, Bool] ()
collapseItem_collapseChildrenSelector = mkSelector "collapseItem:collapseChildren:"

-- | @Selector@ for @collapseItem:@
collapseItemSelector :: Selector '[RawId] ()
collapseItemSelector = mkSelector "collapseItem:"

-- | @Selector@ for @reloadItem:reloadChildren:@
reloadItem_reloadChildrenSelector :: Selector '[RawId, Bool] ()
reloadItem_reloadChildrenSelector = mkSelector "reloadItem:reloadChildren:"

-- | @Selector@ for @reloadItem:@
reloadItemSelector :: Selector '[RawId] ()
reloadItemSelector = mkSelector "reloadItem:"

-- | @Selector@ for @parentForItem:@
parentForItemSelector :: Selector '[RawId] RawId
parentForItemSelector = mkSelector "parentForItem:"

-- | @Selector@ for @childIndexForItem:@
childIndexForItemSelector :: Selector '[RawId] CLong
childIndexForItemSelector = mkSelector "childIndexForItem:"

-- | @Selector@ for @itemAtRow:@
itemAtRowSelector :: Selector '[CLong] RawId
itemAtRowSelector = mkSelector "itemAtRow:"

-- | @Selector@ for @rowForItem:@
rowForItemSelector :: Selector '[RawId] CLong
rowForItemSelector = mkSelector "rowForItem:"

-- | @Selector@ for @levelForItem:@
levelForItemSelector :: Selector '[RawId] CLong
levelForItemSelector = mkSelector "levelForItem:"

-- | @Selector@ for @levelForRow:@
levelForRowSelector :: Selector '[CLong] CLong
levelForRowSelector = mkSelector "levelForRow:"

-- | @Selector@ for @isItemExpanded:@
isItemExpandedSelector :: Selector '[RawId] Bool
isItemExpandedSelector = mkSelector "isItemExpanded:"

-- | @Selector@ for @frameOfOutlineCellAtRow:@
frameOfOutlineCellAtRowSelector :: Selector '[CLong] NSRect
frameOfOutlineCellAtRowSelector = mkSelector "frameOfOutlineCellAtRow:"

-- | @Selector@ for @setDropItem:dropChildIndex:@
setDropItem_dropChildIndexSelector :: Selector '[RawId, CLong] ()
setDropItem_dropChildIndexSelector = mkSelector "setDropItem:dropChildIndex:"

-- | @Selector@ for @shouldCollapseAutoExpandedItemsForDeposited:@
shouldCollapseAutoExpandedItemsForDepositedSelector :: Selector '[Bool] Bool
shouldCollapseAutoExpandedItemsForDepositedSelector = mkSelector "shouldCollapseAutoExpandedItemsForDeposited:"

-- | @Selector@ for @insertItemsAtIndexes:inParent:withAnimation:@
insertItemsAtIndexes_inParent_withAnimationSelector :: Selector '[Id NSIndexSet, RawId, NSTableViewAnimationOptions] ()
insertItemsAtIndexes_inParent_withAnimationSelector = mkSelector "insertItemsAtIndexes:inParent:withAnimation:"

-- | @Selector@ for @removeItemsAtIndexes:inParent:withAnimation:@
removeItemsAtIndexes_inParent_withAnimationSelector :: Selector '[Id NSIndexSet, RawId, NSTableViewAnimationOptions] ()
removeItemsAtIndexes_inParent_withAnimationSelector = mkSelector "removeItemsAtIndexes:inParent:withAnimation:"

-- | @Selector@ for @moveItemAtIndex:inParent:toIndex:inParent:@
moveItemAtIndex_inParent_toIndex_inParentSelector :: Selector '[CLong, RawId, CLong, RawId] ()
moveItemAtIndex_inParent_toIndex_inParentSelector = mkSelector "moveItemAtIndex:inParent:toIndex:inParent:"

-- | @Selector@ for @insertRowsAtIndexes:withAnimation:@
insertRowsAtIndexes_withAnimationSelector :: Selector '[Id NSIndexSet, NSTableViewAnimationOptions] ()
insertRowsAtIndexes_withAnimationSelector = mkSelector "insertRowsAtIndexes:withAnimation:"

-- | @Selector@ for @removeRowsAtIndexes:withAnimation:@
removeRowsAtIndexes_withAnimationSelector :: Selector '[Id NSIndexSet, NSTableViewAnimationOptions] ()
removeRowsAtIndexes_withAnimationSelector = mkSelector "removeRowsAtIndexes:withAnimation:"

-- | @Selector@ for @moveRowAtIndex:toIndex:@
moveRowAtIndex_toIndexSelector :: Selector '[CLong, CLong] ()
moveRowAtIndex_toIndexSelector = mkSelector "moveRowAtIndex:toIndex:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector '[] RawId
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @setDataSource:@
setDataSourceSelector :: Selector '[RawId] ()
setDataSourceSelector = mkSelector "setDataSource:"

-- | @Selector@ for @outlineTableColumn@
outlineTableColumnSelector :: Selector '[] (Id NSTableColumn)
outlineTableColumnSelector = mkSelector "outlineTableColumn"

-- | @Selector@ for @setOutlineTableColumn:@
setOutlineTableColumnSelector :: Selector '[Id NSTableColumn] ()
setOutlineTableColumnSelector = mkSelector "setOutlineTableColumn:"

-- | @Selector@ for @indentationPerLevel@
indentationPerLevelSelector :: Selector '[] CDouble
indentationPerLevelSelector = mkSelector "indentationPerLevel"

-- | @Selector@ for @setIndentationPerLevel:@
setIndentationPerLevelSelector :: Selector '[CDouble] ()
setIndentationPerLevelSelector = mkSelector "setIndentationPerLevel:"

-- | @Selector@ for @indentationMarkerFollowsCell@
indentationMarkerFollowsCellSelector :: Selector '[] Bool
indentationMarkerFollowsCellSelector = mkSelector "indentationMarkerFollowsCell"

-- | @Selector@ for @setIndentationMarkerFollowsCell:@
setIndentationMarkerFollowsCellSelector :: Selector '[Bool] ()
setIndentationMarkerFollowsCellSelector = mkSelector "setIndentationMarkerFollowsCell:"

-- | @Selector@ for @autoresizesOutlineColumn@
autoresizesOutlineColumnSelector :: Selector '[] Bool
autoresizesOutlineColumnSelector = mkSelector "autoresizesOutlineColumn"

-- | @Selector@ for @setAutoresizesOutlineColumn:@
setAutoresizesOutlineColumnSelector :: Selector '[Bool] ()
setAutoresizesOutlineColumnSelector = mkSelector "setAutoresizesOutlineColumn:"

-- | @Selector@ for @autosaveExpandedItems@
autosaveExpandedItemsSelector :: Selector '[] Bool
autosaveExpandedItemsSelector = mkSelector "autosaveExpandedItems"

-- | @Selector@ for @setAutosaveExpandedItems:@
setAutosaveExpandedItemsSelector :: Selector '[Bool] ()
setAutosaveExpandedItemsSelector = mkSelector "setAutosaveExpandedItems:"

-- | @Selector@ for @userInterfaceLayoutDirection@
userInterfaceLayoutDirectionSelector :: Selector '[] NSUserInterfaceLayoutDirection
userInterfaceLayoutDirectionSelector = mkSelector "userInterfaceLayoutDirection"

-- | @Selector@ for @setUserInterfaceLayoutDirection:@
setUserInterfaceLayoutDirectionSelector :: Selector '[NSUserInterfaceLayoutDirection] ()
setUserInterfaceLayoutDirectionSelector = mkSelector "setUserInterfaceLayoutDirection:"

-- | @Selector@ for @stronglyReferencesItems@
stronglyReferencesItemsSelector :: Selector '[] Bool
stronglyReferencesItemsSelector = mkSelector "stronglyReferencesItems"

-- | @Selector@ for @setStronglyReferencesItems:@
setStronglyReferencesItemsSelector :: Selector '[Bool] ()
setStronglyReferencesItemsSelector = mkSelector "setStronglyReferencesItems:"

