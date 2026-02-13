{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTreeController@.
module ObjC.AppKit.NSTreeController
  ( NSTreeController
  , IsNSTreeController(..)
  , rearrangeObjects
  , add
  , remove
  , addChild
  , insert
  , insertChild
  , insertObject_atArrangedObjectIndexPath
  , insertObjects_atArrangedObjectIndexPaths
  , removeObjectAtArrangedObjectIndexPath
  , removeObjectsAtArrangedObjectIndexPaths
  , setSelectionIndexPaths
  , setSelectionIndexPath
  , addSelectionIndexPaths
  , removeSelectionIndexPaths
  , moveNode_toIndexPath
  , moveNodes_toIndexPath
  , childrenKeyPathForNode
  , countKeyPathForNode
  , leafKeyPathForNode
  , arrangedObjects
  , childrenKeyPath
  , setChildrenKeyPath
  , countKeyPath
  , setCountKeyPath
  , leafKeyPath
  , setLeafKeyPath
  , sortDescriptors
  , setSortDescriptors
  , content
  , setContent
  , canInsert
  , canInsertChild
  , canAddChild
  , avoidsEmptySelection
  , setAvoidsEmptySelection
  , preservesSelection
  , setPreservesSelection
  , selectsInsertedObjects
  , setSelectsInsertedObjects
  , alwaysUsesMultipleValuesMarker
  , setAlwaysUsesMultipleValuesMarker
  , selectedObjects
  , selectionIndexPaths
  , selectionIndexPath
  , selectedNodes
  , addChildSelector
  , addSelectionIndexPathsSelector
  , addSelector
  , alwaysUsesMultipleValuesMarkerSelector
  , arrangedObjectsSelector
  , avoidsEmptySelectionSelector
  , canAddChildSelector
  , canInsertChildSelector
  , canInsertSelector
  , childrenKeyPathForNodeSelector
  , childrenKeyPathSelector
  , contentSelector
  , countKeyPathForNodeSelector
  , countKeyPathSelector
  , insertChildSelector
  , insertObject_atArrangedObjectIndexPathSelector
  , insertObjects_atArrangedObjectIndexPathsSelector
  , insertSelector
  , leafKeyPathForNodeSelector
  , leafKeyPathSelector
  , moveNode_toIndexPathSelector
  , moveNodes_toIndexPathSelector
  , preservesSelectionSelector
  , rearrangeObjectsSelector
  , removeObjectAtArrangedObjectIndexPathSelector
  , removeObjectsAtArrangedObjectIndexPathsSelector
  , removeSelectionIndexPathsSelector
  , removeSelector
  , selectedNodesSelector
  , selectedObjectsSelector
  , selectionIndexPathSelector
  , selectionIndexPathsSelector
  , selectsInsertedObjectsSelector
  , setAlwaysUsesMultipleValuesMarkerSelector
  , setAvoidsEmptySelectionSelector
  , setChildrenKeyPathSelector
  , setContentSelector
  , setCountKeyPathSelector
  , setLeafKeyPathSelector
  , setPreservesSelectionSelector
  , setSelectionIndexPathSelector
  , setSelectionIndexPathsSelector
  , setSelectsInsertedObjectsSelector
  , setSortDescriptorsSelector
  , sortDescriptorsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- rearrangeObjects@
rearrangeObjects :: IsNSTreeController nsTreeController => nsTreeController -> IO ()
rearrangeObjects nsTreeController =
  sendMessage nsTreeController rearrangeObjectsSelector

-- | @- add:@
add :: IsNSTreeController nsTreeController => nsTreeController -> RawId -> IO ()
add nsTreeController sender =
  sendMessage nsTreeController addSelector sender

-- | @- remove:@
remove :: IsNSTreeController nsTreeController => nsTreeController -> RawId -> IO ()
remove nsTreeController sender =
  sendMessage nsTreeController removeSelector sender

-- | @- addChild:@
addChild :: IsNSTreeController nsTreeController => nsTreeController -> RawId -> IO ()
addChild nsTreeController sender =
  sendMessage nsTreeController addChildSelector sender

-- | @- insert:@
insert :: IsNSTreeController nsTreeController => nsTreeController -> RawId -> IO ()
insert nsTreeController sender =
  sendMessage nsTreeController insertSelector sender

-- | @- insertChild:@
insertChild :: IsNSTreeController nsTreeController => nsTreeController -> RawId -> IO ()
insertChild nsTreeController sender =
  sendMessage nsTreeController insertChildSelector sender

-- | @- insertObject:atArrangedObjectIndexPath:@
insertObject_atArrangedObjectIndexPath :: (IsNSTreeController nsTreeController, IsNSIndexPath indexPath) => nsTreeController -> RawId -> indexPath -> IO ()
insertObject_atArrangedObjectIndexPath nsTreeController object indexPath =
  sendMessage nsTreeController insertObject_atArrangedObjectIndexPathSelector object (toNSIndexPath indexPath)

-- | @- insertObjects:atArrangedObjectIndexPaths:@
insertObjects_atArrangedObjectIndexPaths :: (IsNSTreeController nsTreeController, IsNSArray objects, IsNSArray indexPaths) => nsTreeController -> objects -> indexPaths -> IO ()
insertObjects_atArrangedObjectIndexPaths nsTreeController objects indexPaths =
  sendMessage nsTreeController insertObjects_atArrangedObjectIndexPathsSelector (toNSArray objects) (toNSArray indexPaths)

-- | @- removeObjectAtArrangedObjectIndexPath:@
removeObjectAtArrangedObjectIndexPath :: (IsNSTreeController nsTreeController, IsNSIndexPath indexPath) => nsTreeController -> indexPath -> IO ()
removeObjectAtArrangedObjectIndexPath nsTreeController indexPath =
  sendMessage nsTreeController removeObjectAtArrangedObjectIndexPathSelector (toNSIndexPath indexPath)

-- | @- removeObjectsAtArrangedObjectIndexPaths:@
removeObjectsAtArrangedObjectIndexPaths :: (IsNSTreeController nsTreeController, IsNSArray indexPaths) => nsTreeController -> indexPaths -> IO ()
removeObjectsAtArrangedObjectIndexPaths nsTreeController indexPaths =
  sendMessage nsTreeController removeObjectsAtArrangedObjectIndexPathsSelector (toNSArray indexPaths)

-- | @- setSelectionIndexPaths:@
setSelectionIndexPaths :: (IsNSTreeController nsTreeController, IsNSArray indexPaths) => nsTreeController -> indexPaths -> IO Bool
setSelectionIndexPaths nsTreeController indexPaths =
  sendMessage nsTreeController setSelectionIndexPathsSelector (toNSArray indexPaths)

-- | @- setSelectionIndexPath:@
setSelectionIndexPath :: (IsNSTreeController nsTreeController, IsNSIndexPath indexPath) => nsTreeController -> indexPath -> IO Bool
setSelectionIndexPath nsTreeController indexPath =
  sendMessage nsTreeController setSelectionIndexPathSelector (toNSIndexPath indexPath)

-- | @- addSelectionIndexPaths:@
addSelectionIndexPaths :: (IsNSTreeController nsTreeController, IsNSArray indexPaths) => nsTreeController -> indexPaths -> IO Bool
addSelectionIndexPaths nsTreeController indexPaths =
  sendMessage nsTreeController addSelectionIndexPathsSelector (toNSArray indexPaths)

-- | @- removeSelectionIndexPaths:@
removeSelectionIndexPaths :: (IsNSTreeController nsTreeController, IsNSArray indexPaths) => nsTreeController -> indexPaths -> IO Bool
removeSelectionIndexPaths nsTreeController indexPaths =
  sendMessage nsTreeController removeSelectionIndexPathsSelector (toNSArray indexPaths)

-- | @- moveNode:toIndexPath:@
moveNode_toIndexPath :: (IsNSTreeController nsTreeController, IsNSTreeNode node, IsNSIndexPath indexPath) => nsTreeController -> node -> indexPath -> IO ()
moveNode_toIndexPath nsTreeController node indexPath =
  sendMessage nsTreeController moveNode_toIndexPathSelector (toNSTreeNode node) (toNSIndexPath indexPath)

-- | @- moveNodes:toIndexPath:@
moveNodes_toIndexPath :: (IsNSTreeController nsTreeController, IsNSArray nodes, IsNSIndexPath startingIndexPath) => nsTreeController -> nodes -> startingIndexPath -> IO ()
moveNodes_toIndexPath nsTreeController nodes startingIndexPath =
  sendMessage nsTreeController moveNodes_toIndexPathSelector (toNSArray nodes) (toNSIndexPath startingIndexPath)

-- | @- childrenKeyPathForNode:@
childrenKeyPathForNode :: (IsNSTreeController nsTreeController, IsNSTreeNode node) => nsTreeController -> node -> IO (Id NSString)
childrenKeyPathForNode nsTreeController node =
  sendMessage nsTreeController childrenKeyPathForNodeSelector (toNSTreeNode node)

-- | @- countKeyPathForNode:@
countKeyPathForNode :: (IsNSTreeController nsTreeController, IsNSTreeNode node) => nsTreeController -> node -> IO (Id NSString)
countKeyPathForNode nsTreeController node =
  sendMessage nsTreeController countKeyPathForNodeSelector (toNSTreeNode node)

-- | @- leafKeyPathForNode:@
leafKeyPathForNode :: (IsNSTreeController nsTreeController, IsNSTreeNode node) => nsTreeController -> node -> IO (Id NSString)
leafKeyPathForNode nsTreeController node =
  sendMessage nsTreeController leafKeyPathForNodeSelector (toNSTreeNode node)

-- | @- arrangedObjects@
arrangedObjects :: IsNSTreeController nsTreeController => nsTreeController -> IO (Id NSTreeNode)
arrangedObjects nsTreeController =
  sendMessage nsTreeController arrangedObjectsSelector

-- | @- childrenKeyPath@
childrenKeyPath :: IsNSTreeController nsTreeController => nsTreeController -> IO (Id NSString)
childrenKeyPath nsTreeController =
  sendMessage nsTreeController childrenKeyPathSelector

-- | @- setChildrenKeyPath:@
setChildrenKeyPath :: (IsNSTreeController nsTreeController, IsNSString value) => nsTreeController -> value -> IO ()
setChildrenKeyPath nsTreeController value =
  sendMessage nsTreeController setChildrenKeyPathSelector (toNSString value)

-- | @- countKeyPath@
countKeyPath :: IsNSTreeController nsTreeController => nsTreeController -> IO (Id NSString)
countKeyPath nsTreeController =
  sendMessage nsTreeController countKeyPathSelector

-- | @- setCountKeyPath:@
setCountKeyPath :: (IsNSTreeController nsTreeController, IsNSString value) => nsTreeController -> value -> IO ()
setCountKeyPath nsTreeController value =
  sendMessage nsTreeController setCountKeyPathSelector (toNSString value)

-- | @- leafKeyPath@
leafKeyPath :: IsNSTreeController nsTreeController => nsTreeController -> IO (Id NSString)
leafKeyPath nsTreeController =
  sendMessage nsTreeController leafKeyPathSelector

-- | @- setLeafKeyPath:@
setLeafKeyPath :: (IsNSTreeController nsTreeController, IsNSString value) => nsTreeController -> value -> IO ()
setLeafKeyPath nsTreeController value =
  sendMessage nsTreeController setLeafKeyPathSelector (toNSString value)

-- | @- sortDescriptors@
sortDescriptors :: IsNSTreeController nsTreeController => nsTreeController -> IO (Id NSArray)
sortDescriptors nsTreeController =
  sendMessage nsTreeController sortDescriptorsSelector

-- | @- setSortDescriptors:@
setSortDescriptors :: (IsNSTreeController nsTreeController, IsNSArray value) => nsTreeController -> value -> IO ()
setSortDescriptors nsTreeController value =
  sendMessage nsTreeController setSortDescriptorsSelector (toNSArray value)

-- | @- content@
content :: IsNSTreeController nsTreeController => nsTreeController -> IO RawId
content nsTreeController =
  sendMessage nsTreeController contentSelector

-- | @- setContent:@
setContent :: IsNSTreeController nsTreeController => nsTreeController -> RawId -> IO ()
setContent nsTreeController value =
  sendMessage nsTreeController setContentSelector value

-- | @- canInsert@
canInsert :: IsNSTreeController nsTreeController => nsTreeController -> IO Bool
canInsert nsTreeController =
  sendMessage nsTreeController canInsertSelector

-- | @- canInsertChild@
canInsertChild :: IsNSTreeController nsTreeController => nsTreeController -> IO Bool
canInsertChild nsTreeController =
  sendMessage nsTreeController canInsertChildSelector

-- | @- canAddChild@
canAddChild :: IsNSTreeController nsTreeController => nsTreeController -> IO Bool
canAddChild nsTreeController =
  sendMessage nsTreeController canAddChildSelector

-- | @- avoidsEmptySelection@
avoidsEmptySelection :: IsNSTreeController nsTreeController => nsTreeController -> IO Bool
avoidsEmptySelection nsTreeController =
  sendMessage nsTreeController avoidsEmptySelectionSelector

-- | @- setAvoidsEmptySelection:@
setAvoidsEmptySelection :: IsNSTreeController nsTreeController => nsTreeController -> Bool -> IO ()
setAvoidsEmptySelection nsTreeController value =
  sendMessage nsTreeController setAvoidsEmptySelectionSelector value

-- | @- preservesSelection@
preservesSelection :: IsNSTreeController nsTreeController => nsTreeController -> IO Bool
preservesSelection nsTreeController =
  sendMessage nsTreeController preservesSelectionSelector

-- | @- setPreservesSelection:@
setPreservesSelection :: IsNSTreeController nsTreeController => nsTreeController -> Bool -> IO ()
setPreservesSelection nsTreeController value =
  sendMessage nsTreeController setPreservesSelectionSelector value

-- | @- selectsInsertedObjects@
selectsInsertedObjects :: IsNSTreeController nsTreeController => nsTreeController -> IO Bool
selectsInsertedObjects nsTreeController =
  sendMessage nsTreeController selectsInsertedObjectsSelector

-- | @- setSelectsInsertedObjects:@
setSelectsInsertedObjects :: IsNSTreeController nsTreeController => nsTreeController -> Bool -> IO ()
setSelectsInsertedObjects nsTreeController value =
  sendMessage nsTreeController setSelectsInsertedObjectsSelector value

-- | @- alwaysUsesMultipleValuesMarker@
alwaysUsesMultipleValuesMarker :: IsNSTreeController nsTreeController => nsTreeController -> IO Bool
alwaysUsesMultipleValuesMarker nsTreeController =
  sendMessage nsTreeController alwaysUsesMultipleValuesMarkerSelector

-- | @- setAlwaysUsesMultipleValuesMarker:@
setAlwaysUsesMultipleValuesMarker :: IsNSTreeController nsTreeController => nsTreeController -> Bool -> IO ()
setAlwaysUsesMultipleValuesMarker nsTreeController value =
  sendMessage nsTreeController setAlwaysUsesMultipleValuesMarkerSelector value

-- | @- selectedObjects@
selectedObjects :: IsNSTreeController nsTreeController => nsTreeController -> IO (Id NSArray)
selectedObjects nsTreeController =
  sendMessage nsTreeController selectedObjectsSelector

-- | @- selectionIndexPaths@
selectionIndexPaths :: IsNSTreeController nsTreeController => nsTreeController -> IO (Id NSArray)
selectionIndexPaths nsTreeController =
  sendMessage nsTreeController selectionIndexPathsSelector

-- | @- selectionIndexPath@
selectionIndexPath :: IsNSTreeController nsTreeController => nsTreeController -> IO (Id NSIndexPath)
selectionIndexPath nsTreeController =
  sendMessage nsTreeController selectionIndexPathSelector

-- | @- selectedNodes@
selectedNodes :: IsNSTreeController nsTreeController => nsTreeController -> IO (Id NSArray)
selectedNodes nsTreeController =
  sendMessage nsTreeController selectedNodesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rearrangeObjects@
rearrangeObjectsSelector :: Selector '[] ()
rearrangeObjectsSelector = mkSelector "rearrangeObjects"

-- | @Selector@ for @add:@
addSelector :: Selector '[RawId] ()
addSelector = mkSelector "add:"

-- | @Selector@ for @remove:@
removeSelector :: Selector '[RawId] ()
removeSelector = mkSelector "remove:"

-- | @Selector@ for @addChild:@
addChildSelector :: Selector '[RawId] ()
addChildSelector = mkSelector "addChild:"

-- | @Selector@ for @insert:@
insertSelector :: Selector '[RawId] ()
insertSelector = mkSelector "insert:"

-- | @Selector@ for @insertChild:@
insertChildSelector :: Selector '[RawId] ()
insertChildSelector = mkSelector "insertChild:"

-- | @Selector@ for @insertObject:atArrangedObjectIndexPath:@
insertObject_atArrangedObjectIndexPathSelector :: Selector '[RawId, Id NSIndexPath] ()
insertObject_atArrangedObjectIndexPathSelector = mkSelector "insertObject:atArrangedObjectIndexPath:"

-- | @Selector@ for @insertObjects:atArrangedObjectIndexPaths:@
insertObjects_atArrangedObjectIndexPathsSelector :: Selector '[Id NSArray, Id NSArray] ()
insertObjects_atArrangedObjectIndexPathsSelector = mkSelector "insertObjects:atArrangedObjectIndexPaths:"

-- | @Selector@ for @removeObjectAtArrangedObjectIndexPath:@
removeObjectAtArrangedObjectIndexPathSelector :: Selector '[Id NSIndexPath] ()
removeObjectAtArrangedObjectIndexPathSelector = mkSelector "removeObjectAtArrangedObjectIndexPath:"

-- | @Selector@ for @removeObjectsAtArrangedObjectIndexPaths:@
removeObjectsAtArrangedObjectIndexPathsSelector :: Selector '[Id NSArray] ()
removeObjectsAtArrangedObjectIndexPathsSelector = mkSelector "removeObjectsAtArrangedObjectIndexPaths:"

-- | @Selector@ for @setSelectionIndexPaths:@
setSelectionIndexPathsSelector :: Selector '[Id NSArray] Bool
setSelectionIndexPathsSelector = mkSelector "setSelectionIndexPaths:"

-- | @Selector@ for @setSelectionIndexPath:@
setSelectionIndexPathSelector :: Selector '[Id NSIndexPath] Bool
setSelectionIndexPathSelector = mkSelector "setSelectionIndexPath:"

-- | @Selector@ for @addSelectionIndexPaths:@
addSelectionIndexPathsSelector :: Selector '[Id NSArray] Bool
addSelectionIndexPathsSelector = mkSelector "addSelectionIndexPaths:"

-- | @Selector@ for @removeSelectionIndexPaths:@
removeSelectionIndexPathsSelector :: Selector '[Id NSArray] Bool
removeSelectionIndexPathsSelector = mkSelector "removeSelectionIndexPaths:"

-- | @Selector@ for @moveNode:toIndexPath:@
moveNode_toIndexPathSelector :: Selector '[Id NSTreeNode, Id NSIndexPath] ()
moveNode_toIndexPathSelector = mkSelector "moveNode:toIndexPath:"

-- | @Selector@ for @moveNodes:toIndexPath:@
moveNodes_toIndexPathSelector :: Selector '[Id NSArray, Id NSIndexPath] ()
moveNodes_toIndexPathSelector = mkSelector "moveNodes:toIndexPath:"

-- | @Selector@ for @childrenKeyPathForNode:@
childrenKeyPathForNodeSelector :: Selector '[Id NSTreeNode] (Id NSString)
childrenKeyPathForNodeSelector = mkSelector "childrenKeyPathForNode:"

-- | @Selector@ for @countKeyPathForNode:@
countKeyPathForNodeSelector :: Selector '[Id NSTreeNode] (Id NSString)
countKeyPathForNodeSelector = mkSelector "countKeyPathForNode:"

-- | @Selector@ for @leafKeyPathForNode:@
leafKeyPathForNodeSelector :: Selector '[Id NSTreeNode] (Id NSString)
leafKeyPathForNodeSelector = mkSelector "leafKeyPathForNode:"

-- | @Selector@ for @arrangedObjects@
arrangedObjectsSelector :: Selector '[] (Id NSTreeNode)
arrangedObjectsSelector = mkSelector "arrangedObjects"

-- | @Selector@ for @childrenKeyPath@
childrenKeyPathSelector :: Selector '[] (Id NSString)
childrenKeyPathSelector = mkSelector "childrenKeyPath"

-- | @Selector@ for @setChildrenKeyPath:@
setChildrenKeyPathSelector :: Selector '[Id NSString] ()
setChildrenKeyPathSelector = mkSelector "setChildrenKeyPath:"

-- | @Selector@ for @countKeyPath@
countKeyPathSelector :: Selector '[] (Id NSString)
countKeyPathSelector = mkSelector "countKeyPath"

-- | @Selector@ for @setCountKeyPath:@
setCountKeyPathSelector :: Selector '[Id NSString] ()
setCountKeyPathSelector = mkSelector "setCountKeyPath:"

-- | @Selector@ for @leafKeyPath@
leafKeyPathSelector :: Selector '[] (Id NSString)
leafKeyPathSelector = mkSelector "leafKeyPath"

-- | @Selector@ for @setLeafKeyPath:@
setLeafKeyPathSelector :: Selector '[Id NSString] ()
setLeafKeyPathSelector = mkSelector "setLeafKeyPath:"

-- | @Selector@ for @sortDescriptors@
sortDescriptorsSelector :: Selector '[] (Id NSArray)
sortDescriptorsSelector = mkSelector "sortDescriptors"

-- | @Selector@ for @setSortDescriptors:@
setSortDescriptorsSelector :: Selector '[Id NSArray] ()
setSortDescriptorsSelector = mkSelector "setSortDescriptors:"

-- | @Selector@ for @content@
contentSelector :: Selector '[] RawId
contentSelector = mkSelector "content"

-- | @Selector@ for @setContent:@
setContentSelector :: Selector '[RawId] ()
setContentSelector = mkSelector "setContent:"

-- | @Selector@ for @canInsert@
canInsertSelector :: Selector '[] Bool
canInsertSelector = mkSelector "canInsert"

-- | @Selector@ for @canInsertChild@
canInsertChildSelector :: Selector '[] Bool
canInsertChildSelector = mkSelector "canInsertChild"

-- | @Selector@ for @canAddChild@
canAddChildSelector :: Selector '[] Bool
canAddChildSelector = mkSelector "canAddChild"

-- | @Selector@ for @avoidsEmptySelection@
avoidsEmptySelectionSelector :: Selector '[] Bool
avoidsEmptySelectionSelector = mkSelector "avoidsEmptySelection"

-- | @Selector@ for @setAvoidsEmptySelection:@
setAvoidsEmptySelectionSelector :: Selector '[Bool] ()
setAvoidsEmptySelectionSelector = mkSelector "setAvoidsEmptySelection:"

-- | @Selector@ for @preservesSelection@
preservesSelectionSelector :: Selector '[] Bool
preservesSelectionSelector = mkSelector "preservesSelection"

-- | @Selector@ for @setPreservesSelection:@
setPreservesSelectionSelector :: Selector '[Bool] ()
setPreservesSelectionSelector = mkSelector "setPreservesSelection:"

-- | @Selector@ for @selectsInsertedObjects@
selectsInsertedObjectsSelector :: Selector '[] Bool
selectsInsertedObjectsSelector = mkSelector "selectsInsertedObjects"

-- | @Selector@ for @setSelectsInsertedObjects:@
setSelectsInsertedObjectsSelector :: Selector '[Bool] ()
setSelectsInsertedObjectsSelector = mkSelector "setSelectsInsertedObjects:"

-- | @Selector@ for @alwaysUsesMultipleValuesMarker@
alwaysUsesMultipleValuesMarkerSelector :: Selector '[] Bool
alwaysUsesMultipleValuesMarkerSelector = mkSelector "alwaysUsesMultipleValuesMarker"

-- | @Selector@ for @setAlwaysUsesMultipleValuesMarker:@
setAlwaysUsesMultipleValuesMarkerSelector :: Selector '[Bool] ()
setAlwaysUsesMultipleValuesMarkerSelector = mkSelector "setAlwaysUsesMultipleValuesMarker:"

-- | @Selector@ for @selectedObjects@
selectedObjectsSelector :: Selector '[] (Id NSArray)
selectedObjectsSelector = mkSelector "selectedObjects"

-- | @Selector@ for @selectionIndexPaths@
selectionIndexPathsSelector :: Selector '[] (Id NSArray)
selectionIndexPathsSelector = mkSelector "selectionIndexPaths"

-- | @Selector@ for @selectionIndexPath@
selectionIndexPathSelector :: Selector '[] (Id NSIndexPath)
selectionIndexPathSelector = mkSelector "selectionIndexPath"

-- | @Selector@ for @selectedNodes@
selectedNodesSelector :: Selector '[] (Id NSArray)
selectedNodesSelector = mkSelector "selectedNodes"

