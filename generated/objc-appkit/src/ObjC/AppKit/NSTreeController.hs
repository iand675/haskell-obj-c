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
  , rearrangeObjectsSelector
  , addSelector
  , removeSelector
  , addChildSelector
  , insertSelector
  , insertChildSelector
  , insertObject_atArrangedObjectIndexPathSelector
  , insertObjects_atArrangedObjectIndexPathsSelector
  , removeObjectAtArrangedObjectIndexPathSelector
  , removeObjectsAtArrangedObjectIndexPathsSelector
  , setSelectionIndexPathsSelector
  , setSelectionIndexPathSelector
  , addSelectionIndexPathsSelector
  , removeSelectionIndexPathsSelector
  , moveNode_toIndexPathSelector
  , moveNodes_toIndexPathSelector
  , childrenKeyPathForNodeSelector
  , countKeyPathForNodeSelector
  , leafKeyPathForNodeSelector
  , arrangedObjectsSelector
  , childrenKeyPathSelector
  , setChildrenKeyPathSelector
  , countKeyPathSelector
  , setCountKeyPathSelector
  , leafKeyPathSelector
  , setLeafKeyPathSelector
  , sortDescriptorsSelector
  , setSortDescriptorsSelector
  , contentSelector
  , setContentSelector
  , canInsertSelector
  , canInsertChildSelector
  , canAddChildSelector
  , avoidsEmptySelectionSelector
  , setAvoidsEmptySelectionSelector
  , preservesSelectionSelector
  , setPreservesSelectionSelector
  , selectsInsertedObjectsSelector
  , setSelectsInsertedObjectsSelector
  , alwaysUsesMultipleValuesMarkerSelector
  , setAlwaysUsesMultipleValuesMarkerSelector
  , selectedObjectsSelector
  , selectionIndexPathsSelector
  , selectionIndexPathSelector


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

-- | @- rearrangeObjects@
rearrangeObjects :: IsNSTreeController nsTreeController => nsTreeController -> IO ()
rearrangeObjects nsTreeController  =
  sendMsg nsTreeController (mkSelector "rearrangeObjects") retVoid []

-- | @- add:@
add :: IsNSTreeController nsTreeController => nsTreeController -> RawId -> IO ()
add nsTreeController  sender =
  sendMsg nsTreeController (mkSelector "add:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- remove:@
remove :: IsNSTreeController nsTreeController => nsTreeController -> RawId -> IO ()
remove nsTreeController  sender =
  sendMsg nsTreeController (mkSelector "remove:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- addChild:@
addChild :: IsNSTreeController nsTreeController => nsTreeController -> RawId -> IO ()
addChild nsTreeController  sender =
  sendMsg nsTreeController (mkSelector "addChild:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- insert:@
insert :: IsNSTreeController nsTreeController => nsTreeController -> RawId -> IO ()
insert nsTreeController  sender =
  sendMsg nsTreeController (mkSelector "insert:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- insertChild:@
insertChild :: IsNSTreeController nsTreeController => nsTreeController -> RawId -> IO ()
insertChild nsTreeController  sender =
  sendMsg nsTreeController (mkSelector "insertChild:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- insertObject:atArrangedObjectIndexPath:@
insertObject_atArrangedObjectIndexPath :: (IsNSTreeController nsTreeController, IsNSIndexPath indexPath) => nsTreeController -> RawId -> indexPath -> IO ()
insertObject_atArrangedObjectIndexPath nsTreeController  object indexPath =
withObjCPtr indexPath $ \raw_indexPath ->
    sendMsg nsTreeController (mkSelector "insertObject:atArrangedObjectIndexPath:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (castPtr raw_indexPath :: Ptr ())]

-- | @- insertObjects:atArrangedObjectIndexPaths:@
insertObjects_atArrangedObjectIndexPaths :: (IsNSTreeController nsTreeController, IsNSArray objects, IsNSArray indexPaths) => nsTreeController -> objects -> indexPaths -> IO ()
insertObjects_atArrangedObjectIndexPaths nsTreeController  objects indexPaths =
withObjCPtr objects $ \raw_objects ->
  withObjCPtr indexPaths $ \raw_indexPaths ->
      sendMsg nsTreeController (mkSelector "insertObjects:atArrangedObjectIndexPaths:") retVoid [argPtr (castPtr raw_objects :: Ptr ()), argPtr (castPtr raw_indexPaths :: Ptr ())]

-- | @- removeObjectAtArrangedObjectIndexPath:@
removeObjectAtArrangedObjectIndexPath :: (IsNSTreeController nsTreeController, IsNSIndexPath indexPath) => nsTreeController -> indexPath -> IO ()
removeObjectAtArrangedObjectIndexPath nsTreeController  indexPath =
withObjCPtr indexPath $ \raw_indexPath ->
    sendMsg nsTreeController (mkSelector "removeObjectAtArrangedObjectIndexPath:") retVoid [argPtr (castPtr raw_indexPath :: Ptr ())]

-- | @- removeObjectsAtArrangedObjectIndexPaths:@
removeObjectsAtArrangedObjectIndexPaths :: (IsNSTreeController nsTreeController, IsNSArray indexPaths) => nsTreeController -> indexPaths -> IO ()
removeObjectsAtArrangedObjectIndexPaths nsTreeController  indexPaths =
withObjCPtr indexPaths $ \raw_indexPaths ->
    sendMsg nsTreeController (mkSelector "removeObjectsAtArrangedObjectIndexPaths:") retVoid [argPtr (castPtr raw_indexPaths :: Ptr ())]

-- | @- setSelectionIndexPaths:@
setSelectionIndexPaths :: (IsNSTreeController nsTreeController, IsNSArray indexPaths) => nsTreeController -> indexPaths -> IO Bool
setSelectionIndexPaths nsTreeController  indexPaths =
withObjCPtr indexPaths $ \raw_indexPaths ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTreeController (mkSelector "setSelectionIndexPaths:") retCULong [argPtr (castPtr raw_indexPaths :: Ptr ())]

-- | @- setSelectionIndexPath:@
setSelectionIndexPath :: (IsNSTreeController nsTreeController, IsNSIndexPath indexPath) => nsTreeController -> indexPath -> IO Bool
setSelectionIndexPath nsTreeController  indexPath =
withObjCPtr indexPath $ \raw_indexPath ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTreeController (mkSelector "setSelectionIndexPath:") retCULong [argPtr (castPtr raw_indexPath :: Ptr ())]

-- | @- addSelectionIndexPaths:@
addSelectionIndexPaths :: (IsNSTreeController nsTreeController, IsNSArray indexPaths) => nsTreeController -> indexPaths -> IO Bool
addSelectionIndexPaths nsTreeController  indexPaths =
withObjCPtr indexPaths $ \raw_indexPaths ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTreeController (mkSelector "addSelectionIndexPaths:") retCULong [argPtr (castPtr raw_indexPaths :: Ptr ())]

-- | @- removeSelectionIndexPaths:@
removeSelectionIndexPaths :: (IsNSTreeController nsTreeController, IsNSArray indexPaths) => nsTreeController -> indexPaths -> IO Bool
removeSelectionIndexPaths nsTreeController  indexPaths =
withObjCPtr indexPaths $ \raw_indexPaths ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTreeController (mkSelector "removeSelectionIndexPaths:") retCULong [argPtr (castPtr raw_indexPaths :: Ptr ())]

-- | @- moveNode:toIndexPath:@
moveNode_toIndexPath :: (IsNSTreeController nsTreeController, IsNSTreeNode node, IsNSIndexPath indexPath) => nsTreeController -> node -> indexPath -> IO ()
moveNode_toIndexPath nsTreeController  node indexPath =
withObjCPtr node $ \raw_node ->
  withObjCPtr indexPath $ \raw_indexPath ->
      sendMsg nsTreeController (mkSelector "moveNode:toIndexPath:") retVoid [argPtr (castPtr raw_node :: Ptr ()), argPtr (castPtr raw_indexPath :: Ptr ())]

-- | @- moveNodes:toIndexPath:@
moveNodes_toIndexPath :: (IsNSTreeController nsTreeController, IsNSArray nodes, IsNSIndexPath startingIndexPath) => nsTreeController -> nodes -> startingIndexPath -> IO ()
moveNodes_toIndexPath nsTreeController  nodes startingIndexPath =
withObjCPtr nodes $ \raw_nodes ->
  withObjCPtr startingIndexPath $ \raw_startingIndexPath ->
      sendMsg nsTreeController (mkSelector "moveNodes:toIndexPath:") retVoid [argPtr (castPtr raw_nodes :: Ptr ()), argPtr (castPtr raw_startingIndexPath :: Ptr ())]

-- | @- childrenKeyPathForNode:@
childrenKeyPathForNode :: (IsNSTreeController nsTreeController, IsNSTreeNode node) => nsTreeController -> node -> IO (Id NSString)
childrenKeyPathForNode nsTreeController  node =
withObjCPtr node $ \raw_node ->
    sendMsg nsTreeController (mkSelector "childrenKeyPathForNode:") (retPtr retVoid) [argPtr (castPtr raw_node :: Ptr ())] >>= retainedObject . castPtr

-- | @- countKeyPathForNode:@
countKeyPathForNode :: (IsNSTreeController nsTreeController, IsNSTreeNode node) => nsTreeController -> node -> IO (Id NSString)
countKeyPathForNode nsTreeController  node =
withObjCPtr node $ \raw_node ->
    sendMsg nsTreeController (mkSelector "countKeyPathForNode:") (retPtr retVoid) [argPtr (castPtr raw_node :: Ptr ())] >>= retainedObject . castPtr

-- | @- leafKeyPathForNode:@
leafKeyPathForNode :: (IsNSTreeController nsTreeController, IsNSTreeNode node) => nsTreeController -> node -> IO (Id NSString)
leafKeyPathForNode nsTreeController  node =
withObjCPtr node $ \raw_node ->
    sendMsg nsTreeController (mkSelector "leafKeyPathForNode:") (retPtr retVoid) [argPtr (castPtr raw_node :: Ptr ())] >>= retainedObject . castPtr

-- | @- arrangedObjects@
arrangedObjects :: IsNSTreeController nsTreeController => nsTreeController -> IO (Id NSTreeNode)
arrangedObjects nsTreeController  =
  sendMsg nsTreeController (mkSelector "arrangedObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- childrenKeyPath@
childrenKeyPath :: IsNSTreeController nsTreeController => nsTreeController -> IO (Id NSString)
childrenKeyPath nsTreeController  =
  sendMsg nsTreeController (mkSelector "childrenKeyPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChildrenKeyPath:@
setChildrenKeyPath :: (IsNSTreeController nsTreeController, IsNSString value) => nsTreeController -> value -> IO ()
setChildrenKeyPath nsTreeController  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTreeController (mkSelector "setChildrenKeyPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- countKeyPath@
countKeyPath :: IsNSTreeController nsTreeController => nsTreeController -> IO (Id NSString)
countKeyPath nsTreeController  =
  sendMsg nsTreeController (mkSelector "countKeyPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCountKeyPath:@
setCountKeyPath :: (IsNSTreeController nsTreeController, IsNSString value) => nsTreeController -> value -> IO ()
setCountKeyPath nsTreeController  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTreeController (mkSelector "setCountKeyPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- leafKeyPath@
leafKeyPath :: IsNSTreeController nsTreeController => nsTreeController -> IO (Id NSString)
leafKeyPath nsTreeController  =
  sendMsg nsTreeController (mkSelector "leafKeyPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLeafKeyPath:@
setLeafKeyPath :: (IsNSTreeController nsTreeController, IsNSString value) => nsTreeController -> value -> IO ()
setLeafKeyPath nsTreeController  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTreeController (mkSelector "setLeafKeyPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sortDescriptors@
sortDescriptors :: IsNSTreeController nsTreeController => nsTreeController -> IO (Id NSArray)
sortDescriptors nsTreeController  =
  sendMsg nsTreeController (mkSelector "sortDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSortDescriptors:@
setSortDescriptors :: (IsNSTreeController nsTreeController, IsNSArray value) => nsTreeController -> value -> IO ()
setSortDescriptors nsTreeController  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTreeController (mkSelector "setSortDescriptors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- content@
content :: IsNSTreeController nsTreeController => nsTreeController -> IO RawId
content nsTreeController  =
  fmap (RawId . castPtr) $ sendMsg nsTreeController (mkSelector "content") (retPtr retVoid) []

-- | @- setContent:@
setContent :: IsNSTreeController nsTreeController => nsTreeController -> RawId -> IO ()
setContent nsTreeController  value =
  sendMsg nsTreeController (mkSelector "setContent:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- canInsert@
canInsert :: IsNSTreeController nsTreeController => nsTreeController -> IO Bool
canInsert nsTreeController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTreeController (mkSelector "canInsert") retCULong []

-- | @- canInsertChild@
canInsertChild :: IsNSTreeController nsTreeController => nsTreeController -> IO Bool
canInsertChild nsTreeController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTreeController (mkSelector "canInsertChild") retCULong []

-- | @- canAddChild@
canAddChild :: IsNSTreeController nsTreeController => nsTreeController -> IO Bool
canAddChild nsTreeController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTreeController (mkSelector "canAddChild") retCULong []

-- | @- avoidsEmptySelection@
avoidsEmptySelection :: IsNSTreeController nsTreeController => nsTreeController -> IO Bool
avoidsEmptySelection nsTreeController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTreeController (mkSelector "avoidsEmptySelection") retCULong []

-- | @- setAvoidsEmptySelection:@
setAvoidsEmptySelection :: IsNSTreeController nsTreeController => nsTreeController -> Bool -> IO ()
setAvoidsEmptySelection nsTreeController  value =
  sendMsg nsTreeController (mkSelector "setAvoidsEmptySelection:") retVoid [argCULong (if value then 1 else 0)]

-- | @- preservesSelection@
preservesSelection :: IsNSTreeController nsTreeController => nsTreeController -> IO Bool
preservesSelection nsTreeController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTreeController (mkSelector "preservesSelection") retCULong []

-- | @- setPreservesSelection:@
setPreservesSelection :: IsNSTreeController nsTreeController => nsTreeController -> Bool -> IO ()
setPreservesSelection nsTreeController  value =
  sendMsg nsTreeController (mkSelector "setPreservesSelection:") retVoid [argCULong (if value then 1 else 0)]

-- | @- selectsInsertedObjects@
selectsInsertedObjects :: IsNSTreeController nsTreeController => nsTreeController -> IO Bool
selectsInsertedObjects nsTreeController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTreeController (mkSelector "selectsInsertedObjects") retCULong []

-- | @- setSelectsInsertedObjects:@
setSelectsInsertedObjects :: IsNSTreeController nsTreeController => nsTreeController -> Bool -> IO ()
setSelectsInsertedObjects nsTreeController  value =
  sendMsg nsTreeController (mkSelector "setSelectsInsertedObjects:") retVoid [argCULong (if value then 1 else 0)]

-- | @- alwaysUsesMultipleValuesMarker@
alwaysUsesMultipleValuesMarker :: IsNSTreeController nsTreeController => nsTreeController -> IO Bool
alwaysUsesMultipleValuesMarker nsTreeController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTreeController (mkSelector "alwaysUsesMultipleValuesMarker") retCULong []

-- | @- setAlwaysUsesMultipleValuesMarker:@
setAlwaysUsesMultipleValuesMarker :: IsNSTreeController nsTreeController => nsTreeController -> Bool -> IO ()
setAlwaysUsesMultipleValuesMarker nsTreeController  value =
  sendMsg nsTreeController (mkSelector "setAlwaysUsesMultipleValuesMarker:") retVoid [argCULong (if value then 1 else 0)]

-- | @- selectedObjects@
selectedObjects :: IsNSTreeController nsTreeController => nsTreeController -> IO (Id NSArray)
selectedObjects nsTreeController  =
  sendMsg nsTreeController (mkSelector "selectedObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selectionIndexPaths@
selectionIndexPaths :: IsNSTreeController nsTreeController => nsTreeController -> IO (Id NSArray)
selectionIndexPaths nsTreeController  =
  sendMsg nsTreeController (mkSelector "selectionIndexPaths") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selectionIndexPath@
selectionIndexPath :: IsNSTreeController nsTreeController => nsTreeController -> IO (Id NSIndexPath)
selectionIndexPath nsTreeController  =
  sendMsg nsTreeController (mkSelector "selectionIndexPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rearrangeObjects@
rearrangeObjectsSelector :: Selector
rearrangeObjectsSelector = mkSelector "rearrangeObjects"

-- | @Selector@ for @add:@
addSelector :: Selector
addSelector = mkSelector "add:"

-- | @Selector@ for @remove:@
removeSelector :: Selector
removeSelector = mkSelector "remove:"

-- | @Selector@ for @addChild:@
addChildSelector :: Selector
addChildSelector = mkSelector "addChild:"

-- | @Selector@ for @insert:@
insertSelector :: Selector
insertSelector = mkSelector "insert:"

-- | @Selector@ for @insertChild:@
insertChildSelector :: Selector
insertChildSelector = mkSelector "insertChild:"

-- | @Selector@ for @insertObject:atArrangedObjectIndexPath:@
insertObject_atArrangedObjectIndexPathSelector :: Selector
insertObject_atArrangedObjectIndexPathSelector = mkSelector "insertObject:atArrangedObjectIndexPath:"

-- | @Selector@ for @insertObjects:atArrangedObjectIndexPaths:@
insertObjects_atArrangedObjectIndexPathsSelector :: Selector
insertObjects_atArrangedObjectIndexPathsSelector = mkSelector "insertObjects:atArrangedObjectIndexPaths:"

-- | @Selector@ for @removeObjectAtArrangedObjectIndexPath:@
removeObjectAtArrangedObjectIndexPathSelector :: Selector
removeObjectAtArrangedObjectIndexPathSelector = mkSelector "removeObjectAtArrangedObjectIndexPath:"

-- | @Selector@ for @removeObjectsAtArrangedObjectIndexPaths:@
removeObjectsAtArrangedObjectIndexPathsSelector :: Selector
removeObjectsAtArrangedObjectIndexPathsSelector = mkSelector "removeObjectsAtArrangedObjectIndexPaths:"

-- | @Selector@ for @setSelectionIndexPaths:@
setSelectionIndexPathsSelector :: Selector
setSelectionIndexPathsSelector = mkSelector "setSelectionIndexPaths:"

-- | @Selector@ for @setSelectionIndexPath:@
setSelectionIndexPathSelector :: Selector
setSelectionIndexPathSelector = mkSelector "setSelectionIndexPath:"

-- | @Selector@ for @addSelectionIndexPaths:@
addSelectionIndexPathsSelector :: Selector
addSelectionIndexPathsSelector = mkSelector "addSelectionIndexPaths:"

-- | @Selector@ for @removeSelectionIndexPaths:@
removeSelectionIndexPathsSelector :: Selector
removeSelectionIndexPathsSelector = mkSelector "removeSelectionIndexPaths:"

-- | @Selector@ for @moveNode:toIndexPath:@
moveNode_toIndexPathSelector :: Selector
moveNode_toIndexPathSelector = mkSelector "moveNode:toIndexPath:"

-- | @Selector@ for @moveNodes:toIndexPath:@
moveNodes_toIndexPathSelector :: Selector
moveNodes_toIndexPathSelector = mkSelector "moveNodes:toIndexPath:"

-- | @Selector@ for @childrenKeyPathForNode:@
childrenKeyPathForNodeSelector :: Selector
childrenKeyPathForNodeSelector = mkSelector "childrenKeyPathForNode:"

-- | @Selector@ for @countKeyPathForNode:@
countKeyPathForNodeSelector :: Selector
countKeyPathForNodeSelector = mkSelector "countKeyPathForNode:"

-- | @Selector@ for @leafKeyPathForNode:@
leafKeyPathForNodeSelector :: Selector
leafKeyPathForNodeSelector = mkSelector "leafKeyPathForNode:"

-- | @Selector@ for @arrangedObjects@
arrangedObjectsSelector :: Selector
arrangedObjectsSelector = mkSelector "arrangedObjects"

-- | @Selector@ for @childrenKeyPath@
childrenKeyPathSelector :: Selector
childrenKeyPathSelector = mkSelector "childrenKeyPath"

-- | @Selector@ for @setChildrenKeyPath:@
setChildrenKeyPathSelector :: Selector
setChildrenKeyPathSelector = mkSelector "setChildrenKeyPath:"

-- | @Selector@ for @countKeyPath@
countKeyPathSelector :: Selector
countKeyPathSelector = mkSelector "countKeyPath"

-- | @Selector@ for @setCountKeyPath:@
setCountKeyPathSelector :: Selector
setCountKeyPathSelector = mkSelector "setCountKeyPath:"

-- | @Selector@ for @leafKeyPath@
leafKeyPathSelector :: Selector
leafKeyPathSelector = mkSelector "leafKeyPath"

-- | @Selector@ for @setLeafKeyPath:@
setLeafKeyPathSelector :: Selector
setLeafKeyPathSelector = mkSelector "setLeafKeyPath:"

-- | @Selector@ for @sortDescriptors@
sortDescriptorsSelector :: Selector
sortDescriptorsSelector = mkSelector "sortDescriptors"

-- | @Selector@ for @setSortDescriptors:@
setSortDescriptorsSelector :: Selector
setSortDescriptorsSelector = mkSelector "setSortDescriptors:"

-- | @Selector@ for @content@
contentSelector :: Selector
contentSelector = mkSelector "content"

-- | @Selector@ for @setContent:@
setContentSelector :: Selector
setContentSelector = mkSelector "setContent:"

-- | @Selector@ for @canInsert@
canInsertSelector :: Selector
canInsertSelector = mkSelector "canInsert"

-- | @Selector@ for @canInsertChild@
canInsertChildSelector :: Selector
canInsertChildSelector = mkSelector "canInsertChild"

-- | @Selector@ for @canAddChild@
canAddChildSelector :: Selector
canAddChildSelector = mkSelector "canAddChild"

-- | @Selector@ for @avoidsEmptySelection@
avoidsEmptySelectionSelector :: Selector
avoidsEmptySelectionSelector = mkSelector "avoidsEmptySelection"

-- | @Selector@ for @setAvoidsEmptySelection:@
setAvoidsEmptySelectionSelector :: Selector
setAvoidsEmptySelectionSelector = mkSelector "setAvoidsEmptySelection:"

-- | @Selector@ for @preservesSelection@
preservesSelectionSelector :: Selector
preservesSelectionSelector = mkSelector "preservesSelection"

-- | @Selector@ for @setPreservesSelection:@
setPreservesSelectionSelector :: Selector
setPreservesSelectionSelector = mkSelector "setPreservesSelection:"

-- | @Selector@ for @selectsInsertedObjects@
selectsInsertedObjectsSelector :: Selector
selectsInsertedObjectsSelector = mkSelector "selectsInsertedObjects"

-- | @Selector@ for @setSelectsInsertedObjects:@
setSelectsInsertedObjectsSelector :: Selector
setSelectsInsertedObjectsSelector = mkSelector "setSelectsInsertedObjects:"

-- | @Selector@ for @alwaysUsesMultipleValuesMarker@
alwaysUsesMultipleValuesMarkerSelector :: Selector
alwaysUsesMultipleValuesMarkerSelector = mkSelector "alwaysUsesMultipleValuesMarker"

-- | @Selector@ for @setAlwaysUsesMultipleValuesMarker:@
setAlwaysUsesMultipleValuesMarkerSelector :: Selector
setAlwaysUsesMultipleValuesMarkerSelector = mkSelector "setAlwaysUsesMultipleValuesMarker:"

-- | @Selector@ for @selectedObjects@
selectedObjectsSelector :: Selector
selectedObjectsSelector = mkSelector "selectedObjects"

-- | @Selector@ for @selectionIndexPaths@
selectionIndexPathsSelector :: Selector
selectionIndexPathsSelector = mkSelector "selectionIndexPaths"

-- | @Selector@ for @selectionIndexPath@
selectionIndexPathSelector :: Selector
selectionIndexPathSelector = mkSelector "selectionIndexPath"

