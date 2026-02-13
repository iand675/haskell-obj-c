{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTreeNode@.
module ObjC.AppKit.NSTreeNode
  ( NSTreeNode
  , IsNSTreeNode(..)
  , treeNodeWithRepresentedObject
  , initWithRepresentedObject
  , descendantNodeAtIndexPath
  , sortWithSortDescriptors_recursively
  , representedObject
  , indexPath
  , leaf
  , childNodes
  , mutableChildNodes
  , parentNode
  , childNodesSelector
  , descendantNodeAtIndexPathSelector
  , indexPathSelector
  , initWithRepresentedObjectSelector
  , leafSelector
  , mutableChildNodesSelector
  , parentNodeSelector
  , representedObjectSelector
  , sortWithSortDescriptors_recursivelySelector
  , treeNodeWithRepresentedObjectSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ treeNodeWithRepresentedObject:@
treeNodeWithRepresentedObject :: RawId -> IO (Id NSTreeNode)
treeNodeWithRepresentedObject modelObject =
  do
    cls' <- getRequiredClass "NSTreeNode"
    sendClassMessage cls' treeNodeWithRepresentedObjectSelector modelObject

-- | @- initWithRepresentedObject:@
initWithRepresentedObject :: IsNSTreeNode nsTreeNode => nsTreeNode -> RawId -> IO (Id NSTreeNode)
initWithRepresentedObject nsTreeNode modelObject =
  sendOwnedMessage nsTreeNode initWithRepresentedObjectSelector modelObject

-- | @- descendantNodeAtIndexPath:@
descendantNodeAtIndexPath :: (IsNSTreeNode nsTreeNode, IsNSIndexPath indexPath) => nsTreeNode -> indexPath -> IO (Id NSTreeNode)
descendantNodeAtIndexPath nsTreeNode indexPath =
  sendMessage nsTreeNode descendantNodeAtIndexPathSelector (toNSIndexPath indexPath)

-- | @- sortWithSortDescriptors:recursively:@
sortWithSortDescriptors_recursively :: (IsNSTreeNode nsTreeNode, IsNSArray sortDescriptors) => nsTreeNode -> sortDescriptors -> Bool -> IO ()
sortWithSortDescriptors_recursively nsTreeNode sortDescriptors recursively =
  sendMessage nsTreeNode sortWithSortDescriptors_recursivelySelector (toNSArray sortDescriptors) recursively

-- | @- representedObject@
representedObject :: IsNSTreeNode nsTreeNode => nsTreeNode -> IO RawId
representedObject nsTreeNode =
  sendMessage nsTreeNode representedObjectSelector

-- | @- indexPath@
indexPath :: IsNSTreeNode nsTreeNode => nsTreeNode -> IO (Id NSIndexPath)
indexPath nsTreeNode =
  sendMessage nsTreeNode indexPathSelector

-- | @- leaf@
leaf :: IsNSTreeNode nsTreeNode => nsTreeNode -> IO Bool
leaf nsTreeNode =
  sendMessage nsTreeNode leafSelector

-- | @- childNodes@
childNodes :: IsNSTreeNode nsTreeNode => nsTreeNode -> IO (Id NSArray)
childNodes nsTreeNode =
  sendMessage nsTreeNode childNodesSelector

-- | @- mutableChildNodes@
mutableChildNodes :: IsNSTreeNode nsTreeNode => nsTreeNode -> IO (Id NSMutableArray)
mutableChildNodes nsTreeNode =
  sendMessage nsTreeNode mutableChildNodesSelector

-- | @- parentNode@
parentNode :: IsNSTreeNode nsTreeNode => nsTreeNode -> IO (Id NSTreeNode)
parentNode nsTreeNode =
  sendMessage nsTreeNode parentNodeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @treeNodeWithRepresentedObject:@
treeNodeWithRepresentedObjectSelector :: Selector '[RawId] (Id NSTreeNode)
treeNodeWithRepresentedObjectSelector = mkSelector "treeNodeWithRepresentedObject:"

-- | @Selector@ for @initWithRepresentedObject:@
initWithRepresentedObjectSelector :: Selector '[RawId] (Id NSTreeNode)
initWithRepresentedObjectSelector = mkSelector "initWithRepresentedObject:"

-- | @Selector@ for @descendantNodeAtIndexPath:@
descendantNodeAtIndexPathSelector :: Selector '[Id NSIndexPath] (Id NSTreeNode)
descendantNodeAtIndexPathSelector = mkSelector "descendantNodeAtIndexPath:"

-- | @Selector@ for @sortWithSortDescriptors:recursively:@
sortWithSortDescriptors_recursivelySelector :: Selector '[Id NSArray, Bool] ()
sortWithSortDescriptors_recursivelySelector = mkSelector "sortWithSortDescriptors:recursively:"

-- | @Selector@ for @representedObject@
representedObjectSelector :: Selector '[] RawId
representedObjectSelector = mkSelector "representedObject"

-- | @Selector@ for @indexPath@
indexPathSelector :: Selector '[] (Id NSIndexPath)
indexPathSelector = mkSelector "indexPath"

-- | @Selector@ for @leaf@
leafSelector :: Selector '[] Bool
leafSelector = mkSelector "leaf"

-- | @Selector@ for @childNodes@
childNodesSelector :: Selector '[] (Id NSArray)
childNodesSelector = mkSelector "childNodes"

-- | @Selector@ for @mutableChildNodes@
mutableChildNodesSelector :: Selector '[] (Id NSMutableArray)
mutableChildNodesSelector = mkSelector "mutableChildNodes"

-- | @Selector@ for @parentNode@
parentNodeSelector :: Selector '[] (Id NSTreeNode)
parentNodeSelector = mkSelector "parentNode"

