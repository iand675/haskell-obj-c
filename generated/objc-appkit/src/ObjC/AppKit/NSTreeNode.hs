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
  , treeNodeWithRepresentedObjectSelector
  , initWithRepresentedObjectSelector
  , descendantNodeAtIndexPathSelector
  , sortWithSortDescriptors_recursivelySelector
  , representedObjectSelector
  , indexPathSelector
  , leafSelector
  , childNodesSelector
  , mutableChildNodesSelector
  , parentNodeSelector


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

-- | @+ treeNodeWithRepresentedObject:@
treeNodeWithRepresentedObject :: RawId -> IO (Id NSTreeNode)
treeNodeWithRepresentedObject modelObject =
  do
    cls' <- getRequiredClass "NSTreeNode"
    sendClassMsg cls' (mkSelector "treeNodeWithRepresentedObject:") (retPtr retVoid) [argPtr (castPtr (unRawId modelObject) :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithRepresentedObject:@
initWithRepresentedObject :: IsNSTreeNode nsTreeNode => nsTreeNode -> RawId -> IO (Id NSTreeNode)
initWithRepresentedObject nsTreeNode  modelObject =
  sendMsg nsTreeNode (mkSelector "initWithRepresentedObject:") (retPtr retVoid) [argPtr (castPtr (unRawId modelObject) :: Ptr ())] >>= ownedObject . castPtr

-- | @- descendantNodeAtIndexPath:@
descendantNodeAtIndexPath :: (IsNSTreeNode nsTreeNode, IsNSIndexPath indexPath) => nsTreeNode -> indexPath -> IO (Id NSTreeNode)
descendantNodeAtIndexPath nsTreeNode  indexPath =
withObjCPtr indexPath $ \raw_indexPath ->
    sendMsg nsTreeNode (mkSelector "descendantNodeAtIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_indexPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- sortWithSortDescriptors:recursively:@
sortWithSortDescriptors_recursively :: (IsNSTreeNode nsTreeNode, IsNSArray sortDescriptors) => nsTreeNode -> sortDescriptors -> Bool -> IO ()
sortWithSortDescriptors_recursively nsTreeNode  sortDescriptors recursively =
withObjCPtr sortDescriptors $ \raw_sortDescriptors ->
    sendMsg nsTreeNode (mkSelector "sortWithSortDescriptors:recursively:") retVoid [argPtr (castPtr raw_sortDescriptors :: Ptr ()), argCULong (if recursively then 1 else 0)]

-- | @- representedObject@
representedObject :: IsNSTreeNode nsTreeNode => nsTreeNode -> IO RawId
representedObject nsTreeNode  =
  fmap (RawId . castPtr) $ sendMsg nsTreeNode (mkSelector "representedObject") (retPtr retVoid) []

-- | @- indexPath@
indexPath :: IsNSTreeNode nsTreeNode => nsTreeNode -> IO (Id NSIndexPath)
indexPath nsTreeNode  =
  sendMsg nsTreeNode (mkSelector "indexPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- leaf@
leaf :: IsNSTreeNode nsTreeNode => nsTreeNode -> IO Bool
leaf nsTreeNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTreeNode (mkSelector "leaf") retCULong []

-- | @- childNodes@
childNodes :: IsNSTreeNode nsTreeNode => nsTreeNode -> IO (Id NSArray)
childNodes nsTreeNode  =
  sendMsg nsTreeNode (mkSelector "childNodes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- mutableChildNodes@
mutableChildNodes :: IsNSTreeNode nsTreeNode => nsTreeNode -> IO (Id NSMutableArray)
mutableChildNodes nsTreeNode  =
  sendMsg nsTreeNode (mkSelector "mutableChildNodes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- parentNode@
parentNode :: IsNSTreeNode nsTreeNode => nsTreeNode -> IO (Id NSTreeNode)
parentNode nsTreeNode  =
  sendMsg nsTreeNode (mkSelector "parentNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @treeNodeWithRepresentedObject:@
treeNodeWithRepresentedObjectSelector :: Selector
treeNodeWithRepresentedObjectSelector = mkSelector "treeNodeWithRepresentedObject:"

-- | @Selector@ for @initWithRepresentedObject:@
initWithRepresentedObjectSelector :: Selector
initWithRepresentedObjectSelector = mkSelector "initWithRepresentedObject:"

-- | @Selector@ for @descendantNodeAtIndexPath:@
descendantNodeAtIndexPathSelector :: Selector
descendantNodeAtIndexPathSelector = mkSelector "descendantNodeAtIndexPath:"

-- | @Selector@ for @sortWithSortDescriptors:recursively:@
sortWithSortDescriptors_recursivelySelector :: Selector
sortWithSortDescriptors_recursivelySelector = mkSelector "sortWithSortDescriptors:recursively:"

-- | @Selector@ for @representedObject@
representedObjectSelector :: Selector
representedObjectSelector = mkSelector "representedObject"

-- | @Selector@ for @indexPath@
indexPathSelector :: Selector
indexPathSelector = mkSelector "indexPath"

-- | @Selector@ for @leaf@
leafSelector :: Selector
leafSelector = mkSelector "leaf"

-- | @Selector@ for @childNodes@
childNodesSelector :: Selector
childNodesSelector = mkSelector "childNodes"

-- | @Selector@ for @mutableChildNodes@
mutableChildNodesSelector :: Selector
mutableChildNodesSelector = mkSelector "mutableChildNodes"

-- | @Selector@ for @parentNode@
parentNodeSelector :: Selector
parentNodeSelector = mkSelector "parentNode"

