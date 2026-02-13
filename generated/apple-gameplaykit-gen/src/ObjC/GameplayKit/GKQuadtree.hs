{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A tree data structure where each level has 4 children that subdivide a given space into the four quadrants. Stores arbitrary NSObject data via points and quads.
--
-- Generated bindings for @GKQuadtree@.
module ObjC.GameplayKit.GKQuadtree
  ( GKQuadtree
  , IsGKQuadtree(..)
  , removeElement
  , removeElement_withNode
  , removeElementSelector
  , removeElement_withNodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.GameplayKit.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | Removes the given NSObject from this quad tree. Note that this is an exhaustive search and is slow. Cache the relevant GKQuadTreeNode and use removeElement:WithNode: for better performance.
--
-- @element@ — the data to be removed
--
-- Returns: returns YES if the data was removed, NO otherwise
--
-- ObjC selector: @- removeElement:@
removeElement :: (IsGKQuadtree gkQuadtree, IsNSObject element) => gkQuadtree -> element -> IO Bool
removeElement gkQuadtree element =
  sendMessage gkQuadtree removeElementSelector (toNSObject element)

-- | Removes the given NSObject from the given quadtree node Note that this is not an exhaustive search and is faster than removeData:
--
-- @data@ — the data to be removed
--
-- @node@ — the node in which this data resides
--
-- Returns: returns YES if the data was removed, NO otherwise
--
-- ObjC selector: @- removeElement:withNode:@
removeElement_withNode :: (IsGKQuadtree gkQuadtree, IsNSObject data_, IsGKQuadtreeNode node) => gkQuadtree -> data_ -> node -> IO Bool
removeElement_withNode gkQuadtree data_ node =
  sendMessage gkQuadtree removeElement_withNodeSelector (toNSObject data_) (toGKQuadtreeNode node)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @removeElement:@
removeElementSelector :: Selector '[Id NSObject] Bool
removeElementSelector = mkSelector "removeElement:"

-- | @Selector@ for @removeElement:withNode:@
removeElement_withNodeSelector :: Selector '[Id NSObject, Id GKQuadtreeNode] Bool
removeElement_withNodeSelector = mkSelector "removeElement:withNode:"

