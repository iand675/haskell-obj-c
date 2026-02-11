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
removeElement gkQuadtree  element =
withObjCPtr element $ \raw_element ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkQuadtree (mkSelector "removeElement:") retCULong [argPtr (castPtr raw_element :: Ptr ())]

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
removeElement_withNode gkQuadtree  data_ node =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr node $ \raw_node ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkQuadtree (mkSelector "removeElement:withNode:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_node :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @removeElement:@
removeElementSelector :: Selector
removeElementSelector = mkSelector "removeElement:"

-- | @Selector@ for @removeElement:withNode:@
removeElement_withNodeSelector :: Selector
removeElement_withNodeSelector = mkSelector "removeElement:withNode:"

