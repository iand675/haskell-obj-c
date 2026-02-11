{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A tree data structure where each level has 8 children that subdivide a given space into the eight octants. Stores arbitrary NSObject elements via points and boxes.
--
-- Generated bindings for @GKOctree@.
module ObjC.GameplayKit.GKOctree
  ( GKOctree
  , IsGKOctree(..)
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

-- | Removes the given NSObject from this octree Note that this is an exhaustive search and is can be slow for larger trees. Cache the relevant GKOctreeNode and use removeElement:WithNode: for better performance.
--
-- @element@ — the element to be removed
--
-- Returns: returns YES if the data was removed, NO otherwise
--
-- ObjC selector: @- removeElement:@
removeElement :: (IsGKOctree gkOctree, IsNSObject element) => gkOctree -> element -> IO Bool
removeElement gkOctree  element =
withObjCPtr element $ \raw_element ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkOctree (mkSelector "removeElement:") retCULong [argPtr (castPtr raw_element :: Ptr ())]

-- | Removes the given NSObject from the given node Note that this is not an exhaustive search and is faster than removeData:
--
-- @element@ — the element to be removed
--
-- @node@ — the node in which this data resides
--
-- Returns: returns YES if the element was removed, NO otherwise
--
-- ObjC selector: @- removeElement:withNode:@
removeElement_withNode :: (IsGKOctree gkOctree, IsNSObject element, IsGKOctreeNode node) => gkOctree -> element -> node -> IO Bool
removeElement_withNode gkOctree  element node =
withObjCPtr element $ \raw_element ->
  withObjCPtr node $ \raw_node ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkOctree (mkSelector "removeElement:withNode:") retCULong [argPtr (castPtr raw_element :: Ptr ()), argPtr (castPtr raw_node :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @removeElement:@
removeElementSelector :: Selector
removeElementSelector = mkSelector "removeElement:"

-- | @Selector@ for @removeElement:withNode:@
removeElement_withNodeSelector :: Selector
removeElement_withNodeSelector = mkSelector "removeElement:withNode:"

