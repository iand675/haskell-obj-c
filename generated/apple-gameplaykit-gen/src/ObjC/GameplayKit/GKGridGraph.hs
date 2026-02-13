{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKGridGraph@.
module ObjC.GameplayKit.GKGridGraph
  ( GKGridGraph
  , IsGKGridGraph(..)
  , connectNodeToAdjacentNodes
  , classForGenericArgumentAtIndex
  , gridWidth
  , gridHeight
  , diagonalsAllowed
  , classForGenericArgumentAtIndexSelector
  , connectNodeToAdjacentNodesSelector
  , diagonalsAllowedSelector
  , gridHeightSelector
  , gridWidthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Connects the given GKGridGraphNode to this graph by connecting it to it's adjacent nodes on the grid Input node must have coordinates within the rectangle specified by minCoordinates and maxCoordinates
--
-- @node@ — the node to be connected
--
-- ObjC selector: @- connectNodeToAdjacentNodes:@
connectNodeToAdjacentNodes :: (IsGKGridGraph gkGridGraph, IsGKGridGraphNode node) => gkGridGraph -> node -> IO ()
connectNodeToAdjacentNodes gkGridGraph node =
  sendMessage gkGridGraph connectNodeToAdjacentNodesSelector (toGKGridGraphNode node)

-- | Returns the class of the specified generic index
--
-- ObjC selector: @- classForGenericArgumentAtIndex:@
classForGenericArgumentAtIndex :: IsGKGridGraph gkGridGraph => gkGridGraph -> CULong -> IO Class
classForGenericArgumentAtIndex gkGridGraph index =
  sendMessage gkGridGraph classForGenericArgumentAtIndexSelector index

-- | @- gridWidth@
gridWidth :: IsGKGridGraph gkGridGraph => gkGridGraph -> IO CULong
gridWidth gkGridGraph =
  sendMessage gkGridGraph gridWidthSelector

-- | @- gridHeight@
gridHeight :: IsGKGridGraph gkGridGraph => gkGridGraph -> IO CULong
gridHeight gkGridGraph =
  sendMessage gkGridGraph gridHeightSelector

-- | @- diagonalsAllowed@
diagonalsAllowed :: IsGKGridGraph gkGridGraph => gkGridGraph -> IO Bool
diagonalsAllowed gkGridGraph =
  sendMessage gkGridGraph diagonalsAllowedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connectNodeToAdjacentNodes:@
connectNodeToAdjacentNodesSelector :: Selector '[Id GKGridGraphNode] ()
connectNodeToAdjacentNodesSelector = mkSelector "connectNodeToAdjacentNodes:"

-- | @Selector@ for @classForGenericArgumentAtIndex:@
classForGenericArgumentAtIndexSelector :: Selector '[CULong] Class
classForGenericArgumentAtIndexSelector = mkSelector "classForGenericArgumentAtIndex:"

-- | @Selector@ for @gridWidth@
gridWidthSelector :: Selector '[] CULong
gridWidthSelector = mkSelector "gridWidth"

-- | @Selector@ for @gridHeight@
gridHeightSelector :: Selector '[] CULong
gridHeightSelector = mkSelector "gridHeight"

-- | @Selector@ for @diagonalsAllowed@
diagonalsAllowedSelector :: Selector '[] Bool
diagonalsAllowedSelector = mkSelector "diagonalsAllowed"

