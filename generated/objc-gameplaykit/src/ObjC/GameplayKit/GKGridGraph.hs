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
  , connectNodeToAdjacentNodesSelector
  , classForGenericArgumentAtIndexSelector
  , gridWidthSelector
  , gridHeightSelector
  , diagonalsAllowedSelector


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
import ObjC.Foundation.Internal.Classes

-- | Connects the given GKGridGraphNode to this graph by connecting it to it's adjacent nodes on the grid Input node must have coordinates within the rectangle specified by minCoordinates and maxCoordinates
--
-- @node@ — the node to be connected
--
-- ObjC selector: @- connectNodeToAdjacentNodes:@
connectNodeToAdjacentNodes :: (IsGKGridGraph gkGridGraph, IsGKGridGraphNode node) => gkGridGraph -> node -> IO ()
connectNodeToAdjacentNodes gkGridGraph  node =
withObjCPtr node $ \raw_node ->
    sendMsg gkGridGraph (mkSelector "connectNodeToAdjacentNodes:") retVoid [argPtr (castPtr raw_node :: Ptr ())]

-- | Returns the class of the specified generic index
--
-- ObjC selector: @- classForGenericArgumentAtIndex:@
classForGenericArgumentAtIndex :: IsGKGridGraph gkGridGraph => gkGridGraph -> CULong -> IO Class
classForGenericArgumentAtIndex gkGridGraph  index =
  fmap (Class . castPtr) $ sendMsg gkGridGraph (mkSelector "classForGenericArgumentAtIndex:") (retPtr retVoid) [argCULong (fromIntegral index)]

-- | @- gridWidth@
gridWidth :: IsGKGridGraph gkGridGraph => gkGridGraph -> IO CULong
gridWidth gkGridGraph  =
  sendMsg gkGridGraph (mkSelector "gridWidth") retCULong []

-- | @- gridHeight@
gridHeight :: IsGKGridGraph gkGridGraph => gkGridGraph -> IO CULong
gridHeight gkGridGraph  =
  sendMsg gkGridGraph (mkSelector "gridHeight") retCULong []

-- | @- diagonalsAllowed@
diagonalsAllowed :: IsGKGridGraph gkGridGraph => gkGridGraph -> IO Bool
diagonalsAllowed gkGridGraph  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkGridGraph (mkSelector "diagonalsAllowed") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connectNodeToAdjacentNodes:@
connectNodeToAdjacentNodesSelector :: Selector
connectNodeToAdjacentNodesSelector = mkSelector "connectNodeToAdjacentNodes:"

-- | @Selector@ for @classForGenericArgumentAtIndex:@
classForGenericArgumentAtIndexSelector :: Selector
classForGenericArgumentAtIndexSelector = mkSelector "classForGenericArgumentAtIndex:"

-- | @Selector@ for @gridWidth@
gridWidthSelector :: Selector
gridWidthSelector = mkSelector "gridWidth"

-- | @Selector@ for @gridHeight@
gridHeightSelector :: Selector
gridHeightSelector = mkSelector "gridHeight"

-- | @Selector@ for @diagonalsAllowed@
diagonalsAllowedSelector :: Selector
diagonalsAllowedSelector = mkSelector "diagonalsAllowed"

