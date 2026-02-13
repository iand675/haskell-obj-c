{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An R-tree is a data structure that partitions axis aligned bounding rectangles into groups spatially. When a group goes to large, it is split according to its split strategy into two new groups. Fast queries can be made on these partition bounding rectangles.
--
-- Generated bindings for @GKRTree@.
module ObjC.GameplayKit.GKRTree
  ( GKRTree
  , IsGKRTree(..)
  , treeWithMaxNumberOfChildren
  , initWithMaxNumberOfChildren
  , queryReserve
  , setQueryReserve
  , initWithMaxNumberOfChildrenSelector
  , queryReserveSelector
  , setQueryReserveSelector
  , treeWithMaxNumberOfChildrenSelector

  -- * Enum types
  , GKRTreeSplitStrategy(GKRTreeSplitStrategy)
  , pattern GKRTreeSplitStrategyHalve
  , pattern GKRTreeSplitStrategyLinear
  , pattern GKRTreeSplitStrategyQuadratic
  , pattern GKRTreeSplitStrategyReduceOverlap

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.GameplayKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates an RTree with a given maximum number of children per node.  Nodes that grow beyond this number of children will be split.
--
-- @maxNumberOfChildren@ — the maximum number of children per node before splitting
--
-- ObjC selector: @+ treeWithMaxNumberOfChildren:@
treeWithMaxNumberOfChildren :: CULong -> IO (Id GKRTree)
treeWithMaxNumberOfChildren maxNumberOfChildren =
  do
    cls' <- getRequiredClass "GKRTree"
    sendClassMessage cls' treeWithMaxNumberOfChildrenSelector maxNumberOfChildren

-- | @- initWithMaxNumberOfChildren:@
initWithMaxNumberOfChildren :: IsGKRTree gkrTree => gkrTree -> CULong -> IO (Id GKRTree)
initWithMaxNumberOfChildren gkrTree maxNumberOfChildren =
  sendOwnedMessage gkrTree initWithMaxNumberOfChildrenSelector maxNumberOfChildren

-- | Amount of array items to reserve before a query. This improves query performance at the cost of memory
--
-- ObjC selector: @- queryReserve@
queryReserve :: IsGKRTree gkrTree => gkrTree -> IO CULong
queryReserve gkrTree =
  sendMessage gkrTree queryReserveSelector

-- | Amount of array items to reserve before a query. This improves query performance at the cost of memory
--
-- ObjC selector: @- setQueryReserve:@
setQueryReserve :: IsGKRTree gkrTree => gkrTree -> CULong -> IO ()
setQueryReserve gkrTree value =
  sendMessage gkrTree setQueryReserveSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @treeWithMaxNumberOfChildren:@
treeWithMaxNumberOfChildrenSelector :: Selector '[CULong] (Id GKRTree)
treeWithMaxNumberOfChildrenSelector = mkSelector "treeWithMaxNumberOfChildren:"

-- | @Selector@ for @initWithMaxNumberOfChildren:@
initWithMaxNumberOfChildrenSelector :: Selector '[CULong] (Id GKRTree)
initWithMaxNumberOfChildrenSelector = mkSelector "initWithMaxNumberOfChildren:"

-- | @Selector@ for @queryReserve@
queryReserveSelector :: Selector '[] CULong
queryReserveSelector = mkSelector "queryReserve"

-- | @Selector@ for @setQueryReserve:@
setQueryReserveSelector :: Selector '[CULong] ()
setQueryReserveSelector = mkSelector "setQueryReserve:"

