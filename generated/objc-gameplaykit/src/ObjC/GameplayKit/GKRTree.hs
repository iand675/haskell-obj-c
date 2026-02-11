{-# LANGUAGE PatternSynonyms #-}
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
  , treeWithMaxNumberOfChildrenSelector
  , initWithMaxNumberOfChildrenSelector
  , queryReserveSelector
  , setQueryReserveSelector

  -- * Enum types
  , GKRTreeSplitStrategy(GKRTreeSplitStrategy)
  , pattern GKRTreeSplitStrategyHalve
  , pattern GKRTreeSplitStrategyLinear
  , pattern GKRTreeSplitStrategyQuadratic
  , pattern GKRTreeSplitStrategyReduceOverlap

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
    sendClassMsg cls' (mkSelector "treeWithMaxNumberOfChildren:") (retPtr retVoid) [argCULong (fromIntegral maxNumberOfChildren)] >>= retainedObject . castPtr

-- | @- initWithMaxNumberOfChildren:@
initWithMaxNumberOfChildren :: IsGKRTree gkrTree => gkrTree -> CULong -> IO (Id GKRTree)
initWithMaxNumberOfChildren gkrTree  maxNumberOfChildren =
  sendMsg gkrTree (mkSelector "initWithMaxNumberOfChildren:") (retPtr retVoid) [argCULong (fromIntegral maxNumberOfChildren)] >>= ownedObject . castPtr

-- | Amount of array items to reserve before a query. This improves query performance at the cost of memory
--
-- ObjC selector: @- queryReserve@
queryReserve :: IsGKRTree gkrTree => gkrTree -> IO CULong
queryReserve gkrTree  =
  sendMsg gkrTree (mkSelector "queryReserve") retCULong []

-- | Amount of array items to reserve before a query. This improves query performance at the cost of memory
--
-- ObjC selector: @- setQueryReserve:@
setQueryReserve :: IsGKRTree gkrTree => gkrTree -> CULong -> IO ()
setQueryReserve gkrTree  value =
  sendMsg gkrTree (mkSelector "setQueryReserve:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @treeWithMaxNumberOfChildren:@
treeWithMaxNumberOfChildrenSelector :: Selector
treeWithMaxNumberOfChildrenSelector = mkSelector "treeWithMaxNumberOfChildren:"

-- | @Selector@ for @initWithMaxNumberOfChildren:@
initWithMaxNumberOfChildrenSelector :: Selector
initWithMaxNumberOfChildrenSelector = mkSelector "initWithMaxNumberOfChildren:"

-- | @Selector@ for @queryReserve@
queryReserveSelector :: Selector
queryReserveSelector = mkSelector "queryReserve"

-- | @Selector@ for @setQueryReserve:@
setQueryReserveSelector :: Selector
setQueryReserveSelector = mkSelector "setQueryReserve:"

