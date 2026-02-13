{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKDecisionNode@.
module ObjC.GameplayKit.GKDecisionNode
  ( GKDecisionNode
  , IsGKDecisionNode(..)
  , createBranchWithValue_attribute
  , createBranchWithPredicate_attribute
  , createBranchWithWeight_attribute
  , createBranchWithPredicate_attributeSelector
  , createBranchWithValue_attributeSelector
  , createBranchWithWeight_attributeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a numeric branch to a node containing the specified attribute
--
-- @value@ — The value to create a branch with
--
-- @attribute@ — The attribute of the created node
--
-- Returns: The node lead to by the branch
--
-- ObjC selector: @- createBranchWithValue:attribute:@
createBranchWithValue_attribute :: (IsGKDecisionNode gkDecisionNode, IsNSNumber value) => gkDecisionNode -> value -> RawId -> IO (Id GKDecisionNode)
createBranchWithValue_attribute gkDecisionNode value attribute =
  sendMessage gkDecisionNode createBranchWithValue_attributeSelector (toNSNumber value) attribute

-- | Creates a predicated branch to a node containing the specified attribute
--
-- @predicate@ — The predicate to create a branch with
--
-- @attribute@ — The attribute of the created node
--
-- Returns: The node lead to by the branch
--
-- ObjC selector: @- createBranchWithPredicate:attribute:@
createBranchWithPredicate_attribute :: (IsGKDecisionNode gkDecisionNode, IsNSPredicate predicate) => gkDecisionNode -> predicate -> RawId -> IO (Id GKDecisionNode)
createBranchWithPredicate_attribute gkDecisionNode predicate attribute =
  sendMessage gkDecisionNode createBranchWithPredicate_attributeSelector (toNSPredicate predicate) attribute

-- | Creates a random branch to a node containing the specified attribute
--
-- @weight@ — The weight to create a branch with (weighted for random selection)
--
-- @attribute@ — The attribute of the created node
--
-- Returns: The node lead to by the branch
--
-- See: GKDecisionTree
--
-- ObjC selector: @- createBranchWithWeight:attribute:@
createBranchWithWeight_attribute :: IsGKDecisionNode gkDecisionNode => gkDecisionNode -> CLong -> RawId -> IO (Id GKDecisionNode)
createBranchWithWeight_attribute gkDecisionNode weight attribute =
  sendMessage gkDecisionNode createBranchWithWeight_attributeSelector weight attribute

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createBranchWithValue:attribute:@
createBranchWithValue_attributeSelector :: Selector '[Id NSNumber, RawId] (Id GKDecisionNode)
createBranchWithValue_attributeSelector = mkSelector "createBranchWithValue:attribute:"

-- | @Selector@ for @createBranchWithPredicate:attribute:@
createBranchWithPredicate_attributeSelector :: Selector '[Id NSPredicate, RawId] (Id GKDecisionNode)
createBranchWithPredicate_attributeSelector = mkSelector "createBranchWithPredicate:attribute:"

-- | @Selector@ for @createBranchWithWeight:attribute:@
createBranchWithWeight_attributeSelector :: Selector '[CLong, RawId] (Id GKDecisionNode)
createBranchWithWeight_attributeSelector = mkSelector "createBranchWithWeight:attribute:"

