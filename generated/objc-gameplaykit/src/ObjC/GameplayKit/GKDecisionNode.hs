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
  , createBranchWithValue_attributeSelector
  , createBranchWithPredicate_attributeSelector
  , createBranchWithWeight_attributeSelector


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
createBranchWithValue_attribute gkDecisionNode  value attribute =
withObjCPtr value $ \raw_value ->
    sendMsg gkDecisionNode (mkSelector "createBranchWithValue:attribute:") (retPtr retVoid) [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr (unRawId attribute) :: Ptr ())] >>= retainedObject . castPtr

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
createBranchWithPredicate_attribute gkDecisionNode  predicate attribute =
withObjCPtr predicate $ \raw_predicate ->
    sendMsg gkDecisionNode (mkSelector "createBranchWithPredicate:attribute:") (retPtr retVoid) [argPtr (castPtr raw_predicate :: Ptr ()), argPtr (castPtr (unRawId attribute) :: Ptr ())] >>= retainedObject . castPtr

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
createBranchWithWeight_attribute gkDecisionNode  weight attribute =
  sendMsg gkDecisionNode (mkSelector "createBranchWithWeight:attribute:") (retPtr retVoid) [argCLong (fromIntegral weight), argPtr (castPtr (unRawId attribute) :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createBranchWithValue:attribute:@
createBranchWithValue_attributeSelector :: Selector
createBranchWithValue_attributeSelector = mkSelector "createBranchWithValue:attribute:"

-- | @Selector@ for @createBranchWithPredicate:attribute:@
createBranchWithPredicate_attributeSelector :: Selector
createBranchWithPredicate_attributeSelector = mkSelector "createBranchWithPredicate:attribute:"

-- | @Selector@ for @createBranchWithWeight:attribute:@
createBranchWithWeight_attributeSelector :: Selector
createBranchWithWeight_attributeSelector = mkSelector "createBranchWithWeight:attribute:"

