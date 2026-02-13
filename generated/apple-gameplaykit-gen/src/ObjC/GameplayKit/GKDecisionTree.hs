{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKDecisionTree@.
module ObjC.GameplayKit.GKDecisionTree
  ( GKDecisionTree
  , IsGKDecisionTree(..)
  , initWithAttribute
  , initWithExamples_actions_attributes
  , initWithURL_error
  , exportToURL_error
  , findActionForAnswers
  , rootNode
  , randomSource
  , setRandomSource
  , exportToURL_errorSelector
  , findActionForAnswersSelector
  , initWithAttributeSelector
  , initWithExamples_actions_attributesSelector
  , initWithURL_errorSelector
  , randomSourceSelector
  , rootNodeSelector
  , setRandomSourceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes the decision tree with a root node containing the provided attribute
--
-- @attribute@ — The attribute to be contained at the root of the tree
--
-- Returns: GKDecisionTree with the set root
--
-- ObjC selector: @- initWithAttribute:@
initWithAttribute :: IsGKDecisionTree gkDecisionTree => gkDecisionTree -> RawId -> IO (Id GKDecisionTree)
initWithAttribute gkDecisionTree attribute =
  sendOwnedMessage gkDecisionTree initWithAttributeSelector attribute

-- | Initializes and constructs a decision tree by learning from the provided examples & attributes
--
-- @examples@ — Must be an array of examples (with each example being a collection of the various attributes at a given state)
--
-- @actions@ — An array of the corresponding actions for each example. Ordered such that the first action matches with the first example in examples.
--
-- @attributes@ — The list of attributes. Ordered such that the first attribute matches with the first result in each example. So if we have two attributes: [distance, jump height], and two examples: [[20, 8], [15, 14]], and the resulting actions here: [Roll, Jump], we can think of this as a matrix:
--
-- distance| height            <-  Attributes           _______|_______          |       |       |          |  20   |   8   |  jump          |-------|-------|-------    <-  Results          |  15   |   14  |  roll          |_______|_______|                  ^                  |               Examples
--
-- Returns: GKDecisionTree created by learning from the provided examples for the provided attributes
--
-- ObjC selector: @- initWithExamples:actions:attributes:@
initWithExamples_actions_attributes :: (IsGKDecisionTree gkDecisionTree, IsNSArray examples, IsNSArray actions, IsNSArray attributes) => gkDecisionTree -> examples -> actions -> attributes -> IO (Id GKDecisionTree)
initWithExamples_actions_attributes gkDecisionTree examples actions attributes =
  sendOwnedMessage gkDecisionTree initWithExamples_actions_attributesSelector (toNSArray examples) (toNSArray actions) (toNSArray attributes)

-- | Initializes a decision tree from the contents of a file
--
-- @url@ — The URL from which the contents will be loaded
--
-- Returns: The instance of the decision tree constructed
--
-- ObjC selector: @- initWithURL:error:@
initWithURL_error :: (IsGKDecisionTree gkDecisionTree, IsNSURL url, IsNSError error_) => gkDecisionTree -> url -> error_ -> IO (Id GKDecisionTree)
initWithURL_error gkDecisionTree url error_ =
  sendOwnedMessage gkDecisionTree initWithURL_errorSelector (toNSURL url) (toNSError error_)

-- | Exports a decision tree to the given URL
--
-- @url@ — The URL to which the contents will be exported
--
-- Returns: The response indicating the status of the decision tree being successfully exported
--
-- ObjC selector: @- exportToURL:error:@
exportToURL_error :: (IsGKDecisionTree gkDecisionTree, IsNSURL url, IsNSError error_) => gkDecisionTree -> url -> error_ -> IO Bool
exportToURL_error gkDecisionTree url error_ =
  sendMessage gkDecisionTree exportToURL_errorSelector (toNSURL url) (toNSError error_)

-- | Will branch down from the root node to find the correct action attribute for the given collection of results and their respective attributes
--
-- @answers@ — The dictionary of attributes (keys) and their answers (values)
--
-- Returns: The attribute found by traversing the tree given the provided answers
--
-- ObjC selector: @- findActionForAnswers:@
findActionForAnswers :: (IsGKDecisionTree gkDecisionTree, IsNSDictionary answers) => gkDecisionTree -> answers -> IO RawId
findActionForAnswers gkDecisionTree answers =
  sendMessage gkDecisionTree findActionForAnswersSelector (toNSDictionary answers)

-- | The node for the decision tree that all other nodes descend from
--
-- ObjC selector: @- rootNode@
rootNode :: IsGKDecisionTree gkDecisionTree => gkDecisionTree -> IO (Id GKDecisionNode)
rootNode gkDecisionTree =
  sendMessage gkDecisionTree rootNodeSelector

-- | The random source used by the decision tree when descending on a random branch This must be set before creating any weighted branches
--
-- See: GKDecisionNode
--
-- ObjC selector: @- randomSource@
randomSource :: IsGKDecisionTree gkDecisionTree => gkDecisionTree -> IO (Id GKRandomSource)
randomSource gkDecisionTree =
  sendMessage gkDecisionTree randomSourceSelector

-- | The random source used by the decision tree when descending on a random branch This must be set before creating any weighted branches
--
-- See: GKDecisionNode
--
-- ObjC selector: @- setRandomSource:@
setRandomSource :: (IsGKDecisionTree gkDecisionTree, IsGKRandomSource value) => gkDecisionTree -> value -> IO ()
setRandomSource gkDecisionTree value =
  sendMessage gkDecisionTree setRandomSourceSelector (toGKRandomSource value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAttribute:@
initWithAttributeSelector :: Selector '[RawId] (Id GKDecisionTree)
initWithAttributeSelector = mkSelector "initWithAttribute:"

-- | @Selector@ for @initWithExamples:actions:attributes:@
initWithExamples_actions_attributesSelector :: Selector '[Id NSArray, Id NSArray, Id NSArray] (Id GKDecisionTree)
initWithExamples_actions_attributesSelector = mkSelector "initWithExamples:actions:attributes:"

-- | @Selector@ for @initWithURL:error:@
initWithURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id GKDecisionTree)
initWithURL_errorSelector = mkSelector "initWithURL:error:"

-- | @Selector@ for @exportToURL:error:@
exportToURL_errorSelector :: Selector '[Id NSURL, Id NSError] Bool
exportToURL_errorSelector = mkSelector "exportToURL:error:"

-- | @Selector@ for @findActionForAnswers:@
findActionForAnswersSelector :: Selector '[Id NSDictionary] RawId
findActionForAnswersSelector = mkSelector "findActionForAnswers:"

-- | @Selector@ for @rootNode@
rootNodeSelector :: Selector '[] (Id GKDecisionNode)
rootNodeSelector = mkSelector "rootNode"

-- | @Selector@ for @randomSource@
randomSourceSelector :: Selector '[] (Id GKRandomSource)
randomSourceSelector = mkSelector "randomSource"

-- | @Selector@ for @setRandomSource:@
setRandomSourceSelector :: Selector '[Id GKRandomSource] ()
setRandomSourceSelector = mkSelector "setRandomSource:"

