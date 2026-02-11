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
  , initWithAttributeSelector
  , initWithExamples_actions_attributesSelector
  , initWithURL_errorSelector
  , exportToURL_errorSelector
  , findActionForAnswersSelector
  , rootNodeSelector
  , randomSourceSelector
  , setRandomSourceSelector


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

-- | Initializes the decision tree with a root node containing the provided attribute
--
-- @attribute@ — The attribute to be contained at the root of the tree
--
-- Returns: GKDecisionTree with the set root
--
-- ObjC selector: @- initWithAttribute:@
initWithAttribute :: IsGKDecisionTree gkDecisionTree => gkDecisionTree -> RawId -> IO (Id GKDecisionTree)
initWithAttribute gkDecisionTree  attribute =
  sendMsg gkDecisionTree (mkSelector "initWithAttribute:") (retPtr retVoid) [argPtr (castPtr (unRawId attribute) :: Ptr ())] >>= ownedObject . castPtr

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
initWithExamples_actions_attributes gkDecisionTree  examples actions attributes =
withObjCPtr examples $ \raw_examples ->
  withObjCPtr actions $ \raw_actions ->
    withObjCPtr attributes $ \raw_attributes ->
        sendMsg gkDecisionTree (mkSelector "initWithExamples:actions:attributes:") (retPtr retVoid) [argPtr (castPtr raw_examples :: Ptr ()), argPtr (castPtr raw_actions :: Ptr ()), argPtr (castPtr raw_attributes :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes a decision tree from the contents of a file
--
-- @url@ — The URL from which the contents will be loaded
--
-- Returns: The instance of the decision tree constructed
--
-- ObjC selector: @- initWithURL:error:@
initWithURL_error :: (IsGKDecisionTree gkDecisionTree, IsNSURL url, IsNSError error_) => gkDecisionTree -> url -> error_ -> IO (Id GKDecisionTree)
initWithURL_error gkDecisionTree  url error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg gkDecisionTree (mkSelector "initWithURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Exports a decision tree to the given URL
--
-- @url@ — The URL to which the contents will be exported
--
-- Returns: The response indicating the status of the decision tree being successfully exported
--
-- ObjC selector: @- exportToURL:error:@
exportToURL_error :: (IsGKDecisionTree gkDecisionTree, IsNSURL url, IsNSError error_) => gkDecisionTree -> url -> error_ -> IO Bool
exportToURL_error gkDecisionTree  url error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkDecisionTree (mkSelector "exportToURL:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Will branch down from the root node to find the correct action attribute for the given collection of results and their respective attributes
--
-- @answers@ — The dictionary of attributes (keys) and their answers (values)
--
-- Returns: The attribute found by traversing the tree given the provided answers
--
-- ObjC selector: @- findActionForAnswers:@
findActionForAnswers :: (IsGKDecisionTree gkDecisionTree, IsNSDictionary answers) => gkDecisionTree -> answers -> IO RawId
findActionForAnswers gkDecisionTree  answers =
withObjCPtr answers $ \raw_answers ->
    fmap (RawId . castPtr) $ sendMsg gkDecisionTree (mkSelector "findActionForAnswers:") (retPtr retVoid) [argPtr (castPtr raw_answers :: Ptr ())]

-- | The node for the decision tree that all other nodes descend from
--
-- ObjC selector: @- rootNode@
rootNode :: IsGKDecisionTree gkDecisionTree => gkDecisionTree -> IO (Id GKDecisionNode)
rootNode gkDecisionTree  =
  sendMsg gkDecisionTree (mkSelector "rootNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The random source used by the decision tree when descending on a random branch This must be set before creating any weighted branches
--
-- See: GKDecisionNode
--
-- ObjC selector: @- randomSource@
randomSource :: IsGKDecisionTree gkDecisionTree => gkDecisionTree -> IO (Id GKRandomSource)
randomSource gkDecisionTree  =
  sendMsg gkDecisionTree (mkSelector "randomSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The random source used by the decision tree when descending on a random branch This must be set before creating any weighted branches
--
-- See: GKDecisionNode
--
-- ObjC selector: @- setRandomSource:@
setRandomSource :: (IsGKDecisionTree gkDecisionTree, IsGKRandomSource value) => gkDecisionTree -> value -> IO ()
setRandomSource gkDecisionTree  value =
withObjCPtr value $ \raw_value ->
    sendMsg gkDecisionTree (mkSelector "setRandomSource:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAttribute:@
initWithAttributeSelector :: Selector
initWithAttributeSelector = mkSelector "initWithAttribute:"

-- | @Selector@ for @initWithExamples:actions:attributes:@
initWithExamples_actions_attributesSelector :: Selector
initWithExamples_actions_attributesSelector = mkSelector "initWithExamples:actions:attributes:"

-- | @Selector@ for @initWithURL:error:@
initWithURL_errorSelector :: Selector
initWithURL_errorSelector = mkSelector "initWithURL:error:"

-- | @Selector@ for @exportToURL:error:@
exportToURL_errorSelector :: Selector
exportToURL_errorSelector = mkSelector "exportToURL:error:"

-- | @Selector@ for @findActionForAnswers:@
findActionForAnswersSelector :: Selector
findActionForAnswersSelector = mkSelector "findActionForAnswers:"

-- | @Selector@ for @rootNode@
rootNodeSelector :: Selector
rootNodeSelector = mkSelector "rootNode"

-- | @Selector@ for @randomSource@
randomSourceSelector :: Selector
randomSourceSelector = mkSelector "randomSource"

-- | @Selector@ for @setRandomSource:@
setRandomSourceSelector :: Selector
setRandomSourceSelector = mkSelector "setRandomSource:"

