{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASERandomNodeDefinition
--
-- An object for defining a random sound event node when building a sound event.
--
-- A random node selects one of its children based on a weighted random choice.
--
-- Generated bindings for @PHASERandomNodeDefinition@.
module ObjC.PHASE.PHASERandomNodeDefinition
  ( PHASERandomNodeDefinition
  , IsPHASERandomNodeDefinition(..)
  , init_
  , initWithIdentifier
  , addSubtree_weight
  , uniqueSelectionQueueLength
  , setUniqueSelectionQueueLength
  , addSubtree_weightSelector
  , initSelector
  , initWithIdentifierSelector
  , setUniqueSelectionQueueLengthSelector
  , uniqueSelectionQueueLengthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | init
--
-- Create a random node definition
--
-- Returns: A new PHASERandomNodeDefinition object
--
-- ObjC selector: @- init@
init_ :: IsPHASERandomNodeDefinition phaseRandomNodeDefinition => phaseRandomNodeDefinition -> IO (Id PHASERandomNodeDefinition)
init_ phaseRandomNodeDefinition =
  sendOwnedMessage phaseRandomNodeDefinition initSelector

-- | initWithIdentifier
--
-- Create a random node definition
--
-- @identifier@ — An optional custom identifier to give to this object
--
-- Returns: A new PHASERandomNodeDefinition object
--
-- ObjC selector: @- initWithIdentifier:@
initWithIdentifier :: (IsPHASERandomNodeDefinition phaseRandomNodeDefinition, IsNSString identifier) => phaseRandomNodeDefinition -> identifier -> IO (Id PHASERandomNodeDefinition)
initWithIdentifier phaseRandomNodeDefinition identifier =
  sendOwnedMessage phaseRandomNodeDefinition initWithIdentifierSelector (toNSString identifier)

-- | addSubtree
--
-- Add a subtree to a random node
--
-- @subtree@ — A PHASESoundEventNodeDefinition that will be a child node of this random node
--
-- @weight@ — The probability weight of this subtree.  Higher numbers compared to other subtree weights will increase the likelihood of being        chosen. This value must be greater than or equal to 1, and is clamped otherwise.
--
-- ObjC selector: @- addSubtree:weight:@
addSubtree_weight :: (IsPHASERandomNodeDefinition phaseRandomNodeDefinition, IsPHASESoundEventNodeDefinition subtree, IsNSNumber weight) => phaseRandomNodeDefinition -> subtree -> weight -> IO ()
addSubtree_weight phaseRandomNodeDefinition subtree weight =
  sendMessage phaseRandomNodeDefinition addSubtree_weightSelector (toPHASESoundEventNodeDefinition subtree) (toNSNumber weight)

-- | uniqueSelectionQueueLength
--
-- Subtrees will not be repeated until after this random node is activated uniqueSelectionQueueLength number of times.
--
-- ObjC selector: @- uniqueSelectionQueueLength@
uniqueSelectionQueueLength :: IsPHASERandomNodeDefinition phaseRandomNodeDefinition => phaseRandomNodeDefinition -> IO CLong
uniqueSelectionQueueLength phaseRandomNodeDefinition =
  sendMessage phaseRandomNodeDefinition uniqueSelectionQueueLengthSelector

-- | uniqueSelectionQueueLength
--
-- Subtrees will not be repeated until after this random node is activated uniqueSelectionQueueLength number of times.
--
-- ObjC selector: @- setUniqueSelectionQueueLength:@
setUniqueSelectionQueueLength :: IsPHASERandomNodeDefinition phaseRandomNodeDefinition => phaseRandomNodeDefinition -> CLong -> IO ()
setUniqueSelectionQueueLength phaseRandomNodeDefinition value =
  sendMessage phaseRandomNodeDefinition setUniqueSelectionQueueLengthSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASERandomNodeDefinition)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector '[Id NSString] (Id PHASERandomNodeDefinition)
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @addSubtree:weight:@
addSubtree_weightSelector :: Selector '[Id PHASESoundEventNodeDefinition, Id NSNumber] ()
addSubtree_weightSelector = mkSelector "addSubtree:weight:"

-- | @Selector@ for @uniqueSelectionQueueLength@
uniqueSelectionQueueLengthSelector :: Selector '[] CLong
uniqueSelectionQueueLengthSelector = mkSelector "uniqueSelectionQueueLength"

-- | @Selector@ for @setUniqueSelectionQueueLength:@
setUniqueSelectionQueueLengthSelector :: Selector '[CLong] ()
setUniqueSelectionQueueLengthSelector = mkSelector "setUniqueSelectionQueueLength:"

