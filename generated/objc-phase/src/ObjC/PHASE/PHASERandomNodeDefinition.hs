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
  , initSelector
  , initWithIdentifierSelector
  , addSubtree_weightSelector
  , uniqueSelectionQueueLengthSelector
  , setUniqueSelectionQueueLengthSelector


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
init_ phaseRandomNodeDefinition  =
  sendMsg phaseRandomNodeDefinition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithIdentifier phaseRandomNodeDefinition  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg phaseRandomNodeDefinition (mkSelector "initWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

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
addSubtree_weight phaseRandomNodeDefinition  subtree weight =
withObjCPtr subtree $ \raw_subtree ->
  withObjCPtr weight $ \raw_weight ->
      sendMsg phaseRandomNodeDefinition (mkSelector "addSubtree:weight:") retVoid [argPtr (castPtr raw_subtree :: Ptr ()), argPtr (castPtr raw_weight :: Ptr ())]

-- | uniqueSelectionQueueLength
--
-- Subtrees will not be repeated until after this random node is activated uniqueSelectionQueueLength number of times.
--
-- ObjC selector: @- uniqueSelectionQueueLength@
uniqueSelectionQueueLength :: IsPHASERandomNodeDefinition phaseRandomNodeDefinition => phaseRandomNodeDefinition -> IO CLong
uniqueSelectionQueueLength phaseRandomNodeDefinition  =
  sendMsg phaseRandomNodeDefinition (mkSelector "uniqueSelectionQueueLength") retCLong []

-- | uniqueSelectionQueueLength
--
-- Subtrees will not be repeated until after this random node is activated uniqueSelectionQueueLength number of times.
--
-- ObjC selector: @- setUniqueSelectionQueueLength:@
setUniqueSelectionQueueLength :: IsPHASERandomNodeDefinition phaseRandomNodeDefinition => phaseRandomNodeDefinition -> CLong -> IO ()
setUniqueSelectionQueueLength phaseRandomNodeDefinition  value =
  sendMsg phaseRandomNodeDefinition (mkSelector "setUniqueSelectionQueueLength:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @addSubtree:weight:@
addSubtree_weightSelector :: Selector
addSubtree_weightSelector = mkSelector "addSubtree:weight:"

-- | @Selector@ for @uniqueSelectionQueueLength@
uniqueSelectionQueueLengthSelector :: Selector
uniqueSelectionQueueLengthSelector = mkSelector "uniqueSelectionQueueLength"

-- | @Selector@ for @setUniqueSelectionQueueLength:@
setUniqueSelectionQueueLengthSelector :: Selector
setUniqueSelectionQueueLengthSelector = mkSelector "setUniqueSelectionQueueLength:"

