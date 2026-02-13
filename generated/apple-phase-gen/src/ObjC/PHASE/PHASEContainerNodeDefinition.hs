{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEContainerNodeDefinition
--
-- An object for defining a container sound event node when building a sound event.
--
-- A container node plays back all its children at once.
--
-- Generated bindings for @PHASEContainerNodeDefinition@.
module ObjC.PHASE.PHASEContainerNodeDefinition
  ( PHASEContainerNodeDefinition
  , IsPHASEContainerNodeDefinition(..)
  , init_
  , new
  , initWithIdentifier
  , addSubtree
  , addSubtreeSelector
  , initSelector
  , initWithIdentifierSelector
  , newSelector


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
-- Create a container node definition
--
-- Returns: A new PHASEContainerNodeDefinition object
--
-- ObjC selector: @- init@
init_ :: IsPHASEContainerNodeDefinition phaseContainerNodeDefinition => phaseContainerNodeDefinition -> IO (Id PHASEContainerNodeDefinition)
init_ phaseContainerNodeDefinition =
  sendOwnedMessage phaseContainerNodeDefinition initSelector

-- | new
--
-- Create a container node definition
--
-- Returns: A new PHASEContainerNodeDefinition object
--
-- ObjC selector: @+ new@
new :: IO (Id PHASEContainerNodeDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASEContainerNodeDefinition"
    sendOwnedClassMessage cls' newSelector

-- | initWithIdentifier
--
-- Create a container node definition
--
-- @identifier@ — An optional custom identifier to give to this object
--
-- Returns: A new PHASEContainerNodeDefinition object
--
-- ObjC selector: @- initWithIdentifier:@
initWithIdentifier :: (IsPHASEContainerNodeDefinition phaseContainerNodeDefinition, IsNSString identifier) => phaseContainerNodeDefinition -> identifier -> IO (Id PHASEContainerNodeDefinition)
initWithIdentifier phaseContainerNodeDefinition identifier =
  sendOwnedMessage phaseContainerNodeDefinition initWithIdentifierSelector (toNSString identifier)

-- | addSubtree
--
-- Add a subtree to this node
--
-- @subtree@ — Add a subtree of PHASESoundEventNodeDefinition nodes beneath this node.
--
-- ObjC selector: @- addSubtree:@
addSubtree :: (IsPHASEContainerNodeDefinition phaseContainerNodeDefinition, IsPHASESoundEventNodeDefinition subtree) => phaseContainerNodeDefinition -> subtree -> IO ()
addSubtree phaseContainerNodeDefinition subtree =
  sendMessage phaseContainerNodeDefinition addSubtreeSelector (toPHASESoundEventNodeDefinition subtree)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEContainerNodeDefinition)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEContainerNodeDefinition)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector '[Id NSString] (Id PHASEContainerNodeDefinition)
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @addSubtree:@
addSubtreeSelector :: Selector '[Id PHASESoundEventNodeDefinition] ()
addSubtreeSelector = mkSelector "addSubtree:"

