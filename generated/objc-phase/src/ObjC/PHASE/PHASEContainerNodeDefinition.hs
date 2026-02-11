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
  , initSelector
  , newSelector
  , initWithIdentifierSelector
  , addSubtreeSelector


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
-- Create a container node definition
--
-- Returns: A new PHASEContainerNodeDefinition object
--
-- ObjC selector: @- init@
init_ :: IsPHASEContainerNodeDefinition phaseContainerNodeDefinition => phaseContainerNodeDefinition -> IO (Id PHASEContainerNodeDefinition)
init_ phaseContainerNodeDefinition  =
  sendMsg phaseContainerNodeDefinition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithIdentifier phaseContainerNodeDefinition  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg phaseContainerNodeDefinition (mkSelector "initWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | addSubtree
--
-- Add a subtree to this node
--
-- @subtree@ — Add a subtree of PHASESoundEventNodeDefinition nodes beneath this node.
--
-- ObjC selector: @- addSubtree:@
addSubtree :: (IsPHASEContainerNodeDefinition phaseContainerNodeDefinition, IsPHASESoundEventNodeDefinition subtree) => phaseContainerNodeDefinition -> subtree -> IO ()
addSubtree phaseContainerNodeDefinition  subtree =
withObjCPtr subtree $ \raw_subtree ->
    sendMsg phaseContainerNodeDefinition (mkSelector "addSubtree:") retVoid [argPtr (castPtr raw_subtree :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @addSubtree:@
addSubtreeSelector :: Selector
addSubtreeSelector = mkSelector "addSubtree:"

