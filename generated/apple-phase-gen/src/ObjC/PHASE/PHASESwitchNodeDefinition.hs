{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASESwitchNodeDefinition
--
-- An object for defining a switch sound event node when building a sound event.
--
-- A switch node switches between its children based on a string parameter.
--
-- Generated bindings for @PHASESwitchNodeDefinition@.
module ObjC.PHASE.PHASESwitchNodeDefinition
  ( PHASESwitchNodeDefinition
  , IsPHASESwitchNodeDefinition(..)
  , init_
  , new
  , initWithSwitchMetaParameterDefinition_identifier
  , initWithSwitchMetaParameterDefinition
  , addSubtree_switchValue
  , switchMetaParameterDefinition
  , addSubtree_switchValueSelector
  , initSelector
  , initWithSwitchMetaParameterDefinitionSelector
  , initWithSwitchMetaParameterDefinition_identifierSelector
  , newSelector
  , switchMetaParameterDefinitionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASESwitchNodeDefinition phaseSwitchNodeDefinition => phaseSwitchNodeDefinition -> IO (Id PHASESwitchNodeDefinition)
init_ phaseSwitchNodeDefinition =
  sendOwnedMessage phaseSwitchNodeDefinition initSelector

-- | @+ new@
new :: IO (Id PHASESwitchNodeDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASESwitchNodeDefinition"
    sendOwnedClassMessage cls' newSelector

-- | initWithSwitchMetaParameterDefinition:identifier
--
-- Create a switch node definition
--
-- @switchMetaParameterDefinition@ — A metaparameter definition that wil be used to control the parameter of the switch node at runtime.
--
-- @identifier@ — An optional custom identifier to give to this object
--
-- Returns: A new PHASESwitchNodeDefinition object
--
-- ObjC selector: @- initWithSwitchMetaParameterDefinition:identifier:@
initWithSwitchMetaParameterDefinition_identifier :: (IsPHASESwitchNodeDefinition phaseSwitchNodeDefinition, IsPHASEStringMetaParameterDefinition switchMetaParameterDefinition, IsNSString identifier) => phaseSwitchNodeDefinition -> switchMetaParameterDefinition -> identifier -> IO (Id PHASESwitchNodeDefinition)
initWithSwitchMetaParameterDefinition_identifier phaseSwitchNodeDefinition switchMetaParameterDefinition identifier =
  sendOwnedMessage phaseSwitchNodeDefinition initWithSwitchMetaParameterDefinition_identifierSelector (toPHASEStringMetaParameterDefinition switchMetaParameterDefinition) (toNSString identifier)

-- | initWithSwitchMetaParameterDefinition
--
-- Create a switch node definition
--
-- @switchMetaParameterDefinition@ — A metaparameter definition that wil be used to control the parameter of the switch node at runtime.
--
-- Returns: A new PHASESwitchNodeDefinition object
--
-- ObjC selector: @- initWithSwitchMetaParameterDefinition:@
initWithSwitchMetaParameterDefinition :: (IsPHASESwitchNodeDefinition phaseSwitchNodeDefinition, IsPHASEStringMetaParameterDefinition switchMetaParameterDefinition) => phaseSwitchNodeDefinition -> switchMetaParameterDefinition -> IO (Id PHASESwitchNodeDefinition)
initWithSwitchMetaParameterDefinition phaseSwitchNodeDefinition switchMetaParameterDefinition =
  sendOwnedMessage phaseSwitchNodeDefinition initWithSwitchMetaParameterDefinitionSelector (toPHASEStringMetaParameterDefinition switchMetaParameterDefinition)

-- | addSubtree
--
-- Add a subtree to a switch node
--
-- @subtree@ — A PHASESoundEventNodeDefinition that will be a child node of this switch node
--
-- @switchValue@ — A string value that the metaparameter will use to activate this subtree
--
-- ObjC selector: @- addSubtree:switchValue:@
addSubtree_switchValue :: (IsPHASESwitchNodeDefinition phaseSwitchNodeDefinition, IsPHASESoundEventNodeDefinition subtree, IsNSString switchValue) => phaseSwitchNodeDefinition -> subtree -> switchValue -> IO ()
addSubtree_switchValue phaseSwitchNodeDefinition subtree switchValue =
  sendMessage phaseSwitchNodeDefinition addSubtree_switchValueSelector (toPHASESoundEventNodeDefinition subtree) (toNSString switchValue)

-- | mixerDefinition
--
-- The readonly property that returns the PHASEMixerDefinition this sampler was created with and assigned to.
--
-- ObjC selector: @- switchMetaParameterDefinition@
switchMetaParameterDefinition :: IsPHASESwitchNodeDefinition phaseSwitchNodeDefinition => phaseSwitchNodeDefinition -> IO (Id PHASEStringMetaParameterDefinition)
switchMetaParameterDefinition phaseSwitchNodeDefinition =
  sendMessage phaseSwitchNodeDefinition switchMetaParameterDefinitionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASESwitchNodeDefinition)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASESwitchNodeDefinition)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithSwitchMetaParameterDefinition:identifier:@
initWithSwitchMetaParameterDefinition_identifierSelector :: Selector '[Id PHASEStringMetaParameterDefinition, Id NSString] (Id PHASESwitchNodeDefinition)
initWithSwitchMetaParameterDefinition_identifierSelector = mkSelector "initWithSwitchMetaParameterDefinition:identifier:"

-- | @Selector@ for @initWithSwitchMetaParameterDefinition:@
initWithSwitchMetaParameterDefinitionSelector :: Selector '[Id PHASEStringMetaParameterDefinition] (Id PHASESwitchNodeDefinition)
initWithSwitchMetaParameterDefinitionSelector = mkSelector "initWithSwitchMetaParameterDefinition:"

-- | @Selector@ for @addSubtree:switchValue:@
addSubtree_switchValueSelector :: Selector '[Id PHASESoundEventNodeDefinition, Id NSString] ()
addSubtree_switchValueSelector = mkSelector "addSubtree:switchValue:"

-- | @Selector@ for @switchMetaParameterDefinition@
switchMetaParameterDefinitionSelector :: Selector '[] (Id PHASEStringMetaParameterDefinition)
switchMetaParameterDefinitionSelector = mkSelector "switchMetaParameterDefinition"

