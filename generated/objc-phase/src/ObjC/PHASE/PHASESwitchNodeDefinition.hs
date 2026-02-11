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
  , initSelector
  , newSelector
  , initWithSwitchMetaParameterDefinition_identifierSelector
  , initWithSwitchMetaParameterDefinitionSelector
  , addSubtree_switchValueSelector
  , switchMetaParameterDefinitionSelector


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

-- | @- init@
init_ :: IsPHASESwitchNodeDefinition phaseSwitchNodeDefinition => phaseSwitchNodeDefinition -> IO (Id PHASESwitchNodeDefinition)
init_ phaseSwitchNodeDefinition  =
  sendMsg phaseSwitchNodeDefinition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASESwitchNodeDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASESwitchNodeDefinition"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithSwitchMetaParameterDefinition_identifier phaseSwitchNodeDefinition  switchMetaParameterDefinition identifier =
withObjCPtr switchMetaParameterDefinition $ \raw_switchMetaParameterDefinition ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg phaseSwitchNodeDefinition (mkSelector "initWithSwitchMetaParameterDefinition:identifier:") (retPtr retVoid) [argPtr (castPtr raw_switchMetaParameterDefinition :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

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
initWithSwitchMetaParameterDefinition phaseSwitchNodeDefinition  switchMetaParameterDefinition =
withObjCPtr switchMetaParameterDefinition $ \raw_switchMetaParameterDefinition ->
    sendMsg phaseSwitchNodeDefinition (mkSelector "initWithSwitchMetaParameterDefinition:") (retPtr retVoid) [argPtr (castPtr raw_switchMetaParameterDefinition :: Ptr ())] >>= ownedObject . castPtr

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
addSubtree_switchValue phaseSwitchNodeDefinition  subtree switchValue =
withObjCPtr subtree $ \raw_subtree ->
  withObjCPtr switchValue $ \raw_switchValue ->
      sendMsg phaseSwitchNodeDefinition (mkSelector "addSubtree:switchValue:") retVoid [argPtr (castPtr raw_subtree :: Ptr ()), argPtr (castPtr raw_switchValue :: Ptr ())]

-- | mixerDefinition
--
-- The readonly property that returns the PHASEMixerDefinition this sampler was created with and assigned to.
--
-- ObjC selector: @- switchMetaParameterDefinition@
switchMetaParameterDefinition :: IsPHASESwitchNodeDefinition phaseSwitchNodeDefinition => phaseSwitchNodeDefinition -> IO (Id PHASEStringMetaParameterDefinition)
switchMetaParameterDefinition phaseSwitchNodeDefinition  =
  sendMsg phaseSwitchNodeDefinition (mkSelector "switchMetaParameterDefinition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithSwitchMetaParameterDefinition:identifier:@
initWithSwitchMetaParameterDefinition_identifierSelector :: Selector
initWithSwitchMetaParameterDefinition_identifierSelector = mkSelector "initWithSwitchMetaParameterDefinition:identifier:"

-- | @Selector@ for @initWithSwitchMetaParameterDefinition:@
initWithSwitchMetaParameterDefinitionSelector :: Selector
initWithSwitchMetaParameterDefinitionSelector = mkSelector "initWithSwitchMetaParameterDefinition:"

-- | @Selector@ for @addSubtree:switchValue:@
addSubtree_switchValueSelector :: Selector
addSubtree_switchValueSelector = mkSelector "addSubtree:switchValue:"

-- | @Selector@ for @switchMetaParameterDefinition@
switchMetaParameterDefinitionSelector :: Selector
switchMetaParameterDefinitionSelector = mkSelector "switchMetaParameterDefinition"

