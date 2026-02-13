{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASENumberMetaParameterDefinition
--
-- A metaparameter that has a numeric value
--
-- Generated bindings for @PHASENumberMetaParameterDefinition@.
module ObjC.PHASE.PHASENumberMetaParameterDefinition
  ( PHASENumberMetaParameterDefinition
  , IsPHASENumberMetaParameterDefinition(..)
  , init_
  , new
  , initWithValue_identifier
  , initWithValue
  , initWithValue_minimum_maximum_identifier
  , initWithValue_minimum_maximum
  , minimum_
  , maximum_
  , initSelector
  , initWithValueSelector
  , initWithValue_identifierSelector
  , initWithValue_minimum_maximumSelector
  , initWithValue_minimum_maximum_identifierSelector
  , maximumSelector
  , minimumSelector
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

-- | @- init@
init_ :: IsPHASENumberMetaParameterDefinition phaseNumberMetaParameterDefinition => phaseNumberMetaParameterDefinition -> IO (Id PHASENumberMetaParameterDefinition)
init_ phaseNumberMetaParameterDefinition =
  sendOwnedMessage phaseNumberMetaParameterDefinition initSelector

-- | @+ new@
new :: IO (Id PHASENumberMetaParameterDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASENumberMetaParameterDefinition"
    sendOwnedClassMessage cls' newSelector

-- | initWithValue:identifier
--
-- Create a new numeric metaparameter definition
--
-- @value@ — The initial value of the metaparameter
--
-- @identifier@ — An optional custom identifier to give to this object
--
-- Returns: The new PHASENumberMetaParameterDefinition object
--
-- ObjC selector: @- initWithValue:identifier:@
initWithValue_identifier :: (IsPHASENumberMetaParameterDefinition phaseNumberMetaParameterDefinition, IsNSString identifier) => phaseNumberMetaParameterDefinition -> CDouble -> identifier -> IO (Id PHASENumberMetaParameterDefinition)
initWithValue_identifier phaseNumberMetaParameterDefinition value identifier =
  sendOwnedMessage phaseNumberMetaParameterDefinition initWithValue_identifierSelector value (toNSString identifier)

-- | initWithValue
--
-- Create a new numeric metaparameter definition
--
-- @value@ — The initial value of the metaparameter
--
-- Returns: The new PHASENumberMetaParameterDefinition object
--
-- ObjC selector: @- initWithValue:@
initWithValue :: IsPHASENumberMetaParameterDefinition phaseNumberMetaParameterDefinition => phaseNumberMetaParameterDefinition -> CDouble -> IO (Id PHASENumberMetaParameterDefinition)
initWithValue phaseNumberMetaParameterDefinition value =
  sendOwnedMessage phaseNumberMetaParameterDefinition initWithValueSelector value

-- | initWithValue:minimum:maximum:identifier
--
-- Create a new numeric metaparameter definition and a predefined min and maximum range
--
-- @value@ — The initial value of the metaparameter
--
-- @minimum@ — The minimum value for this metaparameter.  Values set to this metaparamter less than the minimum will be clamped.
--
-- @maximum@ — The maximum value for this metaparameter.  Values set to this metaparamter less than the minimum will be clamped.
--
-- @identifier@ — An optional custom identifier to give to this object
--
-- Returns: The new PHASENumberMetaParameterDefinition object
--
-- ObjC selector: @- initWithValue:minimum:maximum:identifier:@
initWithValue_minimum_maximum_identifier :: (IsPHASENumberMetaParameterDefinition phaseNumberMetaParameterDefinition, IsNSString identifier) => phaseNumberMetaParameterDefinition -> CDouble -> CDouble -> CDouble -> identifier -> IO (Id PHASENumberMetaParameterDefinition)
initWithValue_minimum_maximum_identifier phaseNumberMetaParameterDefinition value minimum_ maximum_ identifier =
  sendOwnedMessage phaseNumberMetaParameterDefinition initWithValue_minimum_maximum_identifierSelector value minimum_ maximum_ (toNSString identifier)

-- | initWithValue:minimum:maximum
--
-- Create a new numeric metaparameter definition and a predefined min and maximum range
--
-- @value@ — The initial value of the metaparameter
--
-- @minimum@ — The minimum value for this metaparameter.  Values set to this metaparamter less than the minimum will be clamped.
--
-- @maximum@ — The maximum value for this metaparameter.  Values set to this metaparamter less than the minimum will be clamped.
--
-- Returns: The new PHASENumberMetaParameterDefinition object
--
-- ObjC selector: @- initWithValue:minimum:maximum:@
initWithValue_minimum_maximum :: IsPHASENumberMetaParameterDefinition phaseNumberMetaParameterDefinition => phaseNumberMetaParameterDefinition -> CDouble -> CDouble -> CDouble -> IO (Id PHASENumberMetaParameterDefinition)
initWithValue_minimum_maximum phaseNumberMetaParameterDefinition value minimum_ maximum_ =
  sendOwnedMessage phaseNumberMetaParameterDefinition initWithValue_minimum_maximumSelector value minimum_ maximum_

-- | minimum
--
-- The readonly minimum that this metaparameter definition was initialized with
--
-- ObjC selector: @- minimum@
minimum_ :: IsPHASENumberMetaParameterDefinition phaseNumberMetaParameterDefinition => phaseNumberMetaParameterDefinition -> IO CDouble
minimum_ phaseNumberMetaParameterDefinition =
  sendMessage phaseNumberMetaParameterDefinition minimumSelector

-- | maximum
--
-- The readonly maximum that this metaparameter definition was initialized with
--
-- ObjC selector: @- maximum@
maximum_ :: IsPHASENumberMetaParameterDefinition phaseNumberMetaParameterDefinition => phaseNumberMetaParameterDefinition -> IO CDouble
maximum_ phaseNumberMetaParameterDefinition =
  sendMessage phaseNumberMetaParameterDefinition maximumSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASENumberMetaParameterDefinition)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASENumberMetaParameterDefinition)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithValue:identifier:@
initWithValue_identifierSelector :: Selector '[CDouble, Id NSString] (Id PHASENumberMetaParameterDefinition)
initWithValue_identifierSelector = mkSelector "initWithValue:identifier:"

-- | @Selector@ for @initWithValue:@
initWithValueSelector :: Selector '[CDouble] (Id PHASENumberMetaParameterDefinition)
initWithValueSelector = mkSelector "initWithValue:"

-- | @Selector@ for @initWithValue:minimum:maximum:identifier:@
initWithValue_minimum_maximum_identifierSelector :: Selector '[CDouble, CDouble, CDouble, Id NSString] (Id PHASENumberMetaParameterDefinition)
initWithValue_minimum_maximum_identifierSelector = mkSelector "initWithValue:minimum:maximum:identifier:"

-- | @Selector@ for @initWithValue:minimum:maximum:@
initWithValue_minimum_maximumSelector :: Selector '[CDouble, CDouble, CDouble] (Id PHASENumberMetaParameterDefinition)
initWithValue_minimum_maximumSelector = mkSelector "initWithValue:minimum:maximum:"

-- | @Selector@ for @minimum@
minimumSelector :: Selector '[] CDouble
minimumSelector = mkSelector "minimum"

-- | @Selector@ for @maximum@
maximumSelector :: Selector '[] CDouble
maximumSelector = mkSelector "maximum"

