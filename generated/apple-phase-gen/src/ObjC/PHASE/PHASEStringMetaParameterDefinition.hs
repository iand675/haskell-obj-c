{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEStringMetaParameterDefinition
--
-- A Metaparameter that has a string value
--
-- Generated bindings for @PHASEStringMetaParameterDefinition@.
module ObjC.PHASE.PHASEStringMetaParameterDefinition
  ( PHASEStringMetaParameterDefinition
  , IsPHASEStringMetaParameterDefinition(..)
  , init_
  , new
  , initWithValue_identifier
  , initWithValue
  , initSelector
  , initWithValueSelector
  , initWithValue_identifierSelector
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
init_ :: IsPHASEStringMetaParameterDefinition phaseStringMetaParameterDefinition => phaseStringMetaParameterDefinition -> IO (Id PHASEStringMetaParameterDefinition)
init_ phaseStringMetaParameterDefinition =
  sendOwnedMessage phaseStringMetaParameterDefinition initSelector

-- | @+ new@
new :: IO (Id PHASEStringMetaParameterDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASEStringMetaParameterDefinition"
    sendOwnedClassMessage cls' newSelector

-- | initWithValue:identifier
--
-- Create a new string metaparameter definition
--
-- @value@ — The initial value of the metaparameter
--
-- @identifier@ — An optional custom identifier to give to this object
--
-- Returns: The new PHASEStringMetaParameterDefinition object
--
-- ObjC selector: @- initWithValue:identifier:@
initWithValue_identifier :: (IsPHASEStringMetaParameterDefinition phaseStringMetaParameterDefinition, IsNSString value, IsNSString identifier) => phaseStringMetaParameterDefinition -> value -> identifier -> IO (Id PHASEStringMetaParameterDefinition)
initWithValue_identifier phaseStringMetaParameterDefinition value identifier =
  sendOwnedMessage phaseStringMetaParameterDefinition initWithValue_identifierSelector (toNSString value) (toNSString identifier)

-- | initWithValue
--
-- Create a new string metaparameter definition
--
-- @value@ — The initial value of the metaparameter
--
-- Returns: The new PHASEStringMetaParameterDefinition object
--
-- ObjC selector: @- initWithValue:@
initWithValue :: (IsPHASEStringMetaParameterDefinition phaseStringMetaParameterDefinition, IsNSString value) => phaseStringMetaParameterDefinition -> value -> IO (Id PHASEStringMetaParameterDefinition)
initWithValue phaseStringMetaParameterDefinition value =
  sendOwnedMessage phaseStringMetaParameterDefinition initWithValueSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEStringMetaParameterDefinition)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEStringMetaParameterDefinition)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithValue:identifier:@
initWithValue_identifierSelector :: Selector '[Id NSString, Id NSString] (Id PHASEStringMetaParameterDefinition)
initWithValue_identifierSelector = mkSelector "initWithValue:identifier:"

-- | @Selector@ for @initWithValue:@
initWithValueSelector :: Selector '[Id NSString] (Id PHASEStringMetaParameterDefinition)
initWithValueSelector = mkSelector "initWithValue:"

