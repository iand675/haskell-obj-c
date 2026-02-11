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
  , newSelector
  , initWithValue_identifierSelector
  , initWithValueSelector


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
init_ :: IsPHASEStringMetaParameterDefinition phaseStringMetaParameterDefinition => phaseStringMetaParameterDefinition -> IO (Id PHASEStringMetaParameterDefinition)
init_ phaseStringMetaParameterDefinition  =
  sendMsg phaseStringMetaParameterDefinition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEStringMetaParameterDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASEStringMetaParameterDefinition"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithValue_identifier phaseStringMetaParameterDefinition  value identifier =
withObjCPtr value $ \raw_value ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg phaseStringMetaParameterDefinition (mkSelector "initWithValue:identifier:") (retPtr retVoid) [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

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
initWithValue phaseStringMetaParameterDefinition  value =
withObjCPtr value $ \raw_value ->
    sendMsg phaseStringMetaParameterDefinition (mkSelector "initWithValue:") (retPtr retVoid) [argPtr (castPtr raw_value :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithValue:identifier:@
initWithValue_identifierSelector :: Selector
initWithValue_identifierSelector = mkSelector "initWithValue:identifier:"

-- | @Selector@ for @initWithValue:@
initWithValueSelector :: Selector
initWithValueSelector = mkSelector "initWithValue:"

