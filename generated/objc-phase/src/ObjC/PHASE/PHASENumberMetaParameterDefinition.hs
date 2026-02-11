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
  , newSelector
  , initWithValue_identifierSelector
  , initWithValueSelector
  , initWithValue_minimum_maximum_identifierSelector
  , initWithValue_minimum_maximumSelector
  , minimumSelector
  , maximumSelector


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
init_ :: IsPHASENumberMetaParameterDefinition phaseNumberMetaParameterDefinition => phaseNumberMetaParameterDefinition -> IO (Id PHASENumberMetaParameterDefinition)
init_ phaseNumberMetaParameterDefinition  =
  sendMsg phaseNumberMetaParameterDefinition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASENumberMetaParameterDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASENumberMetaParameterDefinition"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithValue_identifier phaseNumberMetaParameterDefinition  value identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg phaseNumberMetaParameterDefinition (mkSelector "initWithValue:identifier:") (retPtr retVoid) [argCDouble (fromIntegral value), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

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
initWithValue phaseNumberMetaParameterDefinition  value =
  sendMsg phaseNumberMetaParameterDefinition (mkSelector "initWithValue:") (retPtr retVoid) [argCDouble (fromIntegral value)] >>= ownedObject . castPtr

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
initWithValue_minimum_maximum_identifier phaseNumberMetaParameterDefinition  value minimum_ maximum_ identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg phaseNumberMetaParameterDefinition (mkSelector "initWithValue:minimum:maximum:identifier:") (retPtr retVoid) [argCDouble (fromIntegral value), argCDouble (fromIntegral minimum_), argCDouble (fromIntegral maximum_), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

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
initWithValue_minimum_maximum phaseNumberMetaParameterDefinition  value minimum_ maximum_ =
  sendMsg phaseNumberMetaParameterDefinition (mkSelector "initWithValue:minimum:maximum:") (retPtr retVoid) [argCDouble (fromIntegral value), argCDouble (fromIntegral minimum_), argCDouble (fromIntegral maximum_)] >>= ownedObject . castPtr

-- | minimum
--
-- The readonly minimum that this metaparameter definition was initialized with
--
-- ObjC selector: @- minimum@
minimum_ :: IsPHASENumberMetaParameterDefinition phaseNumberMetaParameterDefinition => phaseNumberMetaParameterDefinition -> IO CDouble
minimum_ phaseNumberMetaParameterDefinition  =
  sendMsg phaseNumberMetaParameterDefinition (mkSelector "minimum") retCDouble []

-- | maximum
--
-- The readonly maximum that this metaparameter definition was initialized with
--
-- ObjC selector: @- maximum@
maximum_ :: IsPHASENumberMetaParameterDefinition phaseNumberMetaParameterDefinition => phaseNumberMetaParameterDefinition -> IO CDouble
maximum_ phaseNumberMetaParameterDefinition  =
  sendMsg phaseNumberMetaParameterDefinition (mkSelector "maximum") retCDouble []

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

-- | @Selector@ for @initWithValue:minimum:maximum:identifier:@
initWithValue_minimum_maximum_identifierSelector :: Selector
initWithValue_minimum_maximum_identifierSelector = mkSelector "initWithValue:minimum:maximum:identifier:"

-- | @Selector@ for @initWithValue:minimum:maximum:@
initWithValue_minimum_maximumSelector :: Selector
initWithValue_minimum_maximumSelector = mkSelector "initWithValue:minimum:maximum:"

-- | @Selector@ for @minimum@
minimumSelector :: Selector
minimumSelector = mkSelector "minimum"

-- | @Selector@ for @maximum@
maximumSelector :: Selector
maximumSelector = mkSelector "maximum"

