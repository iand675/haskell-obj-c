{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEMappedMetaParameterDefinition
--
-- An object to define a Mapped Metaparameter when building an sound event.
--
-- Generated bindings for @PHASEMappedMetaParameterDefinition@.
module ObjC.PHASE.PHASEMappedMetaParameterDefinition
  ( PHASEMappedMetaParameterDefinition
  , IsPHASEMappedMetaParameterDefinition(..)
  , init_
  , new
  , initWithValue_identifier
  , initWithValue
  , initWithValue_minimum_maximum_identifier
  , initWithValue_minimum_maximum
  , initWithInputMetaParameterDefinition_envelope_identifier
  , initWithInputMetaParameterDefinition_envelope
  , envelope
  , inputMetaParameterDefinition
  , minimum_
  , maximum_
  , envelopeSelector
  , initSelector
  , initWithInputMetaParameterDefinition_envelopeSelector
  , initWithInputMetaParameterDefinition_envelope_identifierSelector
  , initWithValueSelector
  , initWithValue_identifierSelector
  , initWithValue_minimum_maximumSelector
  , initWithValue_minimum_maximum_identifierSelector
  , inputMetaParameterDefinitionSelector
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
init_ :: IsPHASEMappedMetaParameterDefinition phaseMappedMetaParameterDefinition => phaseMappedMetaParameterDefinition -> IO (Id PHASEMappedMetaParameterDefinition)
init_ phaseMappedMetaParameterDefinition =
  sendOwnedMessage phaseMappedMetaParameterDefinition initSelector

-- | @+ new@
new :: IO (Id PHASEMappedMetaParameterDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASEMappedMetaParameterDefinition"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithValue:identifier:@
initWithValue_identifier :: (IsPHASEMappedMetaParameterDefinition phaseMappedMetaParameterDefinition, IsNSString identifier) => phaseMappedMetaParameterDefinition -> CDouble -> identifier -> IO (Id PHASEMappedMetaParameterDefinition)
initWithValue_identifier phaseMappedMetaParameterDefinition value identifier =
  sendOwnedMessage phaseMappedMetaParameterDefinition initWithValue_identifierSelector value (toNSString identifier)

-- | @- initWithValue:@
initWithValue :: IsPHASEMappedMetaParameterDefinition phaseMappedMetaParameterDefinition => phaseMappedMetaParameterDefinition -> CDouble -> IO (Id PHASEMappedMetaParameterDefinition)
initWithValue phaseMappedMetaParameterDefinition value =
  sendOwnedMessage phaseMappedMetaParameterDefinition initWithValueSelector value

-- | @- initWithValue:minimum:maximum:identifier:@
initWithValue_minimum_maximum_identifier :: (IsPHASEMappedMetaParameterDefinition phaseMappedMetaParameterDefinition, IsNSString identifier) => phaseMappedMetaParameterDefinition -> CDouble -> CDouble -> CDouble -> identifier -> IO (Id PHASEMappedMetaParameterDefinition)
initWithValue_minimum_maximum_identifier phaseMappedMetaParameterDefinition value minimum_ maximum_ identifier =
  sendOwnedMessage phaseMappedMetaParameterDefinition initWithValue_minimum_maximum_identifierSelector value minimum_ maximum_ (toNSString identifier)

-- | @- initWithValue:minimum:maximum:@
initWithValue_minimum_maximum :: IsPHASEMappedMetaParameterDefinition phaseMappedMetaParameterDefinition => phaseMappedMetaParameterDefinition -> CDouble -> CDouble -> CDouble -> IO (Id PHASEMappedMetaParameterDefinition)
initWithValue_minimum_maximum phaseMappedMetaParameterDefinition value minimum_ maximum_ =
  sendOwnedMessage phaseMappedMetaParameterDefinition initWithValue_minimum_maximumSelector value minimum_ maximum_

-- | initWithInputMetaParameterDefinition:identifier
--
-- Create a new mapped range metaparameter definition
--
-- @inputMetaParameterDefinition@ — The metaparameter that will provide an input for this mapped metaparameter
--
-- @envelope@ — The envelope to use.
--
-- @identifier@ — An optional custom identifier to give to this object
--
-- Returns: The new PHASEMappedMetaParameterDefinition object
--
-- ObjC selector: @- initWithInputMetaParameterDefinition:envelope:identifier:@
initWithInputMetaParameterDefinition_envelope_identifier :: (IsPHASEMappedMetaParameterDefinition phaseMappedMetaParameterDefinition, IsPHASENumberMetaParameterDefinition inputMetaParameterDefinition, IsPHASEEnvelope envelope, IsNSString identifier) => phaseMappedMetaParameterDefinition -> inputMetaParameterDefinition -> envelope -> identifier -> IO (Id PHASEMappedMetaParameterDefinition)
initWithInputMetaParameterDefinition_envelope_identifier phaseMappedMetaParameterDefinition inputMetaParameterDefinition envelope identifier =
  sendOwnedMessage phaseMappedMetaParameterDefinition initWithInputMetaParameterDefinition_envelope_identifierSelector (toPHASENumberMetaParameterDefinition inputMetaParameterDefinition) (toPHASEEnvelope envelope) (toNSString identifier)

-- | initWithInputMetaParameterDefinition
--
-- Create a new mapped range metaparameter definition
--
-- @inputMetaParameterDefinition@ — The metaparameter that will provide an input for this mapped metaparameter
--
-- @envelope@ — The envelope to use.
--
-- Returns: The new PHASEMappedMetaParameterDefinition object
--
-- ObjC selector: @- initWithInputMetaParameterDefinition:envelope:@
initWithInputMetaParameterDefinition_envelope :: (IsPHASEMappedMetaParameterDefinition phaseMappedMetaParameterDefinition, IsPHASENumberMetaParameterDefinition inputMetaParameterDefinition, IsPHASEEnvelope envelope) => phaseMappedMetaParameterDefinition -> inputMetaParameterDefinition -> envelope -> IO (Id PHASEMappedMetaParameterDefinition)
initWithInputMetaParameterDefinition_envelope phaseMappedMetaParameterDefinition inputMetaParameterDefinition envelope =
  sendOwnedMessage phaseMappedMetaParameterDefinition initWithInputMetaParameterDefinition_envelopeSelector (toPHASENumberMetaParameterDefinition inputMetaParameterDefinition) (toPHASEEnvelope envelope)

-- | envelope
--
-- An Envelope to define segments of curves
--
-- ObjC selector: @- envelope@
envelope :: IsPHASEMappedMetaParameterDefinition phaseMappedMetaParameterDefinition => phaseMappedMetaParameterDefinition -> IO (Id PHASEEnvelope)
envelope phaseMappedMetaParameterDefinition =
  sendMessage phaseMappedMetaParameterDefinition envelopeSelector

-- | inputMetaParameterDefinition
--
-- The readonly PHASENumberMetaParameterDefinition that this metaparameter definition was initialized with
--
-- ObjC selector: @- inputMetaParameterDefinition@
inputMetaParameterDefinition :: IsPHASEMappedMetaParameterDefinition phaseMappedMetaParameterDefinition => phaseMappedMetaParameterDefinition -> IO (Id PHASENumberMetaParameterDefinition)
inputMetaParameterDefinition phaseMappedMetaParameterDefinition =
  sendMessage phaseMappedMetaParameterDefinition inputMetaParameterDefinitionSelector

-- | @- minimum@
minimum_ :: IsPHASEMappedMetaParameterDefinition phaseMappedMetaParameterDefinition => phaseMappedMetaParameterDefinition -> IO CDouble
minimum_ phaseMappedMetaParameterDefinition =
  sendMessage phaseMappedMetaParameterDefinition minimumSelector

-- | @- maximum@
maximum_ :: IsPHASEMappedMetaParameterDefinition phaseMappedMetaParameterDefinition => phaseMappedMetaParameterDefinition -> IO CDouble
maximum_ phaseMappedMetaParameterDefinition =
  sendMessage phaseMappedMetaParameterDefinition maximumSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEMappedMetaParameterDefinition)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEMappedMetaParameterDefinition)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithValue:identifier:@
initWithValue_identifierSelector :: Selector '[CDouble, Id NSString] (Id PHASEMappedMetaParameterDefinition)
initWithValue_identifierSelector = mkSelector "initWithValue:identifier:"

-- | @Selector@ for @initWithValue:@
initWithValueSelector :: Selector '[CDouble] (Id PHASEMappedMetaParameterDefinition)
initWithValueSelector = mkSelector "initWithValue:"

-- | @Selector@ for @initWithValue:minimum:maximum:identifier:@
initWithValue_minimum_maximum_identifierSelector :: Selector '[CDouble, CDouble, CDouble, Id NSString] (Id PHASEMappedMetaParameterDefinition)
initWithValue_minimum_maximum_identifierSelector = mkSelector "initWithValue:minimum:maximum:identifier:"

-- | @Selector@ for @initWithValue:minimum:maximum:@
initWithValue_minimum_maximumSelector :: Selector '[CDouble, CDouble, CDouble] (Id PHASEMappedMetaParameterDefinition)
initWithValue_minimum_maximumSelector = mkSelector "initWithValue:minimum:maximum:"

-- | @Selector@ for @initWithInputMetaParameterDefinition:envelope:identifier:@
initWithInputMetaParameterDefinition_envelope_identifierSelector :: Selector '[Id PHASENumberMetaParameterDefinition, Id PHASEEnvelope, Id NSString] (Id PHASEMappedMetaParameterDefinition)
initWithInputMetaParameterDefinition_envelope_identifierSelector = mkSelector "initWithInputMetaParameterDefinition:envelope:identifier:"

-- | @Selector@ for @initWithInputMetaParameterDefinition:envelope:@
initWithInputMetaParameterDefinition_envelopeSelector :: Selector '[Id PHASENumberMetaParameterDefinition, Id PHASEEnvelope] (Id PHASEMappedMetaParameterDefinition)
initWithInputMetaParameterDefinition_envelopeSelector = mkSelector "initWithInputMetaParameterDefinition:envelope:"

-- | @Selector@ for @envelope@
envelopeSelector :: Selector '[] (Id PHASEEnvelope)
envelopeSelector = mkSelector "envelope"

-- | @Selector@ for @inputMetaParameterDefinition@
inputMetaParameterDefinitionSelector :: Selector '[] (Id PHASENumberMetaParameterDefinition)
inputMetaParameterDefinitionSelector = mkSelector "inputMetaParameterDefinition"

-- | @Selector@ for @minimum@
minimumSelector :: Selector '[] CDouble
minimumSelector = mkSelector "minimum"

-- | @Selector@ for @maximum@
maximumSelector :: Selector '[] CDouble
maximumSelector = mkSelector "maximum"

