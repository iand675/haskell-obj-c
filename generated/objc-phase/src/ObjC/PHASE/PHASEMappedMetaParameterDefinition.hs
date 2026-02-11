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
  , initSelector
  , newSelector
  , initWithValue_identifierSelector
  , initWithValueSelector
  , initWithValue_minimum_maximum_identifierSelector
  , initWithValue_minimum_maximumSelector
  , initWithInputMetaParameterDefinition_envelope_identifierSelector
  , initWithInputMetaParameterDefinition_envelopeSelector
  , envelopeSelector
  , inputMetaParameterDefinitionSelector
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
init_ :: IsPHASEMappedMetaParameterDefinition phaseMappedMetaParameterDefinition => phaseMappedMetaParameterDefinition -> IO (Id PHASEMappedMetaParameterDefinition)
init_ phaseMappedMetaParameterDefinition  =
  sendMsg phaseMappedMetaParameterDefinition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEMappedMetaParameterDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASEMappedMetaParameterDefinition"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithValue:identifier:@
initWithValue_identifier :: (IsPHASEMappedMetaParameterDefinition phaseMappedMetaParameterDefinition, IsNSString identifier) => phaseMappedMetaParameterDefinition -> CDouble -> identifier -> IO (Id PHASEMappedMetaParameterDefinition)
initWithValue_identifier phaseMappedMetaParameterDefinition  value identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg phaseMappedMetaParameterDefinition (mkSelector "initWithValue:identifier:") (retPtr retVoid) [argCDouble (fromIntegral value), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithValue:@
initWithValue :: IsPHASEMappedMetaParameterDefinition phaseMappedMetaParameterDefinition => phaseMappedMetaParameterDefinition -> CDouble -> IO (Id PHASEMappedMetaParameterDefinition)
initWithValue phaseMappedMetaParameterDefinition  value =
  sendMsg phaseMappedMetaParameterDefinition (mkSelector "initWithValue:") (retPtr retVoid) [argCDouble (fromIntegral value)] >>= ownedObject . castPtr

-- | @- initWithValue:minimum:maximum:identifier:@
initWithValue_minimum_maximum_identifier :: (IsPHASEMappedMetaParameterDefinition phaseMappedMetaParameterDefinition, IsNSString identifier) => phaseMappedMetaParameterDefinition -> CDouble -> CDouble -> CDouble -> identifier -> IO (Id PHASEMappedMetaParameterDefinition)
initWithValue_minimum_maximum_identifier phaseMappedMetaParameterDefinition  value minimum_ maximum_ identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg phaseMappedMetaParameterDefinition (mkSelector "initWithValue:minimum:maximum:identifier:") (retPtr retVoid) [argCDouble (fromIntegral value), argCDouble (fromIntegral minimum_), argCDouble (fromIntegral maximum_), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithValue:minimum:maximum:@
initWithValue_minimum_maximum :: IsPHASEMappedMetaParameterDefinition phaseMappedMetaParameterDefinition => phaseMappedMetaParameterDefinition -> CDouble -> CDouble -> CDouble -> IO (Id PHASEMappedMetaParameterDefinition)
initWithValue_minimum_maximum phaseMappedMetaParameterDefinition  value minimum_ maximum_ =
  sendMsg phaseMappedMetaParameterDefinition (mkSelector "initWithValue:minimum:maximum:") (retPtr retVoid) [argCDouble (fromIntegral value), argCDouble (fromIntegral minimum_), argCDouble (fromIntegral maximum_)] >>= ownedObject . castPtr

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
initWithInputMetaParameterDefinition_envelope_identifier phaseMappedMetaParameterDefinition  inputMetaParameterDefinition envelope identifier =
withObjCPtr inputMetaParameterDefinition $ \raw_inputMetaParameterDefinition ->
  withObjCPtr envelope $ \raw_envelope ->
    withObjCPtr identifier $ \raw_identifier ->
        sendMsg phaseMappedMetaParameterDefinition (mkSelector "initWithInputMetaParameterDefinition:envelope:identifier:") (retPtr retVoid) [argPtr (castPtr raw_inputMetaParameterDefinition :: Ptr ()), argPtr (castPtr raw_envelope :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

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
initWithInputMetaParameterDefinition_envelope phaseMappedMetaParameterDefinition  inputMetaParameterDefinition envelope =
withObjCPtr inputMetaParameterDefinition $ \raw_inputMetaParameterDefinition ->
  withObjCPtr envelope $ \raw_envelope ->
      sendMsg phaseMappedMetaParameterDefinition (mkSelector "initWithInputMetaParameterDefinition:envelope:") (retPtr retVoid) [argPtr (castPtr raw_inputMetaParameterDefinition :: Ptr ()), argPtr (castPtr raw_envelope :: Ptr ())] >>= ownedObject . castPtr

-- | envelope
--
-- An Envelope to define segments of curves
--
-- ObjC selector: @- envelope@
envelope :: IsPHASEMappedMetaParameterDefinition phaseMappedMetaParameterDefinition => phaseMappedMetaParameterDefinition -> IO (Id PHASEEnvelope)
envelope phaseMappedMetaParameterDefinition  =
  sendMsg phaseMappedMetaParameterDefinition (mkSelector "envelope") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | inputMetaParameterDefinition
--
-- The readonly PHASENumberMetaParameterDefinition that this metaparameter definition was initialized with
--
-- ObjC selector: @- inputMetaParameterDefinition@
inputMetaParameterDefinition :: IsPHASEMappedMetaParameterDefinition phaseMappedMetaParameterDefinition => phaseMappedMetaParameterDefinition -> IO (Id PHASENumberMetaParameterDefinition)
inputMetaParameterDefinition phaseMappedMetaParameterDefinition  =
  sendMsg phaseMappedMetaParameterDefinition (mkSelector "inputMetaParameterDefinition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- minimum@
minimum_ :: IsPHASEMappedMetaParameterDefinition phaseMappedMetaParameterDefinition => phaseMappedMetaParameterDefinition -> IO CDouble
minimum_ phaseMappedMetaParameterDefinition  =
  sendMsg phaseMappedMetaParameterDefinition (mkSelector "minimum") retCDouble []

-- | @- maximum@
maximum_ :: IsPHASEMappedMetaParameterDefinition phaseMappedMetaParameterDefinition => phaseMappedMetaParameterDefinition -> IO CDouble
maximum_ phaseMappedMetaParameterDefinition  =
  sendMsg phaseMappedMetaParameterDefinition (mkSelector "maximum") retCDouble []

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

-- | @Selector@ for @initWithInputMetaParameterDefinition:envelope:identifier:@
initWithInputMetaParameterDefinition_envelope_identifierSelector :: Selector
initWithInputMetaParameterDefinition_envelope_identifierSelector = mkSelector "initWithInputMetaParameterDefinition:envelope:identifier:"

-- | @Selector@ for @initWithInputMetaParameterDefinition:envelope:@
initWithInputMetaParameterDefinition_envelopeSelector :: Selector
initWithInputMetaParameterDefinition_envelopeSelector = mkSelector "initWithInputMetaParameterDefinition:envelope:"

-- | @Selector@ for @envelope@
envelopeSelector :: Selector
envelopeSelector = mkSelector "envelope"

-- | @Selector@ for @inputMetaParameterDefinition@
inputMetaParameterDefinitionSelector :: Selector
inputMetaParameterDefinitionSelector = mkSelector "inputMetaParameterDefinition"

-- | @Selector@ for @minimum@
minimumSelector :: Selector
minimumSelector = mkSelector "minimum"

-- | @Selector@ for @maximum@
maximumSelector :: Selector
maximumSelector = mkSelector "maximum"

