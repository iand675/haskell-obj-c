{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEMixerParameters
--
-- An object that holds runtime parameters for mixers when creating PHASESoundEvents.
--
-- Generated bindings for @PHASEMixerParameters@.
module ObjC.PHASE.PHASEMixerParameters
  ( PHASEMixerParameters
  , IsPHASEMixerParameters(..)
  , addSpatialMixerParametersWithIdentifier_source_listener
  , addAmbientMixerParametersWithIdentifier_listener
  , addSpatialMixerParametersWithIdentifier_source_listenerSelector
  , addAmbientMixerParametersWithIdentifier_listenerSelector


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

-- | addSpatialMixerParametersWithIdentifier:source:listener
--
-- Adds runtime parameters for a spatial mixer
--
-- @identifier@ — The unique identifier assigned to a spatial submixer object.
--
-- @source@ — The PHASESource object that this mixer will use to spatialize sounds.
--
-- @listener@ — The PHASEListener object that this mixer will use to spatialize sounds.
--
-- ObjC selector: @- addSpatialMixerParametersWithIdentifier:source:listener:@
addSpatialMixerParametersWithIdentifier_source_listener :: (IsPHASEMixerParameters phaseMixerParameters, IsNSString identifier, IsPHASESource source, IsPHASEListener listener) => phaseMixerParameters -> identifier -> source -> listener -> IO ()
addSpatialMixerParametersWithIdentifier_source_listener phaseMixerParameters  identifier source listener =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr source $ \raw_source ->
    withObjCPtr listener $ \raw_listener ->
        sendMsg phaseMixerParameters (mkSelector "addSpatialMixerParametersWithIdentifier:source:listener:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr raw_listener :: Ptr ())]

-- | addAmbientMixerParametersWithIdentifier:listener
--
-- Adds runtime parameters for an ambient mixer
--
-- @identifier@ — The unique identifier assigned to a spatial submixer object.
--
-- @listener@ — The PHASEListener object that this mixer will use to orient sounds.
--
-- ObjC selector: @- addAmbientMixerParametersWithIdentifier:listener:@
addAmbientMixerParametersWithIdentifier_listener :: (IsPHASEMixerParameters phaseMixerParameters, IsNSString identifier, IsPHASEListener listener) => phaseMixerParameters -> identifier -> listener -> IO ()
addAmbientMixerParametersWithIdentifier_listener phaseMixerParameters  identifier listener =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr listener $ \raw_listener ->
      sendMsg phaseMixerParameters (mkSelector "addAmbientMixerParametersWithIdentifier:listener:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_listener :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addSpatialMixerParametersWithIdentifier:source:listener:@
addSpatialMixerParametersWithIdentifier_source_listenerSelector :: Selector
addSpatialMixerParametersWithIdentifier_source_listenerSelector = mkSelector "addSpatialMixerParametersWithIdentifier:source:listener:"

-- | @Selector@ for @addAmbientMixerParametersWithIdentifier:listener:@
addAmbientMixerParametersWithIdentifier_listenerSelector :: Selector
addAmbientMixerParametersWithIdentifier_listenerSelector = mkSelector "addAmbientMixerParametersWithIdentifier:listener:"

