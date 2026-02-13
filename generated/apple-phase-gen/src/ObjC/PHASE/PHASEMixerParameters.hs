{-# LANGUAGE DataKinds #-}
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
  , addAmbientMixerParametersWithIdentifier_listenerSelector
  , addSpatialMixerParametersWithIdentifier_source_listenerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
addSpatialMixerParametersWithIdentifier_source_listener phaseMixerParameters identifier source listener =
  sendMessage phaseMixerParameters addSpatialMixerParametersWithIdentifier_source_listenerSelector (toNSString identifier) (toPHASESource source) (toPHASEListener listener)

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
addAmbientMixerParametersWithIdentifier_listener phaseMixerParameters identifier listener =
  sendMessage phaseMixerParameters addAmbientMixerParametersWithIdentifier_listenerSelector (toNSString identifier) (toPHASEListener listener)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addSpatialMixerParametersWithIdentifier:source:listener:@
addSpatialMixerParametersWithIdentifier_source_listenerSelector :: Selector '[Id NSString, Id PHASESource, Id PHASEListener] ()
addSpatialMixerParametersWithIdentifier_source_listenerSelector = mkSelector "addSpatialMixerParametersWithIdentifier:source:listener:"

-- | @Selector@ for @addAmbientMixerParametersWithIdentifier:listener:@
addAmbientMixerParametersWithIdentifier_listenerSelector :: Selector '[Id NSString, Id PHASEListener] ()
addAmbientMixerParametersWithIdentifier_listenerSelector = mkSelector "addAmbientMixerParametersWithIdentifier:listener:"

