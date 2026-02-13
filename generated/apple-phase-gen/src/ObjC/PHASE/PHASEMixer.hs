{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEMixer
--
-- A generic object the represents an active mixer in the system
--
-- Generated bindings for @PHASEMixer@.
module ObjC.PHASE.PHASEMixer
  ( PHASEMixer
  , IsPHASEMixer(..)
  , init_
  , new
  , identifier
  , gain
  , gainMetaParameter
  , gainMetaParameterSelector
  , gainSelector
  , identifierSelector
  , initSelector
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
init_ :: IsPHASEMixer phaseMixer => phaseMixer -> IO (Id PHASEMixer)
init_ phaseMixer =
  sendOwnedMessage phaseMixer initSelector

-- | @+ new@
new :: IO (Id PHASEMixer)
new  =
  do
    cls' <- getRequiredClass "PHASEMixer"
    sendOwnedClassMessage cls' newSelector

-- | identifier
--
-- The identifier that uniquely represents this mixer.
--
-- ObjC selector: @- identifier@
identifier :: IsPHASEMixer phaseMixer => phaseMixer -> IO (Id NSString)
identifier phaseMixer =
  sendMessage phaseMixer identifierSelector

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- gain@
gain :: IsPHASEMixer phaseMixer => phaseMixer -> IO CDouble
gain phaseMixer =
  sendMessage phaseMixer gainSelector

-- | gainMetaParameter
--
-- The metaparameter that can be used to adjust the gain during playback
--
-- ObjC selector: @- gainMetaParameter@
gainMetaParameter :: IsPHASEMixer phaseMixer => phaseMixer -> IO (Id PHASEMetaParameter)
gainMetaParameter phaseMixer =
  sendMessage phaseMixer gainMetaParameterSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEMixer)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEMixer)
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @gain@
gainSelector :: Selector '[] CDouble
gainSelector = mkSelector "gain"

-- | @Selector@ for @gainMetaParameter@
gainMetaParameterSelector :: Selector '[] (Id PHASEMetaParameter)
gainMetaParameterSelector = mkSelector "gainMetaParameter"

