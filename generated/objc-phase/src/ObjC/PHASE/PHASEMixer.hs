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
  , initSelector
  , newSelector
  , identifierSelector
  , gainSelector
  , gainMetaParameterSelector


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
init_ :: IsPHASEMixer phaseMixer => phaseMixer -> IO (Id PHASEMixer)
init_ phaseMixer  =
  sendMsg phaseMixer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEMixer)
new  =
  do
    cls' <- getRequiredClass "PHASEMixer"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | identifier
--
-- The identifier that uniquely represents this mixer.
--
-- ObjC selector: @- identifier@
identifier :: IsPHASEMixer phaseMixer => phaseMixer -> IO (Id NSString)
identifier phaseMixer  =
  sendMsg phaseMixer (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- gain@
gain :: IsPHASEMixer phaseMixer => phaseMixer -> IO CDouble
gain phaseMixer  =
  sendMsg phaseMixer (mkSelector "gain") retCDouble []

-- | gainMetaParameter
--
-- The metaparameter that can be used to adjust the gain during playback
--
-- ObjC selector: @- gainMetaParameter@
gainMetaParameter :: IsPHASEMixer phaseMixer => phaseMixer -> IO (Id PHASEMetaParameter)
gainMetaParameter phaseMixer  =
  sendMsg phaseMixer (mkSelector "gainMetaParameter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @gain@
gainSelector :: Selector
gainSelector = mkSelector "gain"

-- | @Selector@ for @gainMetaParameter@
gainMetaParameterSelector :: Selector
gainMetaParameterSelector = mkSelector "gainMetaParameter"

