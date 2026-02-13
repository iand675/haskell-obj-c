{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEStreamNode
--
-- The base class for stream nodes, exposing common elements.
--
-- Generated bindings for @PHASEStreamNode@.
module ObjC.PHASE.PHASEStreamNode
  ( PHASEStreamNode
  , IsPHASEStreamNode(..)
  , init_
  , new
  , gainMetaParameter
  , rateMetaParameter
  , mixer
  , format
  , formatSelector
  , gainMetaParameterSelector
  , initSelector
  , mixerSelector
  , newSelector
  , rateMetaParameterSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEStreamNode phaseStreamNode => phaseStreamNode -> IO (Id PHASEStreamNode)
init_ phaseStreamNode =
  sendOwnedMessage phaseStreamNode initSelector

-- | @+ new@
new :: IO (Id PHASEStreamNode)
new  =
  do
    cls' <- getRequiredClass "PHASEStreamNode"
    sendOwnedClassMessage cls' newSelector

-- | gainMetaParameter
--
-- If specified during construction, the metaparameter for controlling gain will be available here
--
-- ObjC selector: @- gainMetaParameter@
gainMetaParameter :: IsPHASEStreamNode phaseStreamNode => phaseStreamNode -> IO (Id PHASENumberMetaParameter)
gainMetaParameter phaseStreamNode =
  sendMessage phaseStreamNode gainMetaParameterSelector

-- | rateMetaParameter
--
-- If specified during construction, the metaparameter for controlling rate/pitch will be available here
--
-- ObjC selector: @- rateMetaParameter@
rateMetaParameter :: IsPHASEStreamNode phaseStreamNode => phaseStreamNode -> IO (Id PHASENumberMetaParameter)
rateMetaParameter phaseStreamNode =
  sendMessage phaseStreamNode rateMetaParameterSelector

-- | mixer
--
-- The readonly property that returns the PHASEMixer this stream was created with and assigned to.
--
-- ObjC selector: @- mixer@
mixer :: IsPHASEStreamNode phaseStreamNode => phaseStreamNode -> IO (Id PHASEMixer)
mixer phaseStreamNode =
  sendMessage phaseStreamNode mixerSelector

-- | format
--
-- The readonly property that returns the AVAudioFormat that this stream was initialized with.
--
-- ObjC selector: @- format@
format :: IsPHASEStreamNode phaseStreamNode => phaseStreamNode -> IO (Id AVAudioFormat)
format phaseStreamNode =
  sendMessage phaseStreamNode formatSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEStreamNode)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEStreamNode)
newSelector = mkSelector "new"

-- | @Selector@ for @gainMetaParameter@
gainMetaParameterSelector :: Selector '[] (Id PHASENumberMetaParameter)
gainMetaParameterSelector = mkSelector "gainMetaParameter"

-- | @Selector@ for @rateMetaParameter@
rateMetaParameterSelector :: Selector '[] (Id PHASENumberMetaParameter)
rateMetaParameterSelector = mkSelector "rateMetaParameter"

-- | @Selector@ for @mixer@
mixerSelector :: Selector '[] (Id PHASEMixer)
mixerSelector = mkSelector "mixer"

-- | @Selector@ for @format@
formatSelector :: Selector '[] (Id AVAudioFormat)
formatSelector = mkSelector "format"

