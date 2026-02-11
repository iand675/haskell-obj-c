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
  , initSelector
  , newSelector
  , gainMetaParameterSelector
  , rateMetaParameterSelector
  , mixerSelector
  , formatSelector


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
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEStreamNode phaseStreamNode => phaseStreamNode -> IO (Id PHASEStreamNode)
init_ phaseStreamNode  =
  sendMsg phaseStreamNode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEStreamNode)
new  =
  do
    cls' <- getRequiredClass "PHASEStreamNode"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | gainMetaParameter
--
-- If specified during construction, the metaparameter for controlling gain will be available here
--
-- ObjC selector: @- gainMetaParameter@
gainMetaParameter :: IsPHASEStreamNode phaseStreamNode => phaseStreamNode -> IO (Id PHASENumberMetaParameter)
gainMetaParameter phaseStreamNode  =
  sendMsg phaseStreamNode (mkSelector "gainMetaParameter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | rateMetaParameter
--
-- If specified during construction, the metaparameter for controlling rate/pitch will be available here
--
-- ObjC selector: @- rateMetaParameter@
rateMetaParameter :: IsPHASEStreamNode phaseStreamNode => phaseStreamNode -> IO (Id PHASENumberMetaParameter)
rateMetaParameter phaseStreamNode  =
  sendMsg phaseStreamNode (mkSelector "rateMetaParameter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mixer
--
-- The readonly property that returns the PHASEMixer this stream was created with and assigned to.
--
-- ObjC selector: @- mixer@
mixer :: IsPHASEStreamNode phaseStreamNode => phaseStreamNode -> IO (Id PHASEMixer)
mixer phaseStreamNode  =
  sendMsg phaseStreamNode (mkSelector "mixer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | format
--
-- The readonly property that returns the AVAudioFormat that this stream was initialized with.
--
-- ObjC selector: @- format@
format :: IsPHASEStreamNode phaseStreamNode => phaseStreamNode -> IO (Id AVAudioFormat)
format phaseStreamNode  =
  sendMsg phaseStreamNode (mkSelector "format") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @gainMetaParameter@
gainMetaParameterSelector :: Selector
gainMetaParameterSelector = mkSelector "gainMetaParameter"

-- | @Selector@ for @rateMetaParameter@
rateMetaParameterSelector :: Selector
rateMetaParameterSelector = mkSelector "rateMetaParameter"

-- | @Selector@ for @mixer@
mixerSelector :: Selector
mixerSelector = mkSelector "mixer"

-- | @Selector@ for @format@
formatSelector :: Selector
formatSelector = mkSelector "format"

