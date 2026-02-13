{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioConnectionPoint
--
-- A representation of either a source or destination connection point in AVAudioEngine.
--
-- AVAudioConnectionPoint describes either a source or destination connection point (node, bus)		in AVAudioEngine's graph.
--
-- Instances of this class are immutable.
--
-- Generated bindings for @AVAudioConnectionPoint@.
module ObjC.AVFAudio.AVAudioConnectionPoint
  ( AVAudioConnectionPoint
  , IsAVAudioConnectionPoint(..)
  , initWithNode_bus
  , init_
  , node
  , bus
  , busSelector
  , initSelector
  , initWithNode_busSelector
  , nodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithNode:bus:
--
-- Create a connection point object.
--
-- @node@ — the source or destination node
--
-- @bus@ — the output or input bus on the node
--
-- If the node is nil, this method fails (returns nil).
--
-- ObjC selector: @- initWithNode:bus:@
initWithNode_bus :: (IsAVAudioConnectionPoint avAudioConnectionPoint, IsAVAudioNode node) => avAudioConnectionPoint -> node -> CULong -> IO (Id AVAudioConnectionPoint)
initWithNode_bus avAudioConnectionPoint node bus =
  sendOwnedMessage avAudioConnectionPoint initWithNode_busSelector (toAVAudioNode node) bus

-- | @- init@
init_ :: IsAVAudioConnectionPoint avAudioConnectionPoint => avAudioConnectionPoint -> IO (Id AVAudioConnectionPoint)
init_ avAudioConnectionPoint =
  sendOwnedMessage avAudioConnectionPoint initSelector

-- | node
--
-- Returns the node in the connection point.
--
-- ObjC selector: @- node@
node :: IsAVAudioConnectionPoint avAudioConnectionPoint => avAudioConnectionPoint -> IO (Id AVAudioNode)
node avAudioConnectionPoint =
  sendMessage avAudioConnectionPoint nodeSelector

-- | bus
--
-- Returns the bus on the node in the connection point.
--
-- ObjC selector: @- bus@
bus :: IsAVAudioConnectionPoint avAudioConnectionPoint => avAudioConnectionPoint -> IO CULong
bus avAudioConnectionPoint =
  sendMessage avAudioConnectionPoint busSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNode:bus:@
initWithNode_busSelector :: Selector '[Id AVAudioNode, CULong] (Id AVAudioConnectionPoint)
initWithNode_busSelector = mkSelector "initWithNode:bus:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAudioConnectionPoint)
initSelector = mkSelector "init"

-- | @Selector@ for @node@
nodeSelector :: Selector '[] (Id AVAudioNode)
nodeSelector = mkSelector "node"

-- | @Selector@ for @bus@
busSelector :: Selector '[] CULong
busSelector = mkSelector "bus"

