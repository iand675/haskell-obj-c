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
  , initWithNode_busSelector
  , initSelector
  , nodeSelector
  , busSelector


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
initWithNode_bus avAudioConnectionPoint  node bus =
withObjCPtr node $ \raw_node ->
    sendMsg avAudioConnectionPoint (mkSelector "initWithNode:bus:") (retPtr retVoid) [argPtr (castPtr raw_node :: Ptr ()), argCULong (fromIntegral bus)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsAVAudioConnectionPoint avAudioConnectionPoint => avAudioConnectionPoint -> IO (Id AVAudioConnectionPoint)
init_ avAudioConnectionPoint  =
  sendMsg avAudioConnectionPoint (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | node
--
-- Returns the node in the connection point.
--
-- ObjC selector: @- node@
node :: IsAVAudioConnectionPoint avAudioConnectionPoint => avAudioConnectionPoint -> IO (Id AVAudioNode)
node avAudioConnectionPoint  =
  sendMsg avAudioConnectionPoint (mkSelector "node") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | bus
--
-- Returns the bus on the node in the connection point.
--
-- ObjC selector: @- bus@
bus :: IsAVAudioConnectionPoint avAudioConnectionPoint => avAudioConnectionPoint -> IO CULong
bus avAudioConnectionPoint  =
  sendMsg avAudioConnectionPoint (mkSelector "bus") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNode:bus:@
initWithNode_busSelector :: Selector
initWithNode_busSelector = mkSelector "initWithNode:bus:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @node@
nodeSelector :: Selector
nodeSelector = mkSelector "node"

-- | @Selector@ for @bus@
busSelector :: Selector
busSelector = mkSelector "bus"

