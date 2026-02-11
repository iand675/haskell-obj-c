{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioMixingDestination
--
-- An object representing a connection to a mixer node from a node that		conforms to AVAudioMixing protocol
--
-- A standalone instance of AVAudioMixingDestination cannot be created.		Only an instance vended by a source node (e.g. AVAudioPlayerNode) can be used		(see @AVAudioMixing@).
--
-- Generated bindings for @AVAudioMixingDestination@.
module ObjC.AVFAudio.AVAudioMixingDestination
  ( AVAudioMixingDestination
  , IsAVAudioMixingDestination(..)
  , init_
  , connectionPoint
  , initSelector
  , connectionPointSelector


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

-- | @- init@
init_ :: IsAVAudioMixingDestination avAudioMixingDestination => avAudioMixingDestination -> IO (Id AVAudioMixingDestination)
init_ avAudioMixingDestination  =
  sendMsg avAudioMixingDestination (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | connectionPoint
--
-- Returns the underlying mixer connection point
--
-- ObjC selector: @- connectionPoint@
connectionPoint :: IsAVAudioMixingDestination avAudioMixingDestination => avAudioMixingDestination -> IO (Id AVAudioConnectionPoint)
connectionPoint avAudioMixingDestination  =
  sendMsg avAudioMixingDestination (mkSelector "connectionPoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @connectionPoint@
connectionPointSelector :: Selector
connectionPointSelector = mkSelector "connectionPoint"

