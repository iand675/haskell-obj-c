{-# LANGUAGE DataKinds #-}
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
  , connectionPointSelector
  , initSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAudioMixingDestination avAudioMixingDestination => avAudioMixingDestination -> IO (Id AVAudioMixingDestination)
init_ avAudioMixingDestination =
  sendOwnedMessage avAudioMixingDestination initSelector

-- | connectionPoint
--
-- Returns the underlying mixer connection point
--
-- ObjC selector: @- connectionPoint@
connectionPoint :: IsAVAudioMixingDestination avAudioMixingDestination => avAudioMixingDestination -> IO (Id AVAudioConnectionPoint)
connectionPoint avAudioMixingDestination =
  sendMessage avAudioMixingDestination connectionPointSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAudioMixingDestination)
initSelector = mkSelector "init"

-- | @Selector@ for @connectionPoint@
connectionPointSelector :: Selector '[] (Id AVAudioConnectionPoint)
connectionPointSelector = mkSelector "connectionPoint"

