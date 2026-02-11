{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that manages playback routing preferences.
--
-- This object manages instances of ``AVRoutingPlaybackParticipant`` for arbitration of media playback routing priorities and preferences on restricted playback interfaces. The playback routing arbiter is responsible for collecting and applying preferences, such as priorities in non-mixable audio routes and external playback states where the number of allowed players is limited.
--
-- Generated bindings for @AVRoutingPlaybackArbiter@.
module ObjC.AVRouting.AVRoutingPlaybackArbiter
  ( AVRoutingPlaybackArbiter
  , IsAVRoutingPlaybackArbiter(..)
  , sharedRoutingPlaybackArbiter
  , init_
  , new
  , sharedRoutingPlaybackArbiterSelector
  , initSelector
  , newSelector


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

import ObjC.AVRouting.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns the singleton playback arbiter instance.
--
-- ObjC selector: @+ sharedRoutingPlaybackArbiter@
sharedRoutingPlaybackArbiter :: IO (Id AVRoutingPlaybackArbiter)
sharedRoutingPlaybackArbiter  =
  do
    cls' <- getRequiredClass "AVRoutingPlaybackArbiter"
    sendClassMsg cls' (mkSelector "sharedRoutingPlaybackArbiter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsAVRoutingPlaybackArbiter avRoutingPlaybackArbiter => avRoutingPlaybackArbiter -> IO (Id AVRoutingPlaybackArbiter)
init_ avRoutingPlaybackArbiter  =
  sendMsg avRoutingPlaybackArbiter (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVRoutingPlaybackArbiter)
new  =
  do
    cls' <- getRequiredClass "AVRoutingPlaybackArbiter"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedRoutingPlaybackArbiter@
sharedRoutingPlaybackArbiterSelector :: Selector
sharedRoutingPlaybackArbiterSelector = mkSelector "sharedRoutingPlaybackArbiter"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

