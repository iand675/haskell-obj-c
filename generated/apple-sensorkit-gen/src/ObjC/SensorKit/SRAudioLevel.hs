{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRAudioLevel@.
module ObjC.SensorKit.SRAudioLevel
  ( SRAudioLevel
  , IsSRAudioLevel(..)
  , init_
  , new
  , loudness
  , initSelector
  , loudnessSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSRAudioLevel srAudioLevel => srAudioLevel -> IO (Id SRAudioLevel)
init_ srAudioLevel =
  sendOwnedMessage srAudioLevel initSelector

-- | @+ new@
new :: IO (Id SRAudioLevel)
new  =
  do
    cls' <- getRequiredClass "SRAudioLevel"
    sendOwnedClassMessage cls' newSelector

-- | loudness
--
-- Measure of the audio level in decibels
--
-- ObjC selector: @- loudness@
loudness :: IsSRAudioLevel srAudioLevel => srAudioLevel -> IO CDouble
loudness srAudioLevel =
  sendMessage srAudioLevel loudnessSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SRAudioLevel)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SRAudioLevel)
newSelector = mkSelector "new"

-- | @Selector@ for @loudness@
loudnessSelector :: Selector '[] CDouble
loudnessSelector = mkSelector "loudness"

