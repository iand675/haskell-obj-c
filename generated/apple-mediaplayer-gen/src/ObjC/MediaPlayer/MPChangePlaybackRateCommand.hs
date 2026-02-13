{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPChangePlaybackRateCommand@.
module ObjC.MediaPlayer.MPChangePlaybackRateCommand
  ( MPChangePlaybackRateCommand
  , IsMPChangePlaybackRateCommand(..)
  , supportedPlaybackRates
  , setSupportedPlaybackRates
  , setSupportedPlaybackRatesSelector
  , supportedPlaybackRatesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | An array of NSNumbers (floats) that contain supported playback rates that the command can send.
--
-- ObjC selector: @- supportedPlaybackRates@
supportedPlaybackRates :: IsMPChangePlaybackRateCommand mpChangePlaybackRateCommand => mpChangePlaybackRateCommand -> IO (Id NSArray)
supportedPlaybackRates mpChangePlaybackRateCommand =
  sendMessage mpChangePlaybackRateCommand supportedPlaybackRatesSelector

-- | An array of NSNumbers (floats) that contain supported playback rates that the command can send.
--
-- ObjC selector: @- setSupportedPlaybackRates:@
setSupportedPlaybackRates :: (IsMPChangePlaybackRateCommand mpChangePlaybackRateCommand, IsNSArray value) => mpChangePlaybackRateCommand -> value -> IO ()
setSupportedPlaybackRates mpChangePlaybackRateCommand value =
  sendMessage mpChangePlaybackRateCommand setSupportedPlaybackRatesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportedPlaybackRates@
supportedPlaybackRatesSelector :: Selector '[] (Id NSArray)
supportedPlaybackRatesSelector = mkSelector "supportedPlaybackRates"

-- | @Selector@ for @setSupportedPlaybackRates:@
setSupportedPlaybackRatesSelector :: Selector '[Id NSArray] ()
setSupportedPlaybackRatesSelector = mkSelector "setSupportedPlaybackRates:"

