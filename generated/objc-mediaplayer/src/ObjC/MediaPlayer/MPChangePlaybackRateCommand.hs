{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPChangePlaybackRateCommand@.
module ObjC.MediaPlayer.MPChangePlaybackRateCommand
  ( MPChangePlaybackRateCommand
  , IsMPChangePlaybackRateCommand(..)
  , supportedPlaybackRates
  , setSupportedPlaybackRates
  , supportedPlaybackRatesSelector
  , setSupportedPlaybackRatesSelector


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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | An array of NSNumbers (floats) that contain supported playback rates that the command can send.
--
-- ObjC selector: @- supportedPlaybackRates@
supportedPlaybackRates :: IsMPChangePlaybackRateCommand mpChangePlaybackRateCommand => mpChangePlaybackRateCommand -> IO (Id NSArray)
supportedPlaybackRates mpChangePlaybackRateCommand  =
  sendMsg mpChangePlaybackRateCommand (mkSelector "supportedPlaybackRates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array of NSNumbers (floats) that contain supported playback rates that the command can send.
--
-- ObjC selector: @- setSupportedPlaybackRates:@
setSupportedPlaybackRates :: (IsMPChangePlaybackRateCommand mpChangePlaybackRateCommand, IsNSArray value) => mpChangePlaybackRateCommand -> value -> IO ()
setSupportedPlaybackRates mpChangePlaybackRateCommand  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpChangePlaybackRateCommand (mkSelector "setSupportedPlaybackRates:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportedPlaybackRates@
supportedPlaybackRatesSelector :: Selector
supportedPlaybackRatesSelector = mkSelector "supportedPlaybackRates"

-- | @Selector@ for @setSupportedPlaybackRates:@
setSupportedPlaybackRatesSelector :: Selector
setSupportedPlaybackRatesSelector = mkSelector "setSupportedPlaybackRates:"

