{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Command for changing the current shuffle mode to use during playback. To update the system's current representation of your app's shuffle mode, set the currentShuffleType property on this command to the proper shuffle type value.
--
-- Generated bindings for @MPChangeShuffleModeCommand@.
module ObjC.MediaPlayer.MPChangeShuffleModeCommand
  ( MPChangeShuffleModeCommand
  , IsMPChangeShuffleModeCommand(..)
  , currentShuffleType
  , setCurrentShuffleType
  , currentShuffleTypeSelector
  , setCurrentShuffleTypeSelector

  -- * Enum types
  , MPShuffleType(MPShuffleType)
  , pattern MPShuffleTypeOff
  , pattern MPShuffleTypeItems
  , pattern MPShuffleTypeCollections

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
import ObjC.MediaPlayer.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The app's current shuffle type.
--
-- ObjC selector: @- currentShuffleType@
currentShuffleType :: IsMPChangeShuffleModeCommand mpChangeShuffleModeCommand => mpChangeShuffleModeCommand -> IO MPShuffleType
currentShuffleType mpChangeShuffleModeCommand  =
  fmap (coerce :: CLong -> MPShuffleType) $ sendMsg mpChangeShuffleModeCommand (mkSelector "currentShuffleType") retCLong []

-- | The app's current shuffle type.
--
-- ObjC selector: @- setCurrentShuffleType:@
setCurrentShuffleType :: IsMPChangeShuffleModeCommand mpChangeShuffleModeCommand => mpChangeShuffleModeCommand -> MPShuffleType -> IO ()
setCurrentShuffleType mpChangeShuffleModeCommand  value =
  sendMsg mpChangeShuffleModeCommand (mkSelector "setCurrentShuffleType:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentShuffleType@
currentShuffleTypeSelector :: Selector
currentShuffleTypeSelector = mkSelector "currentShuffleType"

-- | @Selector@ for @setCurrentShuffleType:@
setCurrentShuffleTypeSelector :: Selector
setCurrentShuffleTypeSelector = mkSelector "setCurrentShuffleType:"

