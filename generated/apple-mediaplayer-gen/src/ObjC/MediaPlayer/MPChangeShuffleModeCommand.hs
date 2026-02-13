{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.MediaPlayer.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The app's current shuffle type.
--
-- ObjC selector: @- currentShuffleType@
currentShuffleType :: IsMPChangeShuffleModeCommand mpChangeShuffleModeCommand => mpChangeShuffleModeCommand -> IO MPShuffleType
currentShuffleType mpChangeShuffleModeCommand =
  sendMessage mpChangeShuffleModeCommand currentShuffleTypeSelector

-- | The app's current shuffle type.
--
-- ObjC selector: @- setCurrentShuffleType:@
setCurrentShuffleType :: IsMPChangeShuffleModeCommand mpChangeShuffleModeCommand => mpChangeShuffleModeCommand -> MPShuffleType -> IO ()
setCurrentShuffleType mpChangeShuffleModeCommand value =
  sendMessage mpChangeShuffleModeCommand setCurrentShuffleTypeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentShuffleType@
currentShuffleTypeSelector :: Selector '[] MPShuffleType
currentShuffleTypeSelector = mkSelector "currentShuffleType"

-- | @Selector@ for @setCurrentShuffleType:@
setCurrentShuffleTypeSelector :: Selector '[MPShuffleType] ()
setCurrentShuffleTypeSelector = mkSelector "setCurrentShuffleType:"

