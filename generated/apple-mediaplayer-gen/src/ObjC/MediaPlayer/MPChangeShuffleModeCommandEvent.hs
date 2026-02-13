{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPChangeShuffleModeCommandEvent@.
module ObjC.MediaPlayer.MPChangeShuffleModeCommandEvent
  ( MPChangeShuffleModeCommandEvent
  , IsMPChangeShuffleModeCommandEvent(..)
  , shuffleType
  , preservesShuffleMode
  , preservesShuffleModeSelector
  , shuffleTypeSelector

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

-- | The desired shuffle type to use when fulfilling the request.
--
-- ObjC selector: @- shuffleType@
shuffleType :: IsMPChangeShuffleModeCommandEvent mpChangeShuffleModeCommandEvent => mpChangeShuffleModeCommandEvent -> IO MPShuffleType
shuffleType mpChangeShuffleModeCommandEvent =
  sendMessage mpChangeShuffleModeCommandEvent shuffleTypeSelector

-- | Whether or not the selection should be preserved between playback sessions
--
-- ObjC selector: @- preservesShuffleMode@
preservesShuffleMode :: IsMPChangeShuffleModeCommandEvent mpChangeShuffleModeCommandEvent => mpChangeShuffleModeCommandEvent -> IO Bool
preservesShuffleMode mpChangeShuffleModeCommandEvent =
  sendMessage mpChangeShuffleModeCommandEvent preservesShuffleModeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shuffleType@
shuffleTypeSelector :: Selector '[] MPShuffleType
shuffleTypeSelector = mkSelector "shuffleType"

-- | @Selector@ for @preservesShuffleMode@
preservesShuffleModeSelector :: Selector '[] Bool
preservesShuffleModeSelector = mkSelector "preservesShuffleMode"

