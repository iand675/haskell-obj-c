{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPChangeShuffleModeCommandEvent@.
module ObjC.MediaPlayer.MPChangeShuffleModeCommandEvent
  ( MPChangeShuffleModeCommandEvent
  , IsMPChangeShuffleModeCommandEvent(..)
  , shuffleType
  , preservesShuffleMode
  , shuffleTypeSelector
  , preservesShuffleModeSelector

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

-- | The desired shuffle type to use when fulfilling the request.
--
-- ObjC selector: @- shuffleType@
shuffleType :: IsMPChangeShuffleModeCommandEvent mpChangeShuffleModeCommandEvent => mpChangeShuffleModeCommandEvent -> IO MPShuffleType
shuffleType mpChangeShuffleModeCommandEvent  =
  fmap (coerce :: CLong -> MPShuffleType) $ sendMsg mpChangeShuffleModeCommandEvent (mkSelector "shuffleType") retCLong []

-- | Whether or not the selection should be preserved between playback sessions
--
-- ObjC selector: @- preservesShuffleMode@
preservesShuffleMode :: IsMPChangeShuffleModeCommandEvent mpChangeShuffleModeCommandEvent => mpChangeShuffleModeCommandEvent -> IO Bool
preservesShuffleMode mpChangeShuffleModeCommandEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpChangeShuffleModeCommandEvent (mkSelector "preservesShuffleMode") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shuffleType@
shuffleTypeSelector :: Selector
shuffleTypeSelector = mkSelector "shuffleType"

-- | @Selector@ for @preservesShuffleMode@
preservesShuffleModeSelector :: Selector
preservesShuffleModeSelector = mkSelector "preservesShuffleMode"

