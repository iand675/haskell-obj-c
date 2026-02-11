{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPChangeRepeatModeCommandEvent@.
module ObjC.MediaPlayer.MPChangeRepeatModeCommandEvent
  ( MPChangeRepeatModeCommandEvent
  , IsMPChangeRepeatModeCommandEvent(..)
  , repeatType
  , preservesRepeatMode
  , repeatTypeSelector
  , preservesRepeatModeSelector

  -- * Enum types
  , MPRepeatType(MPRepeatType)
  , pattern MPRepeatTypeOff
  , pattern MPRepeatTypeOne
  , pattern MPRepeatTypeAll

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

-- | The desired repeat type to use when fulfilling the request.
--
-- ObjC selector: @- repeatType@
repeatType :: IsMPChangeRepeatModeCommandEvent mpChangeRepeatModeCommandEvent => mpChangeRepeatModeCommandEvent -> IO MPRepeatType
repeatType mpChangeRepeatModeCommandEvent  =
  fmap (coerce :: CLong -> MPRepeatType) $ sendMsg mpChangeRepeatModeCommandEvent (mkSelector "repeatType") retCLong []

-- | Whether or not the selection should be preserved between playback sessions
--
-- ObjC selector: @- preservesRepeatMode@
preservesRepeatMode :: IsMPChangeRepeatModeCommandEvent mpChangeRepeatModeCommandEvent => mpChangeRepeatModeCommandEvent -> IO Bool
preservesRepeatMode mpChangeRepeatModeCommandEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpChangeRepeatModeCommandEvent (mkSelector "preservesRepeatMode") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @repeatType@
repeatTypeSelector :: Selector
repeatTypeSelector = mkSelector "repeatType"

-- | @Selector@ for @preservesRepeatMode@
preservesRepeatModeSelector :: Selector
preservesRepeatModeSelector = mkSelector "preservesRepeatMode"

