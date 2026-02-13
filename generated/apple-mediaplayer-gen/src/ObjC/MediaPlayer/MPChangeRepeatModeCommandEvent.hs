{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPChangeRepeatModeCommandEvent@.
module ObjC.MediaPlayer.MPChangeRepeatModeCommandEvent
  ( MPChangeRepeatModeCommandEvent
  , IsMPChangeRepeatModeCommandEvent(..)
  , repeatType
  , preservesRepeatMode
  , preservesRepeatModeSelector
  , repeatTypeSelector

  -- * Enum types
  , MPRepeatType(MPRepeatType)
  , pattern MPRepeatTypeOff
  , pattern MPRepeatTypeOne
  , pattern MPRepeatTypeAll

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

-- | The desired repeat type to use when fulfilling the request.
--
-- ObjC selector: @- repeatType@
repeatType :: IsMPChangeRepeatModeCommandEvent mpChangeRepeatModeCommandEvent => mpChangeRepeatModeCommandEvent -> IO MPRepeatType
repeatType mpChangeRepeatModeCommandEvent =
  sendMessage mpChangeRepeatModeCommandEvent repeatTypeSelector

-- | Whether or not the selection should be preserved between playback sessions
--
-- ObjC selector: @- preservesRepeatMode@
preservesRepeatMode :: IsMPChangeRepeatModeCommandEvent mpChangeRepeatModeCommandEvent => mpChangeRepeatModeCommandEvent -> IO Bool
preservesRepeatMode mpChangeRepeatModeCommandEvent =
  sendMessage mpChangeRepeatModeCommandEvent preservesRepeatModeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @repeatType@
repeatTypeSelector :: Selector '[] MPRepeatType
repeatTypeSelector = mkSelector "repeatType"

-- | @Selector@ for @preservesRepeatMode@
preservesRepeatModeSelector :: Selector '[] Bool
preservesRepeatModeSelector = mkSelector "preservesRepeatMode"

