{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Command for changing the current repeat mode to use during playback. To update the system's current representation of your app's repeat mode, set the currentRepeatType property on this command to the proper repeat type value.
--
-- Generated bindings for @MPChangeRepeatModeCommand@.
module ObjC.MediaPlayer.MPChangeRepeatModeCommand
  ( MPChangeRepeatModeCommand
  , IsMPChangeRepeatModeCommand(..)
  , currentRepeatType
  , setCurrentRepeatType
  , currentRepeatTypeSelector
  , setCurrentRepeatTypeSelector

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

-- | The app's current repeat mode.
--
-- ObjC selector: @- currentRepeatType@
currentRepeatType :: IsMPChangeRepeatModeCommand mpChangeRepeatModeCommand => mpChangeRepeatModeCommand -> IO MPRepeatType
currentRepeatType mpChangeRepeatModeCommand =
  sendMessage mpChangeRepeatModeCommand currentRepeatTypeSelector

-- | The app's current repeat mode.
--
-- ObjC selector: @- setCurrentRepeatType:@
setCurrentRepeatType :: IsMPChangeRepeatModeCommand mpChangeRepeatModeCommand => mpChangeRepeatModeCommand -> MPRepeatType -> IO ()
setCurrentRepeatType mpChangeRepeatModeCommand value =
  sendMessage mpChangeRepeatModeCommand setCurrentRepeatTypeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentRepeatType@
currentRepeatTypeSelector :: Selector '[] MPRepeatType
currentRepeatTypeSelector = mkSelector "currentRepeatType"

-- | @Selector@ for @setCurrentRepeatType:@
setCurrentRepeatTypeSelector :: Selector '[MPRepeatType] ()
setCurrentRepeatTypeSelector = mkSelector "setCurrentRepeatType:"

