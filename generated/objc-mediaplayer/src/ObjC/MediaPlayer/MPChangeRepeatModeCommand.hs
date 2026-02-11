{-# LANGUAGE PatternSynonyms #-}
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

-- | The app's current repeat mode.
--
-- ObjC selector: @- currentRepeatType@
currentRepeatType :: IsMPChangeRepeatModeCommand mpChangeRepeatModeCommand => mpChangeRepeatModeCommand -> IO MPRepeatType
currentRepeatType mpChangeRepeatModeCommand  =
  fmap (coerce :: CLong -> MPRepeatType) $ sendMsg mpChangeRepeatModeCommand (mkSelector "currentRepeatType") retCLong []

-- | The app's current repeat mode.
--
-- ObjC selector: @- setCurrentRepeatType:@
setCurrentRepeatType :: IsMPChangeRepeatModeCommand mpChangeRepeatModeCommand => mpChangeRepeatModeCommand -> MPRepeatType -> IO ()
setCurrentRepeatType mpChangeRepeatModeCommand  value =
  sendMsg mpChangeRepeatModeCommand (mkSelector "setCurrentRepeatType:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentRepeatType@
currentRepeatTypeSelector :: Selector
currentRepeatTypeSelector = mkSelector "currentRepeatType"

-- | @Selector@ for @setCurrentRepeatType:@
setCurrentRepeatTypeSelector :: Selector
setCurrentRepeatTypeSelector = mkSelector "setCurrentRepeatType:"

