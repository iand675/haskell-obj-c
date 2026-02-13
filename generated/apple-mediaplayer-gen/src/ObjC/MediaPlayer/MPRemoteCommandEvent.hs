{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPRemoteCommandEvent@.
module ObjC.MediaPlayer.MPRemoteCommandEvent
  ( MPRemoteCommandEvent
  , IsMPRemoteCommandEvent(..)
  , command
  , timestamp
  , commandSelector
  , timestampSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The command that sent the event.
--
-- ObjC selector: @- command@
command :: IsMPRemoteCommandEvent mpRemoteCommandEvent => mpRemoteCommandEvent -> IO (Id MPRemoteCommand)
command mpRemoteCommandEvent =
  sendMessage mpRemoteCommandEvent commandSelector

-- | The time when the event occurred.
--
-- ObjC selector: @- timestamp@
timestamp :: IsMPRemoteCommandEvent mpRemoteCommandEvent => mpRemoteCommandEvent -> IO CDouble
timestamp mpRemoteCommandEvent =
  sendMessage mpRemoteCommandEvent timestampSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @command@
commandSelector :: Selector '[] (Id MPRemoteCommand)
commandSelector = mkSelector "command"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector '[] CDouble
timestampSelector = mkSelector "timestamp"

