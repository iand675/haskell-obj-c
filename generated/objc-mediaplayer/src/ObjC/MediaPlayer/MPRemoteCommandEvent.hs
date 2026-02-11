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

-- | The command that sent the event.
--
-- ObjC selector: @- command@
command :: IsMPRemoteCommandEvent mpRemoteCommandEvent => mpRemoteCommandEvent -> IO (Id MPRemoteCommand)
command mpRemoteCommandEvent  =
  sendMsg mpRemoteCommandEvent (mkSelector "command") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The time when the event occurred.
--
-- ObjC selector: @- timestamp@
timestamp :: IsMPRemoteCommandEvent mpRemoteCommandEvent => mpRemoteCommandEvent -> IO CDouble
timestamp mpRemoteCommandEvent  =
  sendMsg mpRemoteCommandEvent (mkSelector "timestamp") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @command@
commandSelector :: Selector
commandSelector = mkSelector "command"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector
timestampSelector = mkSelector "timestamp"

