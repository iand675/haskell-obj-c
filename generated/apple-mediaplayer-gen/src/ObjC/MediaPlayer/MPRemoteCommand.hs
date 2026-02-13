{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPRemoteCommand@.
module ObjC.MediaPlayer.MPRemoteCommand
  ( MPRemoteCommand
  , IsMPRemoteCommand(..)
  , new
  , init_
  , addTarget_action
  , removeTarget_action
  , removeTarget
  , addTargetWithHandler
  , enabled
  , setEnabled
  , addTargetWithHandlerSelector
  , addTarget_actionSelector
  , enabledSelector
  , initSelector
  , newSelector
  , removeTargetSelector
  , removeTarget_actionSelector
  , setEnabledSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MPRemoteCommand)
new  =
  do
    cls' <- getRequiredClass "MPRemoteCommand"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMPRemoteCommand mpRemoteCommand => mpRemoteCommand -> IO (Id MPRemoteCommand)
init_ mpRemoteCommand =
  sendOwnedMessage mpRemoteCommand initSelector

-- | @- addTarget:action:@
addTarget_action :: IsMPRemoteCommand mpRemoteCommand => mpRemoteCommand -> RawId -> Sel -> IO ()
addTarget_action mpRemoteCommand target action =
  sendMessage mpRemoteCommand addTarget_actionSelector target action

-- | @- removeTarget:action:@
removeTarget_action :: IsMPRemoteCommand mpRemoteCommand => mpRemoteCommand -> RawId -> Sel -> IO ()
removeTarget_action mpRemoteCommand target action =
  sendMessage mpRemoteCommand removeTarget_actionSelector target action

-- | @- removeTarget:@
removeTarget :: IsMPRemoteCommand mpRemoteCommand => mpRemoteCommand -> RawId -> IO ()
removeTarget mpRemoteCommand target =
  sendMessage mpRemoteCommand removeTargetSelector target

-- | Returns an opaque object to act as the target.
--
-- ObjC selector: @- addTargetWithHandler:@
addTargetWithHandler :: IsMPRemoteCommand mpRemoteCommand => mpRemoteCommand -> Ptr () -> IO RawId
addTargetWithHandler mpRemoteCommand handler =
  sendMessage mpRemoteCommand addTargetWithHandlerSelector handler

-- | Whether a button (for example) should be enabled and tappable for this particular command.
--
-- ObjC selector: @- enabled@
enabled :: IsMPRemoteCommand mpRemoteCommand => mpRemoteCommand -> IO Bool
enabled mpRemoteCommand =
  sendMessage mpRemoteCommand enabledSelector

-- | Whether a button (for example) should be enabled and tappable for this particular command.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsMPRemoteCommand mpRemoteCommand => mpRemoteCommand -> Bool -> IO ()
setEnabled mpRemoteCommand value =
  sendMessage mpRemoteCommand setEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MPRemoteCommand)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPRemoteCommand)
initSelector = mkSelector "init"

-- | @Selector@ for @addTarget:action:@
addTarget_actionSelector :: Selector '[RawId, Sel] ()
addTarget_actionSelector = mkSelector "addTarget:action:"

-- | @Selector@ for @removeTarget:action:@
removeTarget_actionSelector :: Selector '[RawId, Sel] ()
removeTarget_actionSelector = mkSelector "removeTarget:action:"

-- | @Selector@ for @removeTarget:@
removeTargetSelector :: Selector '[RawId] ()
removeTargetSelector = mkSelector "removeTarget:"

-- | @Selector@ for @addTargetWithHandler:@
addTargetWithHandlerSelector :: Selector '[Ptr ()] RawId
addTargetWithHandlerSelector = mkSelector "addTargetWithHandler:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

