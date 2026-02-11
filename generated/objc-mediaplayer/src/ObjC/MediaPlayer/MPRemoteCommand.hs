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
  , newSelector
  , initSelector
  , addTarget_actionSelector
  , removeTarget_actionSelector
  , removeTargetSelector
  , addTargetWithHandlerSelector
  , enabledSelector
  , setEnabledSelector


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

-- | @+ new@
new :: IO (Id MPRemoteCommand)
new  =
  do
    cls' <- getRequiredClass "MPRemoteCommand"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMPRemoteCommand mpRemoteCommand => mpRemoteCommand -> IO (Id MPRemoteCommand)
init_ mpRemoteCommand  =
  sendMsg mpRemoteCommand (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- addTarget:action:@
addTarget_action :: IsMPRemoteCommand mpRemoteCommand => mpRemoteCommand -> RawId -> Selector -> IO ()
addTarget_action mpRemoteCommand  target action =
  sendMsg mpRemoteCommand (mkSelector "addTarget:action:") retVoid [argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)]

-- | @- removeTarget:action:@
removeTarget_action :: IsMPRemoteCommand mpRemoteCommand => mpRemoteCommand -> RawId -> Selector -> IO ()
removeTarget_action mpRemoteCommand  target action =
  sendMsg mpRemoteCommand (mkSelector "removeTarget:action:") retVoid [argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)]

-- | @- removeTarget:@
removeTarget :: IsMPRemoteCommand mpRemoteCommand => mpRemoteCommand -> RawId -> IO ()
removeTarget mpRemoteCommand  target =
  sendMsg mpRemoteCommand (mkSelector "removeTarget:") retVoid [argPtr (castPtr (unRawId target) :: Ptr ())]

-- | Returns an opaque object to act as the target.
--
-- ObjC selector: @- addTargetWithHandler:@
addTargetWithHandler :: IsMPRemoteCommand mpRemoteCommand => mpRemoteCommand -> Ptr () -> IO RawId
addTargetWithHandler mpRemoteCommand  handler =
  fmap (RawId . castPtr) $ sendMsg mpRemoteCommand (mkSelector "addTargetWithHandler:") (retPtr retVoid) [argPtr (castPtr handler :: Ptr ())]

-- | Whether a button (for example) should be enabled and tappable for this particular command.
--
-- ObjC selector: @- enabled@
enabled :: IsMPRemoteCommand mpRemoteCommand => mpRemoteCommand -> IO Bool
enabled mpRemoteCommand  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpRemoteCommand (mkSelector "enabled") retCULong []

-- | Whether a button (for example) should be enabled and tappable for this particular command.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsMPRemoteCommand mpRemoteCommand => mpRemoteCommand -> Bool -> IO ()
setEnabled mpRemoteCommand  value =
  sendMsg mpRemoteCommand (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @addTarget:action:@
addTarget_actionSelector :: Selector
addTarget_actionSelector = mkSelector "addTarget:action:"

-- | @Selector@ for @removeTarget:action:@
removeTarget_actionSelector :: Selector
removeTarget_actionSelector = mkSelector "removeTarget:action:"

-- | @Selector@ for @removeTarget:@
removeTargetSelector :: Selector
removeTargetSelector = mkSelector "removeTarget:"

-- | @Selector@ for @addTargetWithHandler:@
addTargetWithHandlerSelector :: Selector
addTargetWithHandlerSelector = mkSelector "addTargetWithHandler:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

