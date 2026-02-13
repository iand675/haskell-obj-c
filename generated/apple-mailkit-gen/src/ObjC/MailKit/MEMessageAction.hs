{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An action that can be performed on a mail message.
--
-- Generated bindings for @MEMessageAction@.
module ObjC.MailKit.MEMessageAction
  ( MEMessageAction
  , IsMEMessageAction(..)
  , flagActionWithFlag
  , setBackgroundColorActionWithColor
  , init_
  , new
  , moveToTrashAction
  , moveToArchiveAction
  , moveToJunkAction
  , markAsReadAction
  , markAsUnreadAction
  , flagActionWithFlagSelector
  , initSelector
  , markAsReadActionSelector
  , markAsUnreadActionSelector
  , moveToArchiveActionSelector
  , moveToJunkActionSelector
  , moveToTrashActionSelector
  , newSelector
  , setBackgroundColorActionWithColorSelector

  -- * Enum types
  , MEMessageActionFlag(MEMessageActionFlag)
  , pattern MEMessageActionFlagNone
  , pattern MEMessageActionFlagDefaultColor
  , pattern MEMessageActionFlagRed
  , pattern MEMessageActionFlagOrange
  , pattern MEMessageActionFlagYellow
  , pattern MEMessageActionFlagGreen
  , pattern MEMessageActionFlagBlue
  , pattern MEMessageActionFlagPurple
  , pattern MEMessageActionFlagGray
  , MEMessageActionMessageColor(MEMessageActionMessageColor)
  , pattern MEMessageActionMessageColorNone
  , pattern MEMessageActionMessageColorGreen
  , pattern MEMessageActionMessageColorYellow
  , pattern MEMessageActionMessageColorOrange
  , pattern MEMessageActionMessageColorRed
  , pattern MEMessageActionMessageColorPurple
  , pattern MEMessageActionMessageColorBlue
  , pattern MEMessageActionMessageColorGray

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MailKit.Internal.Classes
import ObjC.MailKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Marks the message as flagged with the provided color.
--
-- ObjC selector: @+ flagActionWithFlag:@
flagActionWithFlag :: MEMessageActionFlag -> IO (Id MEMessageAction)
flagActionWithFlag flag =
  do
    cls' <- getRequiredClass "MEMessageAction"
    sendClassMessage cls' flagActionWithFlagSelector flag

-- | Adds a color to the message when shown in the message list.
--
-- ObjC selector: @+ setBackgroundColorActionWithColor:@
setBackgroundColorActionWithColor :: MEMessageActionMessageColor -> IO (Id MEMessageAction)
setBackgroundColorActionWithColor color =
  do
    cls' <- getRequiredClass "MEMessageAction"
    sendClassMessage cls' setBackgroundColorActionWithColorSelector color

-- | @- init@
init_ :: IsMEMessageAction meMessageAction => meMessageAction -> IO (Id MEMessageAction)
init_ meMessageAction =
  sendOwnedMessage meMessageAction initSelector

-- | @+ new@
new :: IO (Id MEMessageAction)
new  =
  do
    cls' <- getRequiredClass "MEMessageAction"
    sendOwnedClassMessage cls' newSelector

-- | Moves the mail message to the user's trash mailbox for the account.
--
-- ObjC selector: @+ moveToTrashAction@
moveToTrashAction :: IO (Id MEMessageAction)
moveToTrashAction  =
  do
    cls' <- getRequiredClass "MEMessageAction"
    sendClassMessage cls' moveToTrashActionSelector

-- | Moves the mail message to the user's archive mailbox for the account.
--
-- ObjC selector: @+ moveToArchiveAction@
moveToArchiveAction :: IO (Id MEMessageAction)
moveToArchiveAction  =
  do
    cls' <- getRequiredClass "MEMessageAction"
    sendClassMessage cls' moveToArchiveActionSelector

-- | Moves the mail message to the user's junk mailbox for the account.
--
-- ObjC selector: @+ moveToJunkAction@
moveToJunkAction :: IO (Id MEMessageAction)
moveToJunkAction  =
  do
    cls' <- getRequiredClass "MEMessageAction"
    sendClassMessage cls' moveToJunkActionSelector

-- | Marks the mail message as read.
--
-- ObjC selector: @+ markAsReadAction@
markAsReadAction :: IO (Id MEMessageAction)
markAsReadAction  =
  do
    cls' <- getRequiredClass "MEMessageAction"
    sendClassMessage cls' markAsReadActionSelector

-- | Marks the mail  message as unread.
--
-- ObjC selector: @+ markAsUnreadAction@
markAsUnreadAction :: IO (Id MEMessageAction)
markAsUnreadAction  =
  do
    cls' <- getRequiredClass "MEMessageAction"
    sendClassMessage cls' markAsUnreadActionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @flagActionWithFlag:@
flagActionWithFlagSelector :: Selector '[MEMessageActionFlag] (Id MEMessageAction)
flagActionWithFlagSelector = mkSelector "flagActionWithFlag:"

-- | @Selector@ for @setBackgroundColorActionWithColor:@
setBackgroundColorActionWithColorSelector :: Selector '[MEMessageActionMessageColor] (Id MEMessageAction)
setBackgroundColorActionWithColorSelector = mkSelector "setBackgroundColorActionWithColor:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MEMessageAction)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MEMessageAction)
newSelector = mkSelector "new"

-- | @Selector@ for @moveToTrashAction@
moveToTrashActionSelector :: Selector '[] (Id MEMessageAction)
moveToTrashActionSelector = mkSelector "moveToTrashAction"

-- | @Selector@ for @moveToArchiveAction@
moveToArchiveActionSelector :: Selector '[] (Id MEMessageAction)
moveToArchiveActionSelector = mkSelector "moveToArchiveAction"

-- | @Selector@ for @moveToJunkAction@
moveToJunkActionSelector :: Selector '[] (Id MEMessageAction)
moveToJunkActionSelector = mkSelector "moveToJunkAction"

-- | @Selector@ for @markAsReadAction@
markAsReadActionSelector :: Selector '[] (Id MEMessageAction)
markAsReadActionSelector = mkSelector "markAsReadAction"

-- | @Selector@ for @markAsUnreadAction@
markAsUnreadActionSelector :: Selector '[] (Id MEMessageAction)
markAsUnreadActionSelector = mkSelector "markAsUnreadAction"

