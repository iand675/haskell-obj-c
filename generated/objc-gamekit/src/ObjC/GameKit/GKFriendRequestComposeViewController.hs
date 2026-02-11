{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Standard view controller for sending friend requests to other players. Present modally from the top view controller.
--
-- Generated bindings for @GKFriendRequestComposeViewController@.
module ObjC.GameKit.GKFriendRequestComposeViewController
  ( GKFriendRequestComposeViewController
  , IsGKFriendRequestComposeViewController(..)
  , maxNumberOfRecipients
  , setMessage
  , addRecipientPlayers
  , addRecipientsWithPlayerIDs
  , addRecipientsWithEmailAddresses
  , maxNumberOfRecipientsSelector
  , setMessageSelector
  , addRecipientPlayersSelector
  , addRecipientsWithPlayerIDsSelector
  , addRecipientsWithEmailAddressesSelector


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

import ObjC.GameKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Get the maximum number of recipients permitted
--
-- ObjC selector: @+ maxNumberOfRecipients@
maxNumberOfRecipients :: IO CULong
maxNumberOfRecipients  =
  do
    cls' <- getRequiredClass "GKFriendRequestComposeViewController"
    sendClassMsg cls' (mkSelector "maxNumberOfRecipients") retCULong []

-- | Specify the message sent to the invitee. A default message will be used if you don't specify one.
--
-- ObjC selector: @- setMessage:@
setMessage :: (IsGKFriendRequestComposeViewController gkFriendRequestComposeViewController, IsNSString message) => gkFriendRequestComposeViewController -> message -> IO ()
setMessage gkFriendRequestComposeViewController  message =
withObjCPtr message $ \raw_message ->
    sendMsg gkFriendRequestComposeViewController (mkSelector "setMessage:") retVoid [argPtr (castPtr raw_message :: Ptr ())]

-- | Add recipients to the request. If you don't specify at least one recipient before presenting the view, the recipients field will be made firstResponder, to encourage the user to add some. If you add more than maxNumberOfRecipients recipients, these methods will throw an exception.
--
-- ObjC selector: @- addRecipientPlayers:@
addRecipientPlayers :: (IsGKFriendRequestComposeViewController gkFriendRequestComposeViewController, IsNSArray players) => gkFriendRequestComposeViewController -> players -> IO ()
addRecipientPlayers gkFriendRequestComposeViewController  players =
withObjCPtr players $ \raw_players ->
    sendMsg gkFriendRequestComposeViewController (mkSelector "addRecipientPlayers:") retVoid [argPtr (castPtr raw_players :: Ptr ())]

-- | @- addRecipientsWithPlayerIDs:@
addRecipientsWithPlayerIDs :: (IsGKFriendRequestComposeViewController gkFriendRequestComposeViewController, IsNSArray playerIDs) => gkFriendRequestComposeViewController -> playerIDs -> IO ()
addRecipientsWithPlayerIDs gkFriendRequestComposeViewController  playerIDs =
withObjCPtr playerIDs $ \raw_playerIDs ->
    sendMsg gkFriendRequestComposeViewController (mkSelector "addRecipientsWithPlayerIDs:") retVoid [argPtr (castPtr raw_playerIDs :: Ptr ())]

-- | @- addRecipientsWithEmailAddresses:@
addRecipientsWithEmailAddresses :: (IsGKFriendRequestComposeViewController gkFriendRequestComposeViewController, IsNSArray emailAddresses) => gkFriendRequestComposeViewController -> emailAddresses -> IO ()
addRecipientsWithEmailAddresses gkFriendRequestComposeViewController  emailAddresses =
withObjCPtr emailAddresses $ \raw_emailAddresses ->
    sendMsg gkFriendRequestComposeViewController (mkSelector "addRecipientsWithEmailAddresses:") retVoid [argPtr (castPtr raw_emailAddresses :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maxNumberOfRecipients@
maxNumberOfRecipientsSelector :: Selector
maxNumberOfRecipientsSelector = mkSelector "maxNumberOfRecipients"

-- | @Selector@ for @setMessage:@
setMessageSelector :: Selector
setMessageSelector = mkSelector "setMessage:"

-- | @Selector@ for @addRecipientPlayers:@
addRecipientPlayersSelector :: Selector
addRecipientPlayersSelector = mkSelector "addRecipientPlayers:"

-- | @Selector@ for @addRecipientsWithPlayerIDs:@
addRecipientsWithPlayerIDsSelector :: Selector
addRecipientsWithPlayerIDsSelector = mkSelector "addRecipientsWithPlayerIDs:"

-- | @Selector@ for @addRecipientsWithEmailAddresses:@
addRecipientsWithEmailAddressesSelector :: Selector
addRecipientsWithEmailAddressesSelector = mkSelector "addRecipientsWithEmailAddresses:"

