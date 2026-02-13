{-# LANGUAGE DataKinds #-}
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
  , composeViewDelegate
  , setComposeViewDelegate
  , addRecipientPlayersSelector
  , addRecipientsWithEmailAddressesSelector
  , addRecipientsWithPlayerIDsSelector
  , composeViewDelegateSelector
  , maxNumberOfRecipientsSelector
  , setComposeViewDelegateSelector
  , setMessageSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' maxNumberOfRecipientsSelector

-- | Specify the message sent to the invitee. A default message will be used if you don't specify one.
--
-- ObjC selector: @- setMessage:@
setMessage :: (IsGKFriendRequestComposeViewController gkFriendRequestComposeViewController, IsNSString message) => gkFriendRequestComposeViewController -> message -> IO ()
setMessage gkFriendRequestComposeViewController message =
  sendMessage gkFriendRequestComposeViewController setMessageSelector (toNSString message)

-- | Add recipients to the request. If you don't specify at least one recipient before presenting the view, the recipients field will be made firstResponder, to encourage the user to add some. If you add more than maxNumberOfRecipients recipients, these methods will throw an exception.
--
-- ObjC selector: @- addRecipientPlayers:@
addRecipientPlayers :: (IsGKFriendRequestComposeViewController gkFriendRequestComposeViewController, IsNSArray players) => gkFriendRequestComposeViewController -> players -> IO ()
addRecipientPlayers gkFriendRequestComposeViewController players =
  sendMessage gkFriendRequestComposeViewController addRecipientPlayersSelector (toNSArray players)

-- | @- addRecipientsWithPlayerIDs:@
addRecipientsWithPlayerIDs :: (IsGKFriendRequestComposeViewController gkFriendRequestComposeViewController, IsNSArray playerIDs) => gkFriendRequestComposeViewController -> playerIDs -> IO ()
addRecipientsWithPlayerIDs gkFriendRequestComposeViewController playerIDs =
  sendMessage gkFriendRequestComposeViewController addRecipientsWithPlayerIDsSelector (toNSArray playerIDs)

-- | @- addRecipientsWithEmailAddresses:@
addRecipientsWithEmailAddresses :: (IsGKFriendRequestComposeViewController gkFriendRequestComposeViewController, IsNSArray emailAddresses) => gkFriendRequestComposeViewController -> emailAddresses -> IO ()
addRecipientsWithEmailAddresses gkFriendRequestComposeViewController emailAddresses =
  sendMessage gkFriendRequestComposeViewController addRecipientsWithEmailAddressesSelector (toNSArray emailAddresses)

-- | @- composeViewDelegate@
composeViewDelegate :: IsGKFriendRequestComposeViewController gkFriendRequestComposeViewController => gkFriendRequestComposeViewController -> IO RawId
composeViewDelegate gkFriendRequestComposeViewController =
  sendMessage gkFriendRequestComposeViewController composeViewDelegateSelector

-- | @- setComposeViewDelegate:@
setComposeViewDelegate :: IsGKFriendRequestComposeViewController gkFriendRequestComposeViewController => gkFriendRequestComposeViewController -> RawId -> IO ()
setComposeViewDelegate gkFriendRequestComposeViewController value =
  sendMessage gkFriendRequestComposeViewController setComposeViewDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maxNumberOfRecipients@
maxNumberOfRecipientsSelector :: Selector '[] CULong
maxNumberOfRecipientsSelector = mkSelector "maxNumberOfRecipients"

-- | @Selector@ for @setMessage:@
setMessageSelector :: Selector '[Id NSString] ()
setMessageSelector = mkSelector "setMessage:"

-- | @Selector@ for @addRecipientPlayers:@
addRecipientPlayersSelector :: Selector '[Id NSArray] ()
addRecipientPlayersSelector = mkSelector "addRecipientPlayers:"

-- | @Selector@ for @addRecipientsWithPlayerIDs:@
addRecipientsWithPlayerIDsSelector :: Selector '[Id NSArray] ()
addRecipientsWithPlayerIDsSelector = mkSelector "addRecipientsWithPlayerIDs:"

-- | @Selector@ for @addRecipientsWithEmailAddresses:@
addRecipientsWithEmailAddressesSelector :: Selector '[Id NSArray] ()
addRecipientsWithEmailAddressesSelector = mkSelector "addRecipientsWithEmailAddresses:"

-- | @Selector@ for @composeViewDelegate@
composeViewDelegateSelector :: Selector '[] RawId
composeViewDelegateSelector = mkSelector "composeViewDelegate"

-- | @Selector@ for @setComposeViewDelegate:@
setComposeViewDelegateSelector :: Selector '[RawId] ()
setComposeViewDelegateSelector = mkSelector "setComposeViewDelegate:"

