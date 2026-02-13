{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKPlayer@.
module ObjC.GameKit.GKPlayer
  ( GKPlayer
  , IsGKPlayer(..)
  , scopedIDsArePersistent
  , anonymousGuestPlayerWithIdentifier
  , loadPhotoForSize_withCompletionHandler
  , gamePlayerID
  , teamPlayerID
  , displayName
  , alias
  , guestIdentifier
  , isInvitable
  , isFriend
  , playerID
  , aliasSelector
  , anonymousGuestPlayerWithIdentifierSelector
  , displayNameSelector
  , gamePlayerIDSelector
  , guestIdentifierSelector
  , isFriendSelector
  , isInvitableSelector
  , loadPhotoForSize_withCompletionHandlerSelector
  , playerIDSelector
  , scopedIDsArePersistentSelector
  , teamPlayerIDSelector

  -- * Enum types
  , GKPhotoSize(GKPhotoSize)
  , pattern GKPhotoSizeSmall
  , pattern GKPhotoSizeNormal

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.GameKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | This convenience method checks if the gamePlayerID and the teamPlayerID (scopedIDs) are persistent or unique for the instantiation of this app.
--
-- ObjC selector: @- scopedIDsArePersistent@
scopedIDsArePersistent :: IsGKPlayer gkPlayer => gkPlayer -> IO Bool
scopedIDsArePersistent gkPlayer =
  sendMessage gkPlayer scopedIDsArePersistentSelector

-- | @+ anonymousGuestPlayerWithIdentifier:@
anonymousGuestPlayerWithIdentifier :: IsNSString guestIdentifier => guestIdentifier -> IO (Id GKPlayer)
anonymousGuestPlayerWithIdentifier guestIdentifier =
  do
    cls' <- getRequiredClass "GKPlayer"
    sendClassMessage cls' anonymousGuestPlayerWithIdentifierSelector (toNSString guestIdentifier)

-- | Asynchronously load the player's photo. Error will be nil on success. Possible reasons for error: 1. Communications failure
--
-- ObjC selector: @- loadPhotoForSize:withCompletionHandler:@
loadPhotoForSize_withCompletionHandler :: IsGKPlayer gkPlayer => gkPlayer -> GKPhotoSize -> Ptr () -> IO ()
loadPhotoForSize_withCompletionHandler gkPlayer size completionHandler =
  sendMessage gkPlayer loadPhotoForSize_withCompletionHandlerSelector size completionHandler

-- | This is the player's unique and persistent ID that is scoped to this application.
--
-- ObjC selector: @- gamePlayerID@
gamePlayerID :: IsGKPlayer gkPlayer => gkPlayer -> IO (Id NSString)
gamePlayerID gkPlayer =
  sendMessage gkPlayer gamePlayerIDSelector

-- | This is the player's unique and persistent ID that is scoped to the Apple Store Connect Team identifier of this application.
--
-- ObjC selector: @- teamPlayerID@
teamPlayerID :: IsGKPlayer gkPlayer => gkPlayer -> IO (Id NSString)
teamPlayerID gkPlayer =
  sendMessage gkPlayer teamPlayerIDSelector

-- | This is player's alias to be displayed. The display name may be very long, so be sure to use appropriate string truncation API when drawing.
--
-- ObjC selector: @- displayName@
displayName :: IsGKPlayer gkPlayer => gkPlayer -> IO (Id NSString)
displayName gkPlayer =
  sendMessage gkPlayer displayNameSelector

-- | The alias property contains the player's nickname. When you need to display the name to the user, consider using displayName instead. The nickname is unique but not invariant: the player may change their nickname. The nickname may be very long, so be sure to use appropriate string truncation API when drawing.
--
-- ObjC selector: @- alias@
alias :: IsGKPlayer gkPlayer => gkPlayer -> IO (Id NSString)
alias gkPlayer =
  sendMessage gkPlayer aliasSelector

-- | @- guestIdentifier@
guestIdentifier :: IsGKPlayer gkPlayer => gkPlayer -> IO (Id NSString)
guestIdentifier gkPlayer =
  sendMessage gkPlayer guestIdentifierSelector

-- | @- isInvitable@
isInvitable :: IsGKPlayer gkPlayer => gkPlayer -> IO Bool
isInvitable gkPlayer =
  sendMessage gkPlayer isInvitableSelector

-- | @- isFriend@
isFriend :: IsGKPlayer gkPlayer => gkPlayer -> IO Bool
isFriend gkPlayer =
  sendMessage gkPlayer isFriendSelector

-- | @- playerID@
playerID :: IsGKPlayer gkPlayer => gkPlayer -> IO (Id NSString)
playerID gkPlayer =
  sendMessage gkPlayer playerIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scopedIDsArePersistent@
scopedIDsArePersistentSelector :: Selector '[] Bool
scopedIDsArePersistentSelector = mkSelector "scopedIDsArePersistent"

-- | @Selector@ for @anonymousGuestPlayerWithIdentifier:@
anonymousGuestPlayerWithIdentifierSelector :: Selector '[Id NSString] (Id GKPlayer)
anonymousGuestPlayerWithIdentifierSelector = mkSelector "anonymousGuestPlayerWithIdentifier:"

-- | @Selector@ for @loadPhotoForSize:withCompletionHandler:@
loadPhotoForSize_withCompletionHandlerSelector :: Selector '[GKPhotoSize, Ptr ()] ()
loadPhotoForSize_withCompletionHandlerSelector = mkSelector "loadPhotoForSize:withCompletionHandler:"

-- | @Selector@ for @gamePlayerID@
gamePlayerIDSelector :: Selector '[] (Id NSString)
gamePlayerIDSelector = mkSelector "gamePlayerID"

-- | @Selector@ for @teamPlayerID@
teamPlayerIDSelector :: Selector '[] (Id NSString)
teamPlayerIDSelector = mkSelector "teamPlayerID"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @alias@
aliasSelector :: Selector '[] (Id NSString)
aliasSelector = mkSelector "alias"

-- | @Selector@ for @guestIdentifier@
guestIdentifierSelector :: Selector '[] (Id NSString)
guestIdentifierSelector = mkSelector "guestIdentifier"

-- | @Selector@ for @isInvitable@
isInvitableSelector :: Selector '[] Bool
isInvitableSelector = mkSelector "isInvitable"

-- | @Selector@ for @isFriend@
isFriendSelector :: Selector '[] Bool
isFriendSelector = mkSelector "isFriend"

-- | @Selector@ for @playerID@
playerIDSelector :: Selector '[] (Id NSString)
playerIDSelector = mkSelector "playerID"

