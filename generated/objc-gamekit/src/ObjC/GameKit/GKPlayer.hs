{-# LANGUAGE PatternSynonyms #-}
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
  , displayName
  , alias
  , isInvitable
  , isFriend
  , playerID
  , scopedIDsArePersistentSelector
  , anonymousGuestPlayerWithIdentifierSelector
  , loadPhotoForSize_withCompletionHandlerSelector
  , displayNameSelector
  , aliasSelector
  , isInvitableSelector
  , isFriendSelector
  , playerIDSelector

  -- * Enum types
  , GKPhotoSize(GKPhotoSize)
  , pattern GKPhotoSizeSmall
  , pattern GKPhotoSizeNormal

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
import ObjC.GameKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | This convenience method checks if the gamePlayerID and the teamPlayerID (scopedIDs) are persistent or unique for the instantiation of this app.
--
-- ObjC selector: @- scopedIDsArePersistent@
scopedIDsArePersistent :: IsGKPlayer gkPlayer => gkPlayer -> IO Bool
scopedIDsArePersistent gkPlayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkPlayer (mkSelector "scopedIDsArePersistent") retCULong []

-- | @+ anonymousGuestPlayerWithIdentifier:@
anonymousGuestPlayerWithIdentifier :: IsNSString guestIdentifier => guestIdentifier -> IO (Id GKPlayer)
anonymousGuestPlayerWithIdentifier guestIdentifier =
  do
    cls' <- getRequiredClass "GKPlayer"
    withObjCPtr guestIdentifier $ \raw_guestIdentifier ->
      sendClassMsg cls' (mkSelector "anonymousGuestPlayerWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_guestIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | Asynchronously load the player's photo. Error will be nil on success. Possible reasons for error: 1. Communications failure
--
-- ObjC selector: @- loadPhotoForSize:withCompletionHandler:@
loadPhotoForSize_withCompletionHandler :: IsGKPlayer gkPlayer => gkPlayer -> GKPhotoSize -> Ptr () -> IO ()
loadPhotoForSize_withCompletionHandler gkPlayer  size completionHandler =
  sendMsg gkPlayer (mkSelector "loadPhotoForSize:withCompletionHandler:") retVoid [argCLong (coerce size), argPtr (castPtr completionHandler :: Ptr ())]

-- | This is player's alias to be displayed. The display name may be very long, so be sure to use appropriate string truncation API when drawing.
--
-- ObjC selector: @- displayName@
displayName :: IsGKPlayer gkPlayer => gkPlayer -> IO (Id NSString)
displayName gkPlayer  =
  sendMsg gkPlayer (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The alias property contains the player's nickname. When you need to display the name to the user, consider using displayName instead. The nickname is unique but not invariant: the player may change their nickname. The nickname may be very long, so be sure to use appropriate string truncation API when drawing.
--
-- ObjC selector: @- alias@
alias :: IsGKPlayer gkPlayer => gkPlayer -> IO (Id NSString)
alias gkPlayer  =
  sendMsg gkPlayer (mkSelector "alias") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- isInvitable@
isInvitable :: IsGKPlayer gkPlayer => gkPlayer -> IO Bool
isInvitable gkPlayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkPlayer (mkSelector "isInvitable") retCULong []

-- | @- isFriend@
isFriend :: IsGKPlayer gkPlayer => gkPlayer -> IO Bool
isFriend gkPlayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkPlayer (mkSelector "isFriend") retCULong []

-- | @- playerID@
playerID :: IsGKPlayer gkPlayer => gkPlayer -> IO (Id NSString)
playerID gkPlayer  =
  sendMsg gkPlayer (mkSelector "playerID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scopedIDsArePersistent@
scopedIDsArePersistentSelector :: Selector
scopedIDsArePersistentSelector = mkSelector "scopedIDsArePersistent"

-- | @Selector@ for @anonymousGuestPlayerWithIdentifier:@
anonymousGuestPlayerWithIdentifierSelector :: Selector
anonymousGuestPlayerWithIdentifierSelector = mkSelector "anonymousGuestPlayerWithIdentifier:"

-- | @Selector@ for @loadPhotoForSize:withCompletionHandler:@
loadPhotoForSize_withCompletionHandlerSelector :: Selector
loadPhotoForSize_withCompletionHandlerSelector = mkSelector "loadPhotoForSize:withCompletionHandler:"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @alias@
aliasSelector :: Selector
aliasSelector = mkSelector "alias"

-- | @Selector@ for @isInvitable@
isInvitableSelector :: Selector
isInvitableSelector = mkSelector "isInvitable"

-- | @Selector@ for @isFriend@
isFriendSelector :: Selector
isFriendSelector = mkSelector "isFriend"

-- | @Selector@ for @playerID@
playerIDSelector :: Selector
playerIDSelector = mkSelector "playerID"

