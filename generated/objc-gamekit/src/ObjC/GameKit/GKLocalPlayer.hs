{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKLocalPlayer@.
module ObjC.GameKit.GKLocalPlayer
  ( GKLocalPlayer
  , IsGKLocalPlayer(..)
  , fetchItemsForIdentityVerificationSignature
  , saveGameData_withName_completionHandler
  , deleteSavedGamesWithName_completionHandler
  , presentFriendRequestCreatorFromViewController_error
  , presentFriendRequestCreatorFromWindow_error
  , loadFriendsAuthorizationStatus
  , setDefaultLeaderboardCategoryID_completionHandler
  , loadDefaultLeaderboardCategoryIDWithCompletionHandler
  , authenticateWithCompletionHandler
  , generateIdentityVerificationSignatureWithCompletionHandler
  , loadDefaultLeaderboardIdentifierWithCompletionHandler
  , setDefaultLeaderboardIdentifier_completionHandler
  , registerListener
  , unregisterListener
  , unregisterAllListeners
  , localPlayer
  , authenticated
  , underage
  , multiplayerGamingRestricted
  , personalizedCommunicationRestricted
  , authenticateHandler
  , setAuthenticateHandler
  , isPresentingFriendRequestViewController
  , fetchItemsForIdentityVerificationSignatureSelector
  , saveGameData_withName_completionHandlerSelector
  , deleteSavedGamesWithName_completionHandlerSelector
  , presentFriendRequestCreatorFromViewController_errorSelector
  , presentFriendRequestCreatorFromWindow_errorSelector
  , loadFriendsAuthorizationStatusSelector
  , setDefaultLeaderboardCategoryID_completionHandlerSelector
  , loadDefaultLeaderboardCategoryIDWithCompletionHandlerSelector
  , authenticateWithCompletionHandlerSelector
  , generateIdentityVerificationSignatureWithCompletionHandlerSelector
  , loadDefaultLeaderboardIdentifierWithCompletionHandlerSelector
  , setDefaultLeaderboardIdentifier_completionHandlerSelector
  , registerListenerSelector
  , unregisterListenerSelector
  , unregisterAllListenersSelector
  , localPlayerSelector
  , authenticatedSelector
  , underageSelector
  , multiplayerGamingRestrictedSelector
  , personalizedCommunicationRestrictedSelector
  , authenticateHandlerSelector
  , setAuthenticateHandlerSelector
  , isPresentingFriendRequestViewControllerSelector


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

-- | Generates a signature allowing 3rd party server to authenticate the GKLocalPlayer    Possible reasons for error:    1. Communications problem    2. Unauthenticated player
--
-- ObjC selector: @- fetchItemsForIdentityVerificationSignature:@
fetchItemsForIdentityVerificationSignature :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> Ptr () -> IO ()
fetchItemsForIdentityVerificationSignature gkLocalPlayer  completionHandler =
  sendMsg gkLocalPlayer (mkSelector "fetchItemsForIdentityVerificationSignature:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Asynchronously save game data. If a saved game with that name already exists it is overwritten, otherwise a new one is created. The completion handler is called with the new / modified GKSavedGame or an error. If the saved game was in conflict then the overwritten version will be the one with the same deviceName if present, otherwise the most recent overall.
--
-- ObjC selector: @- saveGameData:withName:completionHandler:@
saveGameData_withName_completionHandler :: (IsGKLocalPlayer gkLocalPlayer, IsNSData data_, IsNSString name) => gkLocalPlayer -> data_ -> name -> Ptr () -> IO ()
saveGameData_withName_completionHandler gkLocalPlayer  data_ name handler =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr name $ \raw_name ->
      sendMsg gkLocalPlayer (mkSelector "saveGameData:withName:completionHandler:") retVoid [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | Asynchronously delete saved games with the given name. The completion handler will indicate whether or not the deletion was successful.
--
-- ObjC selector: @- deleteSavedGamesWithName:completionHandler:@
deleteSavedGamesWithName_completionHandler :: (IsGKLocalPlayer gkLocalPlayer, IsNSString name) => gkLocalPlayer -> name -> Ptr () -> IO ()
deleteSavedGamesWithName_completionHandler gkLocalPlayer  name handler =
withObjCPtr name $ \raw_name ->
    sendMsg gkLocalPlayer (mkSelector "deleteSavedGamesWithName:completionHandler:") retVoid [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | presentFriendRequestCreatorFromViewController:
--
-- Discussion:      iOS only. When invoked, a Messages sheet will be presented on the viewController passed in, using the existing flow of presentation on behalf of an application.      If an error is returned, control are returned directly to the application, without presentation.
--
-- Possible reasons for error:          - The local player user account is not allowed to add friends            - The device is not allowing outgoing traffic at the time of the operation
--
-- ObjC selector: @- presentFriendRequestCreatorFromViewController:error:@
presentFriendRequestCreatorFromViewController_error :: (IsGKLocalPlayer gkLocalPlayer, IsNSViewController viewController, IsNSError error_) => gkLocalPlayer -> viewController -> error_ -> IO Bool
presentFriendRequestCreatorFromViewController_error gkLocalPlayer  viewController error_ =
withObjCPtr viewController $ \raw_viewController ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkLocalPlayer (mkSelector "presentFriendRequestCreatorFromViewController:error:") retCULong [argPtr (castPtr raw_viewController :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | presentFriendRequestCreatorFromWindow:
--
-- Discussion:      MacOS only. When invoked, if no error is encountered, the caller application is backgrounded and the 'Messages' application is launched/foregrounded, with a formatted friend request message.      If an error occurs, controls are returned to the caller application, with an error describing the error.
--
-- Possible reasons for error:          - The local player user account is not allowed to add friends            - The device is not allowing outgoing traffic at the time of the operation
--
-- ObjC selector: @- presentFriendRequestCreatorFromWindow:error:@
presentFriendRequestCreatorFromWindow_error :: (IsGKLocalPlayer gkLocalPlayer, IsNSWindow window, IsNSError error_) => gkLocalPlayer -> window -> error_ -> IO Bool
presentFriendRequestCreatorFromWindow_error gkLocalPlayer  window error_ =
withObjCPtr window $ \raw_window ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkLocalPlayer (mkSelector "presentFriendRequestCreatorFromWindow:error:") retCULong [argPtr (castPtr raw_window :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- loadFriendsAuthorizationStatus:@
loadFriendsAuthorizationStatus :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> Ptr () -> IO ()
loadFriendsAuthorizationStatus gkLocalPlayer  completionHandler =
  sendMsg gkLocalPlayer (mkSelector "loadFriendsAuthorizationStatus:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- setDefaultLeaderboardCategoryID:completionHandler:@
setDefaultLeaderboardCategoryID_completionHandler :: (IsGKLocalPlayer gkLocalPlayer, IsNSString categoryID) => gkLocalPlayer -> categoryID -> Ptr () -> IO ()
setDefaultLeaderboardCategoryID_completionHandler gkLocalPlayer  categoryID completionHandler =
withObjCPtr categoryID $ \raw_categoryID ->
    sendMsg gkLocalPlayer (mkSelector "setDefaultLeaderboardCategoryID:completionHandler:") retVoid [argPtr (castPtr raw_categoryID :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- loadDefaultLeaderboardCategoryIDWithCompletionHandler:@
loadDefaultLeaderboardCategoryIDWithCompletionHandler :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> Ptr () -> IO ()
loadDefaultLeaderboardCategoryIDWithCompletionHandler gkLocalPlayer  completionHandler =
  sendMsg gkLocalPlayer (mkSelector "loadDefaultLeaderboardCategoryIDWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- authenticateWithCompletionHandler:@
authenticateWithCompletionHandler :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> Ptr () -> IO ()
authenticateWithCompletionHandler gkLocalPlayer  completionHandler =
  sendMsg gkLocalPlayer (mkSelector "authenticateWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Generates a signature allowing 3rd party server to authenticate the GKLocalPlayer    Possible reasons for error:    1. Communications problem    2. Unauthenticated player
--
-- ObjC selector: @- generateIdentityVerificationSignatureWithCompletionHandler:@
generateIdentityVerificationSignatureWithCompletionHandler :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> Ptr () -> IO ()
generateIdentityVerificationSignatureWithCompletionHandler gkLocalPlayer  completionHandler =
  sendMsg gkLocalPlayer (mkSelector "generateIdentityVerificationSignatureWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Load the default leaderboard identifier for the local player    Possible reasons for error:    1. Communications problem    2. Unauthenticated player    3. Leaderboard not present
--
-- ObjC selector: @- loadDefaultLeaderboardIdentifierWithCompletionHandler:@
loadDefaultLeaderboardIdentifierWithCompletionHandler :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> Ptr () -> IO ()
loadDefaultLeaderboardIdentifierWithCompletionHandler gkLocalPlayer  completionHandler =
  sendMsg gkLocalPlayer (mkSelector "loadDefaultLeaderboardIdentifierWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Set the default leaderboard for the current game    Possible reasons for error:    1. Communications problem    2. Unauthenticated player    3. Leaderboard not present
--
-- ObjC selector: @- setDefaultLeaderboardIdentifier:completionHandler:@
setDefaultLeaderboardIdentifier_completionHandler :: (IsGKLocalPlayer gkLocalPlayer, IsNSString leaderboardIdentifier) => gkLocalPlayer -> leaderboardIdentifier -> Ptr () -> IO ()
setDefaultLeaderboardIdentifier_completionHandler gkLocalPlayer  leaderboardIdentifier completionHandler =
withObjCPtr leaderboardIdentifier $ \raw_leaderboardIdentifier ->
    sendMsg gkLocalPlayer (mkSelector "setDefaultLeaderboardIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_leaderboardIdentifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | A single listener may be registered once. Registering multiple times results in undefined behavior. The registered listener will receive callbacks for any selector it responds to.
--
-- ObjC selector: @- registerListener:@
registerListener :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> RawId -> IO ()
registerListener gkLocalPlayer  listener =
  sendMsg gkLocalPlayer (mkSelector "registerListener:") retVoid [argPtr (castPtr (unRawId listener) :: Ptr ())]

-- | @- unregisterListener:@
unregisterListener :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> RawId -> IO ()
unregisterListener gkLocalPlayer  listener =
  sendMsg gkLocalPlayer (mkSelector "unregisterListener:") retVoid [argPtr (castPtr (unRawId listener) :: Ptr ())]

-- | @- unregisterAllListeners@
unregisterAllListeners :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> IO ()
unregisterAllListeners gkLocalPlayer  =
  sendMsg gkLocalPlayer (mkSelector "unregisterAllListeners") retVoid []

-- | @+ localPlayer@
localPlayer :: IO (Id GKLocalPlayer)
localPlayer  =
  do
    cls' <- getRequiredClass "GKLocalPlayer"
    sendClassMsg cls' (mkSelector "localPlayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Authentication state
--
-- ObjC selector: @- authenticated@
authenticated :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> IO Bool
authenticated gkLocalPlayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkLocalPlayer (mkSelector "authenticated") retCULong []

-- | Indicates if a player is under age
--
-- ObjC selector: @- underage@
underage :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> IO Bool
underage gkLocalPlayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkLocalPlayer (mkSelector "underage") retCULong []

-- | A Boolean value that declares whether or not multiplayer gaming is restricted on this device.
--
-- ObjC selector: @- multiplayerGamingRestricted@
multiplayerGamingRestricted :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> IO Bool
multiplayerGamingRestricted gkLocalPlayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkLocalPlayer (mkSelector "multiplayerGamingRestricted") retCULong []

-- | A Boolean value that declares whether personalized communication is restricted on this device. If it is restricted, the player will not be able to read or write personalized messages on game invites, challenges, or enable voice communication in multiplayer games.  Note: this value will always be true when isUnderage is true.
--
-- ObjC selector: @- personalizedCommunicationRestricted@
personalizedCommunicationRestricted :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> IO Bool
personalizedCommunicationRestricted gkLocalPlayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkLocalPlayer (mkSelector "personalizedCommunicationRestricted") retCULong []

-- | @- authenticateHandler@
authenticateHandler :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> IO (Ptr ())
authenticateHandler gkLocalPlayer  =
  fmap castPtr $ sendMsg gkLocalPlayer (mkSelector "authenticateHandler") (retPtr retVoid) []

-- | @- setAuthenticateHandler:@
setAuthenticateHandler :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> Ptr () -> IO ()
setAuthenticateHandler gkLocalPlayer  value =
  sendMsg gkLocalPlayer (mkSelector "setAuthenticateHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | observable property that becomes true when the friend request view controller is displayed.  It becomes false when it is dismissed
--
-- ObjC selector: @- isPresentingFriendRequestViewController@
isPresentingFriendRequestViewController :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> IO Bool
isPresentingFriendRequestViewController gkLocalPlayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkLocalPlayer (mkSelector "isPresentingFriendRequestViewController") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchItemsForIdentityVerificationSignature:@
fetchItemsForIdentityVerificationSignatureSelector :: Selector
fetchItemsForIdentityVerificationSignatureSelector = mkSelector "fetchItemsForIdentityVerificationSignature:"

-- | @Selector@ for @saveGameData:withName:completionHandler:@
saveGameData_withName_completionHandlerSelector :: Selector
saveGameData_withName_completionHandlerSelector = mkSelector "saveGameData:withName:completionHandler:"

-- | @Selector@ for @deleteSavedGamesWithName:completionHandler:@
deleteSavedGamesWithName_completionHandlerSelector :: Selector
deleteSavedGamesWithName_completionHandlerSelector = mkSelector "deleteSavedGamesWithName:completionHandler:"

-- | @Selector@ for @presentFriendRequestCreatorFromViewController:error:@
presentFriendRequestCreatorFromViewController_errorSelector :: Selector
presentFriendRequestCreatorFromViewController_errorSelector = mkSelector "presentFriendRequestCreatorFromViewController:error:"

-- | @Selector@ for @presentFriendRequestCreatorFromWindow:error:@
presentFriendRequestCreatorFromWindow_errorSelector :: Selector
presentFriendRequestCreatorFromWindow_errorSelector = mkSelector "presentFriendRequestCreatorFromWindow:error:"

-- | @Selector@ for @loadFriendsAuthorizationStatus:@
loadFriendsAuthorizationStatusSelector :: Selector
loadFriendsAuthorizationStatusSelector = mkSelector "loadFriendsAuthorizationStatus:"

-- | @Selector@ for @setDefaultLeaderboardCategoryID:completionHandler:@
setDefaultLeaderboardCategoryID_completionHandlerSelector :: Selector
setDefaultLeaderboardCategoryID_completionHandlerSelector = mkSelector "setDefaultLeaderboardCategoryID:completionHandler:"

-- | @Selector@ for @loadDefaultLeaderboardCategoryIDWithCompletionHandler:@
loadDefaultLeaderboardCategoryIDWithCompletionHandlerSelector :: Selector
loadDefaultLeaderboardCategoryIDWithCompletionHandlerSelector = mkSelector "loadDefaultLeaderboardCategoryIDWithCompletionHandler:"

-- | @Selector@ for @authenticateWithCompletionHandler:@
authenticateWithCompletionHandlerSelector :: Selector
authenticateWithCompletionHandlerSelector = mkSelector "authenticateWithCompletionHandler:"

-- | @Selector@ for @generateIdentityVerificationSignatureWithCompletionHandler:@
generateIdentityVerificationSignatureWithCompletionHandlerSelector :: Selector
generateIdentityVerificationSignatureWithCompletionHandlerSelector = mkSelector "generateIdentityVerificationSignatureWithCompletionHandler:"

-- | @Selector@ for @loadDefaultLeaderboardIdentifierWithCompletionHandler:@
loadDefaultLeaderboardIdentifierWithCompletionHandlerSelector :: Selector
loadDefaultLeaderboardIdentifierWithCompletionHandlerSelector = mkSelector "loadDefaultLeaderboardIdentifierWithCompletionHandler:"

-- | @Selector@ for @setDefaultLeaderboardIdentifier:completionHandler:@
setDefaultLeaderboardIdentifier_completionHandlerSelector :: Selector
setDefaultLeaderboardIdentifier_completionHandlerSelector = mkSelector "setDefaultLeaderboardIdentifier:completionHandler:"

-- | @Selector@ for @registerListener:@
registerListenerSelector :: Selector
registerListenerSelector = mkSelector "registerListener:"

-- | @Selector@ for @unregisterListener:@
unregisterListenerSelector :: Selector
unregisterListenerSelector = mkSelector "unregisterListener:"

-- | @Selector@ for @unregisterAllListeners@
unregisterAllListenersSelector :: Selector
unregisterAllListenersSelector = mkSelector "unregisterAllListeners"

-- | @Selector@ for @localPlayer@
localPlayerSelector :: Selector
localPlayerSelector = mkSelector "localPlayer"

-- | @Selector@ for @authenticated@
authenticatedSelector :: Selector
authenticatedSelector = mkSelector "authenticated"

-- | @Selector@ for @underage@
underageSelector :: Selector
underageSelector = mkSelector "underage"

-- | @Selector@ for @multiplayerGamingRestricted@
multiplayerGamingRestrictedSelector :: Selector
multiplayerGamingRestrictedSelector = mkSelector "multiplayerGamingRestricted"

-- | @Selector@ for @personalizedCommunicationRestricted@
personalizedCommunicationRestrictedSelector :: Selector
personalizedCommunicationRestrictedSelector = mkSelector "personalizedCommunicationRestricted"

-- | @Selector@ for @authenticateHandler@
authenticateHandlerSelector :: Selector
authenticateHandlerSelector = mkSelector "authenticateHandler"

-- | @Selector@ for @setAuthenticateHandler:@
setAuthenticateHandlerSelector :: Selector
setAuthenticateHandlerSelector = mkSelector "setAuthenticateHandler:"

-- | @Selector@ for @isPresentingFriendRequestViewController@
isPresentingFriendRequestViewControllerSelector :: Selector
isPresentingFriendRequestViewControllerSelector = mkSelector "isPresentingFriendRequestViewController"

