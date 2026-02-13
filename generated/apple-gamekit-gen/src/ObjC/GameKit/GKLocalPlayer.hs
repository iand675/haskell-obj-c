{-# LANGUAGE DataKinds #-}
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
  , local
  , localPlayer
  , authenticated
  , underage
  , multiplayerGamingRestricted
  , personalizedCommunicationRestricted
  , authenticateHandler
  , setAuthenticateHandler
  , isPresentingFriendRequestViewController
  , friends
  , authenticateHandlerSelector
  , authenticateWithCompletionHandlerSelector
  , authenticatedSelector
  , deleteSavedGamesWithName_completionHandlerSelector
  , fetchItemsForIdentityVerificationSignatureSelector
  , friendsSelector
  , generateIdentityVerificationSignatureWithCompletionHandlerSelector
  , isPresentingFriendRequestViewControllerSelector
  , loadDefaultLeaderboardCategoryIDWithCompletionHandlerSelector
  , loadDefaultLeaderboardIdentifierWithCompletionHandlerSelector
  , loadFriendsAuthorizationStatusSelector
  , localPlayerSelector
  , localSelector
  , multiplayerGamingRestrictedSelector
  , personalizedCommunicationRestrictedSelector
  , presentFriendRequestCreatorFromViewController_errorSelector
  , presentFriendRequestCreatorFromWindow_errorSelector
  , registerListenerSelector
  , saveGameData_withName_completionHandlerSelector
  , setAuthenticateHandlerSelector
  , setDefaultLeaderboardCategoryID_completionHandlerSelector
  , setDefaultLeaderboardIdentifier_completionHandlerSelector
  , underageSelector
  , unregisterAllListenersSelector
  , unregisterListenerSelector


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

-- | Generates a signature allowing 3rd party server to authenticate the GKLocalPlayer    Possible reasons for error:    1. Communications problem    2. Unauthenticated player
--
-- ObjC selector: @- fetchItemsForIdentityVerificationSignature:@
fetchItemsForIdentityVerificationSignature :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> Ptr () -> IO ()
fetchItemsForIdentityVerificationSignature gkLocalPlayer completionHandler =
  sendMessage gkLocalPlayer fetchItemsForIdentityVerificationSignatureSelector completionHandler

-- | Asynchronously save game data. If a saved game with that name already exists it is overwritten, otherwise a new one is created. The completion handler is called with the new / modified GKSavedGame or an error. If the saved game was in conflict then the overwritten version will be the one with the same deviceName if present, otherwise the most recent overall.
--
-- ObjC selector: @- saveGameData:withName:completionHandler:@
saveGameData_withName_completionHandler :: (IsGKLocalPlayer gkLocalPlayer, IsNSData data_, IsNSString name) => gkLocalPlayer -> data_ -> name -> Ptr () -> IO ()
saveGameData_withName_completionHandler gkLocalPlayer data_ name handler =
  sendMessage gkLocalPlayer saveGameData_withName_completionHandlerSelector (toNSData data_) (toNSString name) handler

-- | Asynchronously delete saved games with the given name. The completion handler will indicate whether or not the deletion was successful.
--
-- ObjC selector: @- deleteSavedGamesWithName:completionHandler:@
deleteSavedGamesWithName_completionHandler :: (IsGKLocalPlayer gkLocalPlayer, IsNSString name) => gkLocalPlayer -> name -> Ptr () -> IO ()
deleteSavedGamesWithName_completionHandler gkLocalPlayer name handler =
  sendMessage gkLocalPlayer deleteSavedGamesWithName_completionHandlerSelector (toNSString name) handler

-- | presentFriendRequestCreatorFromViewController:
--
-- Discussion:      iOS only. When invoked, a Messages sheet will be presented on the viewController passed in, using the existing flow of presentation on behalf of an application.      If an error is returned, control are returned directly to the application, without presentation.
--
-- Possible reasons for error:          - The local player user account is not allowed to add friends            - The device is not allowing outgoing traffic at the time of the operation
--
-- ObjC selector: @- presentFriendRequestCreatorFromViewController:error:@
presentFriendRequestCreatorFromViewController_error :: (IsGKLocalPlayer gkLocalPlayer, IsNSViewController viewController, IsNSError error_) => gkLocalPlayer -> viewController -> error_ -> IO Bool
presentFriendRequestCreatorFromViewController_error gkLocalPlayer viewController error_ =
  sendMessage gkLocalPlayer presentFriendRequestCreatorFromViewController_errorSelector (toNSViewController viewController) (toNSError error_)

-- | presentFriendRequestCreatorFromWindow:
--
-- Discussion:      MacOS only. When invoked, if no error is encountered, the caller application is backgrounded and the 'Messages' application is launched/foregrounded, with a formatted friend request message.      If an error occurs, controls are returned to the caller application, with an error describing the error.
--
-- Possible reasons for error:          - The local player user account is not allowed to add friends            - The device is not allowing outgoing traffic at the time of the operation
--
-- ObjC selector: @- presentFriendRequestCreatorFromWindow:error:@
presentFriendRequestCreatorFromWindow_error :: (IsGKLocalPlayer gkLocalPlayer, IsNSWindow window, IsNSError error_) => gkLocalPlayer -> window -> error_ -> IO Bool
presentFriendRequestCreatorFromWindow_error gkLocalPlayer window error_ =
  sendMessage gkLocalPlayer presentFriendRequestCreatorFromWindow_errorSelector (toNSWindow window) (toNSError error_)

-- | @- loadFriendsAuthorizationStatus:@
loadFriendsAuthorizationStatus :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> Ptr () -> IO ()
loadFriendsAuthorizationStatus gkLocalPlayer completionHandler =
  sendMessage gkLocalPlayer loadFriendsAuthorizationStatusSelector completionHandler

-- | @- setDefaultLeaderboardCategoryID:completionHandler:@
setDefaultLeaderboardCategoryID_completionHandler :: (IsGKLocalPlayer gkLocalPlayer, IsNSString categoryID) => gkLocalPlayer -> categoryID -> Ptr () -> IO ()
setDefaultLeaderboardCategoryID_completionHandler gkLocalPlayer categoryID completionHandler =
  sendMessage gkLocalPlayer setDefaultLeaderboardCategoryID_completionHandlerSelector (toNSString categoryID) completionHandler

-- | @- loadDefaultLeaderboardCategoryIDWithCompletionHandler:@
loadDefaultLeaderboardCategoryIDWithCompletionHandler :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> Ptr () -> IO ()
loadDefaultLeaderboardCategoryIDWithCompletionHandler gkLocalPlayer completionHandler =
  sendMessage gkLocalPlayer loadDefaultLeaderboardCategoryIDWithCompletionHandlerSelector completionHandler

-- | @- authenticateWithCompletionHandler:@
authenticateWithCompletionHandler :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> Ptr () -> IO ()
authenticateWithCompletionHandler gkLocalPlayer completionHandler =
  sendMessage gkLocalPlayer authenticateWithCompletionHandlerSelector completionHandler

-- | Generates a signature allowing 3rd party server to authenticate the GKLocalPlayer    Possible reasons for error:    1. Communications problem    2. Unauthenticated player
--
-- ObjC selector: @- generateIdentityVerificationSignatureWithCompletionHandler:@
generateIdentityVerificationSignatureWithCompletionHandler :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> Ptr () -> IO ()
generateIdentityVerificationSignatureWithCompletionHandler gkLocalPlayer completionHandler =
  sendMessage gkLocalPlayer generateIdentityVerificationSignatureWithCompletionHandlerSelector completionHandler

-- | Load the default leaderboard identifier for the local player    Possible reasons for error:    1. Communications problem    2. Unauthenticated player    3. Leaderboard not present
--
-- ObjC selector: @- loadDefaultLeaderboardIdentifierWithCompletionHandler:@
loadDefaultLeaderboardIdentifierWithCompletionHandler :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> Ptr () -> IO ()
loadDefaultLeaderboardIdentifierWithCompletionHandler gkLocalPlayer completionHandler =
  sendMessage gkLocalPlayer loadDefaultLeaderboardIdentifierWithCompletionHandlerSelector completionHandler

-- | Set the default leaderboard for the current game    Possible reasons for error:    1. Communications problem    2. Unauthenticated player    3. Leaderboard not present
--
-- ObjC selector: @- setDefaultLeaderboardIdentifier:completionHandler:@
setDefaultLeaderboardIdentifier_completionHandler :: (IsGKLocalPlayer gkLocalPlayer, IsNSString leaderboardIdentifier) => gkLocalPlayer -> leaderboardIdentifier -> Ptr () -> IO ()
setDefaultLeaderboardIdentifier_completionHandler gkLocalPlayer leaderboardIdentifier completionHandler =
  sendMessage gkLocalPlayer setDefaultLeaderboardIdentifier_completionHandlerSelector (toNSString leaderboardIdentifier) completionHandler

-- | A single listener may be registered once. Registering multiple times results in undefined behavior. The registered listener will receive callbacks for any selector it responds to.
--
-- ObjC selector: @- registerListener:@
registerListener :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> RawId -> IO ()
registerListener gkLocalPlayer listener =
  sendMessage gkLocalPlayer registerListenerSelector listener

-- | @- unregisterListener:@
unregisterListener :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> RawId -> IO ()
unregisterListener gkLocalPlayer listener =
  sendMessage gkLocalPlayer unregisterListenerSelector listener

-- | @- unregisterAllListeners@
unregisterAllListeners :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> IO ()
unregisterAllListeners gkLocalPlayer =
  sendMessage gkLocalPlayer unregisterAllListenersSelector

-- | Obtain the primary GKLocalPlayer object.    The player is only available for offline play until logged in.    A temporary player is created if no account is set up.
--
-- ObjC selector: @+ local@
local :: IO (Id GKLocalPlayer)
local  =
  do
    cls' <- getRequiredClass "GKLocalPlayer"
    sendClassMessage cls' localSelector

-- | @+ localPlayer@
localPlayer :: IO (Id GKLocalPlayer)
localPlayer  =
  do
    cls' <- getRequiredClass "GKLocalPlayer"
    sendClassMessage cls' localPlayerSelector

-- | Authentication state
--
-- ObjC selector: @- authenticated@
authenticated :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> IO Bool
authenticated gkLocalPlayer =
  sendMessage gkLocalPlayer authenticatedSelector

-- | Indicates if a player is under age
--
-- ObjC selector: @- underage@
underage :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> IO Bool
underage gkLocalPlayer =
  sendMessage gkLocalPlayer underageSelector

-- | A Boolean value that declares whether or not multiplayer gaming is restricted on this device.
--
-- ObjC selector: @- multiplayerGamingRestricted@
multiplayerGamingRestricted :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> IO Bool
multiplayerGamingRestricted gkLocalPlayer =
  sendMessage gkLocalPlayer multiplayerGamingRestrictedSelector

-- | A Boolean value that declares whether personalized communication is restricted on this device. If it is restricted, the player will not be able to read or write personalized messages on game invites, challenges, or enable voice communication in multiplayer games.  Note: this value will always be true when isUnderage is true.
--
-- ObjC selector: @- personalizedCommunicationRestricted@
personalizedCommunicationRestricted :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> IO Bool
personalizedCommunicationRestricted gkLocalPlayer =
  sendMessage gkLocalPlayer personalizedCommunicationRestrictedSelector

-- | @- authenticateHandler@
authenticateHandler :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> IO (Ptr ())
authenticateHandler gkLocalPlayer =
  sendMessage gkLocalPlayer authenticateHandlerSelector

-- | @- setAuthenticateHandler:@
setAuthenticateHandler :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> Ptr () -> IO ()
setAuthenticateHandler gkLocalPlayer value =
  sendMessage gkLocalPlayer setAuthenticateHandlerSelector value

-- | observable property that becomes true when the friend request view controller is displayed.  It becomes false when it is dismissed
--
-- ObjC selector: @- isPresentingFriendRequestViewController@
isPresentingFriendRequestViewController :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> IO Bool
isPresentingFriendRequestViewController gkLocalPlayer =
  sendMessage gkLocalPlayer isPresentingFriendRequestViewControllerSelector

-- | This property is obsolete. **
--
-- ObjC selector: @- friends@
friends :: IsGKLocalPlayer gkLocalPlayer => gkLocalPlayer -> IO (Id NSArray)
friends gkLocalPlayer =
  sendMessage gkLocalPlayer friendsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchItemsForIdentityVerificationSignature:@
fetchItemsForIdentityVerificationSignatureSelector :: Selector '[Ptr ()] ()
fetchItemsForIdentityVerificationSignatureSelector = mkSelector "fetchItemsForIdentityVerificationSignature:"

-- | @Selector@ for @saveGameData:withName:completionHandler:@
saveGameData_withName_completionHandlerSelector :: Selector '[Id NSData, Id NSString, Ptr ()] ()
saveGameData_withName_completionHandlerSelector = mkSelector "saveGameData:withName:completionHandler:"

-- | @Selector@ for @deleteSavedGamesWithName:completionHandler:@
deleteSavedGamesWithName_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
deleteSavedGamesWithName_completionHandlerSelector = mkSelector "deleteSavedGamesWithName:completionHandler:"

-- | @Selector@ for @presentFriendRequestCreatorFromViewController:error:@
presentFriendRequestCreatorFromViewController_errorSelector :: Selector '[Id NSViewController, Id NSError] Bool
presentFriendRequestCreatorFromViewController_errorSelector = mkSelector "presentFriendRequestCreatorFromViewController:error:"

-- | @Selector@ for @presentFriendRequestCreatorFromWindow:error:@
presentFriendRequestCreatorFromWindow_errorSelector :: Selector '[Id NSWindow, Id NSError] Bool
presentFriendRequestCreatorFromWindow_errorSelector = mkSelector "presentFriendRequestCreatorFromWindow:error:"

-- | @Selector@ for @loadFriendsAuthorizationStatus:@
loadFriendsAuthorizationStatusSelector :: Selector '[Ptr ()] ()
loadFriendsAuthorizationStatusSelector = mkSelector "loadFriendsAuthorizationStatus:"

-- | @Selector@ for @setDefaultLeaderboardCategoryID:completionHandler:@
setDefaultLeaderboardCategoryID_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
setDefaultLeaderboardCategoryID_completionHandlerSelector = mkSelector "setDefaultLeaderboardCategoryID:completionHandler:"

-- | @Selector@ for @loadDefaultLeaderboardCategoryIDWithCompletionHandler:@
loadDefaultLeaderboardCategoryIDWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
loadDefaultLeaderboardCategoryIDWithCompletionHandlerSelector = mkSelector "loadDefaultLeaderboardCategoryIDWithCompletionHandler:"

-- | @Selector@ for @authenticateWithCompletionHandler:@
authenticateWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
authenticateWithCompletionHandlerSelector = mkSelector "authenticateWithCompletionHandler:"

-- | @Selector@ for @generateIdentityVerificationSignatureWithCompletionHandler:@
generateIdentityVerificationSignatureWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
generateIdentityVerificationSignatureWithCompletionHandlerSelector = mkSelector "generateIdentityVerificationSignatureWithCompletionHandler:"

-- | @Selector@ for @loadDefaultLeaderboardIdentifierWithCompletionHandler:@
loadDefaultLeaderboardIdentifierWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
loadDefaultLeaderboardIdentifierWithCompletionHandlerSelector = mkSelector "loadDefaultLeaderboardIdentifierWithCompletionHandler:"

-- | @Selector@ for @setDefaultLeaderboardIdentifier:completionHandler:@
setDefaultLeaderboardIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
setDefaultLeaderboardIdentifier_completionHandlerSelector = mkSelector "setDefaultLeaderboardIdentifier:completionHandler:"

-- | @Selector@ for @registerListener:@
registerListenerSelector :: Selector '[RawId] ()
registerListenerSelector = mkSelector "registerListener:"

-- | @Selector@ for @unregisterListener:@
unregisterListenerSelector :: Selector '[RawId] ()
unregisterListenerSelector = mkSelector "unregisterListener:"

-- | @Selector@ for @unregisterAllListeners@
unregisterAllListenersSelector :: Selector '[] ()
unregisterAllListenersSelector = mkSelector "unregisterAllListeners"

-- | @Selector@ for @local@
localSelector :: Selector '[] (Id GKLocalPlayer)
localSelector = mkSelector "local"

-- | @Selector@ for @localPlayer@
localPlayerSelector :: Selector '[] (Id GKLocalPlayer)
localPlayerSelector = mkSelector "localPlayer"

-- | @Selector@ for @authenticated@
authenticatedSelector :: Selector '[] Bool
authenticatedSelector = mkSelector "authenticated"

-- | @Selector@ for @underage@
underageSelector :: Selector '[] Bool
underageSelector = mkSelector "underage"

-- | @Selector@ for @multiplayerGamingRestricted@
multiplayerGamingRestrictedSelector :: Selector '[] Bool
multiplayerGamingRestrictedSelector = mkSelector "multiplayerGamingRestricted"

-- | @Selector@ for @personalizedCommunicationRestricted@
personalizedCommunicationRestrictedSelector :: Selector '[] Bool
personalizedCommunicationRestrictedSelector = mkSelector "personalizedCommunicationRestricted"

-- | @Selector@ for @authenticateHandler@
authenticateHandlerSelector :: Selector '[] (Ptr ())
authenticateHandlerSelector = mkSelector "authenticateHandler"

-- | @Selector@ for @setAuthenticateHandler:@
setAuthenticateHandlerSelector :: Selector '[Ptr ()] ()
setAuthenticateHandlerSelector = mkSelector "setAuthenticateHandler:"

-- | @Selector@ for @isPresentingFriendRequestViewController@
isPresentingFriendRequestViewControllerSelector :: Selector '[] Bool
isPresentingFriendRequestViewControllerSelector = mkSelector "isPresentingFriendRequestViewController"

-- | @Selector@ for @friends@
friendsSelector :: Selector '[] (Id NSArray)
friendsSelector = mkSelector "friends"

