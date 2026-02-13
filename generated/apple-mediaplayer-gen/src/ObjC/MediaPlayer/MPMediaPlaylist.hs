{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMediaPlaylist@.
module ObjC.MediaPlayer.MPMediaPlaylist
  ( MPMediaPlaylist
  , IsMPMediaPlaylist(..)
  , addItemWithProductID_completionHandler
  , addMediaItems_completionHandler
  , persistentID
  , cloudGlobalID
  , name
  , playlistAttributes
  , seedItems
  , descriptionText
  , authorDisplayName
  , addItemWithProductID_completionHandlerSelector
  , addMediaItems_completionHandlerSelector
  , authorDisplayNameSelector
  , cloudGlobalIDSelector
  , descriptionTextSelector
  , nameSelector
  , persistentIDSelector
  , playlistAttributesSelector
  , seedItemsSelector

  -- * Enum types
  , MPMediaPlaylistAttribute(MPMediaPlaylistAttribute)
  , pattern MPMediaPlaylistAttributeNone
  , pattern MPMediaPlaylistAttributeOnTheGo
  , pattern MPMediaPlaylistAttributeSmart
  , pattern MPMediaPlaylistAttributeGenius

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.MediaPlayer.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- addItemWithProductID:completionHandler:@
addItemWithProductID_completionHandler :: (IsMPMediaPlaylist mpMediaPlaylist, IsNSString productID) => mpMediaPlaylist -> productID -> Ptr () -> IO ()
addItemWithProductID_completionHandler mpMediaPlaylist productID completionHandler =
  sendMessage mpMediaPlaylist addItemWithProductID_completionHandlerSelector (toNSString productID) completionHandler

-- | @- addMediaItems:completionHandler:@
addMediaItems_completionHandler :: (IsMPMediaPlaylist mpMediaPlaylist, IsNSArray mediaItems) => mpMediaPlaylist -> mediaItems -> Ptr () -> IO ()
addMediaItems_completionHandler mpMediaPlaylist mediaItems completionHandler =
  sendMessage mpMediaPlaylist addMediaItems_completionHandlerSelector (toNSArray mediaItems) completionHandler

-- | @- persistentID@
persistentID :: IsMPMediaPlaylist mpMediaPlaylist => mpMediaPlaylist -> IO CULong
persistentID mpMediaPlaylist =
  sendMessage mpMediaPlaylist persistentIDSelector

-- | @- cloudGlobalID@
cloudGlobalID :: IsMPMediaPlaylist mpMediaPlaylist => mpMediaPlaylist -> IO (Id NSString)
cloudGlobalID mpMediaPlaylist =
  sendMessage mpMediaPlaylist cloudGlobalIDSelector

-- | @- name@
name :: IsMPMediaPlaylist mpMediaPlaylist => mpMediaPlaylist -> IO (Id NSString)
name mpMediaPlaylist =
  sendMessage mpMediaPlaylist nameSelector

-- | @- playlistAttributes@
playlistAttributes :: IsMPMediaPlaylist mpMediaPlaylist => mpMediaPlaylist -> IO MPMediaPlaylistAttribute
playlistAttributes mpMediaPlaylist =
  sendMessage mpMediaPlaylist playlistAttributesSelector

-- | @- seedItems@
seedItems :: IsMPMediaPlaylist mpMediaPlaylist => mpMediaPlaylist -> IO (Id NSArray)
seedItems mpMediaPlaylist =
  sendMessage mpMediaPlaylist seedItemsSelector

-- | @- descriptionText@
descriptionText :: IsMPMediaPlaylist mpMediaPlaylist => mpMediaPlaylist -> IO (Id NSString)
descriptionText mpMediaPlaylist =
  sendMessage mpMediaPlaylist descriptionTextSelector

-- | @- authorDisplayName@
authorDisplayName :: IsMPMediaPlaylist mpMediaPlaylist => mpMediaPlaylist -> IO (Id NSString)
authorDisplayName mpMediaPlaylist =
  sendMessage mpMediaPlaylist authorDisplayNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addItemWithProductID:completionHandler:@
addItemWithProductID_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
addItemWithProductID_completionHandlerSelector = mkSelector "addItemWithProductID:completionHandler:"

-- | @Selector@ for @addMediaItems:completionHandler:@
addMediaItems_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
addMediaItems_completionHandlerSelector = mkSelector "addMediaItems:completionHandler:"

-- | @Selector@ for @persistentID@
persistentIDSelector :: Selector '[] CULong
persistentIDSelector = mkSelector "persistentID"

-- | @Selector@ for @cloudGlobalID@
cloudGlobalIDSelector :: Selector '[] (Id NSString)
cloudGlobalIDSelector = mkSelector "cloudGlobalID"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @playlistAttributes@
playlistAttributesSelector :: Selector '[] MPMediaPlaylistAttribute
playlistAttributesSelector = mkSelector "playlistAttributes"

-- | @Selector@ for @seedItems@
seedItemsSelector :: Selector '[] (Id NSArray)
seedItemsSelector = mkSelector "seedItems"

-- | @Selector@ for @descriptionText@
descriptionTextSelector :: Selector '[] (Id NSString)
descriptionTextSelector = mkSelector "descriptionText"

-- | @Selector@ for @authorDisplayName@
authorDisplayNameSelector :: Selector '[] (Id NSString)
authorDisplayNameSelector = mkSelector "authorDisplayName"

