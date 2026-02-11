{-# LANGUAGE PatternSynonyms #-}
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
  , persistentIDSelector
  , cloudGlobalIDSelector
  , nameSelector
  , playlistAttributesSelector
  , seedItemsSelector
  , descriptionTextSelector
  , authorDisplayNameSelector

  -- * Enum types
  , MPMediaPlaylistAttribute(MPMediaPlaylistAttribute)
  , pattern MPMediaPlaylistAttributeNone
  , pattern MPMediaPlaylistAttributeOnTheGo
  , pattern MPMediaPlaylistAttributeSmart
  , pattern MPMediaPlaylistAttributeGenius

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
import ObjC.MediaPlayer.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- addItemWithProductID:completionHandler:@
addItemWithProductID_completionHandler :: (IsMPMediaPlaylist mpMediaPlaylist, IsNSString productID) => mpMediaPlaylist -> productID -> Ptr () -> IO ()
addItemWithProductID_completionHandler mpMediaPlaylist  productID completionHandler =
withObjCPtr productID $ \raw_productID ->
    sendMsg mpMediaPlaylist (mkSelector "addItemWithProductID:completionHandler:") retVoid [argPtr (castPtr raw_productID :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- addMediaItems:completionHandler:@
addMediaItems_completionHandler :: (IsMPMediaPlaylist mpMediaPlaylist, IsNSArray mediaItems) => mpMediaPlaylist -> mediaItems -> Ptr () -> IO ()
addMediaItems_completionHandler mpMediaPlaylist  mediaItems completionHandler =
withObjCPtr mediaItems $ \raw_mediaItems ->
    sendMsg mpMediaPlaylist (mkSelector "addMediaItems:completionHandler:") retVoid [argPtr (castPtr raw_mediaItems :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- persistentID@
persistentID :: IsMPMediaPlaylist mpMediaPlaylist => mpMediaPlaylist -> IO CULong
persistentID mpMediaPlaylist  =
  sendMsg mpMediaPlaylist (mkSelector "persistentID") retCULong []

-- | @- cloudGlobalID@
cloudGlobalID :: IsMPMediaPlaylist mpMediaPlaylist => mpMediaPlaylist -> IO (Id NSString)
cloudGlobalID mpMediaPlaylist  =
  sendMsg mpMediaPlaylist (mkSelector "cloudGlobalID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- name@
name :: IsMPMediaPlaylist mpMediaPlaylist => mpMediaPlaylist -> IO (Id NSString)
name mpMediaPlaylist  =
  sendMsg mpMediaPlaylist (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- playlistAttributes@
playlistAttributes :: IsMPMediaPlaylist mpMediaPlaylist => mpMediaPlaylist -> IO MPMediaPlaylistAttribute
playlistAttributes mpMediaPlaylist  =
  fmap (coerce :: CULong -> MPMediaPlaylistAttribute) $ sendMsg mpMediaPlaylist (mkSelector "playlistAttributes") retCULong []

-- | @- seedItems@
seedItems :: IsMPMediaPlaylist mpMediaPlaylist => mpMediaPlaylist -> IO (Id NSArray)
seedItems mpMediaPlaylist  =
  sendMsg mpMediaPlaylist (mkSelector "seedItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- descriptionText@
descriptionText :: IsMPMediaPlaylist mpMediaPlaylist => mpMediaPlaylist -> IO (Id NSString)
descriptionText mpMediaPlaylist  =
  sendMsg mpMediaPlaylist (mkSelector "descriptionText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- authorDisplayName@
authorDisplayName :: IsMPMediaPlaylist mpMediaPlaylist => mpMediaPlaylist -> IO (Id NSString)
authorDisplayName mpMediaPlaylist  =
  sendMsg mpMediaPlaylist (mkSelector "authorDisplayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addItemWithProductID:completionHandler:@
addItemWithProductID_completionHandlerSelector :: Selector
addItemWithProductID_completionHandlerSelector = mkSelector "addItemWithProductID:completionHandler:"

-- | @Selector@ for @addMediaItems:completionHandler:@
addMediaItems_completionHandlerSelector :: Selector
addMediaItems_completionHandlerSelector = mkSelector "addMediaItems:completionHandler:"

-- | @Selector@ for @persistentID@
persistentIDSelector :: Selector
persistentIDSelector = mkSelector "persistentID"

-- | @Selector@ for @cloudGlobalID@
cloudGlobalIDSelector :: Selector
cloudGlobalIDSelector = mkSelector "cloudGlobalID"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @playlistAttributes@
playlistAttributesSelector :: Selector
playlistAttributesSelector = mkSelector "playlistAttributes"

-- | @Selector@ for @seedItems@
seedItemsSelector :: Selector
seedItemsSelector = mkSelector "seedItems"

-- | @Selector@ for @descriptionText@
descriptionTextSelector :: Selector
descriptionTextSelector = mkSelector "descriptionText"

-- | @Selector@ for @authorDisplayName@
authorDisplayNameSelector :: Selector
authorDisplayNameSelector = mkSelector "authorDisplayName"

