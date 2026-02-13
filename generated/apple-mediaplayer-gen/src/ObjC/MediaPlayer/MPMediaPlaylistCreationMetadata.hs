{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMediaPlaylistCreationMetadata@.
module ObjC.MediaPlayer.MPMediaPlaylistCreationMetadata
  ( MPMediaPlaylistCreationMetadata
  , IsMPMediaPlaylistCreationMetadata(..)
  , new
  , init_
  , initWithName
  , name
  , authorDisplayName
  , setAuthorDisplayName
  , descriptionText
  , setDescriptionText
  , authorDisplayNameSelector
  , descriptionTextSelector
  , initSelector
  , initWithNameSelector
  , nameSelector
  , newSelector
  , setAuthorDisplayNameSelector
  , setDescriptionTextSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MPMediaPlaylistCreationMetadata)
new  =
  do
    cls' <- getRequiredClass "MPMediaPlaylistCreationMetadata"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMPMediaPlaylistCreationMetadata mpMediaPlaylistCreationMetadata => mpMediaPlaylistCreationMetadata -> IO (Id MPMediaPlaylistCreationMetadata)
init_ mpMediaPlaylistCreationMetadata =
  sendOwnedMessage mpMediaPlaylistCreationMetadata initSelector

-- | @- initWithName:@
initWithName :: (IsMPMediaPlaylistCreationMetadata mpMediaPlaylistCreationMetadata, IsNSString name) => mpMediaPlaylistCreationMetadata -> name -> IO (Id MPMediaPlaylistCreationMetadata)
initWithName mpMediaPlaylistCreationMetadata name =
  sendOwnedMessage mpMediaPlaylistCreationMetadata initWithNameSelector (toNSString name)

-- | The display name of the playlist.
--
-- ObjC selector: @- name@
name :: IsMPMediaPlaylistCreationMetadata mpMediaPlaylistCreationMetadata => mpMediaPlaylistCreationMetadata -> IO (Id NSString)
name mpMediaPlaylistCreationMetadata =
  sendMessage mpMediaPlaylistCreationMetadata nameSelector

-- | Defaults to the requesting app's display name.
--
-- ObjC selector: @- authorDisplayName@
authorDisplayName :: IsMPMediaPlaylistCreationMetadata mpMediaPlaylistCreationMetadata => mpMediaPlaylistCreationMetadata -> IO (Id NSString)
authorDisplayName mpMediaPlaylistCreationMetadata =
  sendMessage mpMediaPlaylistCreationMetadata authorDisplayNameSelector

-- | Defaults to the requesting app's display name.
--
-- ObjC selector: @- setAuthorDisplayName:@
setAuthorDisplayName :: (IsMPMediaPlaylistCreationMetadata mpMediaPlaylistCreationMetadata, IsNSString value) => mpMediaPlaylistCreationMetadata -> value -> IO ()
setAuthorDisplayName mpMediaPlaylistCreationMetadata value =
  sendMessage mpMediaPlaylistCreationMetadata setAuthorDisplayNameSelector (toNSString value)

-- | @- descriptionText@
descriptionText :: IsMPMediaPlaylistCreationMetadata mpMediaPlaylistCreationMetadata => mpMediaPlaylistCreationMetadata -> IO (Id NSString)
descriptionText mpMediaPlaylistCreationMetadata =
  sendMessage mpMediaPlaylistCreationMetadata descriptionTextSelector

-- | @- setDescriptionText:@
setDescriptionText :: (IsMPMediaPlaylistCreationMetadata mpMediaPlaylistCreationMetadata, IsNSString value) => mpMediaPlaylistCreationMetadata -> value -> IO ()
setDescriptionText mpMediaPlaylistCreationMetadata value =
  sendMessage mpMediaPlaylistCreationMetadata setDescriptionTextSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MPMediaPlaylistCreationMetadata)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPMediaPlaylistCreationMetadata)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector '[Id NSString] (Id MPMediaPlaylistCreationMetadata)
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @authorDisplayName@
authorDisplayNameSelector :: Selector '[] (Id NSString)
authorDisplayNameSelector = mkSelector "authorDisplayName"

-- | @Selector@ for @setAuthorDisplayName:@
setAuthorDisplayNameSelector :: Selector '[Id NSString] ()
setAuthorDisplayNameSelector = mkSelector "setAuthorDisplayName:"

-- | @Selector@ for @descriptionText@
descriptionTextSelector :: Selector '[] (Id NSString)
descriptionTextSelector = mkSelector "descriptionText"

-- | @Selector@ for @setDescriptionText:@
setDescriptionTextSelector :: Selector '[Id NSString] ()
setDescriptionTextSelector = mkSelector "setDescriptionText:"

