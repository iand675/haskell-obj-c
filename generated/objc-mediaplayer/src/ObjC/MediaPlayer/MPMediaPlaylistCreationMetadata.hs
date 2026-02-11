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
  , newSelector
  , initSelector
  , initWithNameSelector
  , nameSelector
  , authorDisplayNameSelector
  , setAuthorDisplayNameSelector
  , descriptionTextSelector
  , setDescriptionTextSelector


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
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MPMediaPlaylistCreationMetadata)
new  =
  do
    cls' <- getRequiredClass "MPMediaPlaylistCreationMetadata"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMPMediaPlaylistCreationMetadata mpMediaPlaylistCreationMetadata => mpMediaPlaylistCreationMetadata -> IO (Id MPMediaPlaylistCreationMetadata)
init_ mpMediaPlaylistCreationMetadata  =
  sendMsg mpMediaPlaylistCreationMetadata (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithName:@
initWithName :: (IsMPMediaPlaylistCreationMetadata mpMediaPlaylistCreationMetadata, IsNSString name) => mpMediaPlaylistCreationMetadata -> name -> IO (Id MPMediaPlaylistCreationMetadata)
initWithName mpMediaPlaylistCreationMetadata  name =
withObjCPtr name $ \raw_name ->
    sendMsg mpMediaPlaylistCreationMetadata (mkSelector "initWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= ownedObject . castPtr

-- | The display name of the playlist.
--
-- ObjC selector: @- name@
name :: IsMPMediaPlaylistCreationMetadata mpMediaPlaylistCreationMetadata => mpMediaPlaylistCreationMetadata -> IO (Id NSString)
name mpMediaPlaylistCreationMetadata  =
  sendMsg mpMediaPlaylistCreationMetadata (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Defaults to the requesting app's display name.
--
-- ObjC selector: @- authorDisplayName@
authorDisplayName :: IsMPMediaPlaylistCreationMetadata mpMediaPlaylistCreationMetadata => mpMediaPlaylistCreationMetadata -> IO (Id NSString)
authorDisplayName mpMediaPlaylistCreationMetadata  =
  sendMsg mpMediaPlaylistCreationMetadata (mkSelector "authorDisplayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Defaults to the requesting app's display name.
--
-- ObjC selector: @- setAuthorDisplayName:@
setAuthorDisplayName :: (IsMPMediaPlaylistCreationMetadata mpMediaPlaylistCreationMetadata, IsNSString value) => mpMediaPlaylistCreationMetadata -> value -> IO ()
setAuthorDisplayName mpMediaPlaylistCreationMetadata  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpMediaPlaylistCreationMetadata (mkSelector "setAuthorDisplayName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- descriptionText@
descriptionText :: IsMPMediaPlaylistCreationMetadata mpMediaPlaylistCreationMetadata => mpMediaPlaylistCreationMetadata -> IO (Id NSString)
descriptionText mpMediaPlaylistCreationMetadata  =
  sendMsg mpMediaPlaylistCreationMetadata (mkSelector "descriptionText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDescriptionText:@
setDescriptionText :: (IsMPMediaPlaylistCreationMetadata mpMediaPlaylistCreationMetadata, IsNSString value) => mpMediaPlaylistCreationMetadata -> value -> IO ()
setDescriptionText mpMediaPlaylistCreationMetadata  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpMediaPlaylistCreationMetadata (mkSelector "setDescriptionText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @authorDisplayName@
authorDisplayNameSelector :: Selector
authorDisplayNameSelector = mkSelector "authorDisplayName"

-- | @Selector@ for @setAuthorDisplayName:@
setAuthorDisplayNameSelector :: Selector
setAuthorDisplayNameSelector = mkSelector "setAuthorDisplayName:"

-- | @Selector@ for @descriptionText@
descriptionTextSelector :: Selector
descriptionTextSelector = mkSelector "descriptionText"

-- | @Selector@ for @setDescriptionText:@
setDescriptionTextSelector :: Selector
setDescriptionTextSelector = mkSelector "setDescriptionText:"

