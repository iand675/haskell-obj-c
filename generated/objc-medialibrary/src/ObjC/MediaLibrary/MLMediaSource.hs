{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MLMediaSource@.
module ObjC.MediaLibrary.MLMediaSource
  ( MLMediaSource
  , IsMLMediaSource(..)
  , mediaGroupForIdentifier
  , mediaGroupsForIdentifiers
  , mediaObjectForIdentifier
  , mediaObjectsForIdentifiers
  , mediaLibrary
  , mediaSourceIdentifier
  , attributes
  , rootMediaGroup
  , mediaGroupForIdentifierSelector
  , mediaGroupsForIdentifiersSelector
  , mediaObjectForIdentifierSelector
  , mediaObjectsForIdentifiersSelector
  , mediaLibrarySelector
  , mediaSourceIdentifierSelector
  , attributesSelector
  , rootMediaGroupSelector


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

import ObjC.MediaLibrary.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- mediaGroupForIdentifier:@
mediaGroupForIdentifier :: (IsMLMediaSource mlMediaSource, IsNSString mediaGroupIdentifier) => mlMediaSource -> mediaGroupIdentifier -> IO (Id MLMediaGroup)
mediaGroupForIdentifier mlMediaSource  mediaGroupIdentifier =
withObjCPtr mediaGroupIdentifier $ \raw_mediaGroupIdentifier ->
    sendMsg mlMediaSource (mkSelector "mediaGroupForIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_mediaGroupIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- mediaGroupsForIdentifiers:@
mediaGroupsForIdentifiers :: (IsMLMediaSource mlMediaSource, IsNSArray mediaGroupIdentifiers) => mlMediaSource -> mediaGroupIdentifiers -> IO (Id NSDictionary)
mediaGroupsForIdentifiers mlMediaSource  mediaGroupIdentifiers =
withObjCPtr mediaGroupIdentifiers $ \raw_mediaGroupIdentifiers ->
    sendMsg mlMediaSource (mkSelector "mediaGroupsForIdentifiers:") (retPtr retVoid) [argPtr (castPtr raw_mediaGroupIdentifiers :: Ptr ())] >>= retainedObject . castPtr

-- | @- mediaObjectForIdentifier:@
mediaObjectForIdentifier :: (IsMLMediaSource mlMediaSource, IsNSString mediaObjectIdentifier) => mlMediaSource -> mediaObjectIdentifier -> IO (Id MLMediaObject)
mediaObjectForIdentifier mlMediaSource  mediaObjectIdentifier =
withObjCPtr mediaObjectIdentifier $ \raw_mediaObjectIdentifier ->
    sendMsg mlMediaSource (mkSelector "mediaObjectForIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_mediaObjectIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- mediaObjectsForIdentifiers:@
mediaObjectsForIdentifiers :: (IsMLMediaSource mlMediaSource, IsNSArray mediaObjectIdentifiers) => mlMediaSource -> mediaObjectIdentifiers -> IO (Id NSDictionary)
mediaObjectsForIdentifiers mlMediaSource  mediaObjectIdentifiers =
withObjCPtr mediaObjectIdentifiers $ \raw_mediaObjectIdentifiers ->
    sendMsg mlMediaSource (mkSelector "mediaObjectsForIdentifiers:") (retPtr retVoid) [argPtr (castPtr raw_mediaObjectIdentifiers :: Ptr ())] >>= retainedObject . castPtr

-- | @- mediaLibrary@
mediaLibrary :: IsMLMediaSource mlMediaSource => mlMediaSource -> IO (Id MLMediaLibrary)
mediaLibrary mlMediaSource  =
  sendMsg mlMediaSource (mkSelector "mediaLibrary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- mediaSourceIdentifier@
mediaSourceIdentifier :: IsMLMediaSource mlMediaSource => mlMediaSource -> IO (Id NSString)
mediaSourceIdentifier mlMediaSource  =
  sendMsg mlMediaSource (mkSelector "mediaSourceIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attributes@
attributes :: IsMLMediaSource mlMediaSource => mlMediaSource -> IO (Id NSDictionary)
attributes mlMediaSource  =
  sendMsg mlMediaSource (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rootMediaGroup@
rootMediaGroup :: IsMLMediaSource mlMediaSource => mlMediaSource -> IO (Id MLMediaGroup)
rootMediaGroup mlMediaSource  =
  sendMsg mlMediaSource (mkSelector "rootMediaGroup") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mediaGroupForIdentifier:@
mediaGroupForIdentifierSelector :: Selector
mediaGroupForIdentifierSelector = mkSelector "mediaGroupForIdentifier:"

-- | @Selector@ for @mediaGroupsForIdentifiers:@
mediaGroupsForIdentifiersSelector :: Selector
mediaGroupsForIdentifiersSelector = mkSelector "mediaGroupsForIdentifiers:"

-- | @Selector@ for @mediaObjectForIdentifier:@
mediaObjectForIdentifierSelector :: Selector
mediaObjectForIdentifierSelector = mkSelector "mediaObjectForIdentifier:"

-- | @Selector@ for @mediaObjectsForIdentifiers:@
mediaObjectsForIdentifiersSelector :: Selector
mediaObjectsForIdentifiersSelector = mkSelector "mediaObjectsForIdentifiers:"

-- | @Selector@ for @mediaLibrary@
mediaLibrarySelector :: Selector
mediaLibrarySelector = mkSelector "mediaLibrary"

-- | @Selector@ for @mediaSourceIdentifier@
mediaSourceIdentifierSelector :: Selector
mediaSourceIdentifierSelector = mkSelector "mediaSourceIdentifier"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @rootMediaGroup@
rootMediaGroupSelector :: Selector
rootMediaGroupSelector = mkSelector "rootMediaGroup"

