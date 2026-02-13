{-# LANGUAGE DataKinds #-}
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
  , attributesSelector
  , mediaGroupForIdentifierSelector
  , mediaGroupsForIdentifiersSelector
  , mediaLibrarySelector
  , mediaObjectForIdentifierSelector
  , mediaObjectsForIdentifiersSelector
  , mediaSourceIdentifierSelector
  , rootMediaGroupSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaLibrary.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- mediaGroupForIdentifier:@
mediaGroupForIdentifier :: (IsMLMediaSource mlMediaSource, IsNSString mediaGroupIdentifier) => mlMediaSource -> mediaGroupIdentifier -> IO (Id MLMediaGroup)
mediaGroupForIdentifier mlMediaSource mediaGroupIdentifier =
  sendMessage mlMediaSource mediaGroupForIdentifierSelector (toNSString mediaGroupIdentifier)

-- | @- mediaGroupsForIdentifiers:@
mediaGroupsForIdentifiers :: (IsMLMediaSource mlMediaSource, IsNSArray mediaGroupIdentifiers) => mlMediaSource -> mediaGroupIdentifiers -> IO (Id NSDictionary)
mediaGroupsForIdentifiers mlMediaSource mediaGroupIdentifiers =
  sendMessage mlMediaSource mediaGroupsForIdentifiersSelector (toNSArray mediaGroupIdentifiers)

-- | @- mediaObjectForIdentifier:@
mediaObjectForIdentifier :: (IsMLMediaSource mlMediaSource, IsNSString mediaObjectIdentifier) => mlMediaSource -> mediaObjectIdentifier -> IO (Id MLMediaObject)
mediaObjectForIdentifier mlMediaSource mediaObjectIdentifier =
  sendMessage mlMediaSource mediaObjectForIdentifierSelector (toNSString mediaObjectIdentifier)

-- | @- mediaObjectsForIdentifiers:@
mediaObjectsForIdentifiers :: (IsMLMediaSource mlMediaSource, IsNSArray mediaObjectIdentifiers) => mlMediaSource -> mediaObjectIdentifiers -> IO (Id NSDictionary)
mediaObjectsForIdentifiers mlMediaSource mediaObjectIdentifiers =
  sendMessage mlMediaSource mediaObjectsForIdentifiersSelector (toNSArray mediaObjectIdentifiers)

-- | @- mediaLibrary@
mediaLibrary :: IsMLMediaSource mlMediaSource => mlMediaSource -> IO (Id MLMediaLibrary)
mediaLibrary mlMediaSource =
  sendMessage mlMediaSource mediaLibrarySelector

-- | @- mediaSourceIdentifier@
mediaSourceIdentifier :: IsMLMediaSource mlMediaSource => mlMediaSource -> IO (Id NSString)
mediaSourceIdentifier mlMediaSource =
  sendMessage mlMediaSource mediaSourceIdentifierSelector

-- | @- attributes@
attributes :: IsMLMediaSource mlMediaSource => mlMediaSource -> IO (Id NSDictionary)
attributes mlMediaSource =
  sendMessage mlMediaSource attributesSelector

-- | @- rootMediaGroup@
rootMediaGroup :: IsMLMediaSource mlMediaSource => mlMediaSource -> IO (Id MLMediaGroup)
rootMediaGroup mlMediaSource =
  sendMessage mlMediaSource rootMediaGroupSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mediaGroupForIdentifier:@
mediaGroupForIdentifierSelector :: Selector '[Id NSString] (Id MLMediaGroup)
mediaGroupForIdentifierSelector = mkSelector "mediaGroupForIdentifier:"

-- | @Selector@ for @mediaGroupsForIdentifiers:@
mediaGroupsForIdentifiersSelector :: Selector '[Id NSArray] (Id NSDictionary)
mediaGroupsForIdentifiersSelector = mkSelector "mediaGroupsForIdentifiers:"

-- | @Selector@ for @mediaObjectForIdentifier:@
mediaObjectForIdentifierSelector :: Selector '[Id NSString] (Id MLMediaObject)
mediaObjectForIdentifierSelector = mkSelector "mediaObjectForIdentifier:"

-- | @Selector@ for @mediaObjectsForIdentifiers:@
mediaObjectsForIdentifiersSelector :: Selector '[Id NSArray] (Id NSDictionary)
mediaObjectsForIdentifiersSelector = mkSelector "mediaObjectsForIdentifiers:"

-- | @Selector@ for @mediaLibrary@
mediaLibrarySelector :: Selector '[] (Id MLMediaLibrary)
mediaLibrarySelector = mkSelector "mediaLibrary"

-- | @Selector@ for @mediaSourceIdentifier@
mediaSourceIdentifierSelector :: Selector '[] (Id NSString)
mediaSourceIdentifierSelector = mkSelector "mediaSourceIdentifier"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id NSDictionary)
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @rootMediaGroup@
rootMediaGroupSelector :: Selector '[] (Id MLMediaGroup)
rootMediaGroupSelector = mkSelector "rootMediaGroup"

