{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MLMediaObject@.
module ObjC.MediaLibrary.MLMediaObject
  ( MLMediaObject
  , IsMLMediaObject(..)
  , mediaLibrary
  , identifier
  , mediaSourceIdentifier
  , attributes
  , mediaType
  , contentType
  , name
  , url
  , originalURL
  , fileSize
  , modificationDate
  , thumbnailURL
  , attributesSelector
  , contentTypeSelector
  , fileSizeSelector
  , identifierSelector
  , mediaLibrarySelector
  , mediaSourceIdentifierSelector
  , mediaTypeSelector
  , modificationDateSelector
  , nameSelector
  , originalURLSelector
  , thumbnailURLSelector
  , urlSelector

  -- * Enum types
  , MLMediaType(MLMediaType)
  , pattern MLMediaTypeAudio
  , pattern MLMediaTypeImage
  , pattern MLMediaTypeMovie

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaLibrary.Internal.Classes
import ObjC.MediaLibrary.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- mediaLibrary@
mediaLibrary :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO (Id MLMediaLibrary)
mediaLibrary mlMediaObject =
  sendMessage mlMediaObject mediaLibrarySelector

-- | @- identifier@
identifier :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO (Id NSString)
identifier mlMediaObject =
  sendMessage mlMediaObject identifierSelector

-- | @- mediaSourceIdentifier@
mediaSourceIdentifier :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO (Id NSString)
mediaSourceIdentifier mlMediaObject =
  sendMessage mlMediaObject mediaSourceIdentifierSelector

-- | @- attributes@
attributes :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO (Id NSDictionary)
attributes mlMediaObject =
  sendMessage mlMediaObject attributesSelector

-- | @- mediaType@
mediaType :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO MLMediaType
mediaType mlMediaObject =
  sendMessage mlMediaObject mediaTypeSelector

-- | @- contentType@
contentType :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO (Id NSString)
contentType mlMediaObject =
  sendMessage mlMediaObject contentTypeSelector

-- | @- name@
name :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO (Id NSString)
name mlMediaObject =
  sendMessage mlMediaObject nameSelector

-- | @- URL@
url :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO (Id NSURL)
url mlMediaObject =
  sendMessage mlMediaObject urlSelector

-- | @- originalURL@
originalURL :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO (Id NSURL)
originalURL mlMediaObject =
  sendMessage mlMediaObject originalURLSelector

-- | @- fileSize@
fileSize :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO CULong
fileSize mlMediaObject =
  sendMessage mlMediaObject fileSizeSelector

-- | @- modificationDate@
modificationDate :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO (Id NSDate)
modificationDate mlMediaObject =
  sendMessage mlMediaObject modificationDateSelector

-- | @- thumbnailURL@
thumbnailURL :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO (Id NSURL)
thumbnailURL mlMediaObject =
  sendMessage mlMediaObject thumbnailURLSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mediaLibrary@
mediaLibrarySelector :: Selector '[] (Id MLMediaLibrary)
mediaLibrarySelector = mkSelector "mediaLibrary"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @mediaSourceIdentifier@
mediaSourceIdentifierSelector :: Selector '[] (Id NSString)
mediaSourceIdentifierSelector = mkSelector "mediaSourceIdentifier"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id NSDictionary)
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector '[] MLMediaType
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @contentType@
contentTypeSelector :: Selector '[] (Id NSString)
contentTypeSelector = mkSelector "contentType"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @originalURL@
originalURLSelector :: Selector '[] (Id NSURL)
originalURLSelector = mkSelector "originalURL"

-- | @Selector@ for @fileSize@
fileSizeSelector :: Selector '[] CULong
fileSizeSelector = mkSelector "fileSize"

-- | @Selector@ for @modificationDate@
modificationDateSelector :: Selector '[] (Id NSDate)
modificationDateSelector = mkSelector "modificationDate"

-- | @Selector@ for @thumbnailURL@
thumbnailURLSelector :: Selector '[] (Id NSURL)
thumbnailURLSelector = mkSelector "thumbnailURL"

