{-# LANGUAGE PatternSynonyms #-}
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
  , mediaLibrarySelector
  , identifierSelector
  , mediaSourceIdentifierSelector
  , attributesSelector
  , mediaTypeSelector
  , contentTypeSelector
  , nameSelector
  , urlSelector
  , originalURLSelector
  , fileSizeSelector
  , modificationDateSelector
  , thumbnailURLSelector

  -- * Enum types
  , MLMediaType(MLMediaType)
  , pattern MLMediaTypeAudio
  , pattern MLMediaTypeImage
  , pattern MLMediaTypeMovie

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
import ObjC.MediaLibrary.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- mediaLibrary@
mediaLibrary :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO (Id MLMediaLibrary)
mediaLibrary mlMediaObject  =
  sendMsg mlMediaObject (mkSelector "mediaLibrary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- identifier@
identifier :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO (Id NSString)
identifier mlMediaObject  =
  sendMsg mlMediaObject (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- mediaSourceIdentifier@
mediaSourceIdentifier :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO (Id NSString)
mediaSourceIdentifier mlMediaObject  =
  sendMsg mlMediaObject (mkSelector "mediaSourceIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attributes@
attributes :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO (Id NSDictionary)
attributes mlMediaObject  =
  sendMsg mlMediaObject (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- mediaType@
mediaType :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO MLMediaType
mediaType mlMediaObject  =
  fmap (coerce :: CULong -> MLMediaType) $ sendMsg mlMediaObject (mkSelector "mediaType") retCULong []

-- | @- contentType@
contentType :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO (Id NSString)
contentType mlMediaObject  =
  sendMsg mlMediaObject (mkSelector "contentType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- name@
name :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO (Id NSString)
name mlMediaObject  =
  sendMsg mlMediaObject (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- URL@
url :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO (Id NSURL)
url mlMediaObject  =
  sendMsg mlMediaObject (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- originalURL@
originalURL :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO (Id NSURL)
originalURL mlMediaObject  =
  sendMsg mlMediaObject (mkSelector "originalURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fileSize@
fileSize :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO CULong
fileSize mlMediaObject  =
  sendMsg mlMediaObject (mkSelector "fileSize") retCULong []

-- | @- modificationDate@
modificationDate :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO (Id NSDate)
modificationDate mlMediaObject  =
  sendMsg mlMediaObject (mkSelector "modificationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- thumbnailURL@
thumbnailURL :: IsMLMediaObject mlMediaObject => mlMediaObject -> IO (Id NSURL)
thumbnailURL mlMediaObject  =
  sendMsg mlMediaObject (mkSelector "thumbnailURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mediaLibrary@
mediaLibrarySelector :: Selector
mediaLibrarySelector = mkSelector "mediaLibrary"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @mediaSourceIdentifier@
mediaSourceIdentifierSelector :: Selector
mediaSourceIdentifierSelector = mkSelector "mediaSourceIdentifier"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @contentType@
contentTypeSelector :: Selector
contentTypeSelector = mkSelector "contentType"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @originalURL@
originalURLSelector :: Selector
originalURLSelector = mkSelector "originalURL"

-- | @Selector@ for @fileSize@
fileSizeSelector :: Selector
fileSizeSelector = mkSelector "fileSize"

-- | @Selector@ for @modificationDate@
modificationDateSelector :: Selector
modificationDateSelector = mkSelector "modificationDate"

-- | @Selector@ for @thumbnailURL@
thumbnailURLSelector :: Selector
thumbnailURLSelector = mkSelector "thumbnailURL"

