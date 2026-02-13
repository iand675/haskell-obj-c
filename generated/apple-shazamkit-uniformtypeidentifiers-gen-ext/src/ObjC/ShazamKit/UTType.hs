{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing a type in a type hierarchy.
--
-- Types may represent files on disk, abstract data types with no on-disk	representation, or even entirely unrelated hierarchical classification	systems such as hardware.
--
-- Older API that does not use @UTType@ typically uses an untyped @NSString@	or @CFStringRef@ to refer to a type by its identifier. To get the	identifier of a type for use with these APIs, use the @identifier@ property	of this class.
--
-- https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/understanding_utis/
--
-- Generated bindings for @UTType@.
module ObjC.ShazamKit.UTType
  ( UTType
  , IsUTType(..)
  , shSignatureContentType
  , shCustomCatalogContentType
  , shCustomCatalogContentTypeSelector
  , shSignatureContentTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ShazamKit.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | A type representing the @SHSignature@ file format with the .shazamsignature extension
--
-- ObjC selector: @+ SHSignatureContentType@
shSignatureContentType :: IO (Id UTType)
shSignatureContentType  =
  do
    cls' <- getRequiredClass "UTType"
    sendClassMessage cls' shSignatureContentTypeSelector

-- | A type representing the @SHCustomCatalog@ file format with the .shazamcatalog extension
--
-- ObjC selector: @+ SHCustomCatalogContentType@
shCustomCatalogContentType :: IO (Id UTType)
shCustomCatalogContentType  =
  do
    cls' <- getRequiredClass "UTType"
    sendClassMessage cls' shCustomCatalogContentTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @SHSignatureContentType@
shSignatureContentTypeSelector :: Selector '[] (Id UTType)
shSignatureContentTypeSelector = mkSelector "SHSignatureContentType"

-- | @Selector@ for @SHCustomCatalogContentType@
shCustomCatalogContentTypeSelector :: Selector '[] (Id UTType)
shCustomCatalogContentTypeSelector = mkSelector "SHCustomCatalogContentType"

