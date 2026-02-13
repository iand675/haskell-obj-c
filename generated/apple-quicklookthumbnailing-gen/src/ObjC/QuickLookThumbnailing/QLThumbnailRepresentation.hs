{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @QLThumbnailRepresentation@.
module ObjC.QuickLookThumbnailing.QLThumbnailRepresentation
  ( QLThumbnailRepresentation
  , IsQLThumbnailRepresentation(..)
  , type_
  , cgImage
  , uiImage
  , cgImageSelector
  , typeSelector
  , uiImageSelector

  -- * Enum types
  , QLThumbnailRepresentationType(QLThumbnailRepresentationType)
  , pattern QLThumbnailRepresentationTypeIcon
  , pattern QLThumbnailRepresentationTypeLowQualityThumbnail
  , pattern QLThumbnailRepresentationTypeThumbnail

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuickLookThumbnailing.Internal.Classes
import ObjC.QuickLookThumbnailing.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- type@
type_ :: IsQLThumbnailRepresentation qlThumbnailRepresentation => qlThumbnailRepresentation -> IO QLThumbnailRepresentationType
type_ qlThumbnailRepresentation =
  sendMessage qlThumbnailRepresentation typeSelector

-- | Returns the CGImage representation of the thumbnail.
--
-- ObjC selector: @- CGImage@
cgImage :: IsQLThumbnailRepresentation qlThumbnailRepresentation => qlThumbnailRepresentation -> IO (Ptr ())
cgImage qlThumbnailRepresentation =
  sendMessage qlThumbnailRepresentation cgImageSelector

-- | Returns the UIImage representation of the thumbnail. You need to explicitly link against UIKit to use this property.
--
-- ObjC selector: @- UIImage@
uiImage :: IsQLThumbnailRepresentation qlThumbnailRepresentation => qlThumbnailRepresentation -> IO RawId
uiImage qlThumbnailRepresentation =
  sendMessage qlThumbnailRepresentation uiImageSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector '[] QLThumbnailRepresentationType
typeSelector = mkSelector "type"

-- | @Selector@ for @CGImage@
cgImageSelector :: Selector '[] (Ptr ())
cgImageSelector = mkSelector "CGImage"

-- | @Selector@ for @UIImage@
uiImageSelector :: Selector '[] RawId
uiImageSelector = mkSelector "UIImage"

