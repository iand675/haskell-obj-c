{-# LANGUAGE PatternSynonyms #-}
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
  , typeSelector
  , cgImageSelector
  , uiImageSelector

  -- * Enum types
  , QLThumbnailRepresentationType(QLThumbnailRepresentationType)
  , pattern QLThumbnailRepresentationTypeIcon
  , pattern QLThumbnailRepresentationTypeLowQualityThumbnail
  , pattern QLThumbnailRepresentationTypeThumbnail

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuickLookThumbnailing.Internal.Classes
import ObjC.QuickLookThumbnailing.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- type@
type_ :: IsQLThumbnailRepresentation qlThumbnailRepresentation => qlThumbnailRepresentation -> IO QLThumbnailRepresentationType
type_ qlThumbnailRepresentation  =
    fmap (coerce :: CLong -> QLThumbnailRepresentationType) $ sendMsg qlThumbnailRepresentation (mkSelector "type") retCLong []

-- | Returns the CGImage representation of the thumbnail.
--
-- ObjC selector: @- CGImage@
cgImage :: IsQLThumbnailRepresentation qlThumbnailRepresentation => qlThumbnailRepresentation -> IO (Ptr ())
cgImage qlThumbnailRepresentation  =
    fmap castPtr $ sendMsg qlThumbnailRepresentation (mkSelector "CGImage") (retPtr retVoid) []

-- | Returns the UIImage representation of the thumbnail. You need to explicitly link against UIKit to use this property.
--
-- ObjC selector: @- UIImage@
uiImage :: IsQLThumbnailRepresentation qlThumbnailRepresentation => qlThumbnailRepresentation -> IO RawId
uiImage qlThumbnailRepresentation  =
    fmap (RawId . castPtr) $ sendMsg qlThumbnailRepresentation (mkSelector "UIImage") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @CGImage@
cgImageSelector :: Selector
cgImageSelector = mkSelector "CGImage"

-- | @Selector@ for @UIImage@
uiImageSelector :: Selector
uiImageSelector = mkSelector "UIImage"

