{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @QLThumbnailGenerationRequest@.
module ObjC.QuickLookThumbnailing.QLThumbnailGenerationRequest
  ( QLThumbnailGenerationRequest
  , IsQLThumbnailGenerationRequest(..)
  , init_
  , new
  , contentType
  , setContentType
  , minimumDimension
  , setMinimumDimension
  , iconMode
  , setIconMode
  , scale
  , representationTypes
  , initSelector
  , newSelector
  , contentTypeSelector
  , setContentTypeSelector
  , minimumDimensionSelector
  , setMinimumDimensionSelector
  , iconModeSelector
  , setIconModeSelector
  , scaleSelector
  , representationTypesSelector

  -- * Enum types
  , QLThumbnailGenerationRequestRepresentationTypes(QLThumbnailGenerationRequestRepresentationTypes)
  , pattern QLThumbnailGenerationRequestRepresentationTypeIcon
  , pattern QLThumbnailGenerationRequestRepresentationTypeLowQualityThumbnail
  , pattern QLThumbnailGenerationRequestRepresentationTypeThumbnail
  , pattern QLThumbnailGenerationRequestRepresentationTypeAll

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
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- init@
init_ :: IsQLThumbnailGenerationRequest qlThumbnailGenerationRequest => qlThumbnailGenerationRequest -> IO (Id QLThumbnailGenerationRequest)
init_ qlThumbnailGenerationRequest  =
  sendMsg qlThumbnailGenerationRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id QLThumbnailGenerationRequest)
new  =
  do
    cls' <- getRequiredClass "QLThumbnailGenerationRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The content type of the file being thumbnailed is used to determine the provider of the thumbnail and the icon styles applied if iconMode is requested. By default the content type is derived from the file extension. Setting this property will override the derived content type. This is useful for files that don't have meaningful extensions but for which you may already know the content type.
--
-- ObjC selector: @- contentType@
contentType :: IsQLThumbnailGenerationRequest qlThumbnailGenerationRequest => qlThumbnailGenerationRequest -> IO (Id UTType)
contentType qlThumbnailGenerationRequest  =
  sendMsg qlThumbnailGenerationRequest (mkSelector "contentType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The content type of the file being thumbnailed is used to determine the provider of the thumbnail and the icon styles applied if iconMode is requested. By default the content type is derived from the file extension. Setting this property will override the derived content type. This is useful for files that don't have meaningful extensions but for which you may already know the content type.
--
-- ObjC selector: @- setContentType:@
setContentType :: (IsQLThumbnailGenerationRequest qlThumbnailGenerationRequest, IsUTType value) => qlThumbnailGenerationRequest -> value -> IO ()
setContentType qlThumbnailGenerationRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg qlThumbnailGenerationRequest (mkSelector "setContentType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Defaults to 0. If set, the thumbnail will have a width and height greater or equal to minimumDimension * scale. If set and it is not possible to generate thumbnails of minimumDimension for any of the requested QLThumbnailGenerationRequestRepresentationTypes, no thumbnail will be provided.
--
-- ObjC selector: @- minimumDimension@
minimumDimension :: IsQLThumbnailGenerationRequest qlThumbnailGenerationRequest => qlThumbnailGenerationRequest -> IO CDouble
minimumDimension qlThumbnailGenerationRequest  =
  sendMsg qlThumbnailGenerationRequest (mkSelector "minimumDimension") retCDouble []

-- | Defaults to 0. If set, the thumbnail will have a width and height greater or equal to minimumDimension * scale. If set and it is not possible to generate thumbnails of minimumDimension for any of the requested QLThumbnailGenerationRequestRepresentationTypes, no thumbnail will be provided.
--
-- ObjC selector: @- setMinimumDimension:@
setMinimumDimension :: IsQLThumbnailGenerationRequest qlThumbnailGenerationRequest => qlThumbnailGenerationRequest -> CDouble -> IO ()
setMinimumDimension qlThumbnailGenerationRequest  value =
  sendMsg qlThumbnailGenerationRequest (mkSelector "setMinimumDimension:") retVoid [argCDouble (fromIntegral value)]

-- | If set to YES, this will generate something appropriate for display as a file icon, meaning that the thumbnail might be embedded in a frame, show a curled corner, draw a background and/or a drop shadow, as appropriate for the platform. If set to NO, this will generate a raw undecorated thumbnail. Defaults to NO.
--
-- ObjC selector: @- iconMode@
iconMode :: IsQLThumbnailGenerationRequest qlThumbnailGenerationRequest => qlThumbnailGenerationRequest -> IO Bool
iconMode qlThumbnailGenerationRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg qlThumbnailGenerationRequest (mkSelector "iconMode") retCULong []

-- | If set to YES, this will generate something appropriate for display as a file icon, meaning that the thumbnail might be embedded in a frame, show a curled corner, draw a background and/or a drop shadow, as appropriate for the platform. If set to NO, this will generate a raw undecorated thumbnail. Defaults to NO.
--
-- ObjC selector: @- setIconMode:@
setIconMode :: IsQLThumbnailGenerationRequest qlThumbnailGenerationRequest => qlThumbnailGenerationRequest -> Bool -> IO ()
setIconMode qlThumbnailGenerationRequest  value =
  sendMsg qlThumbnailGenerationRequest (mkSelector "setIconMode:") retVoid [argCULong (if value then 1 else 0)]

-- | @- scale@
scale :: IsQLThumbnailGenerationRequest qlThumbnailGenerationRequest => qlThumbnailGenerationRequest -> IO CDouble
scale qlThumbnailGenerationRequest  =
  sendMsg qlThumbnailGenerationRequest (mkSelector "scale") retCDouble []

-- | @- representationTypes@
representationTypes :: IsQLThumbnailGenerationRequest qlThumbnailGenerationRequest => qlThumbnailGenerationRequest -> IO QLThumbnailGenerationRequestRepresentationTypes
representationTypes qlThumbnailGenerationRequest  =
  fmap (coerce :: CULong -> QLThumbnailGenerationRequestRepresentationTypes) $ sendMsg qlThumbnailGenerationRequest (mkSelector "representationTypes") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @contentType@
contentTypeSelector :: Selector
contentTypeSelector = mkSelector "contentType"

-- | @Selector@ for @setContentType:@
setContentTypeSelector :: Selector
setContentTypeSelector = mkSelector "setContentType:"

-- | @Selector@ for @minimumDimension@
minimumDimensionSelector :: Selector
minimumDimensionSelector = mkSelector "minimumDimension"

-- | @Selector@ for @setMinimumDimension:@
setMinimumDimensionSelector :: Selector
setMinimumDimensionSelector = mkSelector "setMinimumDimension:"

-- | @Selector@ for @iconMode@
iconModeSelector :: Selector
iconModeSelector = mkSelector "iconMode"

-- | @Selector@ for @setIconMode:@
setIconModeSelector :: Selector
setIconModeSelector = mkSelector "setIconMode:"

-- | @Selector@ for @scale@
scaleSelector :: Selector
scaleSelector = mkSelector "scale"

-- | @Selector@ for @representationTypes@
representationTypesSelector :: Selector
representationTypesSelector = mkSelector "representationTypes"

