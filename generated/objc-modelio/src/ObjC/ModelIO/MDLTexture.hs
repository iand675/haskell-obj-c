{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MDLTexture  a description of texels provided by a texture object.
--
-- A texture optionally generates or loads texels             through an access to the data property, or one of the other              properties, otherwise the texture object is a lightweight descriptor              only.
--
-- data
--
-- Texel data that will exist when referenced; it may or may not exist            before
--
-- dimensions
--
-- texel width and height of the texture
--
-- rowStride
--
-- The number of bytes from the first texel in a row to the first texel            in the next row. A rowStride of zero indicates that interleaved x,y            addressing of texels is not possible. This might be the case if the           texture was compressed in some manner, for example.
--
-- channelCount
--
-- The number of channels incoded in a single texel. For example, an RGB            texture has 3 channels. All channels must have the same encoding.
--
-- channelEncoding
--
-- The encoding of a channel in a single texel.
--
-- isCube
--
-- The texture encodes a cube map. If YES, then the layout of the cube            map is deduced as a vertical strip if dimension.y is six times            dimension.x. Other layouts are possible in the future.
--
-- Generated bindings for @MDLTexture@.
module ObjC.ModelIO.MDLTexture
  ( MDLTexture
  , IsMDLTexture(..)
  , init_
  , textureNamed
  , textureNamed_bundle
  , textureNamed_assetResolver
  , textureCubeWithImagesNamed
  , textureCubeWithImagesNamed_bundle
  , writeToURL
  , writeToURL_level
  , writeToURL_type
  , writeToURL_type_level
  , imageFromTexture
  , imageFromTextureAtLevel
  , texelDataWithTopLeftOrigin
  , texelDataWithBottomLeftOrigin
  , texelDataWithTopLeftOriginAtMipLevel_create
  , texelDataWithBottomLeftOriginAtMipLevel_create
  , rowStride
  , channelCount
  , mipLevelCount
  , channelEncoding
  , isCube
  , setIsCube
  , hasAlphaValues
  , setHasAlphaValues
  , initSelector
  , textureNamedSelector
  , textureNamed_bundleSelector
  , textureNamed_assetResolverSelector
  , textureCubeWithImagesNamedSelector
  , textureCubeWithImagesNamed_bundleSelector
  , writeToURLSelector
  , writeToURL_levelSelector
  , writeToURL_typeSelector
  , writeToURL_type_levelSelector
  , imageFromTextureSelector
  , imageFromTextureAtLevelSelector
  , texelDataWithTopLeftOriginSelector
  , texelDataWithBottomLeftOriginSelector
  , texelDataWithTopLeftOriginAtMipLevel_createSelector
  , texelDataWithBottomLeftOriginAtMipLevel_createSelector
  , rowStrideSelector
  , channelCountSelector
  , mipLevelCountSelector
  , channelEncodingSelector
  , isCubeSelector
  , setIsCubeSelector
  , hasAlphaValuesSelector
  , setHasAlphaValuesSelector

  -- * Enum types
  , MDLTextureChannelEncoding(MDLTextureChannelEncoding)
  , pattern MDLTextureChannelEncodingUInt8
  , pattern MDLTextureChannelEncodingUint8
  , pattern MDLTextureChannelEncodingUInt16
  , pattern MDLTextureChannelEncodingUint16
  , pattern MDLTextureChannelEncodingUInt24
  , pattern MDLTextureChannelEncodingUint24
  , pattern MDLTextureChannelEncodingUInt32
  , pattern MDLTextureChannelEncodingUint32
  , pattern MDLTextureChannelEncodingFloat16
  , pattern MDLTextureChannelEncodingFloat16SR
  , pattern MDLTextureChannelEncodingFloat32

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

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMDLTexture mdlTexture => mdlTexture -> IO (Id MDLTexture)
init_ mdlTexture  =
  sendMsg mdlTexture (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates a texture from a source in the main bundle named in a manner matching  name.
--
-- ObjC selector: @+ textureNamed:@
textureNamed :: IsNSString name => name -> IO (Id MDLTexture)
textureNamed name =
  do
    cls' <- getRequiredClass "MDLTexture"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "textureNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @+ textureNamed:bundle:@
textureNamed_bundle :: (IsNSString name, IsNSBundle bundleOrNil) => name -> bundleOrNil -> IO (Id MDLTexture)
textureNamed_bundle name bundleOrNil =
  do
    cls' <- getRequiredClass "MDLTexture"
    withObjCPtr name $ \raw_name ->
      withObjCPtr bundleOrNil $ \raw_bundleOrNil ->
        sendClassMsg cls' (mkSelector "textureNamed:bundle:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_bundleOrNil :: Ptr ())] >>= retainedObject . castPtr

-- | @+ textureNamed:assetResolver:@
textureNamed_assetResolver :: IsNSString name => name -> RawId -> IO (Id MDLTexture)
textureNamed_assetResolver name resolver =
  do
    cls' <- getRequiredClass "MDLTexture"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "textureNamed:assetResolver:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr (unRawId resolver) :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a cube texture map image using 6 faces of the same dimensions,   ordered +X,-X,+Y,-Y,+Z,-Z If the data is read back the image will be compacted   into a single vertical stack where dimensions.y = 6 * dimensions.x  isCube will return YES
--
-- @names@ — a collection of mosaiced images in a cross formation or column or row.  - If 6 individual images are given they are assumed to be in order and will be     loaded as is.  - if 3 images of double height or width are given they will be treated as     pairs of + and - in each axis, the order is must be x, then y, then z.  - if 2 images of triple height or width are given they will be treates as a     positive set and a negative set in the order +x, +y, +z, then -x, -y, -z.  - if a single image is given it will be used without conversion if in column     orientation and demosaiced in all other instances.
--
-- ObjC selector: @+ textureCubeWithImagesNamed:@
textureCubeWithImagesNamed :: IsNSArray names => names -> IO (Id MDLTexture)
textureCubeWithImagesNamed names =
  do
    cls' <- getRequiredClass "MDLTexture"
    withObjCPtr names $ \raw_names ->
      sendClassMsg cls' (mkSelector "textureCubeWithImagesNamed:") (retPtr retVoid) [argPtr (castPtr raw_names :: Ptr ())] >>= retainedObject . castPtr

-- | @+ textureCubeWithImagesNamed:bundle:@
textureCubeWithImagesNamed_bundle :: (IsNSArray names, IsNSBundle bundleOrNil) => names -> bundleOrNil -> IO (Id MDLTexture)
textureCubeWithImagesNamed_bundle names bundleOrNil =
  do
    cls' <- getRequiredClass "MDLTexture"
    withObjCPtr names $ \raw_names ->
      withObjCPtr bundleOrNil $ \raw_bundleOrNil ->
        sendClassMsg cls' (mkSelector "textureCubeWithImagesNamed:bundle:") (retPtr retVoid) [argPtr (castPtr raw_names :: Ptr ()), argPtr (castPtr raw_bundleOrNil :: Ptr ())] >>= retainedObject . castPtr

-- | write a texture to URL, deducing type from path extension
--
-- ObjC selector: @- writeToURL:@
writeToURL :: (IsMDLTexture mdlTexture, IsNSURL url) => mdlTexture -> url -> IO Bool
writeToURL mdlTexture  url =
withObjCPtr url $ \raw_url ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mdlTexture (mkSelector "writeToURL:") retCULong [argPtr (castPtr raw_url :: Ptr ())]

-- | write a particular level of a mipped texture to URL, deducing type from path extension
--
-- ObjC selector: @- writeToURL:level:@
writeToURL_level :: (IsMDLTexture mdlTexture, IsNSURL url) => mdlTexture -> url -> CULong -> IO Bool
writeToURL_level mdlTexture  url level =
withObjCPtr url $ \raw_url ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mdlTexture (mkSelector "writeToURL:level:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argCULong (fromIntegral level)]

-- | write a texture to URL, using a specific UT type
--
-- ObjC selector: @- writeToURL:type:@
writeToURL_type :: (IsMDLTexture mdlTexture, IsNSURL nsurl) => mdlTexture -> nsurl -> RawId -> IO Bool
writeToURL_type mdlTexture  nsurl type_ =
withObjCPtr nsurl $ \raw_nsurl ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mdlTexture (mkSelector "writeToURL:type:") retCULong [argPtr (castPtr raw_nsurl :: Ptr ()), argPtr (castPtr (unRawId type_) :: Ptr ())]

-- | write a particular level of a mipped texture to URL, using a specific UT type
--
-- ObjC selector: @- writeToURL:type:level:@
writeToURL_type_level :: (IsMDLTexture mdlTexture, IsNSURL nsurl) => mdlTexture -> nsurl -> RawId -> CULong -> IO Bool
writeToURL_type_level mdlTexture  nsurl type_ level =
withObjCPtr nsurl $ \raw_nsurl ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mdlTexture (mkSelector "writeToURL:type:level:") retCULong [argPtr (castPtr raw_nsurl :: Ptr ()), argPtr (castPtr (unRawId type_) :: Ptr ()), argCULong (fromIntegral level)]

-- | @- imageFromTexture@
imageFromTexture :: IsMDLTexture mdlTexture => mdlTexture -> IO (Ptr ())
imageFromTexture mdlTexture  =
  fmap castPtr $ sendMsg mdlTexture (mkSelector "imageFromTexture") (retPtr retVoid) []

-- | @- imageFromTextureAtLevel:@
imageFromTextureAtLevel :: IsMDLTexture mdlTexture => mdlTexture -> CULong -> IO (Ptr ())
imageFromTextureAtLevel mdlTexture  level =
  fmap castPtr $ sendMsg mdlTexture (mkSelector "imageFromTextureAtLevel:") (retPtr retVoid) [argCULong (fromIntegral level)]

-- | @- texelDataWithTopLeftOrigin@
texelDataWithTopLeftOrigin :: IsMDLTexture mdlTexture => mdlTexture -> IO (Id NSData)
texelDataWithTopLeftOrigin mdlTexture  =
  sendMsg mdlTexture (mkSelector "texelDataWithTopLeftOrigin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- texelDataWithBottomLeftOrigin@
texelDataWithBottomLeftOrigin :: IsMDLTexture mdlTexture => mdlTexture -> IO (Id NSData)
texelDataWithBottomLeftOrigin mdlTexture  =
  sendMsg mdlTexture (mkSelector "texelDataWithBottomLeftOrigin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- texelDataWithTopLeftOriginAtMipLevel:create:@
texelDataWithTopLeftOriginAtMipLevel_create :: IsMDLTexture mdlTexture => mdlTexture -> CLong -> Bool -> IO (Id NSData)
texelDataWithTopLeftOriginAtMipLevel_create mdlTexture  level create =
  sendMsg mdlTexture (mkSelector "texelDataWithTopLeftOriginAtMipLevel:create:") (retPtr retVoid) [argCLong (fromIntegral level), argCULong (if create then 1 else 0)] >>= retainedObject . castPtr

-- | @- texelDataWithBottomLeftOriginAtMipLevel:create:@
texelDataWithBottomLeftOriginAtMipLevel_create :: IsMDLTexture mdlTexture => mdlTexture -> CLong -> Bool -> IO (Id NSData)
texelDataWithBottomLeftOriginAtMipLevel_create mdlTexture  level create =
  sendMsg mdlTexture (mkSelector "texelDataWithBottomLeftOriginAtMipLevel:create:") (retPtr retVoid) [argCLong (fromIntegral level), argCULong (if create then 1 else 0)] >>= retainedObject . castPtr

-- | @- rowStride@
rowStride :: IsMDLTexture mdlTexture => mdlTexture -> IO CLong
rowStride mdlTexture  =
  sendMsg mdlTexture (mkSelector "rowStride") retCLong []

-- | @- channelCount@
channelCount :: IsMDLTexture mdlTexture => mdlTexture -> IO CULong
channelCount mdlTexture  =
  sendMsg mdlTexture (mkSelector "channelCount") retCULong []

-- | @- mipLevelCount@
mipLevelCount :: IsMDLTexture mdlTexture => mdlTexture -> IO CULong
mipLevelCount mdlTexture  =
  sendMsg mdlTexture (mkSelector "mipLevelCount") retCULong []

-- | @- channelEncoding@
channelEncoding :: IsMDLTexture mdlTexture => mdlTexture -> IO MDLTextureChannelEncoding
channelEncoding mdlTexture  =
  fmap (coerce :: CLong -> MDLTextureChannelEncoding) $ sendMsg mdlTexture (mkSelector "channelEncoding") retCLong []

-- | @- isCube@
isCube :: IsMDLTexture mdlTexture => mdlTexture -> IO Bool
isCube mdlTexture  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mdlTexture (mkSelector "isCube") retCULong []

-- | @- setIsCube:@
setIsCube :: IsMDLTexture mdlTexture => mdlTexture -> Bool -> IO ()
setIsCube mdlTexture  value =
  sendMsg mdlTexture (mkSelector "setIsCube:") retVoid [argCULong (if value then 1 else 0)]

-- | hasAlphaValues  Can be overridden. If not overridden, hasAlpha will be NO if the texture does not have an alpha channel. It wil be YES if the texture has an alpha channel and there is at least one non-opaque texel in it.
--
-- ObjC selector: @- hasAlphaValues@
hasAlphaValues :: IsMDLTexture mdlTexture => mdlTexture -> IO Bool
hasAlphaValues mdlTexture  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mdlTexture (mkSelector "hasAlphaValues") retCULong []

-- | hasAlphaValues  Can be overridden. If not overridden, hasAlpha will be NO if the texture does not have an alpha channel. It wil be YES if the texture has an alpha channel and there is at least one non-opaque texel in it.
--
-- ObjC selector: @- setHasAlphaValues:@
setHasAlphaValues :: IsMDLTexture mdlTexture => mdlTexture -> Bool -> IO ()
setHasAlphaValues mdlTexture  value =
  sendMsg mdlTexture (mkSelector "setHasAlphaValues:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @textureNamed:@
textureNamedSelector :: Selector
textureNamedSelector = mkSelector "textureNamed:"

-- | @Selector@ for @textureNamed:bundle:@
textureNamed_bundleSelector :: Selector
textureNamed_bundleSelector = mkSelector "textureNamed:bundle:"

-- | @Selector@ for @textureNamed:assetResolver:@
textureNamed_assetResolverSelector :: Selector
textureNamed_assetResolverSelector = mkSelector "textureNamed:assetResolver:"

-- | @Selector@ for @textureCubeWithImagesNamed:@
textureCubeWithImagesNamedSelector :: Selector
textureCubeWithImagesNamedSelector = mkSelector "textureCubeWithImagesNamed:"

-- | @Selector@ for @textureCubeWithImagesNamed:bundle:@
textureCubeWithImagesNamed_bundleSelector :: Selector
textureCubeWithImagesNamed_bundleSelector = mkSelector "textureCubeWithImagesNamed:bundle:"

-- | @Selector@ for @writeToURL:@
writeToURLSelector :: Selector
writeToURLSelector = mkSelector "writeToURL:"

-- | @Selector@ for @writeToURL:level:@
writeToURL_levelSelector :: Selector
writeToURL_levelSelector = mkSelector "writeToURL:level:"

-- | @Selector@ for @writeToURL:type:@
writeToURL_typeSelector :: Selector
writeToURL_typeSelector = mkSelector "writeToURL:type:"

-- | @Selector@ for @writeToURL:type:level:@
writeToURL_type_levelSelector :: Selector
writeToURL_type_levelSelector = mkSelector "writeToURL:type:level:"

-- | @Selector@ for @imageFromTexture@
imageFromTextureSelector :: Selector
imageFromTextureSelector = mkSelector "imageFromTexture"

-- | @Selector@ for @imageFromTextureAtLevel:@
imageFromTextureAtLevelSelector :: Selector
imageFromTextureAtLevelSelector = mkSelector "imageFromTextureAtLevel:"

-- | @Selector@ for @texelDataWithTopLeftOrigin@
texelDataWithTopLeftOriginSelector :: Selector
texelDataWithTopLeftOriginSelector = mkSelector "texelDataWithTopLeftOrigin"

-- | @Selector@ for @texelDataWithBottomLeftOrigin@
texelDataWithBottomLeftOriginSelector :: Selector
texelDataWithBottomLeftOriginSelector = mkSelector "texelDataWithBottomLeftOrigin"

-- | @Selector@ for @texelDataWithTopLeftOriginAtMipLevel:create:@
texelDataWithTopLeftOriginAtMipLevel_createSelector :: Selector
texelDataWithTopLeftOriginAtMipLevel_createSelector = mkSelector "texelDataWithTopLeftOriginAtMipLevel:create:"

-- | @Selector@ for @texelDataWithBottomLeftOriginAtMipLevel:create:@
texelDataWithBottomLeftOriginAtMipLevel_createSelector :: Selector
texelDataWithBottomLeftOriginAtMipLevel_createSelector = mkSelector "texelDataWithBottomLeftOriginAtMipLevel:create:"

-- | @Selector@ for @rowStride@
rowStrideSelector :: Selector
rowStrideSelector = mkSelector "rowStride"

-- | @Selector@ for @channelCount@
channelCountSelector :: Selector
channelCountSelector = mkSelector "channelCount"

-- | @Selector@ for @mipLevelCount@
mipLevelCountSelector :: Selector
mipLevelCountSelector = mkSelector "mipLevelCount"

-- | @Selector@ for @channelEncoding@
channelEncodingSelector :: Selector
channelEncodingSelector = mkSelector "channelEncoding"

-- | @Selector@ for @isCube@
isCubeSelector :: Selector
isCubeSelector = mkSelector "isCube"

-- | @Selector@ for @setIsCube:@
setIsCubeSelector :: Selector
setIsCubeSelector = mkSelector "setIsCube:"

-- | @Selector@ for @hasAlphaValues@
hasAlphaValuesSelector :: Selector
hasAlphaValuesSelector = mkSelector "hasAlphaValues"

-- | @Selector@ for @setHasAlphaValues:@
setHasAlphaValuesSelector :: Selector
setHasAlphaValuesSelector = mkSelector "setHasAlphaValues:"

