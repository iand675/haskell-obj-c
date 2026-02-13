{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , channelCountSelector
  , channelEncodingSelector
  , hasAlphaValuesSelector
  , imageFromTextureAtLevelSelector
  , imageFromTextureSelector
  , initSelector
  , isCubeSelector
  , mipLevelCountSelector
  , rowStrideSelector
  , setHasAlphaValuesSelector
  , setIsCubeSelector
  , texelDataWithBottomLeftOriginAtMipLevel_createSelector
  , texelDataWithBottomLeftOriginSelector
  , texelDataWithTopLeftOriginAtMipLevel_createSelector
  , texelDataWithTopLeftOriginSelector
  , textureCubeWithImagesNamedSelector
  , textureCubeWithImagesNamed_bundleSelector
  , textureNamedSelector
  , textureNamed_assetResolverSelector
  , textureNamed_bundleSelector
  , writeToURLSelector
  , writeToURL_levelSelector
  , writeToURL_typeSelector
  , writeToURL_type_levelSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMDLTexture mdlTexture => mdlTexture -> IO (Id MDLTexture)
init_ mdlTexture =
  sendOwnedMessage mdlTexture initSelector

-- | Creates a texture from a source in the main bundle named in a manner matching  name.
--
-- ObjC selector: @+ textureNamed:@
textureNamed :: IsNSString name => name -> IO (Id MDLTexture)
textureNamed name =
  do
    cls' <- getRequiredClass "MDLTexture"
    sendClassMessage cls' textureNamedSelector (toNSString name)

-- | @+ textureNamed:bundle:@
textureNamed_bundle :: (IsNSString name, IsNSBundle bundleOrNil) => name -> bundleOrNil -> IO (Id MDLTexture)
textureNamed_bundle name bundleOrNil =
  do
    cls' <- getRequiredClass "MDLTexture"
    sendClassMessage cls' textureNamed_bundleSelector (toNSString name) (toNSBundle bundleOrNil)

-- | @+ textureNamed:assetResolver:@
textureNamed_assetResolver :: IsNSString name => name -> RawId -> IO (Id MDLTexture)
textureNamed_assetResolver name resolver =
  do
    cls' <- getRequiredClass "MDLTexture"
    sendClassMessage cls' textureNamed_assetResolverSelector (toNSString name) resolver

-- | Creates a cube texture map image using 6 faces of the same dimensions,   ordered +X,-X,+Y,-Y,+Z,-Z If the data is read back the image will be compacted   into a single vertical stack where dimensions.y = 6 * dimensions.x  isCube will return YES
--
-- @names@ — a collection of mosaiced images in a cross formation or column or row.  - If 6 individual images are given they are assumed to be in order and will be     loaded as is.  - if 3 images of double height or width are given they will be treated as     pairs of + and - in each axis, the order is must be x, then y, then z.  - if 2 images of triple height or width are given they will be treates as a     positive set and a negative set in the order +x, +y, +z, then -x, -y, -z.  - if a single image is given it will be used without conversion if in column     orientation and demosaiced in all other instances.
--
-- ObjC selector: @+ textureCubeWithImagesNamed:@
textureCubeWithImagesNamed :: IsNSArray names => names -> IO (Id MDLTexture)
textureCubeWithImagesNamed names =
  do
    cls' <- getRequiredClass "MDLTexture"
    sendClassMessage cls' textureCubeWithImagesNamedSelector (toNSArray names)

-- | @+ textureCubeWithImagesNamed:bundle:@
textureCubeWithImagesNamed_bundle :: (IsNSArray names, IsNSBundle bundleOrNil) => names -> bundleOrNil -> IO (Id MDLTexture)
textureCubeWithImagesNamed_bundle names bundleOrNil =
  do
    cls' <- getRequiredClass "MDLTexture"
    sendClassMessage cls' textureCubeWithImagesNamed_bundleSelector (toNSArray names) (toNSBundle bundleOrNil)

-- | write a texture to URL, deducing type from path extension
--
-- ObjC selector: @- writeToURL:@
writeToURL :: (IsMDLTexture mdlTexture, IsNSURL url) => mdlTexture -> url -> IO Bool
writeToURL mdlTexture url =
  sendMessage mdlTexture writeToURLSelector (toNSURL url)

-- | write a particular level of a mipped texture to URL, deducing type from path extension
--
-- ObjC selector: @- writeToURL:level:@
writeToURL_level :: (IsMDLTexture mdlTexture, IsNSURL url) => mdlTexture -> url -> CULong -> IO Bool
writeToURL_level mdlTexture url level =
  sendMessage mdlTexture writeToURL_levelSelector (toNSURL url) level

-- | write a texture to URL, using a specific UT type
--
-- ObjC selector: @- writeToURL:type:@
writeToURL_type :: (IsMDLTexture mdlTexture, IsNSURL nsurl) => mdlTexture -> nsurl -> RawId -> IO Bool
writeToURL_type mdlTexture nsurl type_ =
  sendMessage mdlTexture writeToURL_typeSelector (toNSURL nsurl) type_

-- | write a particular level of a mipped texture to URL, using a specific UT type
--
-- ObjC selector: @- writeToURL:type:level:@
writeToURL_type_level :: (IsMDLTexture mdlTexture, IsNSURL nsurl) => mdlTexture -> nsurl -> RawId -> CULong -> IO Bool
writeToURL_type_level mdlTexture nsurl type_ level =
  sendMessage mdlTexture writeToURL_type_levelSelector (toNSURL nsurl) type_ level

-- | @- imageFromTexture@
imageFromTexture :: IsMDLTexture mdlTexture => mdlTexture -> IO (Ptr ())
imageFromTexture mdlTexture =
  sendMessage mdlTexture imageFromTextureSelector

-- | @- imageFromTextureAtLevel:@
imageFromTextureAtLevel :: IsMDLTexture mdlTexture => mdlTexture -> CULong -> IO (Ptr ())
imageFromTextureAtLevel mdlTexture level =
  sendMessage mdlTexture imageFromTextureAtLevelSelector level

-- | @- texelDataWithTopLeftOrigin@
texelDataWithTopLeftOrigin :: IsMDLTexture mdlTexture => mdlTexture -> IO (Id NSData)
texelDataWithTopLeftOrigin mdlTexture =
  sendMessage mdlTexture texelDataWithTopLeftOriginSelector

-- | @- texelDataWithBottomLeftOrigin@
texelDataWithBottomLeftOrigin :: IsMDLTexture mdlTexture => mdlTexture -> IO (Id NSData)
texelDataWithBottomLeftOrigin mdlTexture =
  sendMessage mdlTexture texelDataWithBottomLeftOriginSelector

-- | @- texelDataWithTopLeftOriginAtMipLevel:create:@
texelDataWithTopLeftOriginAtMipLevel_create :: IsMDLTexture mdlTexture => mdlTexture -> CLong -> Bool -> IO (Id NSData)
texelDataWithTopLeftOriginAtMipLevel_create mdlTexture level create =
  sendMessage mdlTexture texelDataWithTopLeftOriginAtMipLevel_createSelector level create

-- | @- texelDataWithBottomLeftOriginAtMipLevel:create:@
texelDataWithBottomLeftOriginAtMipLevel_create :: IsMDLTexture mdlTexture => mdlTexture -> CLong -> Bool -> IO (Id NSData)
texelDataWithBottomLeftOriginAtMipLevel_create mdlTexture level create =
  sendMessage mdlTexture texelDataWithBottomLeftOriginAtMipLevel_createSelector level create

-- | @- rowStride@
rowStride :: IsMDLTexture mdlTexture => mdlTexture -> IO CLong
rowStride mdlTexture =
  sendMessage mdlTexture rowStrideSelector

-- | @- channelCount@
channelCount :: IsMDLTexture mdlTexture => mdlTexture -> IO CULong
channelCount mdlTexture =
  sendMessage mdlTexture channelCountSelector

-- | @- mipLevelCount@
mipLevelCount :: IsMDLTexture mdlTexture => mdlTexture -> IO CULong
mipLevelCount mdlTexture =
  sendMessage mdlTexture mipLevelCountSelector

-- | @- channelEncoding@
channelEncoding :: IsMDLTexture mdlTexture => mdlTexture -> IO MDLTextureChannelEncoding
channelEncoding mdlTexture =
  sendMessage mdlTexture channelEncodingSelector

-- | @- isCube@
isCube :: IsMDLTexture mdlTexture => mdlTexture -> IO Bool
isCube mdlTexture =
  sendMessage mdlTexture isCubeSelector

-- | @- setIsCube:@
setIsCube :: IsMDLTexture mdlTexture => mdlTexture -> Bool -> IO ()
setIsCube mdlTexture value =
  sendMessage mdlTexture setIsCubeSelector value

-- | hasAlphaValues  Can be overridden. If not overridden, hasAlpha will be NO if the texture does not have an alpha channel. It wil be YES if the texture has an alpha channel and there is at least one non-opaque texel in it.
--
-- ObjC selector: @- hasAlphaValues@
hasAlphaValues :: IsMDLTexture mdlTexture => mdlTexture -> IO Bool
hasAlphaValues mdlTexture =
  sendMessage mdlTexture hasAlphaValuesSelector

-- | hasAlphaValues  Can be overridden. If not overridden, hasAlpha will be NO if the texture does not have an alpha channel. It wil be YES if the texture has an alpha channel and there is at least one non-opaque texel in it.
--
-- ObjC selector: @- setHasAlphaValues:@
setHasAlphaValues :: IsMDLTexture mdlTexture => mdlTexture -> Bool -> IO ()
setHasAlphaValues mdlTexture value =
  sendMessage mdlTexture setHasAlphaValuesSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MDLTexture)
initSelector = mkSelector "init"

-- | @Selector@ for @textureNamed:@
textureNamedSelector :: Selector '[Id NSString] (Id MDLTexture)
textureNamedSelector = mkSelector "textureNamed:"

-- | @Selector@ for @textureNamed:bundle:@
textureNamed_bundleSelector :: Selector '[Id NSString, Id NSBundle] (Id MDLTexture)
textureNamed_bundleSelector = mkSelector "textureNamed:bundle:"

-- | @Selector@ for @textureNamed:assetResolver:@
textureNamed_assetResolverSelector :: Selector '[Id NSString, RawId] (Id MDLTexture)
textureNamed_assetResolverSelector = mkSelector "textureNamed:assetResolver:"

-- | @Selector@ for @textureCubeWithImagesNamed:@
textureCubeWithImagesNamedSelector :: Selector '[Id NSArray] (Id MDLTexture)
textureCubeWithImagesNamedSelector = mkSelector "textureCubeWithImagesNamed:"

-- | @Selector@ for @textureCubeWithImagesNamed:bundle:@
textureCubeWithImagesNamed_bundleSelector :: Selector '[Id NSArray, Id NSBundle] (Id MDLTexture)
textureCubeWithImagesNamed_bundleSelector = mkSelector "textureCubeWithImagesNamed:bundle:"

-- | @Selector@ for @writeToURL:@
writeToURLSelector :: Selector '[Id NSURL] Bool
writeToURLSelector = mkSelector "writeToURL:"

-- | @Selector@ for @writeToURL:level:@
writeToURL_levelSelector :: Selector '[Id NSURL, CULong] Bool
writeToURL_levelSelector = mkSelector "writeToURL:level:"

-- | @Selector@ for @writeToURL:type:@
writeToURL_typeSelector :: Selector '[Id NSURL, RawId] Bool
writeToURL_typeSelector = mkSelector "writeToURL:type:"

-- | @Selector@ for @writeToURL:type:level:@
writeToURL_type_levelSelector :: Selector '[Id NSURL, RawId, CULong] Bool
writeToURL_type_levelSelector = mkSelector "writeToURL:type:level:"

-- | @Selector@ for @imageFromTexture@
imageFromTextureSelector :: Selector '[] (Ptr ())
imageFromTextureSelector = mkSelector "imageFromTexture"

-- | @Selector@ for @imageFromTextureAtLevel:@
imageFromTextureAtLevelSelector :: Selector '[CULong] (Ptr ())
imageFromTextureAtLevelSelector = mkSelector "imageFromTextureAtLevel:"

-- | @Selector@ for @texelDataWithTopLeftOrigin@
texelDataWithTopLeftOriginSelector :: Selector '[] (Id NSData)
texelDataWithTopLeftOriginSelector = mkSelector "texelDataWithTopLeftOrigin"

-- | @Selector@ for @texelDataWithBottomLeftOrigin@
texelDataWithBottomLeftOriginSelector :: Selector '[] (Id NSData)
texelDataWithBottomLeftOriginSelector = mkSelector "texelDataWithBottomLeftOrigin"

-- | @Selector@ for @texelDataWithTopLeftOriginAtMipLevel:create:@
texelDataWithTopLeftOriginAtMipLevel_createSelector :: Selector '[CLong, Bool] (Id NSData)
texelDataWithTopLeftOriginAtMipLevel_createSelector = mkSelector "texelDataWithTopLeftOriginAtMipLevel:create:"

-- | @Selector@ for @texelDataWithBottomLeftOriginAtMipLevel:create:@
texelDataWithBottomLeftOriginAtMipLevel_createSelector :: Selector '[CLong, Bool] (Id NSData)
texelDataWithBottomLeftOriginAtMipLevel_createSelector = mkSelector "texelDataWithBottomLeftOriginAtMipLevel:create:"

-- | @Selector@ for @rowStride@
rowStrideSelector :: Selector '[] CLong
rowStrideSelector = mkSelector "rowStride"

-- | @Selector@ for @channelCount@
channelCountSelector :: Selector '[] CULong
channelCountSelector = mkSelector "channelCount"

-- | @Selector@ for @mipLevelCount@
mipLevelCountSelector :: Selector '[] CULong
mipLevelCountSelector = mkSelector "mipLevelCount"

-- | @Selector@ for @channelEncoding@
channelEncodingSelector :: Selector '[] MDLTextureChannelEncoding
channelEncodingSelector = mkSelector "channelEncoding"

-- | @Selector@ for @isCube@
isCubeSelector :: Selector '[] Bool
isCubeSelector = mkSelector "isCube"

-- | @Selector@ for @setIsCube:@
setIsCubeSelector :: Selector '[Bool] ()
setIsCubeSelector = mkSelector "setIsCube:"

-- | @Selector@ for @hasAlphaValues@
hasAlphaValuesSelector :: Selector '[] Bool
hasAlphaValuesSelector = mkSelector "hasAlphaValues"

-- | @Selector@ for @setHasAlphaValues:@
setHasAlphaValuesSelector :: Selector '[Bool] ()
setHasAlphaValuesSelector = mkSelector "setHasAlphaValues:"

