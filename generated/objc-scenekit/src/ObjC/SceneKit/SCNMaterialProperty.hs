{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNMaterialProperty
--
-- The contents of a SCNMaterial slot
--
-- This can be used to specify the various properties of SCNMaterial slots such as diffuse, ambient, etc.
--
-- Generated bindings for @SCNMaterialProperty@.
module ObjC.SceneKit.SCNMaterialProperty
  ( SCNMaterialProperty
  , IsSCNMaterialProperty(..)
  , materialPropertyWithContents
  , precomputedLightingEnvironmentContentsWithURL_error
  , precomputedLightingEnvironmentContentsWithData_error
  , precomputedLightingEnvironmentDataForContents_device_error
  , contents
  , setContents
  , intensity
  , setIntensity
  , minificationFilter
  , setMinificationFilter
  , magnificationFilter
  , setMagnificationFilter
  , mipFilter
  , setMipFilter
  , contentsTransform
  , setContentsTransform
  , wrapS
  , setWrapS
  , wrapT
  , setWrapT
  , mappingChannel
  , setMappingChannel
  , textureComponents
  , setTextureComponents
  , maxAnisotropy
  , setMaxAnisotropy
  , borderColor
  , setBorderColor
  , materialPropertyWithContentsSelector
  , precomputedLightingEnvironmentContentsWithURL_errorSelector
  , precomputedLightingEnvironmentContentsWithData_errorSelector
  , precomputedLightingEnvironmentDataForContents_device_errorSelector
  , contentsSelector
  , setContentsSelector
  , intensitySelector
  , setIntensitySelector
  , minificationFilterSelector
  , setMinificationFilterSelector
  , magnificationFilterSelector
  , setMagnificationFilterSelector
  , mipFilterSelector
  , setMipFilterSelector
  , contentsTransformSelector
  , setContentsTransformSelector
  , wrapSSelector
  , setWrapSSelector
  , wrapTSelector
  , setWrapTSelector
  , mappingChannelSelector
  , setMappingChannelSelector
  , textureComponentsSelector
  , setTextureComponentsSelector
  , maxAnisotropySelector
  , setMaxAnisotropySelector
  , borderColorSelector
  , setBorderColorSelector

  -- * Enum types
  , SCNColorMask(SCNColorMask)
  , pattern SCNColorMaskNone
  , pattern SCNColorMaskRed
  , pattern SCNColorMaskGreen
  , pattern SCNColorMaskBlue
  , pattern SCNColorMaskAlpha
  , pattern SCNColorMaskAll
  , SCNFilterMode(SCNFilterMode)
  , pattern SCNFilterModeNone
  , pattern SCNFilterModeNearest
  , pattern SCNFilterModeLinear
  , SCNWrapMode(SCNWrapMode)
  , pattern SCNWrapModeClamp
  , pattern SCNWrapModeRepeat
  , pattern SCNWrapModeClampToBorder
  , pattern SCNWrapModeMirror

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

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Structs
import ObjC.SceneKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | materialPropertyWithContents:
--
-- Creates and initialize a property instance with the specified contents.
--
-- ObjC selector: @+ materialPropertyWithContents:@
materialPropertyWithContents :: RawId -> IO (Id SCNMaterialProperty)
materialPropertyWithContents contents =
  do
    cls' <- getRequiredClass "SCNMaterialProperty"
    sendClassMsg cls' (mkSelector "materialPropertyWithContents:") (retPtr retVoid) [argPtr (castPtr (unRawId contents) :: Ptr ())] >>= retainedObject . castPtr

-- | precomputedLightingEnvironmentContentsWithURL:error:
--
-- Returns an object suitable for a scene's @lightingEnvironment.contents@ and initialized with data that was previously created by @+precomputedLightingEnvironmentDataForContents:device:error:@.
--
-- ObjC selector: @+ precomputedLightingEnvironmentContentsWithURL:error:@
precomputedLightingEnvironmentContentsWithURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO RawId
precomputedLightingEnvironmentContentsWithURL_error url error_ =
  do
    cls' <- getRequiredClass "SCNMaterialProperty"
    withObjCPtr url $ \raw_url ->
      withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "precomputedLightingEnvironmentContentsWithURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | precomputedLightingEnvironmentContentsWithData:error:
--
-- Returns an object suitable for a scene's @lightingEnvironment.contents@ and initialized with data that was previously created by @+precomputedLightingEnvironmentDataForContents:device:error:@.
--
-- ObjC selector: @+ precomputedLightingEnvironmentContentsWithData:error:@
precomputedLightingEnvironmentContentsWithData_error :: (IsNSData data_, IsNSError error_) => data_ -> error_ -> IO RawId
precomputedLightingEnvironmentContentsWithData_error data_ error_ =
  do
    cls' <- getRequiredClass "SCNMaterialProperty"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "precomputedLightingEnvironmentContentsWithData:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | precomputedLightingEnvironmentDataForContents:device:error:
--
-- Returns an @NSData@ instance containing the result of CPU and GPU-intensive operations that is suitable for caching.
--
-- This method can be leveraged in a custom offline asset pipeline, or at run time at a convenient time before the scene is presented to the user.
--
-- ObjC selector: @+ precomputedLightingEnvironmentDataForContents:device:error:@
precomputedLightingEnvironmentDataForContents_device_error :: IsNSError error_ => RawId -> RawId -> error_ -> IO (Id NSData)
precomputedLightingEnvironmentDataForContents_device_error contents device error_ =
  do
    cls' <- getRequiredClass "SCNMaterialProperty"
    withObjCPtr error_ $ \raw_error_ ->
      sendClassMsg cls' (mkSelector "precomputedLightingEnvironmentDataForContents:device:error:") (retPtr retVoid) [argPtr (castPtr (unRawId contents) :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | contents
--
-- Specifies the receiver's contents. This can be a color (NSColor, UIColor, CGColorRef), an image (NSImage, UIImage, CGImageRef), a layer (CALayer), a path (NSString or NSURL), a SpriteKit scene (SKScene), a texture (SKTexture, id<MTLTexture> or GLKTextureInfo), or a floating value between 0 and 1 (NSNumber) for metalness and roughness properties. AVCaptureDevice is supported on iOS 11 and AVPlayer is supported on macOS 10.13, iOS 11 and tvOS 11. Animatable when set to a color.
--
-- Setting the contents to an instance of SKTexture will automatically update the wrapS, wrapT, contentsTransform, minification, magnification and mip filters according to the SKTexture settings.             When a cube map is expected (e.g. SCNMaterial.reflective, SCNScene.background, SCNScene.lightingEnvironment) you can use               1. A horizontal strip image                          where @6 * image.height ==     image.width@               2. A vertical strip image                            where @    image.height == 6 * image.width@               3. A spherical projection image (latitude/longitude) where @2 * image.height ==     image.width@               4. A NSArray of 6 images. This array must contain images of the exact same dimensions, in the following order, in a left-handed coordinate system: +X, -X, +Y, -Y, +Z, -Z (or Right, Left, Top, Bottom, Front, Back).
--
-- ObjC selector: @- contents@
contents :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO RawId
contents scnMaterialProperty  =
  fmap (RawId . castPtr) $ sendMsg scnMaterialProperty (mkSelector "contents") (retPtr retVoid) []

-- | contents
--
-- Specifies the receiver's contents. This can be a color (NSColor, UIColor, CGColorRef), an image (NSImage, UIImage, CGImageRef), a layer (CALayer), a path (NSString or NSURL), a SpriteKit scene (SKScene), a texture (SKTexture, id<MTLTexture> or GLKTextureInfo), or a floating value between 0 and 1 (NSNumber) for metalness and roughness properties. AVCaptureDevice is supported on iOS 11 and AVPlayer is supported on macOS 10.13, iOS 11 and tvOS 11. Animatable when set to a color.
--
-- Setting the contents to an instance of SKTexture will automatically update the wrapS, wrapT, contentsTransform, minification, magnification and mip filters according to the SKTexture settings.             When a cube map is expected (e.g. SCNMaterial.reflective, SCNScene.background, SCNScene.lightingEnvironment) you can use               1. A horizontal strip image                          where @6 * image.height ==     image.width@               2. A vertical strip image                            where @    image.height == 6 * image.width@               3. A spherical projection image (latitude/longitude) where @2 * image.height ==     image.width@               4. A NSArray of 6 images. This array must contain images of the exact same dimensions, in the following order, in a left-handed coordinate system: +X, -X, +Y, -Y, +Z, -Z (or Right, Left, Top, Bottom, Front, Back).
--
-- ObjC selector: @- setContents:@
setContents :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> RawId -> IO ()
setContents scnMaterialProperty  value =
  sendMsg scnMaterialProperty (mkSelector "setContents:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | intensity
--
-- Determines the receiver's intensity. This intensity is used to modulate the properties in several ways. It dims the diffuse, specular and emission properties, it varies the bumpiness of the normal property and the filter property is blended with white. Default value is 1.0. Animatable.
--
-- ObjC selector: @- intensity@
intensity :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO CDouble
intensity scnMaterialProperty  =
  sendMsg scnMaterialProperty (mkSelector "intensity") retCDouble []

-- | intensity
--
-- Determines the receiver's intensity. This intensity is used to modulate the properties in several ways. It dims the diffuse, specular and emission properties, it varies the bumpiness of the normal property and the filter property is blended with white. Default value is 1.0. Animatable.
--
-- ObjC selector: @- setIntensity:@
setIntensity :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> CDouble -> IO ()
setIntensity scnMaterialProperty  value =
  sendMsg scnMaterialProperty (mkSelector "setIntensity:") retVoid [argCDouble (fromIntegral value)]

-- | minificationFilter
--
-- Specifies the filter type to use when rendering the contents (specified in the `contents' property).
--
-- The minification filter is used when to reduce the size of image data. See above the list of available modes. Defaults to SCNFilterModeLinear.
--
-- ObjC selector: @- minificationFilter@
minificationFilter :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO SCNFilterMode
minificationFilter scnMaterialProperty  =
  fmap (coerce :: CLong -> SCNFilterMode) $ sendMsg scnMaterialProperty (mkSelector "minificationFilter") retCLong []

-- | minificationFilter
--
-- Specifies the filter type to use when rendering the contents (specified in the `contents' property).
--
-- The minification filter is used when to reduce the size of image data. See above the list of available modes. Defaults to SCNFilterModeLinear.
--
-- ObjC selector: @- setMinificationFilter:@
setMinificationFilter :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> SCNFilterMode -> IO ()
setMinificationFilter scnMaterialProperty  value =
  sendMsg scnMaterialProperty (mkSelector "setMinificationFilter:") retVoid [argCLong (coerce value)]

-- | magnificationFilter
--
-- Specifies the filter type to use when rendering the the contents (specified in the `contents' property).
--
-- The magnification filter is used when to increase the size of image data. See above the list of available modes. Defaults to SCNFilterModeLinear.
--
-- ObjC selector: @- magnificationFilter@
magnificationFilter :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO SCNFilterMode
magnificationFilter scnMaterialProperty  =
  fmap (coerce :: CLong -> SCNFilterMode) $ sendMsg scnMaterialProperty (mkSelector "magnificationFilter") retCLong []

-- | magnificationFilter
--
-- Specifies the filter type to use when rendering the the contents (specified in the `contents' property).
--
-- The magnification filter is used when to increase the size of image data. See above the list of available modes. Defaults to SCNFilterModeLinear.
--
-- ObjC selector: @- setMagnificationFilter:@
setMagnificationFilter :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> SCNFilterMode -> IO ()
setMagnificationFilter scnMaterialProperty  value =
  sendMsg scnMaterialProperty (mkSelector "setMagnificationFilter:") retVoid [argCLong (coerce value)]

-- | mipFilter
--
-- Specifies the mipmap filter to use during minification.
--
-- Defaults to SCNFilterModeNearest starting macOS 10.12, iOS 10, tvOS 10 and watchOS 3. Defaults to SCNFilterModeNone in previous versions.
--
-- ObjC selector: @- mipFilter@
mipFilter :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO SCNFilterMode
mipFilter scnMaterialProperty  =
  fmap (coerce :: CLong -> SCNFilterMode) $ sendMsg scnMaterialProperty (mkSelector "mipFilter") retCLong []

-- | mipFilter
--
-- Specifies the mipmap filter to use during minification.
--
-- Defaults to SCNFilterModeNearest starting macOS 10.12, iOS 10, tvOS 10 and watchOS 3. Defaults to SCNFilterModeNone in previous versions.
--
-- ObjC selector: @- setMipFilter:@
setMipFilter :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> SCNFilterMode -> IO ()
setMipFilter scnMaterialProperty  value =
  sendMsg scnMaterialProperty (mkSelector "setMipFilter:") retVoid [argCLong (coerce value)]

-- | contentsTransform
--
-- Determines the receiver's contents transform. Animatable.
--
-- ObjC selector: @- contentsTransform@
contentsTransform :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO SCNMatrix4
contentsTransform scnMaterialProperty  =
  sendMsgStret scnMaterialProperty (mkSelector "contentsTransform") retSCNMatrix4 []

-- | contentsTransform
--
-- Determines the receiver's contents transform. Animatable.
--
-- ObjC selector: @- setContentsTransform:@
setContentsTransform :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> SCNMatrix4 -> IO ()
setContentsTransform scnMaterialProperty  value =
  sendMsg scnMaterialProperty (mkSelector "setContentsTransform:") retVoid [argSCNMatrix4 value]

-- | wrapS
--
-- Determines the receiver's wrap mode for the s texture coordinate. Defaults to SCNWrapModeClamp.
--
-- ObjC selector: @- wrapS@
wrapS :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO SCNWrapMode
wrapS scnMaterialProperty  =
  fmap (coerce :: CLong -> SCNWrapMode) $ sendMsg scnMaterialProperty (mkSelector "wrapS") retCLong []

-- | wrapS
--
-- Determines the receiver's wrap mode for the s texture coordinate. Defaults to SCNWrapModeClamp.
--
-- ObjC selector: @- setWrapS:@
setWrapS :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> SCNWrapMode -> IO ()
setWrapS scnMaterialProperty  value =
  sendMsg scnMaterialProperty (mkSelector "setWrapS:") retVoid [argCLong (coerce value)]

-- | wrapT
--
-- Determines the receiver's wrap mode for the t texture coordinate. Defaults to SCNWrapModeClamp.
--
-- ObjC selector: @- wrapT@
wrapT :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO SCNWrapMode
wrapT scnMaterialProperty  =
  fmap (coerce :: CLong -> SCNWrapMode) $ sendMsg scnMaterialProperty (mkSelector "wrapT") retCLong []

-- | wrapT
--
-- Determines the receiver's wrap mode for the t texture coordinate. Defaults to SCNWrapModeClamp.
--
-- ObjC selector: @- setWrapT:@
setWrapT :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> SCNWrapMode -> IO ()
setWrapT scnMaterialProperty  value =
  sendMsg scnMaterialProperty (mkSelector "setWrapT:") retVoid [argCLong (coerce value)]

-- | mappingChannel
--
-- Determines the receiver's mapping channel. Defaults to 0.
--
-- Geometries potentially have multiple sources of texture coordinates. Every source has a unique mapping channel index. The mapping channel allows to select which source of texture coordinates is used to map the content of the receiver.
--
-- ObjC selector: @- mappingChannel@
mappingChannel :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO CLong
mappingChannel scnMaterialProperty  =
  sendMsg scnMaterialProperty (mkSelector "mappingChannel") retCLong []

-- | mappingChannel
--
-- Determines the receiver's mapping channel. Defaults to 0.
--
-- Geometries potentially have multiple sources of texture coordinates. Every source has a unique mapping channel index. The mapping channel allows to select which source of texture coordinates is used to map the content of the receiver.
--
-- ObjC selector: @- setMappingChannel:@
setMappingChannel :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> CLong -> IO ()
setMappingChannel scnMaterialProperty  value =
  sendMsg scnMaterialProperty (mkSelector "setMappingChannel:") retVoid [argCLong (fromIntegral value)]

-- | textureComponents
--
-- Specifies the texture components to sample in the shader. Defaults to SCNColorMaskRed for displacement property, and to SCNColorMaskAll for other properties.
--
-- Use this property to when using a texture that combine multiple informations in the different texture components. For example if you pack the roughness in red and metalness in blue etc... You can specify what component to use from the texture for this given material property. This property is only supported by Metal renderers.
--
-- ObjC selector: @- textureComponents@
textureComponents :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO SCNColorMask
textureComponents scnMaterialProperty  =
  fmap (coerce :: CLong -> SCNColorMask) $ sendMsg scnMaterialProperty (mkSelector "textureComponents") retCLong []

-- | textureComponents
--
-- Specifies the texture components to sample in the shader. Defaults to SCNColorMaskRed for displacement property, and to SCNColorMaskAll for other properties.
--
-- Use this property to when using a texture that combine multiple informations in the different texture components. For example if you pack the roughness in red and metalness in blue etc... You can specify what component to use from the texture for this given material property. This property is only supported by Metal renderers.
--
-- ObjC selector: @- setTextureComponents:@
setTextureComponents :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> SCNColorMask -> IO ()
setTextureComponents scnMaterialProperty  value =
  sendMsg scnMaterialProperty (mkSelector "setTextureComponents:") retVoid [argCLong (coerce value)]

-- | maxAnisotropy
--
-- Specifies the receiver's max anisotropy. Defaults to MAXFLOAT.
--
-- Anisotropic filtering reduces blur and preserves detail at extreme viewing angles.
--
-- ObjC selector: @- maxAnisotropy@
maxAnisotropy :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO CDouble
maxAnisotropy scnMaterialProperty  =
  sendMsg scnMaterialProperty (mkSelector "maxAnisotropy") retCDouble []

-- | maxAnisotropy
--
-- Specifies the receiver's max anisotropy. Defaults to MAXFLOAT.
--
-- Anisotropic filtering reduces blur and preserves detail at extreme viewing angles.
--
-- ObjC selector: @- setMaxAnisotropy:@
setMaxAnisotropy :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> CDouble -> IO ()
setMaxAnisotropy scnMaterialProperty  value =
  sendMsg scnMaterialProperty (mkSelector "setMaxAnisotropy:") retVoid [argCDouble (fromIntegral value)]

-- | borderColor
--
-- Determines the receiver's border color (CGColorRef or NSColor). Animatable.
--
-- The border color is ignored on iOS and is always considered as clear color (0,0,0,0) when the texture has an alpha channel and opaque back (0,0,0,1) otherwise.
--
-- ObjC selector: @- borderColor@
borderColor :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO RawId
borderColor scnMaterialProperty  =
  fmap (RawId . castPtr) $ sendMsg scnMaterialProperty (mkSelector "borderColor") (retPtr retVoid) []

-- | borderColor
--
-- Determines the receiver's border color (CGColorRef or NSColor). Animatable.
--
-- The border color is ignored on iOS and is always considered as clear color (0,0,0,0) when the texture has an alpha channel and opaque back (0,0,0,1) otherwise.
--
-- ObjC selector: @- setBorderColor:@
setBorderColor :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> RawId -> IO ()
setBorderColor scnMaterialProperty  value =
  sendMsg scnMaterialProperty (mkSelector "setBorderColor:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @materialPropertyWithContents:@
materialPropertyWithContentsSelector :: Selector
materialPropertyWithContentsSelector = mkSelector "materialPropertyWithContents:"

-- | @Selector@ for @precomputedLightingEnvironmentContentsWithURL:error:@
precomputedLightingEnvironmentContentsWithURL_errorSelector :: Selector
precomputedLightingEnvironmentContentsWithURL_errorSelector = mkSelector "precomputedLightingEnvironmentContentsWithURL:error:"

-- | @Selector@ for @precomputedLightingEnvironmentContentsWithData:error:@
precomputedLightingEnvironmentContentsWithData_errorSelector :: Selector
precomputedLightingEnvironmentContentsWithData_errorSelector = mkSelector "precomputedLightingEnvironmentContentsWithData:error:"

-- | @Selector@ for @precomputedLightingEnvironmentDataForContents:device:error:@
precomputedLightingEnvironmentDataForContents_device_errorSelector :: Selector
precomputedLightingEnvironmentDataForContents_device_errorSelector = mkSelector "precomputedLightingEnvironmentDataForContents:device:error:"

-- | @Selector@ for @contents@
contentsSelector :: Selector
contentsSelector = mkSelector "contents"

-- | @Selector@ for @setContents:@
setContentsSelector :: Selector
setContentsSelector = mkSelector "setContents:"

-- | @Selector@ for @intensity@
intensitySelector :: Selector
intensitySelector = mkSelector "intensity"

-- | @Selector@ for @setIntensity:@
setIntensitySelector :: Selector
setIntensitySelector = mkSelector "setIntensity:"

-- | @Selector@ for @minificationFilter@
minificationFilterSelector :: Selector
minificationFilterSelector = mkSelector "minificationFilter"

-- | @Selector@ for @setMinificationFilter:@
setMinificationFilterSelector :: Selector
setMinificationFilterSelector = mkSelector "setMinificationFilter:"

-- | @Selector@ for @magnificationFilter@
magnificationFilterSelector :: Selector
magnificationFilterSelector = mkSelector "magnificationFilter"

-- | @Selector@ for @setMagnificationFilter:@
setMagnificationFilterSelector :: Selector
setMagnificationFilterSelector = mkSelector "setMagnificationFilter:"

-- | @Selector@ for @mipFilter@
mipFilterSelector :: Selector
mipFilterSelector = mkSelector "mipFilter"

-- | @Selector@ for @setMipFilter:@
setMipFilterSelector :: Selector
setMipFilterSelector = mkSelector "setMipFilter:"

-- | @Selector@ for @contentsTransform@
contentsTransformSelector :: Selector
contentsTransformSelector = mkSelector "contentsTransform"

-- | @Selector@ for @setContentsTransform:@
setContentsTransformSelector :: Selector
setContentsTransformSelector = mkSelector "setContentsTransform:"

-- | @Selector@ for @wrapS@
wrapSSelector :: Selector
wrapSSelector = mkSelector "wrapS"

-- | @Selector@ for @setWrapS:@
setWrapSSelector :: Selector
setWrapSSelector = mkSelector "setWrapS:"

-- | @Selector@ for @wrapT@
wrapTSelector :: Selector
wrapTSelector = mkSelector "wrapT"

-- | @Selector@ for @setWrapT:@
setWrapTSelector :: Selector
setWrapTSelector = mkSelector "setWrapT:"

-- | @Selector@ for @mappingChannel@
mappingChannelSelector :: Selector
mappingChannelSelector = mkSelector "mappingChannel"

-- | @Selector@ for @setMappingChannel:@
setMappingChannelSelector :: Selector
setMappingChannelSelector = mkSelector "setMappingChannel:"

-- | @Selector@ for @textureComponents@
textureComponentsSelector :: Selector
textureComponentsSelector = mkSelector "textureComponents"

-- | @Selector@ for @setTextureComponents:@
setTextureComponentsSelector :: Selector
setTextureComponentsSelector = mkSelector "setTextureComponents:"

-- | @Selector@ for @maxAnisotropy@
maxAnisotropySelector :: Selector
maxAnisotropySelector = mkSelector "maxAnisotropy"

-- | @Selector@ for @setMaxAnisotropy:@
setMaxAnisotropySelector :: Selector
setMaxAnisotropySelector = mkSelector "setMaxAnisotropy:"

-- | @Selector@ for @borderColor@
borderColorSelector :: Selector
borderColorSelector = mkSelector "borderColor"

-- | @Selector@ for @setBorderColor:@
setBorderColorSelector :: Selector
setBorderColorSelector = mkSelector "setBorderColor:"

