{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNScene
--
-- SCNScene is the class that describes a 3d scene. It encapsulates a node hierarchy.
--
-- Generated bindings for @SCNScene@.
module ObjC.SceneKit.SCNScene
  ( SCNScene
  , IsSCNScene(..)
  , scene
  , attributeForKey
  , setAttribute_forKey
  , sceneNamed
  , sceneNamed_inDirectory_options
  , sceneWithURL_options_error
  , writeToURL_options_delegate_progressHandler
  , addParticleSystem_withTransform
  , removeAllParticleSystems
  , removeParticleSystem
  , rootNode
  , lightingEnvironment
  , fogStartDistance
  , setFogStartDistance
  , fogEndDistance
  , setFogEndDistance
  , fogDensityExponent
  , setFogDensityExponent
  , fogColor
  , setFogColor
  , wantsScreenSpaceReflection
  , setWantsScreenSpaceReflection
  , screenSpaceReflectionSampleCount
  , setScreenSpaceReflectionSampleCount
  , screenSpaceReflectionMaximumDistance
  , setScreenSpaceReflectionMaximumDistance
  , screenSpaceReflectionStride
  , setScreenSpaceReflectionStride
  , paused
  , setPaused
  , sceneSelector
  , attributeForKeySelector
  , setAttribute_forKeySelector
  , sceneNamedSelector
  , sceneNamed_inDirectory_optionsSelector
  , sceneWithURL_options_errorSelector
  , writeToURL_options_delegate_progressHandlerSelector
  , addParticleSystem_withTransformSelector
  , removeAllParticleSystemsSelector
  , removeParticleSystemSelector
  , rootNodeSelector
  , lightingEnvironmentSelector
  , fogStartDistanceSelector
  , setFogStartDistanceSelector
  , fogEndDistanceSelector
  , setFogEndDistanceSelector
  , fogDensityExponentSelector
  , setFogDensityExponentSelector
  , fogColorSelector
  , setFogColorSelector
  , wantsScreenSpaceReflectionSelector
  , setWantsScreenSpaceReflectionSelector
  , screenSpaceReflectionSampleCountSelector
  , setScreenSpaceReflectionSampleCountSelector
  , screenSpaceReflectionMaximumDistanceSelector
  , setScreenSpaceReflectionMaximumDistanceSelector
  , screenSpaceReflectionStrideSelector
  , setScreenSpaceReflectionStrideSelector
  , pausedSelector
  , setPausedSelector


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

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ scene@
scene :: IO (Id SCNScene)
scene  =
  do
    cls' <- getRequiredClass "SCNScene"
    sendClassMsg cls' (mkSelector "scene") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | attributeForKey:
--
-- Retrieves a scene attribute.
--
-- The available keys are listed in the "Scene attributes" group.
--
-- @key@ — An NSString object that specifies the attribute to be read
--
-- ObjC selector: @- attributeForKey:@
attributeForKey :: (IsSCNScene scnScene, IsNSString key) => scnScene -> key -> IO RawId
attributeForKey scnScene  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg scnScene (mkSelector "attributeForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | setAttribute:forKey:
--
-- Sets a scene attribute
--
-- The available keys are listed in the "Scene attributes" group.
--
-- @attribute@ — An object that specifies the value of the attribute to be written.
--
-- @key@ — An NSString object that specifies the attribute to be written
--
-- ObjC selector: @- setAttribute:forKey:@
setAttribute_forKey :: (IsSCNScene scnScene, IsNSString key) => scnScene -> RawId -> key -> IO ()
setAttribute_forKey scnScene  attribute key =
withObjCPtr key $ \raw_key ->
    sendMsg scnScene (mkSelector "setAttribute:forKey:") retVoid [argPtr (castPtr (unRawId attribute) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | sceneNamed:
--
-- Creates and returns a scene associated with the specified filename.
--
-- @name@ — The name of the file. The method looks for a file with the specified name in the application’s main bundle.
--
-- This method initializes with no options and does not check for errors. The resulting object is not cached.
--
-- ObjC selector: @+ sceneNamed:@
sceneNamed :: IsNSString name => name -> IO (Id SCNScene)
sceneNamed name =
  do
    cls' <- getRequiredClass "SCNScene"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "sceneNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | sceneNamed:options:
--
-- Creates and returns a scene associated with the specified filename.
--
-- @name@ — The name of the file. The method looks for a file with the specified name in the application’s main bundle.
--
-- @directory@ — The name of the bundle sub-directory to search into.
--
-- @options@ — An options dictionary. The relevant keys are documented in the SCNSceneSource class.
--
-- This method initializes with no options and does not check for errors. The resulting object is not cached.
--
-- ObjC selector: @+ sceneNamed:inDirectory:options:@
sceneNamed_inDirectory_options :: (IsNSString name, IsNSString directory, IsNSDictionary options) => name -> directory -> options -> IO (Id SCNScene)
sceneNamed_inDirectory_options name directory options =
  do
    cls' <- getRequiredClass "SCNScene"
    withObjCPtr name $ \raw_name ->
      withObjCPtr directory $ \raw_directory ->
        withObjCPtr options $ \raw_options ->
          sendClassMsg cls' (mkSelector "sceneNamed:inDirectory:options:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_directory :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | sceneWithURL:options:error:
--
-- Creates and returns a scene from the specified URL.
--
-- @url@ — The URL to the 3D file.
--
-- @options@ — An options dictionary. The relevant keys are documented in the SCNSceneSource class.
--
-- @error@ — A NSError object passed by reference to get more information about the error when a nil is returned.
--
-- This method is here for convenience. It is equivalent to initializing a SCNSceneSource with the specified url and options, and asking it for its scene with the same options.
--
-- ObjC selector: @+ sceneWithURL:options:error:@
sceneWithURL_options_error :: (IsNSURL url, IsNSDictionary options, IsNSError error_) => url -> options -> error_ -> IO (Id SCNScene)
sceneWithURL_options_error url options error_ =
  do
    cls' <- getRequiredClass "SCNScene"
    withObjCPtr url $ \raw_url ->
      withObjCPtr options $ \raw_options ->
        withObjCPtr error_ $ \raw_error_ ->
          sendClassMsg cls' (mkSelector "sceneWithURL:options:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | writeToURL:options:delegate:progressHandler:
--
-- write the scene to the specified url.
--
-- @url@ — the destination url to write the scene to.
--
-- @options@ — A dictionary of options. The valid keys are described in the "Scene writing options" section.
--
-- @delegate@ — an optional delegate to manage external references such as images.
--
-- @progressHandler@ — an optional block to handle the progress of the operation.
--
-- Returns: Returns YES if the operation succeeded, NO otherwise. Errors checking can be done via the "error" parameter of the 'progressHandler'.
--
-- macOS 10.10 and lower only supports exporting to .dae files.             Starting macOS 10.11 exporting supports .dae, .scn as well as file all formats supported by Model I/O.             Starting iOS 10 exporting supports .scn as well as all file formats supported by Model I/O.
--
-- ObjC selector: @- writeToURL:options:delegate:progressHandler:@
writeToURL_options_delegate_progressHandler :: (IsSCNScene scnScene, IsNSURL url, IsNSDictionary options) => scnScene -> url -> options -> RawId -> Ptr () -> IO Bool
writeToURL_options_delegate_progressHandler scnScene  url options delegate progressHandler =
withObjCPtr url $ \raw_url ->
  withObjCPtr options $ \raw_options ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnScene (mkSelector "writeToURL:options:delegate:progressHandler:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr progressHandler :: Ptr ())]

-- | @- addParticleSystem:withTransform:@
addParticleSystem_withTransform :: (IsSCNScene scnScene, IsSCNParticleSystem system) => scnScene -> system -> SCNMatrix4 -> IO ()
addParticleSystem_withTransform scnScene  system transform =
withObjCPtr system $ \raw_system ->
    sendMsg scnScene (mkSelector "addParticleSystem:withTransform:") retVoid [argPtr (castPtr raw_system :: Ptr ()), argSCNMatrix4 transform]

-- | @- removeAllParticleSystems@
removeAllParticleSystems :: IsSCNScene scnScene => scnScene -> IO ()
removeAllParticleSystems scnScene  =
  sendMsg scnScene (mkSelector "removeAllParticleSystems") retVoid []

-- | @- removeParticleSystem:@
removeParticleSystem :: (IsSCNScene scnScene, IsSCNParticleSystem system) => scnScene -> system -> IO ()
removeParticleSystem scnScene  system =
withObjCPtr system $ \raw_system ->
    sendMsg scnScene (mkSelector "removeParticleSystem:") retVoid [argPtr (castPtr raw_system :: Ptr ())]

-- | root
--
-- Specifies the root node of the node hierarchy.
--
-- Note that we have only one root node, whereas some file formats might have many nodes at the root of their hierarchies. The root node(s) of the imported files will therefore be children of the SCNScene's root node.
--
-- ObjC selector: @- rootNode@
rootNode :: IsSCNScene scnScene => scnScene -> IO (Id SCNNode)
rootNode scnScene  =
  sendMsg scnScene (mkSelector "rootNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | lightingEnvironment
--
-- Specifies the receiver's environment for image-based lighting (IBL).
--
-- The environment can be              - a cube map (as described in SCNMaterialProperty.h)              - an instance of @MDLSkyCubeTexture@ (supported since macOS 10.13 and iOS 11)              - an object returned by @+[SCNMaterialProperty precomputedLightingEnvironmentContentsWithURL:error:]@ or @+[SCNMaterialProperty precomputedLightingEnvironmentContentsWithData:error:]@
--
-- ObjC selector: @- lightingEnvironment@
lightingEnvironment :: IsSCNScene scnScene => scnScene -> IO (Id SCNMaterialProperty)
lightingEnvironment scnScene  =
  sendMsg scnScene (mkSelector "lightingEnvironment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | fogStartDistance
--
-- Specifies the receiver's fog start distance. Animatable. Defaults to 0.
--
-- ObjC selector: @- fogStartDistance@
fogStartDistance :: IsSCNScene scnScene => scnScene -> IO CDouble
fogStartDistance scnScene  =
  sendMsg scnScene (mkSelector "fogStartDistance") retCDouble []

-- | fogStartDistance
--
-- Specifies the receiver's fog start distance. Animatable. Defaults to 0.
--
-- ObjC selector: @- setFogStartDistance:@
setFogStartDistance :: IsSCNScene scnScene => scnScene -> CDouble -> IO ()
setFogStartDistance scnScene  value =
  sendMsg scnScene (mkSelector "setFogStartDistance:") retVoid [argCDouble (fromIntegral value)]

-- | fogEndDistance
--
-- Specifies the receiver's fog end distance. Animatable. Defaults to 0.
--
-- ObjC selector: @- fogEndDistance@
fogEndDistance :: IsSCNScene scnScene => scnScene -> IO CDouble
fogEndDistance scnScene  =
  sendMsg scnScene (mkSelector "fogEndDistance") retCDouble []

-- | fogEndDistance
--
-- Specifies the receiver's fog end distance. Animatable. Defaults to 0.
--
-- ObjC selector: @- setFogEndDistance:@
setFogEndDistance :: IsSCNScene scnScene => scnScene -> CDouble -> IO ()
setFogEndDistance scnScene  value =
  sendMsg scnScene (mkSelector "setFogEndDistance:") retVoid [argCDouble (fromIntegral value)]

-- | fogDensityExponent
--
-- Specifies the receiver's fog power exponent. Animatable. Defaults to 1.
--
-- Controls the attenuation between the start and end fog distances. 0 means a constant fog, 1 a linear fog and 2 a quadratic fog, but any positive value will work.
--
-- ObjC selector: @- fogDensityExponent@
fogDensityExponent :: IsSCNScene scnScene => scnScene -> IO CDouble
fogDensityExponent scnScene  =
  sendMsg scnScene (mkSelector "fogDensityExponent") retCDouble []

-- | fogDensityExponent
--
-- Specifies the receiver's fog power exponent. Animatable. Defaults to 1.
--
-- Controls the attenuation between the start and end fog distances. 0 means a constant fog, 1 a linear fog and 2 a quadratic fog, but any positive value will work.
--
-- ObjC selector: @- setFogDensityExponent:@
setFogDensityExponent :: IsSCNScene scnScene => scnScene -> CDouble -> IO ()
setFogDensityExponent scnScene  value =
  sendMsg scnScene (mkSelector "setFogDensityExponent:") retVoid [argCDouble (fromIntegral value)]

-- | fogColor
--
-- Specifies the receiver's fog color (NSColor or CGColorRef). Animatable. Defaults to white.
--
-- The initial value is a NSColor.
--
-- ObjC selector: @- fogColor@
fogColor :: IsSCNScene scnScene => scnScene -> IO RawId
fogColor scnScene  =
  fmap (RawId . castPtr) $ sendMsg scnScene (mkSelector "fogColor") (retPtr retVoid) []

-- | fogColor
--
-- Specifies the receiver's fog color (NSColor or CGColorRef). Animatable. Defaults to white.
--
-- The initial value is a NSColor.
--
-- ObjC selector: @- setFogColor:@
setFogColor :: IsSCNScene scnScene => scnScene -> RawId -> IO ()
setFogColor scnScene  value =
  sendMsg scnScene (mkSelector "setFogColor:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | wantsScreenSpaceReflection
--
-- Determines if the scene use screen space reflection.
--
-- Defaults to NO.
--
-- ObjC selector: @- wantsScreenSpaceReflection@
wantsScreenSpaceReflection :: IsSCNScene scnScene => scnScene -> IO Bool
wantsScreenSpaceReflection scnScene  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnScene (mkSelector "wantsScreenSpaceReflection") retCULong []

-- | wantsScreenSpaceReflection
--
-- Determines if the scene use screen space reflection.
--
-- Defaults to NO.
--
-- ObjC selector: @- setWantsScreenSpaceReflection:@
setWantsScreenSpaceReflection :: IsSCNScene scnScene => scnScene -> Bool -> IO ()
setWantsScreenSpaceReflection scnScene  value =
  sendMsg scnScene (mkSelector "setWantsScreenSpaceReflection:") retVoid [argCULong (if value then 1 else 0)]

-- | screenSpaceReflectionSampleCount
--
-- Determines the sample count of the screen space reflection.
--
-- Defaults to 64.
--
-- ObjC selector: @- screenSpaceReflectionSampleCount@
screenSpaceReflectionSampleCount :: IsSCNScene scnScene => scnScene -> IO CLong
screenSpaceReflectionSampleCount scnScene  =
  sendMsg scnScene (mkSelector "screenSpaceReflectionSampleCount") retCLong []

-- | screenSpaceReflectionSampleCount
--
-- Determines the sample count of the screen space reflection.
--
-- Defaults to 64.
--
-- ObjC selector: @- setScreenSpaceReflectionSampleCount:@
setScreenSpaceReflectionSampleCount :: IsSCNScene scnScene => scnScene -> CLong -> IO ()
setScreenSpaceReflectionSampleCount scnScene  value =
  sendMsg scnScene (mkSelector "setScreenSpaceReflectionSampleCount:") retVoid [argCLong (fromIntegral value)]

-- | screenSpaceReflectionMaximumDistance
--
-- Determines the maximum distance in world units.
--
-- Defaults to 1000.
--
-- ObjC selector: @- screenSpaceReflectionMaximumDistance@
screenSpaceReflectionMaximumDistance :: IsSCNScene scnScene => scnScene -> IO CDouble
screenSpaceReflectionMaximumDistance scnScene  =
  sendMsg scnScene (mkSelector "screenSpaceReflectionMaximumDistance") retCDouble []

-- | screenSpaceReflectionMaximumDistance
--
-- Determines the maximum distance in world units.
--
-- Defaults to 1000.
--
-- ObjC selector: @- setScreenSpaceReflectionMaximumDistance:@
setScreenSpaceReflectionMaximumDistance :: IsSCNScene scnScene => scnScene -> CDouble -> IO ()
setScreenSpaceReflectionMaximumDistance scnScene  value =
  sendMsg scnScene (mkSelector "setScreenSpaceReflectionMaximumDistance:") retVoid [argCDouble (fromIntegral value)]

-- | screenSpaceReflectionStride
--
-- Raytracing step size in pixel. The lower the better, the higher the faster.
--
-- Defaults to 8.
--
-- ObjC selector: @- screenSpaceReflectionStride@
screenSpaceReflectionStride :: IsSCNScene scnScene => scnScene -> IO CDouble
screenSpaceReflectionStride scnScene  =
  sendMsg scnScene (mkSelector "screenSpaceReflectionStride") retCDouble []

-- | screenSpaceReflectionStride
--
-- Raytracing step size in pixel. The lower the better, the higher the faster.
--
-- Defaults to 8.
--
-- ObjC selector: @- setScreenSpaceReflectionStride:@
setScreenSpaceReflectionStride :: IsSCNScene scnScene => scnScene -> CDouble -> IO ()
setScreenSpaceReflectionStride scnScene  value =
  sendMsg scnScene (mkSelector "setScreenSpaceReflectionStride:") retVoid [argCDouble (fromIntegral value)]

-- | paused
--
-- Controls whether or not the scene is paused. Defaults to NO.
--
-- Pausing a scene will pause animations, actions, particles and physics.
--
-- ObjC selector: @- paused@
paused :: IsSCNScene scnScene => scnScene -> IO Bool
paused scnScene  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnScene (mkSelector "paused") retCULong []

-- | paused
--
-- Controls whether or not the scene is paused. Defaults to NO.
--
-- Pausing a scene will pause animations, actions, particles and physics.
--
-- ObjC selector: @- setPaused:@
setPaused :: IsSCNScene scnScene => scnScene -> Bool -> IO ()
setPaused scnScene  value =
  sendMsg scnScene (mkSelector "setPaused:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scene@
sceneSelector :: Selector
sceneSelector = mkSelector "scene"

-- | @Selector@ for @attributeForKey:@
attributeForKeySelector :: Selector
attributeForKeySelector = mkSelector "attributeForKey:"

-- | @Selector@ for @setAttribute:forKey:@
setAttribute_forKeySelector :: Selector
setAttribute_forKeySelector = mkSelector "setAttribute:forKey:"

-- | @Selector@ for @sceneNamed:@
sceneNamedSelector :: Selector
sceneNamedSelector = mkSelector "sceneNamed:"

-- | @Selector@ for @sceneNamed:inDirectory:options:@
sceneNamed_inDirectory_optionsSelector :: Selector
sceneNamed_inDirectory_optionsSelector = mkSelector "sceneNamed:inDirectory:options:"

-- | @Selector@ for @sceneWithURL:options:error:@
sceneWithURL_options_errorSelector :: Selector
sceneWithURL_options_errorSelector = mkSelector "sceneWithURL:options:error:"

-- | @Selector@ for @writeToURL:options:delegate:progressHandler:@
writeToURL_options_delegate_progressHandlerSelector :: Selector
writeToURL_options_delegate_progressHandlerSelector = mkSelector "writeToURL:options:delegate:progressHandler:"

-- | @Selector@ for @addParticleSystem:withTransform:@
addParticleSystem_withTransformSelector :: Selector
addParticleSystem_withTransformSelector = mkSelector "addParticleSystem:withTransform:"

-- | @Selector@ for @removeAllParticleSystems@
removeAllParticleSystemsSelector :: Selector
removeAllParticleSystemsSelector = mkSelector "removeAllParticleSystems"

-- | @Selector@ for @removeParticleSystem:@
removeParticleSystemSelector :: Selector
removeParticleSystemSelector = mkSelector "removeParticleSystem:"

-- | @Selector@ for @rootNode@
rootNodeSelector :: Selector
rootNodeSelector = mkSelector "rootNode"

-- | @Selector@ for @lightingEnvironment@
lightingEnvironmentSelector :: Selector
lightingEnvironmentSelector = mkSelector "lightingEnvironment"

-- | @Selector@ for @fogStartDistance@
fogStartDistanceSelector :: Selector
fogStartDistanceSelector = mkSelector "fogStartDistance"

-- | @Selector@ for @setFogStartDistance:@
setFogStartDistanceSelector :: Selector
setFogStartDistanceSelector = mkSelector "setFogStartDistance:"

-- | @Selector@ for @fogEndDistance@
fogEndDistanceSelector :: Selector
fogEndDistanceSelector = mkSelector "fogEndDistance"

-- | @Selector@ for @setFogEndDistance:@
setFogEndDistanceSelector :: Selector
setFogEndDistanceSelector = mkSelector "setFogEndDistance:"

-- | @Selector@ for @fogDensityExponent@
fogDensityExponentSelector :: Selector
fogDensityExponentSelector = mkSelector "fogDensityExponent"

-- | @Selector@ for @setFogDensityExponent:@
setFogDensityExponentSelector :: Selector
setFogDensityExponentSelector = mkSelector "setFogDensityExponent:"

-- | @Selector@ for @fogColor@
fogColorSelector :: Selector
fogColorSelector = mkSelector "fogColor"

-- | @Selector@ for @setFogColor:@
setFogColorSelector :: Selector
setFogColorSelector = mkSelector "setFogColor:"

-- | @Selector@ for @wantsScreenSpaceReflection@
wantsScreenSpaceReflectionSelector :: Selector
wantsScreenSpaceReflectionSelector = mkSelector "wantsScreenSpaceReflection"

-- | @Selector@ for @setWantsScreenSpaceReflection:@
setWantsScreenSpaceReflectionSelector :: Selector
setWantsScreenSpaceReflectionSelector = mkSelector "setWantsScreenSpaceReflection:"

-- | @Selector@ for @screenSpaceReflectionSampleCount@
screenSpaceReflectionSampleCountSelector :: Selector
screenSpaceReflectionSampleCountSelector = mkSelector "screenSpaceReflectionSampleCount"

-- | @Selector@ for @setScreenSpaceReflectionSampleCount:@
setScreenSpaceReflectionSampleCountSelector :: Selector
setScreenSpaceReflectionSampleCountSelector = mkSelector "setScreenSpaceReflectionSampleCount:"

-- | @Selector@ for @screenSpaceReflectionMaximumDistance@
screenSpaceReflectionMaximumDistanceSelector :: Selector
screenSpaceReflectionMaximumDistanceSelector = mkSelector "screenSpaceReflectionMaximumDistance"

-- | @Selector@ for @setScreenSpaceReflectionMaximumDistance:@
setScreenSpaceReflectionMaximumDistanceSelector :: Selector
setScreenSpaceReflectionMaximumDistanceSelector = mkSelector "setScreenSpaceReflectionMaximumDistance:"

-- | @Selector@ for @screenSpaceReflectionStride@
screenSpaceReflectionStrideSelector :: Selector
screenSpaceReflectionStrideSelector = mkSelector "screenSpaceReflectionStride"

-- | @Selector@ for @setScreenSpaceReflectionStride:@
setScreenSpaceReflectionStrideSelector :: Selector
setScreenSpaceReflectionStrideSelector = mkSelector "setScreenSpaceReflectionStride:"

-- | @Selector@ for @paused@
pausedSelector :: Selector
pausedSelector = mkSelector "paused"

-- | @Selector@ for @setPaused:@
setPausedSelector :: Selector
setPausedSelector = mkSelector "setPaused:"

