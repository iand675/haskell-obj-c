{-# LANGUAGE DataKinds #-}
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
  , physicsWorld
  , background
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
  , particleSystems
  , addParticleSystem_withTransformSelector
  , attributeForKeySelector
  , backgroundSelector
  , fogColorSelector
  , fogDensityExponentSelector
  , fogEndDistanceSelector
  , fogStartDistanceSelector
  , lightingEnvironmentSelector
  , particleSystemsSelector
  , pausedSelector
  , physicsWorldSelector
  , removeAllParticleSystemsSelector
  , removeParticleSystemSelector
  , rootNodeSelector
  , sceneNamedSelector
  , sceneNamed_inDirectory_optionsSelector
  , sceneSelector
  , sceneWithURL_options_errorSelector
  , screenSpaceReflectionMaximumDistanceSelector
  , screenSpaceReflectionSampleCountSelector
  , screenSpaceReflectionStrideSelector
  , setAttribute_forKeySelector
  , setFogColorSelector
  , setFogDensityExponentSelector
  , setFogEndDistanceSelector
  , setFogStartDistanceSelector
  , setPausedSelector
  , setScreenSpaceReflectionMaximumDistanceSelector
  , setScreenSpaceReflectionSampleCountSelector
  , setScreenSpaceReflectionStrideSelector
  , setWantsScreenSpaceReflectionSelector
  , wantsScreenSpaceReflectionSelector
  , writeToURL_options_delegate_progressHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' sceneSelector

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
attributeForKey scnScene key =
  sendMessage scnScene attributeForKeySelector (toNSString key)

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
setAttribute_forKey scnScene attribute key =
  sendMessage scnScene setAttribute_forKeySelector attribute (toNSString key)

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
    sendClassMessage cls' sceneNamedSelector (toNSString name)

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
    sendClassMessage cls' sceneNamed_inDirectory_optionsSelector (toNSString name) (toNSString directory) (toNSDictionary options)

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
    sendClassMessage cls' sceneWithURL_options_errorSelector (toNSURL url) (toNSDictionary options) (toNSError error_)

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
writeToURL_options_delegate_progressHandler scnScene url options delegate progressHandler =
  sendMessage scnScene writeToURL_options_delegate_progressHandlerSelector (toNSURL url) (toNSDictionary options) delegate progressHandler

-- | @- addParticleSystem:withTransform:@
addParticleSystem_withTransform :: (IsSCNScene scnScene, IsSCNParticleSystem system) => scnScene -> system -> SCNMatrix4 -> IO ()
addParticleSystem_withTransform scnScene system transform =
  sendMessage scnScene addParticleSystem_withTransformSelector (toSCNParticleSystem system) transform

-- | @- removeAllParticleSystems@
removeAllParticleSystems :: IsSCNScene scnScene => scnScene -> IO ()
removeAllParticleSystems scnScene =
  sendMessage scnScene removeAllParticleSystemsSelector

-- | @- removeParticleSystem:@
removeParticleSystem :: (IsSCNScene scnScene, IsSCNParticleSystem system) => scnScene -> system -> IO ()
removeParticleSystem scnScene system =
  sendMessage scnScene removeParticleSystemSelector (toSCNParticleSystem system)

-- | root
--
-- Specifies the root node of the node hierarchy.
--
-- Note that we have only one root node, whereas some file formats might have many nodes at the root of their hierarchies. The root node(s) of the imported files will therefore be children of the SCNScene's root node.
--
-- ObjC selector: @- rootNode@
rootNode :: IsSCNScene scnScene => scnScene -> IO (Id SCNNode)
rootNode scnScene =
  sendMessage scnScene rootNodeSelector

-- | physicsWorld
--
-- Specifies the physics world of the receiver.
--
-- Every scene automatically creates a physics world object to simulate physics on nodes in the scene. You use this property to access the scene’s global physics properties, such as gravity. To add physics to a particular node, see physicsBody.
--
-- ObjC selector: @- physicsWorld@
physicsWorld :: IsSCNScene scnScene => scnScene -> IO (Id SCNPhysicsWorld)
physicsWorld scnScene =
  sendMessage scnScene physicsWorldSelector

-- | background
--
-- Specifies the background of the receiver.
--
-- The background is rendered before the rest of the scene.             The background can be rendered as a skybox by setting a cube map as described in SCNMaterialProperty.h             Colors are supported starting in macOS 10.12 and iOS 10. Prior to that you can use SCNView.backgroundColor.             MDLSkyCubeTexture is supported starting in macOS 10.13 and iOS 11.
--
-- ObjC selector: @- background@
background :: IsSCNScene scnScene => scnScene -> IO (Id SCNMaterialProperty)
background scnScene =
  sendMessage scnScene backgroundSelector

-- | lightingEnvironment
--
-- Specifies the receiver's environment for image-based lighting (IBL).
--
-- The environment can be              - a cube map (as described in SCNMaterialProperty.h)              - an instance of @MDLSkyCubeTexture@ (supported since macOS 10.13 and iOS 11)              - an object returned by @+[SCNMaterialProperty precomputedLightingEnvironmentContentsWithURL:error:]@ or @+[SCNMaterialProperty precomputedLightingEnvironmentContentsWithData:error:]@
--
-- ObjC selector: @- lightingEnvironment@
lightingEnvironment :: IsSCNScene scnScene => scnScene -> IO (Id SCNMaterialProperty)
lightingEnvironment scnScene =
  sendMessage scnScene lightingEnvironmentSelector

-- | fogStartDistance
--
-- Specifies the receiver's fog start distance. Animatable. Defaults to 0.
--
-- ObjC selector: @- fogStartDistance@
fogStartDistance :: IsSCNScene scnScene => scnScene -> IO CDouble
fogStartDistance scnScene =
  sendMessage scnScene fogStartDistanceSelector

-- | fogStartDistance
--
-- Specifies the receiver's fog start distance. Animatable. Defaults to 0.
--
-- ObjC selector: @- setFogStartDistance:@
setFogStartDistance :: IsSCNScene scnScene => scnScene -> CDouble -> IO ()
setFogStartDistance scnScene value =
  sendMessage scnScene setFogStartDistanceSelector value

-- | fogEndDistance
--
-- Specifies the receiver's fog end distance. Animatable. Defaults to 0.
--
-- ObjC selector: @- fogEndDistance@
fogEndDistance :: IsSCNScene scnScene => scnScene -> IO CDouble
fogEndDistance scnScene =
  sendMessage scnScene fogEndDistanceSelector

-- | fogEndDistance
--
-- Specifies the receiver's fog end distance. Animatable. Defaults to 0.
--
-- ObjC selector: @- setFogEndDistance:@
setFogEndDistance :: IsSCNScene scnScene => scnScene -> CDouble -> IO ()
setFogEndDistance scnScene value =
  sendMessage scnScene setFogEndDistanceSelector value

-- | fogDensityExponent
--
-- Specifies the receiver's fog power exponent. Animatable. Defaults to 1.
--
-- Controls the attenuation between the start and end fog distances. 0 means a constant fog, 1 a linear fog and 2 a quadratic fog, but any positive value will work.
--
-- ObjC selector: @- fogDensityExponent@
fogDensityExponent :: IsSCNScene scnScene => scnScene -> IO CDouble
fogDensityExponent scnScene =
  sendMessage scnScene fogDensityExponentSelector

-- | fogDensityExponent
--
-- Specifies the receiver's fog power exponent. Animatable. Defaults to 1.
--
-- Controls the attenuation between the start and end fog distances. 0 means a constant fog, 1 a linear fog and 2 a quadratic fog, but any positive value will work.
--
-- ObjC selector: @- setFogDensityExponent:@
setFogDensityExponent :: IsSCNScene scnScene => scnScene -> CDouble -> IO ()
setFogDensityExponent scnScene value =
  sendMessage scnScene setFogDensityExponentSelector value

-- | fogColor
--
-- Specifies the receiver's fog color (NSColor or CGColorRef). Animatable. Defaults to white.
--
-- The initial value is a NSColor.
--
-- ObjC selector: @- fogColor@
fogColor :: IsSCNScene scnScene => scnScene -> IO RawId
fogColor scnScene =
  sendMessage scnScene fogColorSelector

-- | fogColor
--
-- Specifies the receiver's fog color (NSColor or CGColorRef). Animatable. Defaults to white.
--
-- The initial value is a NSColor.
--
-- ObjC selector: @- setFogColor:@
setFogColor :: IsSCNScene scnScene => scnScene -> RawId -> IO ()
setFogColor scnScene value =
  sendMessage scnScene setFogColorSelector value

-- | wantsScreenSpaceReflection
--
-- Determines if the scene use screen space reflection.
--
-- Defaults to NO.
--
-- ObjC selector: @- wantsScreenSpaceReflection@
wantsScreenSpaceReflection :: IsSCNScene scnScene => scnScene -> IO Bool
wantsScreenSpaceReflection scnScene =
  sendMessage scnScene wantsScreenSpaceReflectionSelector

-- | wantsScreenSpaceReflection
--
-- Determines if the scene use screen space reflection.
--
-- Defaults to NO.
--
-- ObjC selector: @- setWantsScreenSpaceReflection:@
setWantsScreenSpaceReflection :: IsSCNScene scnScene => scnScene -> Bool -> IO ()
setWantsScreenSpaceReflection scnScene value =
  sendMessage scnScene setWantsScreenSpaceReflectionSelector value

-- | screenSpaceReflectionSampleCount
--
-- Determines the sample count of the screen space reflection.
--
-- Defaults to 64.
--
-- ObjC selector: @- screenSpaceReflectionSampleCount@
screenSpaceReflectionSampleCount :: IsSCNScene scnScene => scnScene -> IO CLong
screenSpaceReflectionSampleCount scnScene =
  sendMessage scnScene screenSpaceReflectionSampleCountSelector

-- | screenSpaceReflectionSampleCount
--
-- Determines the sample count of the screen space reflection.
--
-- Defaults to 64.
--
-- ObjC selector: @- setScreenSpaceReflectionSampleCount:@
setScreenSpaceReflectionSampleCount :: IsSCNScene scnScene => scnScene -> CLong -> IO ()
setScreenSpaceReflectionSampleCount scnScene value =
  sendMessage scnScene setScreenSpaceReflectionSampleCountSelector value

-- | screenSpaceReflectionMaximumDistance
--
-- Determines the maximum distance in world units.
--
-- Defaults to 1000.
--
-- ObjC selector: @- screenSpaceReflectionMaximumDistance@
screenSpaceReflectionMaximumDistance :: IsSCNScene scnScene => scnScene -> IO CDouble
screenSpaceReflectionMaximumDistance scnScene =
  sendMessage scnScene screenSpaceReflectionMaximumDistanceSelector

-- | screenSpaceReflectionMaximumDistance
--
-- Determines the maximum distance in world units.
--
-- Defaults to 1000.
--
-- ObjC selector: @- setScreenSpaceReflectionMaximumDistance:@
setScreenSpaceReflectionMaximumDistance :: IsSCNScene scnScene => scnScene -> CDouble -> IO ()
setScreenSpaceReflectionMaximumDistance scnScene value =
  sendMessage scnScene setScreenSpaceReflectionMaximumDistanceSelector value

-- | screenSpaceReflectionStride
--
-- Raytracing step size in pixel. The lower the better, the higher the faster.
--
-- Defaults to 8.
--
-- ObjC selector: @- screenSpaceReflectionStride@
screenSpaceReflectionStride :: IsSCNScene scnScene => scnScene -> IO CDouble
screenSpaceReflectionStride scnScene =
  sendMessage scnScene screenSpaceReflectionStrideSelector

-- | screenSpaceReflectionStride
--
-- Raytracing step size in pixel. The lower the better, the higher the faster.
--
-- Defaults to 8.
--
-- ObjC selector: @- setScreenSpaceReflectionStride:@
setScreenSpaceReflectionStride :: IsSCNScene scnScene => scnScene -> CDouble -> IO ()
setScreenSpaceReflectionStride scnScene value =
  sendMessage scnScene setScreenSpaceReflectionStrideSelector value

-- | paused
--
-- Controls whether or not the scene is paused. Defaults to NO.
--
-- Pausing a scene will pause animations, actions, particles and physics.
--
-- ObjC selector: @- paused@
paused :: IsSCNScene scnScene => scnScene -> IO Bool
paused scnScene =
  sendMessage scnScene pausedSelector

-- | paused
--
-- Controls whether or not the scene is paused. Defaults to NO.
--
-- Pausing a scene will pause animations, actions, particles and physics.
--
-- ObjC selector: @- setPaused:@
setPaused :: IsSCNScene scnScene => scnScene -> Bool -> IO ()
setPaused scnScene value =
  sendMessage scnScene setPausedSelector value

-- | @- particleSystems@
particleSystems :: IsSCNScene scnScene => scnScene -> IO (Id NSArray)
particleSystems scnScene =
  sendMessage scnScene particleSystemsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scene@
sceneSelector :: Selector '[] (Id SCNScene)
sceneSelector = mkSelector "scene"

-- | @Selector@ for @attributeForKey:@
attributeForKeySelector :: Selector '[Id NSString] RawId
attributeForKeySelector = mkSelector "attributeForKey:"

-- | @Selector@ for @setAttribute:forKey:@
setAttribute_forKeySelector :: Selector '[RawId, Id NSString] ()
setAttribute_forKeySelector = mkSelector "setAttribute:forKey:"

-- | @Selector@ for @sceneNamed:@
sceneNamedSelector :: Selector '[Id NSString] (Id SCNScene)
sceneNamedSelector = mkSelector "sceneNamed:"

-- | @Selector@ for @sceneNamed:inDirectory:options:@
sceneNamed_inDirectory_optionsSelector :: Selector '[Id NSString, Id NSString, Id NSDictionary] (Id SCNScene)
sceneNamed_inDirectory_optionsSelector = mkSelector "sceneNamed:inDirectory:options:"

-- | @Selector@ for @sceneWithURL:options:error:@
sceneWithURL_options_errorSelector :: Selector '[Id NSURL, Id NSDictionary, Id NSError] (Id SCNScene)
sceneWithURL_options_errorSelector = mkSelector "sceneWithURL:options:error:"

-- | @Selector@ for @writeToURL:options:delegate:progressHandler:@
writeToURL_options_delegate_progressHandlerSelector :: Selector '[Id NSURL, Id NSDictionary, RawId, Ptr ()] Bool
writeToURL_options_delegate_progressHandlerSelector = mkSelector "writeToURL:options:delegate:progressHandler:"

-- | @Selector@ for @addParticleSystem:withTransform:@
addParticleSystem_withTransformSelector :: Selector '[Id SCNParticleSystem, SCNMatrix4] ()
addParticleSystem_withTransformSelector = mkSelector "addParticleSystem:withTransform:"

-- | @Selector@ for @removeAllParticleSystems@
removeAllParticleSystemsSelector :: Selector '[] ()
removeAllParticleSystemsSelector = mkSelector "removeAllParticleSystems"

-- | @Selector@ for @removeParticleSystem:@
removeParticleSystemSelector :: Selector '[Id SCNParticleSystem] ()
removeParticleSystemSelector = mkSelector "removeParticleSystem:"

-- | @Selector@ for @rootNode@
rootNodeSelector :: Selector '[] (Id SCNNode)
rootNodeSelector = mkSelector "rootNode"

-- | @Selector@ for @physicsWorld@
physicsWorldSelector :: Selector '[] (Id SCNPhysicsWorld)
physicsWorldSelector = mkSelector "physicsWorld"

-- | @Selector@ for @background@
backgroundSelector :: Selector '[] (Id SCNMaterialProperty)
backgroundSelector = mkSelector "background"

-- | @Selector@ for @lightingEnvironment@
lightingEnvironmentSelector :: Selector '[] (Id SCNMaterialProperty)
lightingEnvironmentSelector = mkSelector "lightingEnvironment"

-- | @Selector@ for @fogStartDistance@
fogStartDistanceSelector :: Selector '[] CDouble
fogStartDistanceSelector = mkSelector "fogStartDistance"

-- | @Selector@ for @setFogStartDistance:@
setFogStartDistanceSelector :: Selector '[CDouble] ()
setFogStartDistanceSelector = mkSelector "setFogStartDistance:"

-- | @Selector@ for @fogEndDistance@
fogEndDistanceSelector :: Selector '[] CDouble
fogEndDistanceSelector = mkSelector "fogEndDistance"

-- | @Selector@ for @setFogEndDistance:@
setFogEndDistanceSelector :: Selector '[CDouble] ()
setFogEndDistanceSelector = mkSelector "setFogEndDistance:"

-- | @Selector@ for @fogDensityExponent@
fogDensityExponentSelector :: Selector '[] CDouble
fogDensityExponentSelector = mkSelector "fogDensityExponent"

-- | @Selector@ for @setFogDensityExponent:@
setFogDensityExponentSelector :: Selector '[CDouble] ()
setFogDensityExponentSelector = mkSelector "setFogDensityExponent:"

-- | @Selector@ for @fogColor@
fogColorSelector :: Selector '[] RawId
fogColorSelector = mkSelector "fogColor"

-- | @Selector@ for @setFogColor:@
setFogColorSelector :: Selector '[RawId] ()
setFogColorSelector = mkSelector "setFogColor:"

-- | @Selector@ for @wantsScreenSpaceReflection@
wantsScreenSpaceReflectionSelector :: Selector '[] Bool
wantsScreenSpaceReflectionSelector = mkSelector "wantsScreenSpaceReflection"

-- | @Selector@ for @setWantsScreenSpaceReflection:@
setWantsScreenSpaceReflectionSelector :: Selector '[Bool] ()
setWantsScreenSpaceReflectionSelector = mkSelector "setWantsScreenSpaceReflection:"

-- | @Selector@ for @screenSpaceReflectionSampleCount@
screenSpaceReflectionSampleCountSelector :: Selector '[] CLong
screenSpaceReflectionSampleCountSelector = mkSelector "screenSpaceReflectionSampleCount"

-- | @Selector@ for @setScreenSpaceReflectionSampleCount:@
setScreenSpaceReflectionSampleCountSelector :: Selector '[CLong] ()
setScreenSpaceReflectionSampleCountSelector = mkSelector "setScreenSpaceReflectionSampleCount:"

-- | @Selector@ for @screenSpaceReflectionMaximumDistance@
screenSpaceReflectionMaximumDistanceSelector :: Selector '[] CDouble
screenSpaceReflectionMaximumDistanceSelector = mkSelector "screenSpaceReflectionMaximumDistance"

-- | @Selector@ for @setScreenSpaceReflectionMaximumDistance:@
setScreenSpaceReflectionMaximumDistanceSelector :: Selector '[CDouble] ()
setScreenSpaceReflectionMaximumDistanceSelector = mkSelector "setScreenSpaceReflectionMaximumDistance:"

-- | @Selector@ for @screenSpaceReflectionStride@
screenSpaceReflectionStrideSelector :: Selector '[] CDouble
screenSpaceReflectionStrideSelector = mkSelector "screenSpaceReflectionStride"

-- | @Selector@ for @setScreenSpaceReflectionStride:@
setScreenSpaceReflectionStrideSelector :: Selector '[CDouble] ()
setScreenSpaceReflectionStrideSelector = mkSelector "setScreenSpaceReflectionStride:"

-- | @Selector@ for @paused@
pausedSelector :: Selector '[] Bool
pausedSelector = mkSelector "paused"

-- | @Selector@ for @setPaused:@
setPausedSelector :: Selector '[Bool] ()
setPausedSelector = mkSelector "setPaused:"

-- | @Selector@ for @particleSystems@
particleSystemsSelector :: Selector '[] (Id NSArray)
particleSystemsSelector = mkSelector "particleSystems"

