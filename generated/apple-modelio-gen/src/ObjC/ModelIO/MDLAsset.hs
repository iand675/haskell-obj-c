{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MDLAsset
--
-- An MDLAsset represents the contents of a model file.
--
-- Each asset contains a collection of hierarchies of objects, where              each object in the asset is the top level of a hierarchy. Objects             include transforms, lights, cameras, and meshes.
--
-- MDLAssets are typically instantiated from NSURLs that refer to a model resource.
--
-- The model resource may represented timed information, for example, a series of  mesh morphs. If the asset is timed, then the framerate will be non-zero, and the  firstFrame and lastFrame properties will indicate the range for which sample  data exists. Samples before or after that range will be clamped. Some model  resource representations allow continuous sampling, others are discrete. In the  discrete case, if a requested sample time is not on a discrete boundary the  returned sample will be the sample exactly on the sample time, or if no such is  available, the immediately preceding sample. If no time is specified for a  sample, the first data will be returned.
--
-- An asset's bounding box can be queried without traversing the hierarchy of  objects.
--
-- Fast enumeration of an MDLAsset iterates the top level objects contained within.
--
-- Generated bindings for @MDLAsset@.
module ObjC.ModelIO.MDLAsset
  ( MDLAsset
  , IsMDLAsset(..)
  , initWithURL
  , initWithURL_vertexDescriptor_bufferAllocator
  , initWithBufferAllocator
  , initWithURL_vertexDescriptor_bufferAllocator_preserveTopology_error
  , exportAssetToURL
  , exportAssetToURL_error
  , objectAtPath
  , canImportFileExtension
  , canExportFileExtension
  , childObjectsOfClass
  , loadTextures
  , addObject
  , removeObject
  , objectAtIndexedSubscript
  , objectAtIndex
  , placeLightProbesWithDensity_heuristic_usingIrradianceDataSource
  , frameInterval
  , setFrameInterval
  , startTime
  , setStartTime
  , endTime
  , setEndTime
  , url
  , resolver
  , setResolver
  , bufferAllocator
  , vertexDescriptor
  , count
  , masters
  , setMasters
  , originals
  , setOriginals
  , animations
  , setAnimations
  , initWithURLSelector
  , initWithURL_vertexDescriptor_bufferAllocatorSelector
  , initWithBufferAllocatorSelector
  , initWithURL_vertexDescriptor_bufferAllocator_preserveTopology_errorSelector
  , exportAssetToURLSelector
  , exportAssetToURL_errorSelector
  , objectAtPathSelector
  , canImportFileExtensionSelector
  , canExportFileExtensionSelector
  , childObjectsOfClassSelector
  , loadTexturesSelector
  , addObjectSelector
  , removeObjectSelector
  , objectAtIndexedSubscriptSelector
  , objectAtIndexSelector
  , placeLightProbesWithDensity_heuristic_usingIrradianceDataSourceSelector
  , frameIntervalSelector
  , setFrameIntervalSelector
  , startTimeSelector
  , setStartTimeSelector
  , endTimeSelector
  , setEndTimeSelector
  , urlSelector
  , resolverSelector
  , setResolverSelector
  , bufferAllocatorSelector
  , vertexDescriptorSelector
  , countSelector
  , mastersSelector
  , setMastersSelector
  , originalsSelector
  , setOriginalsSelector
  , animationsSelector
  , setAnimationsSelector

  -- * Enum types
  , MDLProbePlacement(MDLProbePlacement)
  , pattern MDLProbePlacementUniformGrid
  , pattern MDLProbePlacementIrradianceDistribution

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

-- | initWithURL:
--
-- Initialize an MDLAsset using the contents of the resource located at            the indicated URL
--
-- Vertex layout (i.e. vertexDescriptor) will be specified by ModelIO              depending on attributes of the resource.  Buffers will be allocated              using a default NSData based allocator
--
-- Submeshes will be converted to triangle topology.
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsMDLAsset mdlAsset, IsNSURL url) => mdlAsset -> url -> IO (Id MDLAsset)
initWithURL mdlAsset  url =
  withObjCPtr url $ \raw_url ->
      sendMsg mdlAsset (mkSelector "initWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | initWithURL:vertexDescriptor:bufferAllocator:
--
-- Initialize an MDLAsset using the contents of the resource located at            URL, ensuring that the asset conforms to the supplied vertexDescriptor,            and buffers are allocated in the supplied allocator
--
-- The default behavior is to triangulate any discovered meshes and to             conform the mesh to the supplied vertexDescriptor.
--
-- If nil is passed as the vertexDescriptor, then a vertexDescriptor             will be created according to the attributes of the resource.
--
-- If nil is passed as the bufferAllocator, buffers will be allocated             using a default NSData based allocator.
--
-- Submeshes will be converted to triangle topology.
--
-- ObjC selector: @- initWithURL:vertexDescriptor:bufferAllocator:@
initWithURL_vertexDescriptor_bufferAllocator :: (IsMDLAsset mdlAsset, IsNSURL url, IsMDLVertexDescriptor vertexDescriptor) => mdlAsset -> url -> vertexDescriptor -> RawId -> IO (Id MDLAsset)
initWithURL_vertexDescriptor_bufferAllocator mdlAsset  url vertexDescriptor bufferAllocator =
  withObjCPtr url $ \raw_url ->
    withObjCPtr vertexDescriptor $ \raw_vertexDescriptor ->
        sendMsg mdlAsset (mkSelector "initWithURL:vertexDescriptor:bufferAllocator:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_vertexDescriptor :: Ptr ()), argPtr (castPtr (unRawId bufferAllocator) :: Ptr ())] >>= ownedObject . castPtr

-- | initWithBufferAllocator:
--
-- Initialize an empty MDLAsset with a buffer allocator to be used during           other operations.
--
-- ObjC selector: @- initWithBufferAllocator:@
initWithBufferAllocator :: IsMDLAsset mdlAsset => mdlAsset -> RawId -> IO (Id MDLAsset)
initWithBufferAllocator mdlAsset  bufferAllocator =
    sendMsg mdlAsset (mkSelector "initWithBufferAllocator:") (retPtr retVoid) [argPtr (castPtr (unRawId bufferAllocator) :: Ptr ())] >>= ownedObject . castPtr

-- | initWithURL:vertexDescriptor:bufferAllocator:preserveTopology:error:
--
-- Same as initWithURL:vertexDescriptor:bufferAllocator: except that           if preserveTopology is YES, a topology buffer might be created on the           submeshes.
--
-- If all faces in a submesh have the same vertex count, then the              submesh will a geometry type corresponding to that vertex count.             For example, if all faces have four vertices, then the geometry             type will be MDLGeometryTypeQuads. If faces have a varying number             of vertices, then the the submesh type will be              MDLGeometryTypeVariableTopology, and a faceTopologyBuffer will be             created.
--
-- ObjC selector: @- initWithURL:vertexDescriptor:bufferAllocator:preserveTopology:error:@
initWithURL_vertexDescriptor_bufferAllocator_preserveTopology_error :: (IsMDLAsset mdlAsset, IsNSURL url, IsMDLVertexDescriptor vertexDescriptor, IsNSError error_) => mdlAsset -> url -> vertexDescriptor -> RawId -> Bool -> error_ -> IO (Id MDLAsset)
initWithURL_vertexDescriptor_bufferAllocator_preserveTopology_error mdlAsset  url vertexDescriptor bufferAllocator preserveTopology error_ =
  withObjCPtr url $ \raw_url ->
    withObjCPtr vertexDescriptor $ \raw_vertexDescriptor ->
      withObjCPtr error_ $ \raw_error_ ->
          sendMsg mdlAsset (mkSelector "initWithURL:vertexDescriptor:bufferAllocator:preserveTopology:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_vertexDescriptor :: Ptr ()), argPtr (castPtr (unRawId bufferAllocator) :: Ptr ()), argCULong (if preserveTopology then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | exportAssetToURL:
--
-- Export an asset to the specified URL.
--
-- Returns: YES is returned if exporting proceeded successfully,
--
-- ObjC selector: @- exportAssetToURL:@
exportAssetToURL :: (IsMDLAsset mdlAsset, IsNSURL url) => mdlAsset -> url -> IO Bool
exportAssetToURL mdlAsset  url =
  withObjCPtr url $ \raw_url ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg mdlAsset (mkSelector "exportAssetToURL:") retCULong [argPtr (castPtr raw_url :: Ptr ())]

-- | exportAssetToURL:error:
--
-- Export an asset to the specified URL.
--
-- Returns: YES is returned if exporting proceeded successfully,
--
-- ObjC selector: @- exportAssetToURL:error:@
exportAssetToURL_error :: (IsMDLAsset mdlAsset, IsNSURL url, IsNSError error_) => mdlAsset -> url -> error_ -> IO Bool
exportAssetToURL_error mdlAsset  url error_ =
  withObjCPtr url $ \raw_url ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg mdlAsset (mkSelector "exportAssetToURL:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Return the object at the specified path, or nil if none exists there
--
-- ObjC selector: @- objectAtPath:@
objectAtPath :: (IsMDLAsset mdlAsset, IsNSString path) => mdlAsset -> path -> IO (Id MDLObject)
objectAtPath mdlAsset  path =
  withObjCPtr path $ \raw_path ->
      sendMsg mdlAsset (mkSelector "objectAtPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | canImportFileExtension:
--
-- Indicates whether MDLAsset object can be initialized with resource           with the given extension
--
-- Returns: YES is returned if MDLAsset is able to load and represent assets with            the given extension
--
-- ObjC selector: @+ canImportFileExtension:@
canImportFileExtension :: IsNSString extension => extension -> IO Bool
canImportFileExtension extension =
  do
    cls' <- getRequiredClass "MDLAsset"
    withObjCPtr extension $ \raw_extension ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canImportFileExtension:") retCULong [argPtr (castPtr raw_extension :: Ptr ())]

-- | canImportFileExtension:
--
-- Indicates whether MDLAsset object can export asset to resource with           the given extension
--
-- Returns: YES is returned if MDLAsset is able is able to export assets to          resources with the given extension
--
-- ObjC selector: @+ canExportFileExtension:@
canExportFileExtension :: IsNSString extension => extension -> IO Bool
canExportFileExtension extension =
  do
    cls' <- getRequiredClass "MDLAsset"
    withObjCPtr extension $ \raw_extension ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canExportFileExtension:") retCULong [argPtr (castPtr raw_extension :: Ptr ())]

-- | childObjectsOfClass:
--
-- Inspects an asset's hierarchy for objects of the specified class type
--
-- Returns: returns an NSArray of all objects in the asset matching the requested class
--
-- This can be used to get references to all MDLMesh objects, MDLLights,             etc. if objectClass is not a subclass of MDLObject, an exception will be             raised.
--
-- ObjC selector: @- childObjectsOfClass:@
childObjectsOfClass :: IsMDLAsset mdlAsset => mdlAsset -> Class -> IO (Id NSArray)
childObjectsOfClass mdlAsset  objectClass =
    sendMsg mdlAsset (mkSelector "childObjectsOfClass:") (retPtr retVoid) [argPtr (unClass objectClass)] >>= retainedObject . castPtr

-- | loadTextures
--
-- Iterates over all material properties on all materials. If they are string           values or NSURL values, and can be resolved as textures, then the string            and NSURL values will be replaced by MDLTextureSampler values.
--
-- ObjC selector: @- loadTextures@
loadTextures :: IsMDLAsset mdlAsset => mdlAsset -> IO ()
loadTextures mdlAsset  =
    sendMsg mdlAsset (mkSelector "loadTextures") retVoid []

-- | addObject:
--
-- Add a top level object to an asset.
--
-- If the object was already in the asset, this has no effect.
--
-- ObjC selector: @- addObject:@
addObject :: (IsMDLAsset mdlAsset, IsMDLObject object) => mdlAsset -> object -> IO ()
addObject mdlAsset  object =
  withObjCPtr object $ \raw_object ->
      sendMsg mdlAsset (mkSelector "addObject:") retVoid [argPtr (castPtr raw_object :: Ptr ())]

-- | removeObject:
--
-- Remove a top level object from an asset.
--
-- If the object not in the asset, this has no effect.
--
-- ObjC selector: @- removeObject:@
removeObject :: (IsMDLAsset mdlAsset, IsMDLObject object) => mdlAsset -> object -> IO ()
removeObject mdlAsset  object =
  withObjCPtr object $ \raw_object ->
      sendMsg mdlAsset (mkSelector "removeObject:") retVoid [argPtr (castPtr raw_object :: Ptr ())]

-- | objectAtIndexedSubscript:
--
-- return the indexed top level object
--
-- ObjC selector: @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsMDLAsset mdlAsset => mdlAsset -> CULong -> IO (Id MDLObject)
objectAtIndexedSubscript mdlAsset  index =
    sendMsg mdlAsset (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong index] >>= retainedObject . castPtr

-- | objectAtIndex:
--
-- return the indexed top level object
--
-- ObjC selector: @- objectAtIndex:@
objectAtIndex :: IsMDLAsset mdlAsset => mdlAsset -> CULong -> IO (Id MDLObject)
objectAtIndex mdlAsset  index =
    sendMsg mdlAsset (mkSelector "objectAtIndex:") (retPtr retVoid) [argCULong index] >>= retainedObject . castPtr

-- | @+ placeLightProbesWithDensity:heuristic:usingIrradianceDataSource:@
placeLightProbesWithDensity_heuristic_usingIrradianceDataSource :: CFloat -> MDLProbePlacement -> RawId -> IO (Id NSArray)
placeLightProbesWithDensity_heuristic_usingIrradianceDataSource value type_ dataSource =
  do
    cls' <- getRequiredClass "MDLAsset"
    sendClassMsg cls' (mkSelector "placeLightProbesWithDensity:heuristic:usingIrradianceDataSource:") (retPtr retVoid) [argCFloat value, argCLong (coerce type_), argPtr (castPtr (unRawId dataSource) :: Ptr ())] >>= retainedObject . castPtr

-- | frameInterval
--
-- Inherent frame rate of an asset
--
-- If no framerate was specified by resource or resource uncapable of              specifying framerate, this value defaults to 0
--
-- ObjC selector: @- frameInterval@
frameInterval :: IsMDLAsset mdlAsset => mdlAsset -> IO CDouble
frameInterval mdlAsset  =
    sendMsg mdlAsset (mkSelector "frameInterval") retCDouble []

-- | frameInterval
--
-- Inherent frame rate of an asset
--
-- If no framerate was specified by resource or resource uncapable of              specifying framerate, this value defaults to 0
--
-- ObjC selector: @- setFrameInterval:@
setFrameInterval :: IsMDLAsset mdlAsset => mdlAsset -> CDouble -> IO ()
setFrameInterval mdlAsset  value =
    sendMsg mdlAsset (mkSelector "setFrameInterval:") retVoid [argCDouble value]

-- | startTime
--
-- Start time bracket of animation data
--
-- If no animation data was specified by resource or resource incapable              of specifying animation data, this value defaults to 0. If startTime             was set explicitly, then the value of startTime will be the lesser             of the set value and the animated values.
--
-- ObjC selector: @- startTime@
startTime :: IsMDLAsset mdlAsset => mdlAsset -> IO CDouble
startTime mdlAsset  =
    sendMsg mdlAsset (mkSelector "startTime") retCDouble []

-- | startTime
--
-- Start time bracket of animation data
--
-- If no animation data was specified by resource or resource incapable              of specifying animation data, this value defaults to 0. If startTime             was set explicitly, then the value of startTime will be the lesser             of the set value and the animated values.
--
-- ObjC selector: @- setStartTime:@
setStartTime :: IsMDLAsset mdlAsset => mdlAsset -> CDouble -> IO ()
setStartTime mdlAsset  value =
    sendMsg mdlAsset (mkSelector "setStartTime:") retVoid [argCDouble value]

-- | endTime
--
-- End time bracket of animation data
--
-- If no animation data was specified by resource or resource incapable             of specifying animation data, this value defaults to 0. If the             endTime was set explicitly, then the value of endTime will be the             greater of the set value and the animated values.
--
-- ObjC selector: @- endTime@
endTime :: IsMDLAsset mdlAsset => mdlAsset -> IO CDouble
endTime mdlAsset  =
    sendMsg mdlAsset (mkSelector "endTime") retCDouble []

-- | endTime
--
-- End time bracket of animation data
--
-- If no animation data was specified by resource or resource incapable             of specifying animation data, this value defaults to 0. If the             endTime was set explicitly, then the value of endTime will be the             greater of the set value and the animated values.
--
-- ObjC selector: @- setEndTime:@
setEndTime :: IsMDLAsset mdlAsset => mdlAsset -> CDouble -> IO ()
setEndTime mdlAsset  value =
    sendMsg mdlAsset (mkSelector "setEndTime:") retVoid [argCDouble value]

-- | URL
--
-- URL used to create the asset
--
-- If the asset was not created with a URL, nil will be returned.
--
-- ObjC selector: @- URL@
url :: IsMDLAsset mdlAsset => mdlAsset -> IO (Id NSURL)
url mdlAsset  =
    sendMsg mdlAsset (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | AssetResolver
--
-- Resolver asset that helps find associated files
--
-- The default asset resolver is the RelativeAssetResolver
--
-- ObjC selector: @- resolver@
resolver :: IsMDLAsset mdlAsset => mdlAsset -> IO RawId
resolver mdlAsset  =
    fmap (RawId . castPtr) $ sendMsg mdlAsset (mkSelector "resolver") (retPtr retVoid) []

-- | AssetResolver
--
-- Resolver asset that helps find associated files
--
-- The default asset resolver is the RelativeAssetResolver
--
-- ObjC selector: @- setResolver:@
setResolver :: IsMDLAsset mdlAsset => mdlAsset -> RawId -> IO ()
setResolver mdlAsset  value =
    sendMsg mdlAsset (mkSelector "setResolver:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | bufferAllocator
--
-- Allocator used to create vertex and index buffers
--
-- ObjC selector: @- bufferAllocator@
bufferAllocator :: IsMDLAsset mdlAsset => mdlAsset -> IO RawId
bufferAllocator mdlAsset  =
    fmap (RawId . castPtr) $ sendMsg mdlAsset (mkSelector "bufferAllocator") (retPtr retVoid) []

-- | vertexDescriptor
--
-- Vertex descriptor set upon asset initialization
--
-- Will be nil if there was no descriptor set
--
-- ObjC selector: @- vertexDescriptor@
vertexDescriptor :: IsMDLAsset mdlAsset => mdlAsset -> IO (Id MDLVertexDescriptor)
vertexDescriptor mdlAsset  =
    sendMsg mdlAsset (mkSelector "vertexDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | count
--
-- The number of top level objects
--
-- ObjC selector: @- count@
count :: IsMDLAsset mdlAsset => mdlAsset -> IO CULong
count mdlAsset  =
    sendMsg mdlAsset (mkSelector "count") retCULong []

-- | @- masters@
masters :: IsMDLAsset mdlAsset => mdlAsset -> IO RawId
masters mdlAsset  =
    fmap (RawId . castPtr) $ sendMsg mdlAsset (mkSelector "masters") (retPtr retVoid) []

-- | @- setMasters:@
setMasters :: IsMDLAsset mdlAsset => mdlAsset -> RawId -> IO ()
setMasters mdlAsset  value =
    sendMsg mdlAsset (mkSelector "setMasters:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | originals
--
-- Original objects that can be instanced into the asset's object hierarchy
--
-- See: MDLObjectContainerComponent
--
-- ObjC selector: @- originals@
originals :: IsMDLAsset mdlAsset => mdlAsset -> IO RawId
originals mdlAsset  =
    fmap (RawId . castPtr) $ sendMsg mdlAsset (mkSelector "originals") (retPtr retVoid) []

-- | originals
--
-- Original objects that can be instanced into the asset's object hierarchy
--
-- See: MDLObjectContainerComponent
--
-- ObjC selector: @- setOriginals:@
setOriginals :: IsMDLAsset mdlAsset => mdlAsset -> RawId -> IO ()
setOriginals mdlAsset  value =
    sendMsg mdlAsset (mkSelector "setOriginals:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | animations
--
-- Animations that can be bound to MDLObjects (
--
-- See: MDLAnimationBindComponent)
--
-- See: MDLObjectContainerComponent
--
-- ObjC selector: @- animations@
animations :: IsMDLAsset mdlAsset => mdlAsset -> IO RawId
animations mdlAsset  =
    fmap (RawId . castPtr) $ sendMsg mdlAsset (mkSelector "animations") (retPtr retVoid) []

-- | animations
--
-- Animations that can be bound to MDLObjects (
--
-- See: MDLAnimationBindComponent)
--
-- See: MDLObjectContainerComponent
--
-- ObjC selector: @- setAnimations:@
setAnimations :: IsMDLAsset mdlAsset => mdlAsset -> RawId -> IO ()
setAnimations mdlAsset  value =
    sendMsg mdlAsset (mkSelector "setAnimations:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @initWithURL:vertexDescriptor:bufferAllocator:@
initWithURL_vertexDescriptor_bufferAllocatorSelector :: Selector
initWithURL_vertexDescriptor_bufferAllocatorSelector = mkSelector "initWithURL:vertexDescriptor:bufferAllocator:"

-- | @Selector@ for @initWithBufferAllocator:@
initWithBufferAllocatorSelector :: Selector
initWithBufferAllocatorSelector = mkSelector "initWithBufferAllocator:"

-- | @Selector@ for @initWithURL:vertexDescriptor:bufferAllocator:preserveTopology:error:@
initWithURL_vertexDescriptor_bufferAllocator_preserveTopology_errorSelector :: Selector
initWithURL_vertexDescriptor_bufferAllocator_preserveTopology_errorSelector = mkSelector "initWithURL:vertexDescriptor:bufferAllocator:preserveTopology:error:"

-- | @Selector@ for @exportAssetToURL:@
exportAssetToURLSelector :: Selector
exportAssetToURLSelector = mkSelector "exportAssetToURL:"

-- | @Selector@ for @exportAssetToURL:error:@
exportAssetToURL_errorSelector :: Selector
exportAssetToURL_errorSelector = mkSelector "exportAssetToURL:error:"

-- | @Selector@ for @objectAtPath:@
objectAtPathSelector :: Selector
objectAtPathSelector = mkSelector "objectAtPath:"

-- | @Selector@ for @canImportFileExtension:@
canImportFileExtensionSelector :: Selector
canImportFileExtensionSelector = mkSelector "canImportFileExtension:"

-- | @Selector@ for @canExportFileExtension:@
canExportFileExtensionSelector :: Selector
canExportFileExtensionSelector = mkSelector "canExportFileExtension:"

-- | @Selector@ for @childObjectsOfClass:@
childObjectsOfClassSelector :: Selector
childObjectsOfClassSelector = mkSelector "childObjectsOfClass:"

-- | @Selector@ for @loadTextures@
loadTexturesSelector :: Selector
loadTexturesSelector = mkSelector "loadTextures"

-- | @Selector@ for @addObject:@
addObjectSelector :: Selector
addObjectSelector = mkSelector "addObject:"

-- | @Selector@ for @removeObject:@
removeObjectSelector :: Selector
removeObjectSelector = mkSelector "removeObject:"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @objectAtIndex:@
objectAtIndexSelector :: Selector
objectAtIndexSelector = mkSelector "objectAtIndex:"

-- | @Selector@ for @placeLightProbesWithDensity:heuristic:usingIrradianceDataSource:@
placeLightProbesWithDensity_heuristic_usingIrradianceDataSourceSelector :: Selector
placeLightProbesWithDensity_heuristic_usingIrradianceDataSourceSelector = mkSelector "placeLightProbesWithDensity:heuristic:usingIrradianceDataSource:"

-- | @Selector@ for @frameInterval@
frameIntervalSelector :: Selector
frameIntervalSelector = mkSelector "frameInterval"

-- | @Selector@ for @setFrameInterval:@
setFrameIntervalSelector :: Selector
setFrameIntervalSelector = mkSelector "setFrameInterval:"

-- | @Selector@ for @startTime@
startTimeSelector :: Selector
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @setStartTime:@
setStartTimeSelector :: Selector
setStartTimeSelector = mkSelector "setStartTime:"

-- | @Selector@ for @endTime@
endTimeSelector :: Selector
endTimeSelector = mkSelector "endTime"

-- | @Selector@ for @setEndTime:@
setEndTimeSelector :: Selector
setEndTimeSelector = mkSelector "setEndTime:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @resolver@
resolverSelector :: Selector
resolverSelector = mkSelector "resolver"

-- | @Selector@ for @setResolver:@
setResolverSelector :: Selector
setResolverSelector = mkSelector "setResolver:"

-- | @Selector@ for @bufferAllocator@
bufferAllocatorSelector :: Selector
bufferAllocatorSelector = mkSelector "bufferAllocator"

-- | @Selector@ for @vertexDescriptor@
vertexDescriptorSelector :: Selector
vertexDescriptorSelector = mkSelector "vertexDescriptor"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @masters@
mastersSelector :: Selector
mastersSelector = mkSelector "masters"

-- | @Selector@ for @setMasters:@
setMastersSelector :: Selector
setMastersSelector = mkSelector "setMasters:"

-- | @Selector@ for @originals@
originalsSelector :: Selector
originalsSelector = mkSelector "originals"

-- | @Selector@ for @setOriginals:@
setOriginalsSelector :: Selector
setOriginalsSelector = mkSelector "setOriginals:"

-- | @Selector@ for @animations@
animationsSelector :: Selector
animationsSelector = mkSelector "animations"

-- | @Selector@ for @setAnimations:@
setAnimationsSelector :: Selector
setAnimationsSelector = mkSelector "setAnimations:"

