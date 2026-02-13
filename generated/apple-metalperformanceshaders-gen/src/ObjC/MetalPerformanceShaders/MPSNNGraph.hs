{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNGraph
--
-- Optimized representation of a graph of MPSNNImageNodes and MPSNNFilterNodes
--
-- Once you have prepared a graph of MPSNNImageNodes and MPSNNFilterNodes              (and if needed MPSNNStateNodes), you may initialize a MPSNNGraph using               the MPSNNImageNode that you wish to appear as the result. The MPSNNGraph               object will introspect the graph representation and determine which nodes              are needed for inputs, and which nodes are produced as output state (if any).              Nodes which are not needed to calculate the result image node are ignored.              Some nodes may be internally concatenated with other nodes for better               performance.
--
-- Note: the MPSNNImageNode that you choose as the result node may be interior                    to a graph. This feature is provided as a means to examine intermediate                    computations in the full graph for debugging purposes.
--
-- During MPSNNGraph construction, the graph attached to the result node will               be parsed and reduced to an optimized representation. This representation may              be saved using the NSSecureCoding protocol for later recall.
--
-- When decoding a MPSNNGraph using a NSCoder, it will be created against               the system default MTLDevice. If you would like to set the MTLDevice,              your NSCoder should conform to the <MPSDeviceProvider> protocol.
--
-- You may find it helpful to set MPSKernelOptionsVerbose on the graph when              debugging. To turn this on during MPSKernel initialization (including              MPSNNGraph initialization) set the MPS_LOG_INFO environment variable.              There is a lot of information about what optimizations are done to your              graph, including some information on why certain optimizations were not              made.
--
-- Generated bindings for @MPSNNGraph@.
module ObjC.MetalPerformanceShaders.MPSNNGraph
  ( MPSNNGraph
  , IsMPSNNGraph(..)
  , initWithDevice_resultImage_resultImageIsNeeded
  , graphWithDevice_resultImage_resultImageIsNeeded
  , initWithDevice_resultImages_resultsAreNeeded
  , graphWithDevice_resultImages_resultsAreNeeded
  , initWithDevice_resultImage
  , graphWithDevice_resultImage
  , initWithCoder_device
  , initWithDevice
  , reloadFromDataSources
  , encodeToCommandBuffer_sourceImages_sourceStates_intermediateImages_destinationStates
  , encodeBatchToCommandBuffer_sourceImages_sourceStates_intermediateImages_destinationStates
  , encodeToCommandBuffer_sourceImages
  , encodeBatchToCommandBuffer_sourceImages_sourceStates
  , executeAsyncWithSourceImages_completionHandler
  , readCountForSourceImageAtIndex
  , readCountForSourceStateAtIndex
  , sourceImageHandles
  , sourceStateHandles
  , intermediateImageHandles
  , resultStateHandles
  , resultHandle
  , outputStateIsTemporary
  , setOutputStateIsTemporary
  , destinationImageAllocator
  , setDestinationImageAllocator
  , format
  , setFormat
  , resultImageIsNeeded
  , destinationImageAllocatorSelector
  , encodeBatchToCommandBuffer_sourceImages_sourceStatesSelector
  , encodeBatchToCommandBuffer_sourceImages_sourceStates_intermediateImages_destinationStatesSelector
  , encodeToCommandBuffer_sourceImagesSelector
  , encodeToCommandBuffer_sourceImages_sourceStates_intermediateImages_destinationStatesSelector
  , executeAsyncWithSourceImages_completionHandlerSelector
  , formatSelector
  , graphWithDevice_resultImageSelector
  , graphWithDevice_resultImage_resultImageIsNeededSelector
  , graphWithDevice_resultImages_resultsAreNeededSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_resultImageSelector
  , initWithDevice_resultImage_resultImageIsNeededSelector
  , initWithDevice_resultImages_resultsAreNeededSelector
  , intermediateImageHandlesSelector
  , outputStateIsTemporarySelector
  , readCountForSourceImageAtIndexSelector
  , readCountForSourceStateAtIndexSelector
  , reloadFromDataSourcesSelector
  , resultHandleSelector
  , resultImageIsNeededSelector
  , resultStateHandlesSelector
  , setDestinationImageAllocatorSelector
  , setFormatSelector
  , setOutputStateIsTemporarySelector
  , sourceImageHandlesSelector
  , sourceStateHandlesSelector

  -- * Enum types
  , MPSImageFeatureChannelFormat(MPSImageFeatureChannelFormat)
  , pattern MPSImageFeatureChannelFormatNone
  , pattern MPSImageFeatureChannelFormatUnorm8
  , pattern MPSImageFeatureChannelFormatUnorm16
  , pattern MPSImageFeatureChannelFormatFloat16
  , pattern MPSImageFeatureChannelFormatFloat32
  , pattern MPSImageFeatureChannelFormat_reserved0
  , pattern MPSImageFeatureChannelFormatCount

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initialize a MPSNNGraph object on a device starting with resultImage working backward
--
-- The MPSNNGraph constructor will start with the indicated result image, and look              to see what MPSNNFilterNode produced it, then look to its dependencies and so              forth to reveal the subsection of the graph necessary to compute the image.
--
-- @device@ — The MTLDevice on which to run the graph
--
-- @resultImage@ — The MPSNNImageNode corresponding to the last image in the graph.                          This is the image that will be returned.  Note: the imageAllocator                          for this node is ignored and the MPSNNGraph.destinationImageAllocator                          is used for this node instead.
--
-- @resultIsNeeded@ — Commonly, when training a graph, the last MPSImage out of the                             graph is not used. The final gradient filter is run solely to update                             some weights. If resultIsNeeded is set to NO, nil will                             be returned from the left hand side of the -encode call instead,                             and computation to produce the last image may be pruned away.
--
-- Returns: A new MPSNNGraph.
--
-- ObjC selector: @- initWithDevice:resultImage:resultImageIsNeeded:@
initWithDevice_resultImage_resultImageIsNeeded :: (IsMPSNNGraph mpsnnGraph, IsMPSNNImageNode resultImage) => mpsnnGraph -> RawId -> resultImage -> Bool -> IO (Id MPSNNGraph)
initWithDevice_resultImage_resultImageIsNeeded mpsnnGraph device resultImage resultIsNeeded =
  sendOwnedMessage mpsnnGraph initWithDevice_resultImage_resultImageIsNeededSelector device (toMPSNNImageNode resultImage) resultIsNeeded

-- | @+ graphWithDevice:resultImage:resultImageIsNeeded:@
graphWithDevice_resultImage_resultImageIsNeeded :: IsMPSNNImageNode resultImage => RawId -> resultImage -> Bool -> IO (Id MPSNNGraph)
graphWithDevice_resultImage_resultImageIsNeeded device resultImage resultIsNeeded =
  do
    cls' <- getRequiredClass "MPSNNGraph"
    sendClassMessage cls' graphWithDevice_resultImage_resultImageIsNeededSelector device (toMPSNNImageNode resultImage) resultIsNeeded

-- | Initialize a MPSNNGraph object on a device starting with resultImage working backward
--
-- The MPSNNGraph constructor will start with the indicated result images, and look              to see what MPSNNFilterNode produced them, then look to its dependencies and so              forth to reveal the subsection of the graph necessary to compute the image. This variant              is provided to support graphs and subgraphs with multiple image outputs.
--
-- @device@ — The MTLDevice on which to run the graph
--
-- @resultImages@ — The MPSNNImageNodes corresponding to the last images in the graph.                           The first image in the array will be returned from the -encode method                           LHS. The rest will be included in the list of intermediate images.
--
-- @areResultsNeeded@ — An array of BOOL values with count equal to resultImages.count.                                If NO is passed for a given image, the image itself is marked unneeded                                and might be skipped. The graph will prune this branch back to the                                first requred filter. A filter is required if it generates a needed                                result image, or is needed to update training parameters.
--
-- Returns: A new MPSNNGraph.
--
-- ObjC selector: @- initWithDevice:resultImages:resultsAreNeeded:@
initWithDevice_resultImages_resultsAreNeeded :: (IsMPSNNGraph mpsnnGraph, IsNSArray resultImages) => mpsnnGraph -> RawId -> resultImages -> Ptr Bool -> IO (Id MPSNNGraph)
initWithDevice_resultImages_resultsAreNeeded mpsnnGraph device resultImages areResultsNeeded =
  sendOwnedMessage mpsnnGraph initWithDevice_resultImages_resultsAreNeededSelector device (toNSArray resultImages) areResultsNeeded

-- | @+ graphWithDevice:resultImages:resultsAreNeeded:@
graphWithDevice_resultImages_resultsAreNeeded :: IsNSArray resultImages => RawId -> resultImages -> Ptr Bool -> IO (Id MPSNNGraph)
graphWithDevice_resultImages_resultsAreNeeded device resultImages areResultsNeeded =
  do
    cls' <- getRequiredClass "MPSNNGraph"
    sendClassMessage cls' graphWithDevice_resultImages_resultsAreNeededSelector device (toNSArray resultImages) areResultsNeeded

-- | @- initWithDevice:resultImage:@
initWithDevice_resultImage :: (IsMPSNNGraph mpsnnGraph, IsMPSNNImageNode resultImage) => mpsnnGraph -> RawId -> resultImage -> IO (Id MPSNNGraph)
initWithDevice_resultImage mpsnnGraph device resultImage =
  sendOwnedMessage mpsnnGraph initWithDevice_resultImageSelector device (toMPSNNImageNode resultImage)

-- | @+ graphWithDevice:resultImage:@
graphWithDevice_resultImage :: IsMPSNNImageNode resultImage => RawId -> resultImage -> IO (Id MPSNNGraph)
graphWithDevice_resultImage device resultImage =
  do
    cls' <- getRequiredClass "MPSNNGraph"
    sendClassMessage cls' graphWithDevice_resultImageSelector device (toMPSNNImageNode resultImage)

-- | NSSecureCoding compatability
--
-- While the standard NSSecureCoding/NSCoding method              -initWithCoder: should work, since the file can't              know which device your data is allocated on, we              have to guess and may guess incorrectly.  To avoid              that problem, use initWithCoder:device instead.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSKernel
--
-- @device@ — The MTLDevice on which to make the MPSKernel
--
-- Returns: A new MPSKernel object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNGraph mpsnnGraph, IsNSCoder aDecoder) => mpsnnGraph -> aDecoder -> RawId -> IO (Id MPSNNGraph)
initWithCoder_device mpsnnGraph aDecoder device =
  sendOwnedMessage mpsnnGraph initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Use initWithDevice:resultImage: instead
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNGraph mpsnnGraph => mpsnnGraph -> RawId -> IO (Id MPSNNGraph)
initWithDevice mpsnnGraph device =
  sendOwnedMessage mpsnnGraph initWithDeviceSelector device

-- | Reinitialize all graph nodes from data sources
--
-- A number of the nodes that make up a graph have a data source              associated with them, for example a MPSCNNConvolutionDataSource              or a MPSCNNBatchNormalizationDataSource. Generally, the data              is read from these once at graph initialization time and then              not looked at again, except during the weight / parameter update              phase of the corresponding gradient nodes and then only if CPU              updates are requested.  Otherwise, update occurs on the GPU,              and the data in the data source is thereafter ignored.
--
-- It can happen, though, that your application has determined the              graph should load a new set of weights from the data source.              When this method is called, the graph will find all nodes that              support reloading and direct them to reinitialize themselves              based on their data source.
--
-- This process occurs immediately. Your application will              need to make sure any GPU work being done by the graph is complete              to ensure data coherency. Most nodes do not have a data source              and will not be modified. Nodes that are not used by the graph              will not be updated.
--
-- ObjC selector: @- reloadFromDataSources@
reloadFromDataSources :: IsMPSNNGraph mpsnnGraph => mpsnnGraph -> IO ()
reloadFromDataSources mpsnnGraph =
  sendMessage mpsnnGraph reloadFromDataSourcesSelector

-- | Encode the graph to a MTLCommandBuffer
--
-- @commandBuffer@ — The command buffer. If the command buffer is a MPSCommandBuffer,                                      the work will be committed to Metal in small pieces so that                                      the CPU-side latency is much reduced.
--
-- @sourceImages@ — A list of MPSImages to use as the source images for the graph.                                      These should be in the same order as the list returned from MPSNNGraph.sourceImageHandles.                                      The images may be image arrays. Typically, this is only one or two images                                      such as a .JPG decoded into a MPSImage*.  If the sourceImages are MPSTemporaryImages,                                      the graph will decrement the readCount by 1, even if the graph actually                                      reads an image multiple times.
--
-- @sourceStates@ — A list of MPSState objects to use as state for a graph.                                      These should be in the same order as the list returned from MPSNNGraph.sourceStateHandles.                                      May be nil, if there is no source state. If the sourceStates are temporary,                                      the graph will decrement the readCount by 1, even if the graph actually                                      reads the state multiple times.
--
-- @intermediateImages@ — An optional NSMutableArray to receive any MPSImage objects exported as part of its operation.                                      These are only the images that were tagged with MPSNNImageNode.exportFromGraph = YES. The                                       identity of the states is given by -resultStateHandles.  If temporary, each intermediateImage                                       will have a readCount of 1.  If the result was tagged exportFromGraph = YES, it will be here                                      too, with a readCount of 2. To be able to access the images from outside the graph on the CPU,                                      your application must also set MPSNNImageNode.synchronizeResource = YES,                                      and MPSNNImageNode.imageAllocator = [MPSImage defaultAllocator]; The defaultAllocator creates                                      a permanent image that can be read with readBytes.
--
-- @destinationStates@ — An optional NSMutableArray to receive any MPSState objects created as part of its operation.                                      The identity of the states is given by -resultStateHandles.
--
-- Returns: A MPSImage or MPSTemporaryImage allocated per the destinationImageAllocator containing the output of the graph.              It will be automatically released when commandBuffer completes.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceImages:sourceStates:intermediateImages:destinationStates:@
encodeToCommandBuffer_sourceImages_sourceStates_intermediateImages_destinationStates :: (IsMPSNNGraph mpsnnGraph, IsNSArray sourceImages, IsNSArray sourceStates, IsNSMutableArray intermediateImages, IsNSMutableArray destinationStates) => mpsnnGraph -> RawId -> sourceImages -> sourceStates -> intermediateImages -> destinationStates -> IO (Id MPSImage)
encodeToCommandBuffer_sourceImages_sourceStates_intermediateImages_destinationStates mpsnnGraph commandBuffer sourceImages sourceStates intermediateImages destinationStates =
  sendMessage mpsnnGraph encodeToCommandBuffer_sourceImages_sourceStates_intermediateImages_destinationStatesSelector commandBuffer (toNSArray sourceImages) (toNSArray sourceStates) (toNSMutableArray intermediateImages) (toNSMutableArray destinationStates)

-- | Encode the graph to a MTLCommandBuffer
--
-- This interface is like the other except that it operates on a batch of images all                  at once.  In addition, you may specify whether the result is needed.
--
-- @commandBuffer@ — The command buffer. If the command buffer is a MPSCommandBuffer,                                      the work will be committed to Metal in small pieces so that                                      the CPU-side latency is much reduced.
--
-- @sourceImages@ — A list of MPSImages to use as the source images for the graph.                                      These should be in the same order as the list returned from MPSNNGraph.sourceImageHandles.                                      The images may be image arrays. Typically, this is only one or two images                                      such as a .JPG decoded into a MPSImage*.  If the sourceImages are MPSTemporaryImages,                                      the graph will decrement the readCount by 1, even if the graph actually                                      reads an image multiple times.
--
-- @sourceStates@ — A list of MPSState objects to use as state for a graph.                                      These should be in the same order as the list returned from MPSNNGraph.sourceStateHandles.                                      May be nil, if there is no source state. If the sourceStates are temporary,                                      the graph will decrement the readCount by 1, even if the graph actually                                      reads the state multiple times.
--
-- @intermediateImages@ — An optional NSMutableArray to receive any MPSImage objects exported as part of its operation.                                      These are only the images that were tagged with MPSNNImageNode.exportFromGraph = YES. The                                      identity of the states is given by -resultStateHandles.  If temporary, each intermediateImage                                      will have a readCount of 1.  If the result was tagged exportFromGraph = YES, it will be here                                      too, with a readCount of 2. To be able to access the images from outside the graph on the CPU,                                      your application must also set MPSNNImageNode.synchronizeResource = YES,                                      and MPSNNImageNode.imageAllocator = [MPSImage defaultAllocator]; The defaultAllocator creates                                      a permanent image that can be read with readBytes.
--
-- @destinationStates@ — An optional NSMutableArray to receive any MPSState objects created as part of its operation.                                      The identity of the states is given by -resultStateHandles.
--
-- Returns: A MPSImageBatch or MPSTemporaryImageBatch allocated per the destinationImageAllocator containing the output of the graph.              It will be automatically released when commandBuffer completes. If resultIsNeeded == NO, then this              will return nil.
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceImages:sourceStates:intermediateImages:destinationStates:@
encodeBatchToCommandBuffer_sourceImages_sourceStates_intermediateImages_destinationStates :: (IsMPSNNGraph mpsnnGraph, IsNSArray sourceImages, IsNSArray sourceStates, IsNSMutableArray intermediateImages, IsNSMutableArray destinationStates) => mpsnnGraph -> RawId -> sourceImages -> sourceStates -> intermediateImages -> destinationStates -> IO RawId
encodeBatchToCommandBuffer_sourceImages_sourceStates_intermediateImages_destinationStates mpsnnGraph commandBuffer sourceImages sourceStates intermediateImages destinationStates =
  sendMessage mpsnnGraph encodeBatchToCommandBuffer_sourceImages_sourceStates_intermediateImages_destinationStatesSelector commandBuffer (toNSArray sourceImages) (toNSArray sourceStates) (toNSMutableArray intermediateImages) (toNSMutableArray destinationStates)

-- | Encode the graph to a MTLCommandBuffer
--
-- IMPORTANT:  Please use [MTLCommandBuffer addCompletedHandler:] to determine when this work is                  done. Use CPU time that would have been spent waiting for the GPU to encode the next command                  buffer and commit it too.  That way, the work for the next command buffer is ready to go the                  moment the GPU is done. This will keep the GPU busy and running at top speed.
--
-- Those who ignore this advice and use [MTLCommandBuffer waitUntilCompleted] instead will likely                  cause their code to slow down by a factor of two or more. The CPU clock spins down while it                  waits for the GPU. When the GPU completes, the CPU runs slowly for a while until it spins up.                  The GPU has to wait for the CPU to  encode more work (at low clock), giving it plenty of time to                  spin its own clock down. In typical CNN graph usage, neither may ever reach maximum clock                  frequency, causing slow down far beyond what otherwise would be expected from simple failure                  to schedule CPU and GPU work concurrently. Regrattably, it is probable that every performance                  benchmark you see on the net will be based on [MTLCommandBuffer waitUntilCompleted].
--
-- @commandBuffer@ — The command buffer. If the command buffer is a MPSCommandBuffer,                                      the work will be committed to Metal in small pieces so that                                      the CPU-side latency is much reduced.
--
-- @sourceImages@ — A list of MPSImages to use as the source images for the graph.                                      These should be in the same order as the list returned from MPSNNGraph.sourceImageHandles.
--
-- Returns: A MPSImage or MPSTemporaryImage allocated per the destinationImageAllocator containing the output of the graph.              It will be automatically released when commandBuffer completes.  It can be nil if resultImageIsNeeded == NO
--
-- ObjC selector: @- encodeToCommandBuffer:sourceImages:@
encodeToCommandBuffer_sourceImages :: (IsMPSNNGraph mpsnnGraph, IsNSArray sourceImages) => mpsnnGraph -> RawId -> sourceImages -> IO (Id MPSImage)
encodeToCommandBuffer_sourceImages mpsnnGraph commandBuffer sourceImages =
  sendMessage mpsnnGraph encodeToCommandBuffer_sourceImagesSelector commandBuffer (toNSArray sourceImages)

-- | Convenience method to encode a batch of images
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceImages:sourceStates:@
encodeBatchToCommandBuffer_sourceImages_sourceStates :: (IsMPSNNGraph mpsnnGraph, IsNSArray sourceImages, IsNSArray sourceStates) => mpsnnGraph -> RawId -> sourceImages -> sourceStates -> IO RawId
encodeBatchToCommandBuffer_sourceImages_sourceStates mpsnnGraph commandBuffer sourceImages sourceStates =
  sendMessage mpsnnGraph encodeBatchToCommandBuffer_sourceImages_sourceStatesSelector commandBuffer (toNSArray sourceImages) (toNSArray sourceStates)

-- | Convenience method to execute a graph without having to manage many Metal details
--
-- This function will synchronously encode the graph on a private command buffer,                commit it to a MPS internal command queue and return. The GPU will start working.                When the GPU is done, the completion handler will be called.  You should use                the intervening time to encode other work for execution on the GPU, so that                the GPU stays busy and doesn't clock down.
--
-- The work will be performed on the MTLDevice that hosts the source images.
--
-- This is a convenience API.  There are a few situations it does not handle optimally.                These may be better handled using [encodeToCommandBuffer:sourceImages:].                Specifically:
--
-- o     If the graph needs to be run multiple times for different images,
-- it would be better to encode the graph multiple times on the same
-- command buffer using [encodeToCommandBuffer:sourceImages:]  This
-- will allow the multiple graphs to share memory for intermediate
-- storage, dramatically reducing memory usage.
--
-- o     If preprocessing or post-processing of the MPSImage is required,
-- such as resizing or normalization outside of a convolution, it would
-- be better to encode those things on the same command buffer.
-- Memory may be saved here too for intermediate storage. (MPSTemporaryImage
-- lifetime does not span multiple command buffers.)
--
-- @sourceImages@ — A list of MPSImages to use as the source images for the graph.                          These should be in the same order as the list returned from                          MPSNNGraph.sourceImageHandles. They should be allocated against                          the same MTLDevice. There must be at least one source image.                          Note: this array is intended to handle the case where multiple                          input images are required to generate a single graph result.                          That is, the graph itself has multiple inputs.  If you need to                          execute the graph multiple times, then call this API multiple                          times, or (faster) make use of MPSImageBatches using                          -executeBatchToCommandBuffer:sourceImages:sourceStates:...                          (See discussion)
--
-- @handler@ — A block to receive any errors generated. This block may run                          on any thread and may be called before this method returns.                          The image, if any, passed to this callback is the same image                          as that returned from the left hand side.
--
-- Returns: A MPSImage to receive the result. The data in the image will not be valid until             the completionHandler is called.
--
-- ObjC selector: @- executeAsyncWithSourceImages:completionHandler:@
executeAsyncWithSourceImages_completionHandler :: (IsMPSNNGraph mpsnnGraph, IsNSArray sourceImages) => mpsnnGraph -> sourceImages -> Ptr () -> IO (Id MPSImage)
executeAsyncWithSourceImages_completionHandler mpsnnGraph sourceImages handler =
  sendMessage mpsnnGraph executeAsyncWithSourceImages_completionHandlerSelector (toNSArray sourceImages) handler

-- | Find the number of times a image will be read by the graph *
--
-- From the set of images (or image batches) passed in to the graph, find              the number of times the graph will read an image.  This may be needed              by your application to correctly set the MPSImage.readCount property.
--
-- @index@ — The index of the image. The index of the image matches the index of the image in the array returned              by the sourceImageHandles property.
--
-- Returns: The read count of the image(s) at the index will be reduced by the value returned              when the graph is finished encoding. The readcount of the image(s) must be at least              this value when it is passed into the -encode... method.
--
-- ObjC selector: @- readCountForSourceImageAtIndex:@
readCountForSourceImageAtIndex :: IsMPSNNGraph mpsnnGraph => mpsnnGraph -> CULong -> IO CULong
readCountForSourceImageAtIndex mpsnnGraph index =
  sendMessage mpsnnGraph readCountForSourceImageAtIndexSelector index

-- | Find the number of times a state will be read by the graph *
--
-- From the set of state (or state batches) passed in to the graph, find              the number of times the graph will read a state.  This may be needed              by your application to correctly set the MPSState.readCount property.
--
-- @index@ — The index of the state. The index of the state matches the index of the state in the array returned              by the sourceStateHandles property.
--
-- Returns: The read count of the state(s) at the index will be reduced by the value returned              when the graph is finished encoding. The read count of the state(s) must be at least              this value when it is passed into the -encode... method.
--
-- ObjC selector: @- readCountForSourceStateAtIndex:@
readCountForSourceStateAtIndex :: IsMPSNNGraph mpsnnGraph => mpsnnGraph -> CULong -> IO CULong
readCountForSourceStateAtIndex mpsnnGraph index =
  sendMessage mpsnnGraph readCountForSourceStateAtIndexSelector index

-- | Get a list of identifiers for source images needed to calculate the result image
--
-- ObjC selector: @- sourceImageHandles@
sourceImageHandles :: IsMPSNNGraph mpsnnGraph => mpsnnGraph -> IO (Id NSArray)
sourceImageHandles mpsnnGraph =
  sendMessage mpsnnGraph sourceImageHandlesSelector

-- | Get a list of identifiers for source state objects needed to calculate the result image
--
-- Not guaranteed to be in the same order as resultStateHandles
--
-- ObjC selector: @- sourceStateHandles@
sourceStateHandles :: IsMPSNNGraph mpsnnGraph => mpsnnGraph -> IO (Id NSArray)
sourceStateHandles mpsnnGraph =
  sendMessage mpsnnGraph sourceStateHandlesSelector

-- | Get a list of identifiers for intermediate images objects produced by the graph
--
-- ObjC selector: @- intermediateImageHandles@
intermediateImageHandles :: IsMPSNNGraph mpsnnGraph => mpsnnGraph -> IO (Id NSArray)
intermediateImageHandles mpsnnGraph =
  sendMessage mpsnnGraph intermediateImageHandlesSelector

-- | Get a list of identifiers for result state objects produced by the graph
--
-- Not guaranteed to be in the same order as sourceStateHandles
--
-- ObjC selector: @- resultStateHandles@
resultStateHandles :: IsMPSNNGraph mpsnnGraph => mpsnnGraph -> IO (Id NSArray)
resultStateHandles mpsnnGraph =
  sendMessage mpsnnGraph resultStateHandlesSelector

-- | Get a handle for the graph result image
--
-- ObjC selector: @- resultHandle@
resultHandle :: IsMPSNNGraph mpsnnGraph => mpsnnGraph -> IO RawId
resultHandle mpsnnGraph =
  sendMessage mpsnnGraph resultHandleSelector

-- | Should MPSState objects produced by -encodeToCommandBuffer... be temporary objects.
--
-- See MPSState description. Default: NO
--
-- ObjC selector: @- outputStateIsTemporary@
outputStateIsTemporary :: IsMPSNNGraph mpsnnGraph => mpsnnGraph -> IO Bool
outputStateIsTemporary mpsnnGraph =
  sendMessage mpsnnGraph outputStateIsTemporarySelector

-- | Should MPSState objects produced by -encodeToCommandBuffer... be temporary objects.
--
-- See MPSState description. Default: NO
--
-- ObjC selector: @- setOutputStateIsTemporary:@
setOutputStateIsTemporary :: IsMPSNNGraph mpsnnGraph => mpsnnGraph -> Bool -> IO ()
setOutputStateIsTemporary mpsnnGraph value =
  sendMessage mpsnnGraph setOutputStateIsTemporarySelector value

-- | Method to allocate the result image from -encodeToCommandBuffer...
--
-- This property overrides the allocator for the final result image in              the graph. Default: MPSImage.defaultAllocator
--
-- ObjC selector: @- destinationImageAllocator@
destinationImageAllocator :: IsMPSNNGraph mpsnnGraph => mpsnnGraph -> IO RawId
destinationImageAllocator mpsnnGraph =
  sendMessage mpsnnGraph destinationImageAllocatorSelector

-- | Method to allocate the result image from -encodeToCommandBuffer...
--
-- This property overrides the allocator for the final result image in              the graph. Default: MPSImage.defaultAllocator
--
-- ObjC selector: @- setDestinationImageAllocator:@
setDestinationImageAllocator :: IsMPSNNGraph mpsnnGraph => mpsnnGraph -> RawId -> IO ()
setDestinationImageAllocator mpsnnGraph value =
  sendMessage mpsnnGraph setDestinationImageAllocatorSelector value

-- | The default storage format used for graph intermediate images
--
-- This doesn't affect how data is stored in buffers in states.              Nor does it affect the storage format for weights              such as convolution weights stored by individual filters.              Default: MPSImageFeatureChannelFormatFloat16
--
-- ObjC selector: @- format@
format :: IsMPSNNGraph mpsnnGraph => mpsnnGraph -> IO MPSImageFeatureChannelFormat
format mpsnnGraph =
  sendMessage mpsnnGraph formatSelector

-- | The default storage format used for graph intermediate images
--
-- This doesn't affect how data is stored in buffers in states.              Nor does it affect the storage format for weights              such as convolution weights stored by individual filters.              Default: MPSImageFeatureChannelFormatFloat16
--
-- ObjC selector: @- setFormat:@
setFormat :: IsMPSNNGraph mpsnnGraph => mpsnnGraph -> MPSImageFeatureChannelFormat -> IO ()
setFormat mpsnnGraph value =
  sendMessage mpsnnGraph setFormatSelector value

-- | Set at -init time.
--
-- If NO, nil will be returned from -encode calls and some computation              may be omitted.
--
-- ObjC selector: @- resultImageIsNeeded@
resultImageIsNeeded :: IsMPSNNGraph mpsnnGraph => mpsnnGraph -> IO Bool
resultImageIsNeeded mpsnnGraph =
  sendMessage mpsnnGraph resultImageIsNeededSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:resultImage:resultImageIsNeeded:@
initWithDevice_resultImage_resultImageIsNeededSelector :: Selector '[RawId, Id MPSNNImageNode, Bool] (Id MPSNNGraph)
initWithDevice_resultImage_resultImageIsNeededSelector = mkSelector "initWithDevice:resultImage:resultImageIsNeeded:"

-- | @Selector@ for @graphWithDevice:resultImage:resultImageIsNeeded:@
graphWithDevice_resultImage_resultImageIsNeededSelector :: Selector '[RawId, Id MPSNNImageNode, Bool] (Id MPSNNGraph)
graphWithDevice_resultImage_resultImageIsNeededSelector = mkSelector "graphWithDevice:resultImage:resultImageIsNeeded:"

-- | @Selector@ for @initWithDevice:resultImages:resultsAreNeeded:@
initWithDevice_resultImages_resultsAreNeededSelector :: Selector '[RawId, Id NSArray, Ptr Bool] (Id MPSNNGraph)
initWithDevice_resultImages_resultsAreNeededSelector = mkSelector "initWithDevice:resultImages:resultsAreNeeded:"

-- | @Selector@ for @graphWithDevice:resultImages:resultsAreNeeded:@
graphWithDevice_resultImages_resultsAreNeededSelector :: Selector '[RawId, Id NSArray, Ptr Bool] (Id MPSNNGraph)
graphWithDevice_resultImages_resultsAreNeededSelector = mkSelector "graphWithDevice:resultImages:resultsAreNeeded:"

-- | @Selector@ for @initWithDevice:resultImage:@
initWithDevice_resultImageSelector :: Selector '[RawId, Id MPSNNImageNode] (Id MPSNNGraph)
initWithDevice_resultImageSelector = mkSelector "initWithDevice:resultImage:"

-- | @Selector@ for @graphWithDevice:resultImage:@
graphWithDevice_resultImageSelector :: Selector '[RawId, Id MPSNNImageNode] (Id MPSNNGraph)
graphWithDevice_resultImageSelector = mkSelector "graphWithDevice:resultImage:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNGraph)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNGraph)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @reloadFromDataSources@
reloadFromDataSourcesSelector :: Selector '[] ()
reloadFromDataSourcesSelector = mkSelector "reloadFromDataSources"

-- | @Selector@ for @encodeToCommandBuffer:sourceImages:sourceStates:intermediateImages:destinationStates:@
encodeToCommandBuffer_sourceImages_sourceStates_intermediateImages_destinationStatesSelector :: Selector '[RawId, Id NSArray, Id NSArray, Id NSMutableArray, Id NSMutableArray] (Id MPSImage)
encodeToCommandBuffer_sourceImages_sourceStates_intermediateImages_destinationStatesSelector = mkSelector "encodeToCommandBuffer:sourceImages:sourceStates:intermediateImages:destinationStates:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:sourceStates:intermediateImages:destinationStates:@
encodeBatchToCommandBuffer_sourceImages_sourceStates_intermediateImages_destinationStatesSelector :: Selector '[RawId, Id NSArray, Id NSArray, Id NSMutableArray, Id NSMutableArray] RawId
encodeBatchToCommandBuffer_sourceImages_sourceStates_intermediateImages_destinationStatesSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:sourceStates:intermediateImages:destinationStates:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImages:@
encodeToCommandBuffer_sourceImagesSelector :: Selector '[RawId, Id NSArray] (Id MPSImage)
encodeToCommandBuffer_sourceImagesSelector = mkSelector "encodeToCommandBuffer:sourceImages:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:sourceStates:@
encodeBatchToCommandBuffer_sourceImages_sourceStatesSelector :: Selector '[RawId, Id NSArray, Id NSArray] RawId
encodeBatchToCommandBuffer_sourceImages_sourceStatesSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:sourceStates:"

-- | @Selector@ for @executeAsyncWithSourceImages:completionHandler:@
executeAsyncWithSourceImages_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] (Id MPSImage)
executeAsyncWithSourceImages_completionHandlerSelector = mkSelector "executeAsyncWithSourceImages:completionHandler:"

-- | @Selector@ for @readCountForSourceImageAtIndex:@
readCountForSourceImageAtIndexSelector :: Selector '[CULong] CULong
readCountForSourceImageAtIndexSelector = mkSelector "readCountForSourceImageAtIndex:"

-- | @Selector@ for @readCountForSourceStateAtIndex:@
readCountForSourceStateAtIndexSelector :: Selector '[CULong] CULong
readCountForSourceStateAtIndexSelector = mkSelector "readCountForSourceStateAtIndex:"

-- | @Selector@ for @sourceImageHandles@
sourceImageHandlesSelector :: Selector '[] (Id NSArray)
sourceImageHandlesSelector = mkSelector "sourceImageHandles"

-- | @Selector@ for @sourceStateHandles@
sourceStateHandlesSelector :: Selector '[] (Id NSArray)
sourceStateHandlesSelector = mkSelector "sourceStateHandles"

-- | @Selector@ for @intermediateImageHandles@
intermediateImageHandlesSelector :: Selector '[] (Id NSArray)
intermediateImageHandlesSelector = mkSelector "intermediateImageHandles"

-- | @Selector@ for @resultStateHandles@
resultStateHandlesSelector :: Selector '[] (Id NSArray)
resultStateHandlesSelector = mkSelector "resultStateHandles"

-- | @Selector@ for @resultHandle@
resultHandleSelector :: Selector '[] RawId
resultHandleSelector = mkSelector "resultHandle"

-- | @Selector@ for @outputStateIsTemporary@
outputStateIsTemporarySelector :: Selector '[] Bool
outputStateIsTemporarySelector = mkSelector "outputStateIsTemporary"

-- | @Selector@ for @setOutputStateIsTemporary:@
setOutputStateIsTemporarySelector :: Selector '[Bool] ()
setOutputStateIsTemporarySelector = mkSelector "setOutputStateIsTemporary:"

-- | @Selector@ for @destinationImageAllocator@
destinationImageAllocatorSelector :: Selector '[] RawId
destinationImageAllocatorSelector = mkSelector "destinationImageAllocator"

-- | @Selector@ for @setDestinationImageAllocator:@
setDestinationImageAllocatorSelector :: Selector '[RawId] ()
setDestinationImageAllocatorSelector = mkSelector "setDestinationImageAllocator:"

-- | @Selector@ for @format@
formatSelector :: Selector '[] MPSImageFeatureChannelFormat
formatSelector = mkSelector "format"

-- | @Selector@ for @setFormat:@
setFormatSelector :: Selector '[MPSImageFeatureChannelFormat] ()
setFormatSelector = mkSelector "setFormat:"

-- | @Selector@ for @resultImageIsNeeded@
resultImageIsNeededSelector :: Selector '[] Bool
resultImageIsNeededSelector = mkSelector "resultImageIsNeeded"

