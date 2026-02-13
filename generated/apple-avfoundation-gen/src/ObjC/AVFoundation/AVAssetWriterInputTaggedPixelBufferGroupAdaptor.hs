{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVAssetWriterInputTaggedPixelBufferGroupAdaptor@.
module ObjC.AVFoundation.AVAssetWriterInputTaggedPixelBufferGroupAdaptor
  ( AVAssetWriterInputTaggedPixelBufferGroupAdaptor
  , IsAVAssetWriterInputTaggedPixelBufferGroupAdaptor(..)
  , init_
  , new
  , assetWriterInputTaggedPixelBufferGroupAdaptorWithAssetWriterInput_sourcePixelBufferAttributes
  , initWithAssetWriterInput_sourcePixelBufferAttributes
  , assetWriterInput
  , sourcePixelBufferAttributes
  , pixelBufferPool
  , assetWriterInputSelector
  , assetWriterInputTaggedPixelBufferGroupAdaptorWithAssetWriterInput_sourcePixelBufferAttributesSelector
  , initSelector
  , initWithAssetWriterInput_sourcePixelBufferAttributesSelector
  , newSelector
  , pixelBufferPoolSelector
  , sourcePixelBufferAttributesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetWriterInputTaggedPixelBufferGroupAdaptor avAssetWriterInputTaggedPixelBufferGroupAdaptor => avAssetWriterInputTaggedPixelBufferGroupAdaptor -> IO (Id AVAssetWriterInputTaggedPixelBufferGroupAdaptor)
init_ avAssetWriterInputTaggedPixelBufferGroupAdaptor =
  sendOwnedMessage avAssetWriterInputTaggedPixelBufferGroupAdaptor initSelector

-- | @+ new@
new :: IO (Id AVAssetWriterInputTaggedPixelBufferGroupAdaptor)
new  =
  do
    cls' <- getRequiredClass "AVAssetWriterInputTaggedPixelBufferGroupAdaptor"
    sendOwnedClassMessage cls' newSelector

-- | Creates a new tagged buffer adaptor to receive tagged buffer groups for writing to the output file.
--
-- In order to take advantage of the improved efficiency of appending buffers created from the adaptor's pixel buffer pool, clients should specify pixel buffer attributes that most closely accommodate the source format of the video frames being appended.
--
-- Pixel buffer attributes keys for the pixel buffer pool are defined in <CoreVideo/CVPixelBuffer.h>. To specify the pixel format type, the pixelBufferAttributes dictionary should contain a value for kCVPixelBufferPixelFormatTypeKey. For example, use [NSNumber numberWithInt:kCVPixelFormatType_32BGRA] for 8-bit-per-channel BGRA. See the discussion under appendPixelBuffer:withPresentationTime: for advice on choosing a pixel format.
--
-- Clients that do not need a pixel buffer pool for allocating buffers should set sourcePixelBufferAttributes to nil.
--
-- This method throws an exception if the input is already attached to another asset writer input tagged buffer group adaptor or if the input has already started writing (the asset writer has progressed beyond AVAssetWriterStatusUnknown).
--
-- - Parameter input: An instance of AVAssetWriterInput to which the receiver should append tagged buffer groups. Currently, only asset writer inputs that accept media data of type AVMediaTypeVideo can be used to initialize a tagged buffer adaptor. - Parameter sourcePixelBufferAttributes: Specifies the attributes of pixel buffers of tagged buffer groups that will be vended by the input's CVPixelBufferPool.
--
-- - Returns: An instance of AVAssetWriterInputTaggedPixelBufferGroupAdaptor.
--
-- ObjC selector: @+ assetWriterInputTaggedPixelBufferGroupAdaptorWithAssetWriterInput:sourcePixelBufferAttributes:@
assetWriterInputTaggedPixelBufferGroupAdaptorWithAssetWriterInput_sourcePixelBufferAttributes :: (IsAVAssetWriterInput input, IsNSDictionary sourcePixelBufferAttributes) => input -> sourcePixelBufferAttributes -> IO (Id AVAssetWriterInputTaggedPixelBufferGroupAdaptor)
assetWriterInputTaggedPixelBufferGroupAdaptorWithAssetWriterInput_sourcePixelBufferAttributes input sourcePixelBufferAttributes =
  do
    cls' <- getRequiredClass "AVAssetWriterInputTaggedPixelBufferGroupAdaptor"
    sendClassMessage cls' assetWriterInputTaggedPixelBufferGroupAdaptorWithAssetWriterInput_sourcePixelBufferAttributesSelector (toAVAssetWriterInput input) (toNSDictionary sourcePixelBufferAttributes)

-- | Creates a new tagged buffer group adaptor to receive tagged buffer groups for writing to the output file.
--
-- In order to take advantage of the improved efficiency of appending buffers created from the adaptor's pixel buffer pool, clients should specify pixel buffer attributes that most closely accommodate the source format of the video frames of tagged buffer groups being appended.
--
-- Pixel buffer attributes keys for the pixel buffer pool are defined in <CoreVideo/CVPixelBuffer.h>. To specify the pixel format type, the pixelBufferAttributes dictionary should contain a value for kCVPixelBufferPixelFormatTypeKey. For example, use [NSNumber numberWithInt:kCVPixelFormatType_32BGRA] for 8-bit-per-channel BGRA. See the discussion under appendPixelBuffer:withPresentationTime: in AVAssetWriterInputPixelBufferAdaptor for advice on choosing a pixel format.
--
-- Clients that do not need a pixel buffer pool for allocating buffers should set sourcePixelBufferAttributes to nil.
--
-- It is an error to initialize an instance of AVAssetWriterInputTaggedPixelBufferGroupAdaptor with an asset writer input that is already attached to another instance of AVAssetWriterInputTaggedPixelBufferGroupAdaptor. It is also an error to initialize an instance of AVAssetWriterInputTaggedPixelBufferGroupAdaptor with an asset writer input whose asset writer has progressed beyond AVAssetWriterStatusUnknown.
--
-- - Parameter input: An instance of AVAssetWriterInput to which the receiver should append tagged buffer groups. In addition to the pixel buffer adaptor, asset writer inputs with media data of type AVMediaTypeVideo can be used to initialize a tagged buffer group adaptor. - Parameter sourcePixelBufferAttributes: Specifies the attributes of pixel buffers of tagged buffer groups that will be vended by the input's CVPixelBufferPool.
--
-- - Returns: An instance of AVAssetWriterInputTaggedPixelBufferGroupAdaptor.
--
-- ObjC selector: @- initWithAssetWriterInput:sourcePixelBufferAttributes:@
initWithAssetWriterInput_sourcePixelBufferAttributes :: (IsAVAssetWriterInputTaggedPixelBufferGroupAdaptor avAssetWriterInputTaggedPixelBufferGroupAdaptor, IsAVAssetWriterInput input, IsNSDictionary sourcePixelBufferAttributes) => avAssetWriterInputTaggedPixelBufferGroupAdaptor -> input -> sourcePixelBufferAttributes -> IO (Id AVAssetWriterInputTaggedPixelBufferGroupAdaptor)
initWithAssetWriterInput_sourcePixelBufferAttributes avAssetWriterInputTaggedPixelBufferGroupAdaptor input sourcePixelBufferAttributes =
  sendOwnedMessage avAssetWriterInputTaggedPixelBufferGroupAdaptor initWithAssetWriterInput_sourcePixelBufferAttributesSelector (toAVAssetWriterInput input) (toNSDictionary sourcePixelBufferAttributes)

-- | The asset writer input to which the receiver should append tagged buffer groups.
--
-- ObjC selector: @- assetWriterInput@
assetWriterInput :: IsAVAssetWriterInputTaggedPixelBufferGroupAdaptor avAssetWriterInputTaggedPixelBufferGroupAdaptor => avAssetWriterInputTaggedPixelBufferGroupAdaptor -> IO (Id AVAssetWriterInput)
assetWriterInput avAssetWriterInputTaggedPixelBufferGroupAdaptor =
  sendMessage avAssetWriterInputTaggedPixelBufferGroupAdaptor assetWriterInputSelector

-- | The pixel buffer attributes of pixel buffers that will be vended by the receiver's CVPixelBufferPool.
--
-- The value of this property is a dictionary containing pixel buffer attributes keys defined in <CoreVideo/CVPixelBuffer.h>.
--
-- ObjC selector: @- sourcePixelBufferAttributes@
sourcePixelBufferAttributes :: IsAVAssetWriterInputTaggedPixelBufferGroupAdaptor avAssetWriterInputTaggedPixelBufferGroupAdaptor => avAssetWriterInputTaggedPixelBufferGroupAdaptor -> IO (Id NSDictionary)
sourcePixelBufferAttributes avAssetWriterInputTaggedPixelBufferGroupAdaptor =
  sendMessage avAssetWriterInputTaggedPixelBufferGroupAdaptor sourcePixelBufferAttributesSelector

-- | A pixel buffer pool that will vend and efficiently recycle CVPixelBuffer objects of tagged buffer groups that can be appended to the receiver.
--
-- For maximum efficiency, clients should create CVPixelBuffer objects of tagged buffer groups for appendTaggedPixelBufferGroup:withPresentationTime: by using this pool with the CVPixelBufferPoolCreatePixelBuffer() function.
--
-- The value of this property will be NULL before -[AVAssetWriter startWriting] is called on the associated AVAssetWriter object. Clients should read this property after -[AVAssetWriter startWriting] calling to get a non-NULL value.
--
-- This property is not key value observable.
--
-- ObjC selector: @- pixelBufferPool@
pixelBufferPool :: IsAVAssetWriterInputTaggedPixelBufferGroupAdaptor avAssetWriterInputTaggedPixelBufferGroupAdaptor => avAssetWriterInputTaggedPixelBufferGroupAdaptor -> IO (Ptr ())
pixelBufferPool avAssetWriterInputTaggedPixelBufferGroupAdaptor =
  sendMessage avAssetWriterInputTaggedPixelBufferGroupAdaptor pixelBufferPoolSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetWriterInputTaggedPixelBufferGroupAdaptor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetWriterInputTaggedPixelBufferGroupAdaptor)
newSelector = mkSelector "new"

-- | @Selector@ for @assetWriterInputTaggedPixelBufferGroupAdaptorWithAssetWriterInput:sourcePixelBufferAttributes:@
assetWriterInputTaggedPixelBufferGroupAdaptorWithAssetWriterInput_sourcePixelBufferAttributesSelector :: Selector '[Id AVAssetWriterInput, Id NSDictionary] (Id AVAssetWriterInputTaggedPixelBufferGroupAdaptor)
assetWriterInputTaggedPixelBufferGroupAdaptorWithAssetWriterInput_sourcePixelBufferAttributesSelector = mkSelector "assetWriterInputTaggedPixelBufferGroupAdaptorWithAssetWriterInput:sourcePixelBufferAttributes:"

-- | @Selector@ for @initWithAssetWriterInput:sourcePixelBufferAttributes:@
initWithAssetWriterInput_sourcePixelBufferAttributesSelector :: Selector '[Id AVAssetWriterInput, Id NSDictionary] (Id AVAssetWriterInputTaggedPixelBufferGroupAdaptor)
initWithAssetWriterInput_sourcePixelBufferAttributesSelector = mkSelector "initWithAssetWriterInput:sourcePixelBufferAttributes:"

-- | @Selector@ for @assetWriterInput@
assetWriterInputSelector :: Selector '[] (Id AVAssetWriterInput)
assetWriterInputSelector = mkSelector "assetWriterInput"

-- | @Selector@ for @sourcePixelBufferAttributes@
sourcePixelBufferAttributesSelector :: Selector '[] (Id NSDictionary)
sourcePixelBufferAttributesSelector = mkSelector "sourcePixelBufferAttributes"

-- | @Selector@ for @pixelBufferPool@
pixelBufferPoolSelector :: Selector '[] (Ptr ())
pixelBufferPoolSelector = mkSelector "pixelBufferPool"

