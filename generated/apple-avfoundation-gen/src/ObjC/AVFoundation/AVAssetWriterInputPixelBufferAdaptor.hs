{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVAssetWriterInputPixelBufferAdaptor@.
module ObjC.AVFoundation.AVAssetWriterInputPixelBufferAdaptor
  ( AVAssetWriterInputPixelBufferAdaptor
  , IsAVAssetWriterInputPixelBufferAdaptor(..)
  , init_
  , new
  , assetWriterInputPixelBufferAdaptorWithAssetWriterInput_sourcePixelBufferAttributes
  , initWithAssetWriterInput_sourcePixelBufferAttributes
  , assetWriterInput
  , sourcePixelBufferAttributes
  , pixelBufferPool
  , assetWriterInputPixelBufferAdaptorWithAssetWriterInput_sourcePixelBufferAttributesSelector
  , assetWriterInputSelector
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
init_ :: IsAVAssetWriterInputPixelBufferAdaptor avAssetWriterInputPixelBufferAdaptor => avAssetWriterInputPixelBufferAdaptor -> IO (Id AVAssetWriterInputPixelBufferAdaptor)
init_ avAssetWriterInputPixelBufferAdaptor =
  sendOwnedMessage avAssetWriterInputPixelBufferAdaptor initSelector

-- | @+ new@
new :: IO (Id AVAssetWriterInputPixelBufferAdaptor)
new  =
  do
    cls' <- getRequiredClass "AVAssetWriterInputPixelBufferAdaptor"
    sendOwnedClassMessage cls' newSelector

-- | Creates a new pixel buffer adaptor to receive pixel buffers for writing to the output file.
--
-- In order to take advantage of the improved efficiency of appending buffers created from the adaptor's pixel buffer pool, clients should specify pixel buffer attributes that most closely accommodate the source format of the video frames being appended.
--
-- Pixel buffer attributes keys for the pixel buffer pool are defined in <CoreVideo/CVPixelBuffer.h>. To specify the pixel format type, the pixelBufferAttributes dictionary should contain a value for kCVPixelBufferPixelFormatTypeKey. For example, use [NSNumber numberWithInt:kCVPixelFormatType_32BGRA] for 8-bit-per-channel BGRA. See the discussion under appendPixelBuffer:withPresentationTime: for advice on choosing a pixel format.
--
-- Clients that do not need a pixel buffer pool for allocating buffers should set sourcePixelBufferAttributes to nil.
--
-- This method throws an exception if the input is already attached to another asset writer input pixel buffer adaptor or if the input has already started writing (the asset writer has progressed beyond AVAssetWriterStatusUnknown).
--
-- - Parameter input: An instance of AVAssetWriterInput to which the receiver should append pixel buffers. Currently, only asset writer inputs that accept media data of type AVMediaTypeVideo can be used to initialize a pixel buffer adaptor. - Parameter sourcePixelBufferAttributes: Specifies the attributes of pixel buffers that will be vended by the input's CVPixelBufferPool.
--
-- - Returns: An instance of AVAssetWriterInputPixelBufferAdaptor.
--
-- ObjC selector: @+ assetWriterInputPixelBufferAdaptorWithAssetWriterInput:sourcePixelBufferAttributes:@
assetWriterInputPixelBufferAdaptorWithAssetWriterInput_sourcePixelBufferAttributes :: (IsAVAssetWriterInput input, IsNSDictionary sourcePixelBufferAttributes) => input -> sourcePixelBufferAttributes -> IO (Id AVAssetWriterInputPixelBufferAdaptor)
assetWriterInputPixelBufferAdaptorWithAssetWriterInput_sourcePixelBufferAttributes input sourcePixelBufferAttributes =
  do
    cls' <- getRequiredClass "AVAssetWriterInputPixelBufferAdaptor"
    sendClassMessage cls' assetWriterInputPixelBufferAdaptorWithAssetWriterInput_sourcePixelBufferAttributesSelector (toAVAssetWriterInput input) (toNSDictionary sourcePixelBufferAttributes)

-- | Creates a new pixel buffer adaptor to receive pixel buffers for writing to the output file.
--
-- In order to take advantage of the improved efficiency of appending buffers created from the adaptor's pixel buffer pool, clients should specify pixel buffer attributes that most closely accommodate the source format of the video frames being appended.
--
-- Pixel buffer attributes keys for the pixel buffer pool are defined in <CoreVideo/CVPixelBuffer.h>. To specify the pixel format type, the pixelBufferAttributes dictionary should contain a value for kCVPixelBufferPixelFormatTypeKey. For example, use [NSNumber numberWithInt:kCVPixelFormatType_32BGRA] for 8-bit-per-channel BGRA. See the discussion under appendPixelBuffer:withPresentationTime: for advice on choosing a pixel format.
--
-- Clients that do not need a pixel buffer pool for allocating buffers should set sourcePixelBufferAttributes to nil.
--
-- This method throws an exception if the input is already attached to another asset writer input pixel buffer adaptor or if the input has already started writing (the asset writer has progressed beyond AVAssetWriterStatusUnknown).
--
-- - Parameter input: An instance of AVAssetWriterInput to which the receiver should append pixel buffers. Currently, only asset writer inputs that accept media data of type AVMediaTypeVideo can be used to initialize a pixel buffer adaptor. - Parameter sourcePixelBufferAttributes: Specifies the attributes of pixel buffers that will be vended by the input's CVPixelBufferPool.
--
-- - Returns: An instance of AVAssetWriterInputPixelBufferAdaptor.
--
-- ObjC selector: @- initWithAssetWriterInput:sourcePixelBufferAttributes:@
initWithAssetWriterInput_sourcePixelBufferAttributes :: (IsAVAssetWriterInputPixelBufferAdaptor avAssetWriterInputPixelBufferAdaptor, IsAVAssetWriterInput input, IsNSDictionary sourcePixelBufferAttributes) => avAssetWriterInputPixelBufferAdaptor -> input -> sourcePixelBufferAttributes -> IO (Id AVAssetWriterInputPixelBufferAdaptor)
initWithAssetWriterInput_sourcePixelBufferAttributes avAssetWriterInputPixelBufferAdaptor input sourcePixelBufferAttributes =
  sendOwnedMessage avAssetWriterInputPixelBufferAdaptor initWithAssetWriterInput_sourcePixelBufferAttributesSelector (toAVAssetWriterInput input) (toNSDictionary sourcePixelBufferAttributes)

-- | The asset writer input to which the receiver should append pixel buffers.
--
-- ObjC selector: @- assetWriterInput@
assetWriterInput :: IsAVAssetWriterInputPixelBufferAdaptor avAssetWriterInputPixelBufferAdaptor => avAssetWriterInputPixelBufferAdaptor -> IO (Id AVAssetWriterInput)
assetWriterInput avAssetWriterInputPixelBufferAdaptor =
  sendMessage avAssetWriterInputPixelBufferAdaptor assetWriterInputSelector

-- | The pixel buffer attributes of pixel buffers that will be vended by the receiver's CVPixelBufferPool.
--
-- The value of this property is a dictionary containing pixel buffer attributes keys defined in <CoreVideo/CVPixelBuffer.h>.
--
-- ObjC selector: @- sourcePixelBufferAttributes@
sourcePixelBufferAttributes :: IsAVAssetWriterInputPixelBufferAdaptor avAssetWriterInputPixelBufferAdaptor => avAssetWriterInputPixelBufferAdaptor -> IO (Id NSDictionary)
sourcePixelBufferAttributes avAssetWriterInputPixelBufferAdaptor =
  sendMessage avAssetWriterInputPixelBufferAdaptor sourcePixelBufferAttributesSelector

-- | A pixel buffer pool that will vend and efficiently recycle CVPixelBuffer objects that can be appended to the receiver.
--
-- For maximum efficiency, clients should create CVPixelBuffer objects for appendPixelBuffer:withPresentationTime: by using this pool with the CVPixelBufferPoolCreatePixelBuffer() function.
--
-- The value of this property will be NULL before -[AVAssetWriter startWriting] is called on the associated AVAssetWriter object.
--
-- This property is key value observable.
--
-- This property throws an exception if a pixel buffer pool cannot be created with this asset writer input pixel buffer adaptor's source pixel buffer attributes (must specify width, height, and either pixel format or pixel format description).
--
-- ObjC selector: @- pixelBufferPool@
pixelBufferPool :: IsAVAssetWriterInputPixelBufferAdaptor avAssetWriterInputPixelBufferAdaptor => avAssetWriterInputPixelBufferAdaptor -> IO (Ptr ())
pixelBufferPool avAssetWriterInputPixelBufferAdaptor =
  sendMessage avAssetWriterInputPixelBufferAdaptor pixelBufferPoolSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetWriterInputPixelBufferAdaptor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetWriterInputPixelBufferAdaptor)
newSelector = mkSelector "new"

-- | @Selector@ for @assetWriterInputPixelBufferAdaptorWithAssetWriterInput:sourcePixelBufferAttributes:@
assetWriterInputPixelBufferAdaptorWithAssetWriterInput_sourcePixelBufferAttributesSelector :: Selector '[Id AVAssetWriterInput, Id NSDictionary] (Id AVAssetWriterInputPixelBufferAdaptor)
assetWriterInputPixelBufferAdaptorWithAssetWriterInput_sourcePixelBufferAttributesSelector = mkSelector "assetWriterInputPixelBufferAdaptorWithAssetWriterInput:sourcePixelBufferAttributes:"

-- | @Selector@ for @initWithAssetWriterInput:sourcePixelBufferAttributes:@
initWithAssetWriterInput_sourcePixelBufferAttributesSelector :: Selector '[Id AVAssetWriterInput, Id NSDictionary] (Id AVAssetWriterInputPixelBufferAdaptor)
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

