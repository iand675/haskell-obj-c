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
  , initSelector
  , newSelector
  , assetWriterInputTaggedPixelBufferGroupAdaptorWithAssetWriterInput_sourcePixelBufferAttributesSelector
  , initWithAssetWriterInput_sourcePixelBufferAttributesSelector
  , assetWriterInputSelector
  , sourcePixelBufferAttributesSelector
  , pixelBufferPoolSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetWriterInputTaggedPixelBufferGroupAdaptor avAssetWriterInputTaggedPixelBufferGroupAdaptor => avAssetWriterInputTaggedPixelBufferGroupAdaptor -> IO (Id AVAssetWriterInputTaggedPixelBufferGroupAdaptor)
init_ avAssetWriterInputTaggedPixelBufferGroupAdaptor  =
  sendMsg avAssetWriterInputTaggedPixelBufferGroupAdaptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetWriterInputTaggedPixelBufferGroupAdaptor)
new  =
  do
    cls' <- getRequiredClass "AVAssetWriterInputTaggedPixelBufferGroupAdaptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    withObjCPtr input $ \raw_input ->
      withObjCPtr sourcePixelBufferAttributes $ \raw_sourcePixelBufferAttributes ->
        sendClassMsg cls' (mkSelector "assetWriterInputTaggedPixelBufferGroupAdaptorWithAssetWriterInput:sourcePixelBufferAttributes:") (retPtr retVoid) [argPtr (castPtr raw_input :: Ptr ()), argPtr (castPtr raw_sourcePixelBufferAttributes :: Ptr ())] >>= retainedObject . castPtr

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
initWithAssetWriterInput_sourcePixelBufferAttributes avAssetWriterInputTaggedPixelBufferGroupAdaptor  input sourcePixelBufferAttributes =
withObjCPtr input $ \raw_input ->
  withObjCPtr sourcePixelBufferAttributes $ \raw_sourcePixelBufferAttributes ->
      sendMsg avAssetWriterInputTaggedPixelBufferGroupAdaptor (mkSelector "initWithAssetWriterInput:sourcePixelBufferAttributes:") (retPtr retVoid) [argPtr (castPtr raw_input :: Ptr ()), argPtr (castPtr raw_sourcePixelBufferAttributes :: Ptr ())] >>= ownedObject . castPtr

-- | The asset writer input to which the receiver should append tagged buffer groups.
--
-- ObjC selector: @- assetWriterInput@
assetWriterInput :: IsAVAssetWriterInputTaggedPixelBufferGroupAdaptor avAssetWriterInputTaggedPixelBufferGroupAdaptor => avAssetWriterInputTaggedPixelBufferGroupAdaptor -> IO (Id AVAssetWriterInput)
assetWriterInput avAssetWriterInputTaggedPixelBufferGroupAdaptor  =
  sendMsg avAssetWriterInputTaggedPixelBufferGroupAdaptor (mkSelector "assetWriterInput") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The pixel buffer attributes of pixel buffers that will be vended by the receiver's CVPixelBufferPool.
--
-- The value of this property is a dictionary containing pixel buffer attributes keys defined in <CoreVideo/CVPixelBuffer.h>.
--
-- ObjC selector: @- sourcePixelBufferAttributes@
sourcePixelBufferAttributes :: IsAVAssetWriterInputTaggedPixelBufferGroupAdaptor avAssetWriterInputTaggedPixelBufferGroupAdaptor => avAssetWriterInputTaggedPixelBufferGroupAdaptor -> IO (Id NSDictionary)
sourcePixelBufferAttributes avAssetWriterInputTaggedPixelBufferGroupAdaptor  =
  sendMsg avAssetWriterInputTaggedPixelBufferGroupAdaptor (mkSelector "sourcePixelBufferAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

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
pixelBufferPool avAssetWriterInputTaggedPixelBufferGroupAdaptor  =
  fmap castPtr $ sendMsg avAssetWriterInputTaggedPixelBufferGroupAdaptor (mkSelector "pixelBufferPool") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @assetWriterInputTaggedPixelBufferGroupAdaptorWithAssetWriterInput:sourcePixelBufferAttributes:@
assetWriterInputTaggedPixelBufferGroupAdaptorWithAssetWriterInput_sourcePixelBufferAttributesSelector :: Selector
assetWriterInputTaggedPixelBufferGroupAdaptorWithAssetWriterInput_sourcePixelBufferAttributesSelector = mkSelector "assetWriterInputTaggedPixelBufferGroupAdaptorWithAssetWriterInput:sourcePixelBufferAttributes:"

-- | @Selector@ for @initWithAssetWriterInput:sourcePixelBufferAttributes:@
initWithAssetWriterInput_sourcePixelBufferAttributesSelector :: Selector
initWithAssetWriterInput_sourcePixelBufferAttributesSelector = mkSelector "initWithAssetWriterInput:sourcePixelBufferAttributes:"

-- | @Selector@ for @assetWriterInput@
assetWriterInputSelector :: Selector
assetWriterInputSelector = mkSelector "assetWriterInput"

-- | @Selector@ for @sourcePixelBufferAttributes@
sourcePixelBufferAttributesSelector :: Selector
sourcePixelBufferAttributesSelector = mkSelector "sourcePixelBufferAttributes"

-- | @Selector@ for @pixelBufferPool@
pixelBufferPoolSelector :: Selector
pixelBufferPoolSelector = mkSelector "pixelBufferPool"

