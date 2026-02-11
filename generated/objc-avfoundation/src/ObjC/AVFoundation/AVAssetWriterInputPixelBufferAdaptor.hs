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
  , initSelector
  , newSelector
  , assetWriterInputPixelBufferAdaptorWithAssetWriterInput_sourcePixelBufferAttributesSelector
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
init_ :: IsAVAssetWriterInputPixelBufferAdaptor avAssetWriterInputPixelBufferAdaptor => avAssetWriterInputPixelBufferAdaptor -> IO (Id AVAssetWriterInputPixelBufferAdaptor)
init_ avAssetWriterInputPixelBufferAdaptor  =
  sendMsg avAssetWriterInputPixelBufferAdaptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetWriterInputPixelBufferAdaptor)
new  =
  do
    cls' <- getRequiredClass "AVAssetWriterInputPixelBufferAdaptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    withObjCPtr input $ \raw_input ->
      withObjCPtr sourcePixelBufferAttributes $ \raw_sourcePixelBufferAttributes ->
        sendClassMsg cls' (mkSelector "assetWriterInputPixelBufferAdaptorWithAssetWriterInput:sourcePixelBufferAttributes:") (retPtr retVoid) [argPtr (castPtr raw_input :: Ptr ()), argPtr (castPtr raw_sourcePixelBufferAttributes :: Ptr ())] >>= retainedObject . castPtr

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
initWithAssetWriterInput_sourcePixelBufferAttributes avAssetWriterInputPixelBufferAdaptor  input sourcePixelBufferAttributes =
withObjCPtr input $ \raw_input ->
  withObjCPtr sourcePixelBufferAttributes $ \raw_sourcePixelBufferAttributes ->
      sendMsg avAssetWriterInputPixelBufferAdaptor (mkSelector "initWithAssetWriterInput:sourcePixelBufferAttributes:") (retPtr retVoid) [argPtr (castPtr raw_input :: Ptr ()), argPtr (castPtr raw_sourcePixelBufferAttributes :: Ptr ())] >>= ownedObject . castPtr

-- | The asset writer input to which the receiver should append pixel buffers.
--
-- ObjC selector: @- assetWriterInput@
assetWriterInput :: IsAVAssetWriterInputPixelBufferAdaptor avAssetWriterInputPixelBufferAdaptor => avAssetWriterInputPixelBufferAdaptor -> IO (Id AVAssetWriterInput)
assetWriterInput avAssetWriterInputPixelBufferAdaptor  =
  sendMsg avAssetWriterInputPixelBufferAdaptor (mkSelector "assetWriterInput") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The pixel buffer attributes of pixel buffers that will be vended by the receiver's CVPixelBufferPool.
--
-- The value of this property is a dictionary containing pixel buffer attributes keys defined in <CoreVideo/CVPixelBuffer.h>.
--
-- ObjC selector: @- sourcePixelBufferAttributes@
sourcePixelBufferAttributes :: IsAVAssetWriterInputPixelBufferAdaptor avAssetWriterInputPixelBufferAdaptor => avAssetWriterInputPixelBufferAdaptor -> IO (Id NSDictionary)
sourcePixelBufferAttributes avAssetWriterInputPixelBufferAdaptor  =
  sendMsg avAssetWriterInputPixelBufferAdaptor (mkSelector "sourcePixelBufferAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

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
pixelBufferPool avAssetWriterInputPixelBufferAdaptor  =
  fmap castPtr $ sendMsg avAssetWriterInputPixelBufferAdaptor (mkSelector "pixelBufferPool") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @assetWriterInputPixelBufferAdaptorWithAssetWriterInput:sourcePixelBufferAttributes:@
assetWriterInputPixelBufferAdaptorWithAssetWriterInput_sourcePixelBufferAttributesSelector :: Selector
assetWriterInputPixelBufferAdaptorWithAssetWriterInput_sourcePixelBufferAttributesSelector = mkSelector "assetWriterInputPixelBufferAdaptorWithAssetWriterInput:sourcePixelBufferAttributes:"

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

