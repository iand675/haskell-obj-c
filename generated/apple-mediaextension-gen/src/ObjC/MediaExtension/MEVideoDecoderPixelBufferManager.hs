{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MEVideoDecoderPixelBufferManager
--
-- Describes pixel buffer requirements and creates new pixel buffers.
--
-- Contains the interfaces that the App Extension video decoder uses for two tasks. First, to declare its set of requirements for output CVPixelBuffers in the form of a pixelBufferAttributes dictionary. Second, to create pixelBuffers which match decoder output requirements but also satisfy VideoToolbox and client requirements.
--
-- Generated bindings for @MEVideoDecoderPixelBufferManager@.
module ObjC.MediaExtension.MEVideoDecoderPixelBufferManager
  ( MEVideoDecoderPixelBufferManager
  , IsMEVideoDecoderPixelBufferManager(..)
  , createPixelBufferAndReturnError
  , registerCustomPixelFormat
  , pixelBufferAttributes
  , setPixelBufferAttributes
  , createPixelBufferAndReturnErrorSelector
  , pixelBufferAttributesSelector
  , registerCustomPixelFormatSelector
  , setPixelBufferAttributesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | createPixelBufferAndReturnError:
--
-- Generates a pixel buffer using the session's pixel buffer pool.
--
-- If implemented in Objective-C, the caller is responsible for releasing the returned CVPixelBuffer.
--
-- @error@ — If provided, returns error information in the event that the method fails.
--
-- Returns: A pixel buffer compatible with the extension's most recently set pixelBufferAttributes
--
-- ObjC selector: @- createPixelBufferAndReturnError:@
createPixelBufferAndReturnError :: (IsMEVideoDecoderPixelBufferManager meVideoDecoderPixelBufferManager, IsNSError error_) => meVideoDecoderPixelBufferManager -> error_ -> IO (Ptr ())
createPixelBufferAndReturnError meVideoDecoderPixelBufferManager error_ =
  sendMessage meVideoDecoderPixelBufferManager createPixelBufferAndReturnErrorSelector (toNSError error_)

-- | registerCustomPixelFormat
--
-- VideoToolbox will register the described pixelFormat in both the Extension process and the client process.
--
-- This property is appropriate for decoders which produce output in a custom pixel format.  This will generally only be used by decoders which produce RAW output, where the decoder's output buffers will only be consumed by an MERAWProcessor extension which registers the same pixel format. MERAWProcessor needs to manually register the custom pixel format using CVPixelFormatDescriptionRegisterDescriptionWithPixelFormatType().
--
-- @customPixelFormat@ — This dictionary contains a set of keys and values as described in CoreVideo/CVPixelFormatDescription.h suitable for providing		as the 'description' parameter to CVPixelFormatDescriptionRegisterDescriptionWithPixelFormatType.  This must contain the		custom pixel format fourCC as the value for the kCVPixelFormatCodecType key.
--
-- ObjC selector: @- registerCustomPixelFormat:@
registerCustomPixelFormat :: (IsMEVideoDecoderPixelBufferManager meVideoDecoderPixelBufferManager, IsNSDictionary customPixelFormat) => meVideoDecoderPixelBufferManager -> customPixelFormat -> IO ()
registerCustomPixelFormat meVideoDecoderPixelBufferManager customPixelFormat =
  sendMessage meVideoDecoderPixelBufferManager registerCustomPixelFormatSelector (toNSDictionary customPixelFormat)

-- | pixelBufferAttributes
--
-- VideoToolbox will use these attributes when creating a PixelBuffer for the decoder.
--
-- This can be updated by the decoder before requesting a new pixelBuffer.
--
-- ObjC selector: @- pixelBufferAttributes@
pixelBufferAttributes :: IsMEVideoDecoderPixelBufferManager meVideoDecoderPixelBufferManager => meVideoDecoderPixelBufferManager -> IO (Id NSDictionary)
pixelBufferAttributes meVideoDecoderPixelBufferManager =
  sendMessage meVideoDecoderPixelBufferManager pixelBufferAttributesSelector

-- | pixelBufferAttributes
--
-- VideoToolbox will use these attributes when creating a PixelBuffer for the decoder.
--
-- This can be updated by the decoder before requesting a new pixelBuffer.
--
-- ObjC selector: @- setPixelBufferAttributes:@
setPixelBufferAttributes :: (IsMEVideoDecoderPixelBufferManager meVideoDecoderPixelBufferManager, IsNSDictionary value) => meVideoDecoderPixelBufferManager -> value -> IO ()
setPixelBufferAttributes meVideoDecoderPixelBufferManager value =
  sendMessage meVideoDecoderPixelBufferManager setPixelBufferAttributesSelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createPixelBufferAndReturnError:@
createPixelBufferAndReturnErrorSelector :: Selector '[Id NSError] (Ptr ())
createPixelBufferAndReturnErrorSelector = mkSelector "createPixelBufferAndReturnError:"

-- | @Selector@ for @registerCustomPixelFormat:@
registerCustomPixelFormatSelector :: Selector '[Id NSDictionary] ()
registerCustomPixelFormatSelector = mkSelector "registerCustomPixelFormat:"

-- | @Selector@ for @pixelBufferAttributes@
pixelBufferAttributesSelector :: Selector '[] (Id NSDictionary)
pixelBufferAttributesSelector = mkSelector "pixelBufferAttributes"

-- | @Selector@ for @setPixelBufferAttributes:@
setPixelBufferAttributesSelector :: Selector '[Id NSDictionary] ()
setPixelBufferAttributesSelector = mkSelector "setPixelBufferAttributes:"

