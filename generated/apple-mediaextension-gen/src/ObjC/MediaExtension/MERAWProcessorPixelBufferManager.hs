{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MERAWProcessorPixelBufferManager
--
-- Describes pixel buffer requirements and creates new pixel buffers.
--
-- Contains the interfaces that the App Extension RAW processor uses for two tasks. First, to declare its set of requirements for output CVPixelBuffers in the form of a pixelBufferAttributes dictionary. Second, to create pixelBuffers which match processor output requirements but also satisfy VideoToolbox and client requirements.
--
-- Generated bindings for @MERAWProcessorPixelBufferManager@.
module ObjC.MediaExtension.MERAWProcessorPixelBufferManager
  ( MERAWProcessorPixelBufferManager
  , IsMERAWProcessorPixelBufferManager(..)
  , createPixelBufferAndReturnError
  , pixelBufferAttributes
  , setPixelBufferAttributes
  , createPixelBufferAndReturnErrorSelector
  , pixelBufferAttributesSelector
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
createPixelBufferAndReturnError :: (IsMERAWProcessorPixelBufferManager merawProcessorPixelBufferManager, IsNSError error_) => merawProcessorPixelBufferManager -> error_ -> IO (Ptr ())
createPixelBufferAndReturnError merawProcessorPixelBufferManager error_ =
  sendMessage merawProcessorPixelBufferManager createPixelBufferAndReturnErrorSelector (toNSError error_)

-- | pixelBufferAttributes
--
-- VideoToolbox will use these attributes when creating a pixelBuffer for the RAW Processor.
--
-- This can be updated by the processor before requesting a new pixelBuffer.
--
-- ObjC selector: @- pixelBufferAttributes@
pixelBufferAttributes :: IsMERAWProcessorPixelBufferManager merawProcessorPixelBufferManager => merawProcessorPixelBufferManager -> IO (Id NSDictionary)
pixelBufferAttributes merawProcessorPixelBufferManager =
  sendMessage merawProcessorPixelBufferManager pixelBufferAttributesSelector

-- | pixelBufferAttributes
--
-- VideoToolbox will use these attributes when creating a pixelBuffer for the RAW Processor.
--
-- This can be updated by the processor before requesting a new pixelBuffer.
--
-- ObjC selector: @- setPixelBufferAttributes:@
setPixelBufferAttributes :: (IsMERAWProcessorPixelBufferManager merawProcessorPixelBufferManager, IsNSDictionary value) => merawProcessorPixelBufferManager -> value -> IO ()
setPixelBufferAttributes merawProcessorPixelBufferManager value =
  sendMessage merawProcessorPixelBufferManager setPixelBufferAttributesSelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createPixelBufferAndReturnError:@
createPixelBufferAndReturnErrorSelector :: Selector '[Id NSError] (Ptr ())
createPixelBufferAndReturnErrorSelector = mkSelector "createPixelBufferAndReturnError:"

-- | @Selector@ for @pixelBufferAttributes@
pixelBufferAttributesSelector :: Selector '[] (Id NSDictionary)
pixelBufferAttributesSelector = mkSelector "pixelBufferAttributes"

-- | @Selector@ for @setPixelBufferAttributes:@
setPixelBufferAttributesSelector :: Selector '[Id NSDictionary] ()
setPixelBufferAttributesSelector = mkSelector "setPixelBufferAttributes:"

