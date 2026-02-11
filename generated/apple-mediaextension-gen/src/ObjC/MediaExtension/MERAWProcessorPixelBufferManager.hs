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
createPixelBufferAndReturnError merawProcessorPixelBufferManager  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap castPtr $ sendMsg merawProcessorPixelBufferManager (mkSelector "createPixelBufferAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())]

-- | pixelBufferAttributes
--
-- VideoToolbox will use these attributes when creating a pixelBuffer for the RAW Processor.
--
-- This can be updated by the processor before requesting a new pixelBuffer.
--
-- ObjC selector: @- pixelBufferAttributes@
pixelBufferAttributes :: IsMERAWProcessorPixelBufferManager merawProcessorPixelBufferManager => merawProcessorPixelBufferManager -> IO (Id NSDictionary)
pixelBufferAttributes merawProcessorPixelBufferManager  =
    sendMsg merawProcessorPixelBufferManager (mkSelector "pixelBufferAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | pixelBufferAttributes
--
-- VideoToolbox will use these attributes when creating a pixelBuffer for the RAW Processor.
--
-- This can be updated by the processor before requesting a new pixelBuffer.
--
-- ObjC selector: @- setPixelBufferAttributes:@
setPixelBufferAttributes :: (IsMERAWProcessorPixelBufferManager merawProcessorPixelBufferManager, IsNSDictionary value) => merawProcessorPixelBufferManager -> value -> IO ()
setPixelBufferAttributes merawProcessorPixelBufferManager  value =
  withObjCPtr value $ \raw_value ->
      sendMsg merawProcessorPixelBufferManager (mkSelector "setPixelBufferAttributes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createPixelBufferAndReturnError:@
createPixelBufferAndReturnErrorSelector :: Selector
createPixelBufferAndReturnErrorSelector = mkSelector "createPixelBufferAndReturnError:"

-- | @Selector@ for @pixelBufferAttributes@
pixelBufferAttributesSelector :: Selector
pixelBufferAttributesSelector = mkSelector "pixelBufferAttributes"

-- | @Selector@ for @setPixelBufferAttributes:@
setPixelBufferAttributesSelector :: Selector
setPixelBufferAttributesSelector = mkSelector "setPixelBufferAttributes:"

