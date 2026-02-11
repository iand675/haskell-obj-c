{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSObject@.
module ObjC.CoreImage.NSObject
  ( NSObject
  , IsNSObject(..)
  , provideImageData_bytesPerRow_origin__size__userInfo
  , provideImageToMTLTexture_commandBuffer_originx_originy_width_height_userInfo
  , provideImageData_bytesPerRow_origin__size__userInfoSelector
  , provideImageToMTLTexture_commandBuffer_originx_originy_width_height_userInfoSelector


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

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The method that an image provider object must implement.   This method provides pixel data when the image object is rendered.
--
-- The implementation should provide pixels for the requested sub-rect @x,y,width,height@ of the image.  The sub-rect is in defined in the image's local coordinate space,  where the origin is relative to the top left corner of the image.
--
-- By default, this method will be called to request the full image regardless of what sub-rect is needed for the current render. In this case the requested @x,y,width,height@ will be @0,0,imageWidth,imageHeight@
--
-- If the ``kCIImageProviderTileSize`` option is specified when the ``CIImage`` was created, then this method may be called once for each tile that is needed for the current render.
--
-- - Parameters:   - data: A pointer into which the provider should copy the pixels for the requested sub-rect.    - rowbytes: The number of bytes per row for the requested pixels.   - originx: The x origin of the requested sub-rect relative to the upper left corner of the image.   - originy: The y origin of the requested sub-rect relative to the upper left corner of the image.   - width: The width of the requested sub-rect.   - height: The height of the requested sub-rect.   - info: The value of the @kCIImageProviderTileSize@` option specified when calling:           * ``/CIImage/imageWithImageProvider:size::format:colorSpace:options:``           * ``/CIImage/initWithImageProvider:size::format:colorSpace:options:``
--
-- ObjC selector: @- provideImageData:bytesPerRow:origin::size::userInfo:@
provideImageData_bytesPerRow_origin__size__userInfo :: IsNSObject nsObject => nsObject -> Ptr () -> CULong -> CULong -> CULong -> CULong -> CULong -> RawId -> IO ()
provideImageData_bytesPerRow_origin__size__userInfo nsObject  data_ rowbytes originx originy width height info =
  sendMsg nsObject (mkSelector "provideImageData:bytesPerRow:origin::size::userInfo:") retVoid [argPtr data_, argCULong (fromIntegral rowbytes), argCULong (fromIntegral originx), argCULong (fromIntegral originy), argCULong (fromIntegral width), argCULong (fromIntegral height), argPtr (castPtr (unRawId info) :: Ptr ())]

-- | An optional method that an image provider object way implement.   With this method, the provider object can use the Metal API to provide pixel   data into a MTLTexture when the image object is rendered.
--
-- The implementation should provide pixels for the requested sub-rect @x,y,width,height@ of the image.  The sub-rect is in defined in the image's local coordinate space,  where the origin is relative to the top left corner of the image.
--
-- The work to fill the @MTLTexture@ should be encoded on the specified @commandBuffer@.   If the implementation uses its own commandBuffer,  then it should call @waitUntilCompleted@ before returning.  If the texture is surface-backed then you only need to call @waitUntilScheduled@ before returning.
--
-- By default, this method will be called to request the full image regardless of what sub-rect is needed for the current render. In this case the requested @x,y,width,height@ will be @0,0,imageWidth,imageHeight@
--
-- If the ``kCIImageProviderTileSize`` option is specified when the ``CIImage`` was created, then this method may be called once for each tile that is needed for the current render.
--
-- - Parameters:   - texture: The `<id>MTLTexture` into which the provider should copy the pixels for the requested sub-rect.    - commandBuffer: The `<id>MTLCommandBuffer` that the provider should use encoded the copy.   - originx: The x origin of the requested sub-rect relative to the upper left corner of the image.   - originy: The y origin of the requested sub-rect relative to the upper left corner of the image.   - width: The width of the requested sub-rect.   - height: The height of the requested sub-rect.   - info: The value of the @kCIImageProviderTileSize@` option specified when calling:           * ``/CIImage/imageWithImageProvider:size::format:colorSpace:options:``           * ``/CIImage/initWithImageProvider:size::format:colorSpace:options:``
--
-- ObjC selector: @- provideImageToMTLTexture:commandBuffer:originx:originy:width:height:userInfo:@
provideImageToMTLTexture_commandBuffer_originx_originy_width_height_userInfo :: IsNSObject nsObject => nsObject -> RawId -> RawId -> CULong -> CULong -> CULong -> CULong -> RawId -> IO ()
provideImageToMTLTexture_commandBuffer_originx_originy_width_height_userInfo nsObject  texture commandBuffer originx originy width height info =
  sendMsg nsObject (mkSelector "provideImageToMTLTexture:commandBuffer:originx:originy:width:height:userInfo:") retVoid [argPtr (castPtr (unRawId texture) :: Ptr ()), argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argCULong (fromIntegral originx), argCULong (fromIntegral originy), argCULong (fromIntegral width), argCULong (fromIntegral height), argPtr (castPtr (unRawId info) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @provideImageData:bytesPerRow:origin::size::userInfo:@
provideImageData_bytesPerRow_origin__size__userInfoSelector :: Selector
provideImageData_bytesPerRow_origin__size__userInfoSelector = mkSelector "provideImageData:bytesPerRow:origin::size::userInfo:"

-- | @Selector@ for @provideImageToMTLTexture:commandBuffer:originx:originy:width:height:userInfo:@
provideImageToMTLTexture_commandBuffer_originx_originy_width_height_userInfoSelector :: Selector
provideImageToMTLTexture_commandBuffer_originx_originy_width_height_userInfoSelector = mkSelector "provideImageToMTLTexture:commandBuffer:originx:originy:width:height:userInfo:"

