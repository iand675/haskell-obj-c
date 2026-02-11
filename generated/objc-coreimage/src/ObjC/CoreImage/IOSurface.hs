{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @IOSurface@.
module ObjC.CoreImage.IOSurface
  ( IOSurface
  , IsIOSurface(..)
  , initWithProperties
  , lockWithOptions_seed
  , unlockWithOptions_seed
  , widthOfPlaneAtIndex
  , heightOfPlaneAtIndex
  , bytesPerRowOfPlaneAtIndex
  , bytesPerElementOfPlaneAtIndex
  , elementWidthOfPlaneAtIndex
  , elementHeightOfPlaneAtIndex
  , baseAddressOfPlaneAtIndex
  , setAttachment_forKey
  , attachmentForKey
  , removeAttachmentForKey
  , setAllAttachments
  , allAttachments
  , removeAllAttachments
  , incrementUseCount
  , decrementUseCount
  , setPurgeable_oldState
  , allocationSize
  , width
  , height
  , pixelFormat
  , bytesPerRow
  , bytesPerElement
  , elementWidth
  , elementHeight
  , surfaceID
  , seed
  , planeCount
  , inUse
  , localUseCount
  , allowsPixelSizeCasting
  , initWithPropertiesSelector
  , lockWithOptions_seedSelector
  , unlockWithOptions_seedSelector
  , widthOfPlaneAtIndexSelector
  , heightOfPlaneAtIndexSelector
  , bytesPerRowOfPlaneAtIndexSelector
  , bytesPerElementOfPlaneAtIndexSelector
  , elementWidthOfPlaneAtIndexSelector
  , elementHeightOfPlaneAtIndexSelector
  , baseAddressOfPlaneAtIndexSelector
  , setAttachment_forKeySelector
  , attachmentForKeySelector
  , removeAttachmentForKeySelector
  , setAllAttachmentsSelector
  , allAttachmentsSelector
  , removeAllAttachmentsSelector
  , incrementUseCountSelector
  , decrementUseCountSelector
  , setPurgeable_oldStateSelector
  , allocationSizeSelector
  , widthSelector
  , heightSelector
  , pixelFormatSelector
  , bytesPerRowSelector
  , bytesPerElementSelector
  , elementWidthSelector
  , elementHeightSelector
  , surfaceIDSelector
  , seedSelector
  , planeCountSelector
  , inUseSelector
  , localUseCountSelector
  , allowsPixelSizeCastingSelector


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

-- | @- initWithProperties:@
initWithProperties :: (IsIOSurface ioSurface, IsNSDictionary properties) => ioSurface -> properties -> IO (Id IOSurface)
initWithProperties ioSurface  properties =
withObjCPtr properties $ \raw_properties ->
    sendMsg ioSurface (mkSelector "initWithProperties:") (retPtr retVoid) [argPtr (castPtr raw_properties :: Ptr ())] >>= ownedObject . castPtr

-- | @- lockWithOptions:seed:@
lockWithOptions_seed :: IsIOSurface ioSurface => ioSurface -> CInt -> Ptr CUInt -> IO CInt
lockWithOptions_seed ioSurface  options seed =
  sendMsg ioSurface (mkSelector "lockWithOptions:seed:") retCInt [argCInt (fromIntegral options), argPtr seed]

-- | @- unlockWithOptions:seed:@
unlockWithOptions_seed :: IsIOSurface ioSurface => ioSurface -> CInt -> Ptr CUInt -> IO CInt
unlockWithOptions_seed ioSurface  options seed =
  sendMsg ioSurface (mkSelector "unlockWithOptions:seed:") retCInt [argCInt (fromIntegral options), argPtr seed]

-- | @- widthOfPlaneAtIndex:@
widthOfPlaneAtIndex :: IsIOSurface ioSurface => ioSurface -> CULong -> IO CLong
widthOfPlaneAtIndex ioSurface  planeIndex =
  sendMsg ioSurface (mkSelector "widthOfPlaneAtIndex:") retCLong [argCULong (fromIntegral planeIndex)]

-- | @- heightOfPlaneAtIndex:@
heightOfPlaneAtIndex :: IsIOSurface ioSurface => ioSurface -> CULong -> IO CLong
heightOfPlaneAtIndex ioSurface  planeIndex =
  sendMsg ioSurface (mkSelector "heightOfPlaneAtIndex:") retCLong [argCULong (fromIntegral planeIndex)]

-- | @- bytesPerRowOfPlaneAtIndex:@
bytesPerRowOfPlaneAtIndex :: IsIOSurface ioSurface => ioSurface -> CULong -> IO CLong
bytesPerRowOfPlaneAtIndex ioSurface  planeIndex =
  sendMsg ioSurface (mkSelector "bytesPerRowOfPlaneAtIndex:") retCLong [argCULong (fromIntegral planeIndex)]

-- | @- bytesPerElementOfPlaneAtIndex:@
bytesPerElementOfPlaneAtIndex :: IsIOSurface ioSurface => ioSurface -> CULong -> IO CLong
bytesPerElementOfPlaneAtIndex ioSurface  planeIndex =
  sendMsg ioSurface (mkSelector "bytesPerElementOfPlaneAtIndex:") retCLong [argCULong (fromIntegral planeIndex)]

-- | @- elementWidthOfPlaneAtIndex:@
elementWidthOfPlaneAtIndex :: IsIOSurface ioSurface => ioSurface -> CULong -> IO CLong
elementWidthOfPlaneAtIndex ioSurface  planeIndex =
  sendMsg ioSurface (mkSelector "elementWidthOfPlaneAtIndex:") retCLong [argCULong (fromIntegral planeIndex)]

-- | @- elementHeightOfPlaneAtIndex:@
elementHeightOfPlaneAtIndex :: IsIOSurface ioSurface => ioSurface -> CULong -> IO CLong
elementHeightOfPlaneAtIndex ioSurface  planeIndex =
  sendMsg ioSurface (mkSelector "elementHeightOfPlaneAtIndex:") retCLong [argCULong (fromIntegral planeIndex)]

-- | @- baseAddressOfPlaneAtIndex:@
baseAddressOfPlaneAtIndex :: IsIOSurface ioSurface => ioSurface -> CULong -> IO (Ptr ())
baseAddressOfPlaneAtIndex ioSurface  planeIndex =
  fmap castPtr $ sendMsg ioSurface (mkSelector "baseAddressOfPlaneAtIndex:") (retPtr retVoid) [argCULong (fromIntegral planeIndex)]

-- | @- setAttachment:forKey:@
setAttachment_forKey :: (IsIOSurface ioSurface, IsNSString key) => ioSurface -> RawId -> key -> IO ()
setAttachment_forKey ioSurface  anObject key =
withObjCPtr key $ \raw_key ->
    sendMsg ioSurface (mkSelector "setAttachment:forKey:") retVoid [argPtr (castPtr (unRawId anObject) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- attachmentForKey:@
attachmentForKey :: (IsIOSurface ioSurface, IsNSString key) => ioSurface -> key -> IO RawId
attachmentForKey ioSurface  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg ioSurface (mkSelector "attachmentForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- removeAttachmentForKey:@
removeAttachmentForKey :: (IsIOSurface ioSurface, IsNSString key) => ioSurface -> key -> IO ()
removeAttachmentForKey ioSurface  key =
withObjCPtr key $ \raw_key ->
    sendMsg ioSurface (mkSelector "removeAttachmentForKey:") retVoid [argPtr (castPtr raw_key :: Ptr ())]

-- | @- setAllAttachments:@
setAllAttachments :: (IsIOSurface ioSurface, IsNSDictionary dict) => ioSurface -> dict -> IO ()
setAllAttachments ioSurface  dict =
withObjCPtr dict $ \raw_dict ->
    sendMsg ioSurface (mkSelector "setAllAttachments:") retVoid [argPtr (castPtr raw_dict :: Ptr ())]

-- | @- allAttachments@
allAttachments :: IsIOSurface ioSurface => ioSurface -> IO (Id NSDictionary)
allAttachments ioSurface  =
  sendMsg ioSurface (mkSelector "allAttachments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- removeAllAttachments@
removeAllAttachments :: IsIOSurface ioSurface => ioSurface -> IO ()
removeAllAttachments ioSurface  =
  sendMsg ioSurface (mkSelector "removeAllAttachments") retVoid []

-- | @- incrementUseCount@
incrementUseCount :: IsIOSurface ioSurface => ioSurface -> IO ()
incrementUseCount ioSurface  =
  sendMsg ioSurface (mkSelector "incrementUseCount") retVoid []

-- | @- decrementUseCount@
decrementUseCount :: IsIOSurface ioSurface => ioSurface -> IO ()
decrementUseCount ioSurface  =
  sendMsg ioSurface (mkSelector "decrementUseCount") retVoid []

-- | @- setPurgeable:oldState:@
setPurgeable_oldState :: IsIOSurface ioSurface => ioSurface -> CInt -> RawId -> IO CInt
setPurgeable_oldState ioSurface  newState oldState =
  sendMsg ioSurface (mkSelector "setPurgeable:oldState:") retCInt [argCInt (fromIntegral newState), argPtr oldState]

-- | @- allocationSize@
allocationSize :: IsIOSurface ioSurface => ioSurface -> IO CLong
allocationSize ioSurface  =
  sendMsg ioSurface (mkSelector "allocationSize") retCLong []

-- | @- width@
width :: IsIOSurface ioSurface => ioSurface -> IO CLong
width ioSurface  =
  sendMsg ioSurface (mkSelector "width") retCLong []

-- | @- height@
height :: IsIOSurface ioSurface => ioSurface -> IO CLong
height ioSurface  =
  sendMsg ioSurface (mkSelector "height") retCLong []

-- | @- pixelFormat@
pixelFormat :: IsIOSurface ioSurface => ioSurface -> IO CUInt
pixelFormat ioSurface  =
  sendMsg ioSurface (mkSelector "pixelFormat") retCUInt []

-- | @- bytesPerRow@
bytesPerRow :: IsIOSurface ioSurface => ioSurface -> IO CLong
bytesPerRow ioSurface  =
  sendMsg ioSurface (mkSelector "bytesPerRow") retCLong []

-- | @- bytesPerElement@
bytesPerElement :: IsIOSurface ioSurface => ioSurface -> IO CLong
bytesPerElement ioSurface  =
  sendMsg ioSurface (mkSelector "bytesPerElement") retCLong []

-- | @- elementWidth@
elementWidth :: IsIOSurface ioSurface => ioSurface -> IO CLong
elementWidth ioSurface  =
  sendMsg ioSurface (mkSelector "elementWidth") retCLong []

-- | @- elementHeight@
elementHeight :: IsIOSurface ioSurface => ioSurface -> IO CLong
elementHeight ioSurface  =
  sendMsg ioSurface (mkSelector "elementHeight") retCLong []

-- | @- surfaceID@
surfaceID :: IsIOSurface ioSurface => ioSurface -> IO CUInt
surfaceID ioSurface  =
  sendMsg ioSurface (mkSelector "surfaceID") retCUInt []

-- | @- seed@
seed :: IsIOSurface ioSurface => ioSurface -> IO CUInt
seed ioSurface  =
  sendMsg ioSurface (mkSelector "seed") retCUInt []

-- | @- planeCount@
planeCount :: IsIOSurface ioSurface => ioSurface -> IO CULong
planeCount ioSurface  =
  sendMsg ioSurface (mkSelector "planeCount") retCULong []

-- | @- inUse@
inUse :: IsIOSurface ioSurface => ioSurface -> IO Bool
inUse ioSurface  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ioSurface (mkSelector "inUse") retCULong []

-- | @- localUseCount@
localUseCount :: IsIOSurface ioSurface => ioSurface -> IO CInt
localUseCount ioSurface  =
  sendMsg ioSurface (mkSelector "localUseCount") retCInt []

-- | @- allowsPixelSizeCasting@
allowsPixelSizeCasting :: IsIOSurface ioSurface => ioSurface -> IO Bool
allowsPixelSizeCasting ioSurface  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ioSurface (mkSelector "allowsPixelSizeCasting") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProperties:@
initWithPropertiesSelector :: Selector
initWithPropertiesSelector = mkSelector "initWithProperties:"

-- | @Selector@ for @lockWithOptions:seed:@
lockWithOptions_seedSelector :: Selector
lockWithOptions_seedSelector = mkSelector "lockWithOptions:seed:"

-- | @Selector@ for @unlockWithOptions:seed:@
unlockWithOptions_seedSelector :: Selector
unlockWithOptions_seedSelector = mkSelector "unlockWithOptions:seed:"

-- | @Selector@ for @widthOfPlaneAtIndex:@
widthOfPlaneAtIndexSelector :: Selector
widthOfPlaneAtIndexSelector = mkSelector "widthOfPlaneAtIndex:"

-- | @Selector@ for @heightOfPlaneAtIndex:@
heightOfPlaneAtIndexSelector :: Selector
heightOfPlaneAtIndexSelector = mkSelector "heightOfPlaneAtIndex:"

-- | @Selector@ for @bytesPerRowOfPlaneAtIndex:@
bytesPerRowOfPlaneAtIndexSelector :: Selector
bytesPerRowOfPlaneAtIndexSelector = mkSelector "bytesPerRowOfPlaneAtIndex:"

-- | @Selector@ for @bytesPerElementOfPlaneAtIndex:@
bytesPerElementOfPlaneAtIndexSelector :: Selector
bytesPerElementOfPlaneAtIndexSelector = mkSelector "bytesPerElementOfPlaneAtIndex:"

-- | @Selector@ for @elementWidthOfPlaneAtIndex:@
elementWidthOfPlaneAtIndexSelector :: Selector
elementWidthOfPlaneAtIndexSelector = mkSelector "elementWidthOfPlaneAtIndex:"

-- | @Selector@ for @elementHeightOfPlaneAtIndex:@
elementHeightOfPlaneAtIndexSelector :: Selector
elementHeightOfPlaneAtIndexSelector = mkSelector "elementHeightOfPlaneAtIndex:"

-- | @Selector@ for @baseAddressOfPlaneAtIndex:@
baseAddressOfPlaneAtIndexSelector :: Selector
baseAddressOfPlaneAtIndexSelector = mkSelector "baseAddressOfPlaneAtIndex:"

-- | @Selector@ for @setAttachment:forKey:@
setAttachment_forKeySelector :: Selector
setAttachment_forKeySelector = mkSelector "setAttachment:forKey:"

-- | @Selector@ for @attachmentForKey:@
attachmentForKeySelector :: Selector
attachmentForKeySelector = mkSelector "attachmentForKey:"

-- | @Selector@ for @removeAttachmentForKey:@
removeAttachmentForKeySelector :: Selector
removeAttachmentForKeySelector = mkSelector "removeAttachmentForKey:"

-- | @Selector@ for @setAllAttachments:@
setAllAttachmentsSelector :: Selector
setAllAttachmentsSelector = mkSelector "setAllAttachments:"

-- | @Selector@ for @allAttachments@
allAttachmentsSelector :: Selector
allAttachmentsSelector = mkSelector "allAttachments"

-- | @Selector@ for @removeAllAttachments@
removeAllAttachmentsSelector :: Selector
removeAllAttachmentsSelector = mkSelector "removeAllAttachments"

-- | @Selector@ for @incrementUseCount@
incrementUseCountSelector :: Selector
incrementUseCountSelector = mkSelector "incrementUseCount"

-- | @Selector@ for @decrementUseCount@
decrementUseCountSelector :: Selector
decrementUseCountSelector = mkSelector "decrementUseCount"

-- | @Selector@ for @setPurgeable:oldState:@
setPurgeable_oldStateSelector :: Selector
setPurgeable_oldStateSelector = mkSelector "setPurgeable:oldState:"

-- | @Selector@ for @allocationSize@
allocationSizeSelector :: Selector
allocationSizeSelector = mkSelector "allocationSize"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @pixelFormat@
pixelFormatSelector :: Selector
pixelFormatSelector = mkSelector "pixelFormat"

-- | @Selector@ for @bytesPerRow@
bytesPerRowSelector :: Selector
bytesPerRowSelector = mkSelector "bytesPerRow"

-- | @Selector@ for @bytesPerElement@
bytesPerElementSelector :: Selector
bytesPerElementSelector = mkSelector "bytesPerElement"

-- | @Selector@ for @elementWidth@
elementWidthSelector :: Selector
elementWidthSelector = mkSelector "elementWidth"

-- | @Selector@ for @elementHeight@
elementHeightSelector :: Selector
elementHeightSelector = mkSelector "elementHeight"

-- | @Selector@ for @surfaceID@
surfaceIDSelector :: Selector
surfaceIDSelector = mkSelector "surfaceID"

-- | @Selector@ for @seed@
seedSelector :: Selector
seedSelector = mkSelector "seed"

-- | @Selector@ for @planeCount@
planeCountSelector :: Selector
planeCountSelector = mkSelector "planeCount"

-- | @Selector@ for @inUse@
inUseSelector :: Selector
inUseSelector = mkSelector "inUse"

-- | @Selector@ for @localUseCount@
localUseCountSelector :: Selector
localUseCountSelector = mkSelector "localUseCount"

-- | @Selector@ for @allowsPixelSizeCasting@
allowsPixelSizeCastingSelector :: Selector
allowsPixelSizeCastingSelector = mkSelector "allowsPixelSizeCasting"

