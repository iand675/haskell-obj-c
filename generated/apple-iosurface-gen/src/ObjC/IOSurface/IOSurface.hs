{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @IOSurface@.
module ObjC.IOSurface.IOSurface
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
  , baseAddress
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
  , allAttachmentsSelector
  , allocationSizeSelector
  , allowsPixelSizeCastingSelector
  , attachmentForKeySelector
  , baseAddressOfPlaneAtIndexSelector
  , baseAddressSelector
  , bytesPerElementOfPlaneAtIndexSelector
  , bytesPerElementSelector
  , bytesPerRowOfPlaneAtIndexSelector
  , bytesPerRowSelector
  , decrementUseCountSelector
  , elementHeightOfPlaneAtIndexSelector
  , elementHeightSelector
  , elementWidthOfPlaneAtIndexSelector
  , elementWidthSelector
  , heightOfPlaneAtIndexSelector
  , heightSelector
  , inUseSelector
  , incrementUseCountSelector
  , initWithPropertiesSelector
  , localUseCountSelector
  , lockWithOptions_seedSelector
  , pixelFormatSelector
  , planeCountSelector
  , removeAllAttachmentsSelector
  , removeAttachmentForKeySelector
  , seedSelector
  , setAllAttachmentsSelector
  , setAttachment_forKeySelector
  , setPurgeable_oldStateSelector
  , surfaceIDSelector
  , unlockWithOptions_seedSelector
  , widthOfPlaneAtIndexSelector
  , widthSelector

  -- * Enum types
  , IOSurfaceLockOptions(IOSurfaceLockOptions)
  , pattern KIOSurfaceLockReadOnly
  , pattern KIOSurfaceLockAvoidSync
  , IOSurfacePurgeabilityState(IOSurfacePurgeabilityState)
  , pattern KIOSurfacePurgeableNonVolatile
  , pattern KIOSurfacePurgeableVolatile
  , pattern KIOSurfacePurgeableEmpty
  , pattern KIOSurfacePurgeableKeepCurrent

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IOSurface.Internal.Classes
import ObjC.IOSurface.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithProperties:@
initWithProperties :: (IsIOSurface ioSurface, IsNSDictionary properties) => ioSurface -> properties -> IO (Id IOSurface)
initWithProperties ioSurface properties =
  sendOwnedMessage ioSurface initWithPropertiesSelector (toNSDictionary properties)

-- | @- lockWithOptions:seed:@
lockWithOptions_seed :: IsIOSurface ioSurface => ioSurface -> IOSurfaceLockOptions -> Ptr CUInt -> IO CInt
lockWithOptions_seed ioSurface options seed =
  sendMessage ioSurface lockWithOptions_seedSelector options seed

-- | @- unlockWithOptions:seed:@
unlockWithOptions_seed :: IsIOSurface ioSurface => ioSurface -> IOSurfaceLockOptions -> Ptr CUInt -> IO CInt
unlockWithOptions_seed ioSurface options seed =
  sendMessage ioSurface unlockWithOptions_seedSelector options seed

-- | @- widthOfPlaneAtIndex:@
widthOfPlaneAtIndex :: IsIOSurface ioSurface => ioSurface -> CULong -> IO CLong
widthOfPlaneAtIndex ioSurface planeIndex =
  sendMessage ioSurface widthOfPlaneAtIndexSelector planeIndex

-- | @- heightOfPlaneAtIndex:@
heightOfPlaneAtIndex :: IsIOSurface ioSurface => ioSurface -> CULong -> IO CLong
heightOfPlaneAtIndex ioSurface planeIndex =
  sendMessage ioSurface heightOfPlaneAtIndexSelector planeIndex

-- | @- bytesPerRowOfPlaneAtIndex:@
bytesPerRowOfPlaneAtIndex :: IsIOSurface ioSurface => ioSurface -> CULong -> IO CLong
bytesPerRowOfPlaneAtIndex ioSurface planeIndex =
  sendMessage ioSurface bytesPerRowOfPlaneAtIndexSelector planeIndex

-- | @- bytesPerElementOfPlaneAtIndex:@
bytesPerElementOfPlaneAtIndex :: IsIOSurface ioSurface => ioSurface -> CULong -> IO CLong
bytesPerElementOfPlaneAtIndex ioSurface planeIndex =
  sendMessage ioSurface bytesPerElementOfPlaneAtIndexSelector planeIndex

-- | @- elementWidthOfPlaneAtIndex:@
elementWidthOfPlaneAtIndex :: IsIOSurface ioSurface => ioSurface -> CULong -> IO CLong
elementWidthOfPlaneAtIndex ioSurface planeIndex =
  sendMessage ioSurface elementWidthOfPlaneAtIndexSelector planeIndex

-- | @- elementHeightOfPlaneAtIndex:@
elementHeightOfPlaneAtIndex :: IsIOSurface ioSurface => ioSurface -> CULong -> IO CLong
elementHeightOfPlaneAtIndex ioSurface planeIndex =
  sendMessage ioSurface elementHeightOfPlaneAtIndexSelector planeIndex

-- | @- baseAddressOfPlaneAtIndex:@
baseAddressOfPlaneAtIndex :: IsIOSurface ioSurface => ioSurface -> CULong -> IO (Ptr ())
baseAddressOfPlaneAtIndex ioSurface planeIndex =
  sendMessage ioSurface baseAddressOfPlaneAtIndexSelector planeIndex

-- | @- setAttachment:forKey:@
setAttachment_forKey :: (IsIOSurface ioSurface, IsNSString key) => ioSurface -> RawId -> key -> IO ()
setAttachment_forKey ioSurface anObject key =
  sendMessage ioSurface setAttachment_forKeySelector anObject (toNSString key)

-- | @- attachmentForKey:@
attachmentForKey :: (IsIOSurface ioSurface, IsNSString key) => ioSurface -> key -> IO RawId
attachmentForKey ioSurface key =
  sendMessage ioSurface attachmentForKeySelector (toNSString key)

-- | @- removeAttachmentForKey:@
removeAttachmentForKey :: (IsIOSurface ioSurface, IsNSString key) => ioSurface -> key -> IO ()
removeAttachmentForKey ioSurface key =
  sendMessage ioSurface removeAttachmentForKeySelector (toNSString key)

-- | @- setAllAttachments:@
setAllAttachments :: (IsIOSurface ioSurface, IsNSDictionary dict) => ioSurface -> dict -> IO ()
setAllAttachments ioSurface dict =
  sendMessage ioSurface setAllAttachmentsSelector (toNSDictionary dict)

-- | @- allAttachments@
allAttachments :: IsIOSurface ioSurface => ioSurface -> IO (Id NSDictionary)
allAttachments ioSurface =
  sendMessage ioSurface allAttachmentsSelector

-- | @- removeAllAttachments@
removeAllAttachments :: IsIOSurface ioSurface => ioSurface -> IO ()
removeAllAttachments ioSurface =
  sendMessage ioSurface removeAllAttachmentsSelector

-- | @- incrementUseCount@
incrementUseCount :: IsIOSurface ioSurface => ioSurface -> IO ()
incrementUseCount ioSurface =
  sendMessage ioSurface incrementUseCountSelector

-- | @- decrementUseCount@
decrementUseCount :: IsIOSurface ioSurface => ioSurface -> IO ()
decrementUseCount ioSurface =
  sendMessage ioSurface decrementUseCountSelector

-- | @- setPurgeable:oldState:@
setPurgeable_oldState :: IsIOSurface ioSurface => ioSurface -> IOSurfacePurgeabilityState -> Ptr IOSurfacePurgeabilityState -> IO CInt
setPurgeable_oldState ioSurface newState oldState =
  sendMessage ioSurface setPurgeable_oldStateSelector newState oldState

-- | @- allocationSize@
allocationSize :: IsIOSurface ioSurface => ioSurface -> IO CLong
allocationSize ioSurface =
  sendOwnedMessage ioSurface allocationSizeSelector

-- | @- width@
width :: IsIOSurface ioSurface => ioSurface -> IO CLong
width ioSurface =
  sendMessage ioSurface widthSelector

-- | @- height@
height :: IsIOSurface ioSurface => ioSurface -> IO CLong
height ioSurface =
  sendMessage ioSurface heightSelector

-- | @- baseAddress@
baseAddress :: IsIOSurface ioSurface => ioSurface -> IO (Ptr ())
baseAddress ioSurface =
  sendMessage ioSurface baseAddressSelector

-- | @- pixelFormat@
pixelFormat :: IsIOSurface ioSurface => ioSurface -> IO CUInt
pixelFormat ioSurface =
  sendMessage ioSurface pixelFormatSelector

-- | @- bytesPerRow@
bytesPerRow :: IsIOSurface ioSurface => ioSurface -> IO CLong
bytesPerRow ioSurface =
  sendMessage ioSurface bytesPerRowSelector

-- | @- bytesPerElement@
bytesPerElement :: IsIOSurface ioSurface => ioSurface -> IO CLong
bytesPerElement ioSurface =
  sendMessage ioSurface bytesPerElementSelector

-- | @- elementWidth@
elementWidth :: IsIOSurface ioSurface => ioSurface -> IO CLong
elementWidth ioSurface =
  sendMessage ioSurface elementWidthSelector

-- | @- elementHeight@
elementHeight :: IsIOSurface ioSurface => ioSurface -> IO CLong
elementHeight ioSurface =
  sendMessage ioSurface elementHeightSelector

-- | @- surfaceID@
surfaceID :: IsIOSurface ioSurface => ioSurface -> IO CUInt
surfaceID ioSurface =
  sendMessage ioSurface surfaceIDSelector

-- | @- seed@
seed :: IsIOSurface ioSurface => ioSurface -> IO CUInt
seed ioSurface =
  sendMessage ioSurface seedSelector

-- | @- planeCount@
planeCount :: IsIOSurface ioSurface => ioSurface -> IO CULong
planeCount ioSurface =
  sendMessage ioSurface planeCountSelector

-- | @- inUse@
inUse :: IsIOSurface ioSurface => ioSurface -> IO Bool
inUse ioSurface =
  sendMessage ioSurface inUseSelector

-- | @- localUseCount@
localUseCount :: IsIOSurface ioSurface => ioSurface -> IO CInt
localUseCount ioSurface =
  sendMessage ioSurface localUseCountSelector

-- | @- allowsPixelSizeCasting@
allowsPixelSizeCasting :: IsIOSurface ioSurface => ioSurface -> IO Bool
allowsPixelSizeCasting ioSurface =
  sendMessage ioSurface allowsPixelSizeCastingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProperties:@
initWithPropertiesSelector :: Selector '[Id NSDictionary] (Id IOSurface)
initWithPropertiesSelector = mkSelector "initWithProperties:"

-- | @Selector@ for @lockWithOptions:seed:@
lockWithOptions_seedSelector :: Selector '[IOSurfaceLockOptions, Ptr CUInt] CInt
lockWithOptions_seedSelector = mkSelector "lockWithOptions:seed:"

-- | @Selector@ for @unlockWithOptions:seed:@
unlockWithOptions_seedSelector :: Selector '[IOSurfaceLockOptions, Ptr CUInt] CInt
unlockWithOptions_seedSelector = mkSelector "unlockWithOptions:seed:"

-- | @Selector@ for @widthOfPlaneAtIndex:@
widthOfPlaneAtIndexSelector :: Selector '[CULong] CLong
widthOfPlaneAtIndexSelector = mkSelector "widthOfPlaneAtIndex:"

-- | @Selector@ for @heightOfPlaneAtIndex:@
heightOfPlaneAtIndexSelector :: Selector '[CULong] CLong
heightOfPlaneAtIndexSelector = mkSelector "heightOfPlaneAtIndex:"

-- | @Selector@ for @bytesPerRowOfPlaneAtIndex:@
bytesPerRowOfPlaneAtIndexSelector :: Selector '[CULong] CLong
bytesPerRowOfPlaneAtIndexSelector = mkSelector "bytesPerRowOfPlaneAtIndex:"

-- | @Selector@ for @bytesPerElementOfPlaneAtIndex:@
bytesPerElementOfPlaneAtIndexSelector :: Selector '[CULong] CLong
bytesPerElementOfPlaneAtIndexSelector = mkSelector "bytesPerElementOfPlaneAtIndex:"

-- | @Selector@ for @elementWidthOfPlaneAtIndex:@
elementWidthOfPlaneAtIndexSelector :: Selector '[CULong] CLong
elementWidthOfPlaneAtIndexSelector = mkSelector "elementWidthOfPlaneAtIndex:"

-- | @Selector@ for @elementHeightOfPlaneAtIndex:@
elementHeightOfPlaneAtIndexSelector :: Selector '[CULong] CLong
elementHeightOfPlaneAtIndexSelector = mkSelector "elementHeightOfPlaneAtIndex:"

-- | @Selector@ for @baseAddressOfPlaneAtIndex:@
baseAddressOfPlaneAtIndexSelector :: Selector '[CULong] (Ptr ())
baseAddressOfPlaneAtIndexSelector = mkSelector "baseAddressOfPlaneAtIndex:"

-- | @Selector@ for @setAttachment:forKey:@
setAttachment_forKeySelector :: Selector '[RawId, Id NSString] ()
setAttachment_forKeySelector = mkSelector "setAttachment:forKey:"

-- | @Selector@ for @attachmentForKey:@
attachmentForKeySelector :: Selector '[Id NSString] RawId
attachmentForKeySelector = mkSelector "attachmentForKey:"

-- | @Selector@ for @removeAttachmentForKey:@
removeAttachmentForKeySelector :: Selector '[Id NSString] ()
removeAttachmentForKeySelector = mkSelector "removeAttachmentForKey:"

-- | @Selector@ for @setAllAttachments:@
setAllAttachmentsSelector :: Selector '[Id NSDictionary] ()
setAllAttachmentsSelector = mkSelector "setAllAttachments:"

-- | @Selector@ for @allAttachments@
allAttachmentsSelector :: Selector '[] (Id NSDictionary)
allAttachmentsSelector = mkSelector "allAttachments"

-- | @Selector@ for @removeAllAttachments@
removeAllAttachmentsSelector :: Selector '[] ()
removeAllAttachmentsSelector = mkSelector "removeAllAttachments"

-- | @Selector@ for @incrementUseCount@
incrementUseCountSelector :: Selector '[] ()
incrementUseCountSelector = mkSelector "incrementUseCount"

-- | @Selector@ for @decrementUseCount@
decrementUseCountSelector :: Selector '[] ()
decrementUseCountSelector = mkSelector "decrementUseCount"

-- | @Selector@ for @setPurgeable:oldState:@
setPurgeable_oldStateSelector :: Selector '[IOSurfacePurgeabilityState, Ptr IOSurfacePurgeabilityState] CInt
setPurgeable_oldStateSelector = mkSelector "setPurgeable:oldState:"

-- | @Selector@ for @allocationSize@
allocationSizeSelector :: Selector '[] CLong
allocationSizeSelector = mkSelector "allocationSize"

-- | @Selector@ for @width@
widthSelector :: Selector '[] CLong
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector '[] CLong
heightSelector = mkSelector "height"

-- | @Selector@ for @baseAddress@
baseAddressSelector :: Selector '[] (Ptr ())
baseAddressSelector = mkSelector "baseAddress"

-- | @Selector@ for @pixelFormat@
pixelFormatSelector :: Selector '[] CUInt
pixelFormatSelector = mkSelector "pixelFormat"

-- | @Selector@ for @bytesPerRow@
bytesPerRowSelector :: Selector '[] CLong
bytesPerRowSelector = mkSelector "bytesPerRow"

-- | @Selector@ for @bytesPerElement@
bytesPerElementSelector :: Selector '[] CLong
bytesPerElementSelector = mkSelector "bytesPerElement"

-- | @Selector@ for @elementWidth@
elementWidthSelector :: Selector '[] CLong
elementWidthSelector = mkSelector "elementWidth"

-- | @Selector@ for @elementHeight@
elementHeightSelector :: Selector '[] CLong
elementHeightSelector = mkSelector "elementHeight"

-- | @Selector@ for @surfaceID@
surfaceIDSelector :: Selector '[] CUInt
surfaceIDSelector = mkSelector "surfaceID"

-- | @Selector@ for @seed@
seedSelector :: Selector '[] CUInt
seedSelector = mkSelector "seed"

-- | @Selector@ for @planeCount@
planeCountSelector :: Selector '[] CULong
planeCountSelector = mkSelector "planeCount"

-- | @Selector@ for @inUse@
inUseSelector :: Selector '[] Bool
inUseSelector = mkSelector "inUse"

-- | @Selector@ for @localUseCount@
localUseCountSelector :: Selector '[] CInt
localUseCountSelector = mkSelector "localUseCount"

-- | @Selector@ for @allowsPixelSizeCasting@
allowsPixelSizeCastingSelector :: Selector '[] Bool
allowsPixelSizeCastingSelector = mkSelector "allowsPixelSizeCasting"

