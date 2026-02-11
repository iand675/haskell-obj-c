{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSBitmapImageRep@.
module ObjC.AppKit.NSBitmapImageRep
  ( NSBitmapImageRep
  , IsNSBitmapImageRep(..)
  , initWithFocusedViewRect
  , initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsPerPixel
  , initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixel
  , initWithCGImage
  , initWithCIImage
  , imageRepsWithData
  , imageRepWithData
  , initWithData
  , getBitmapDataPlanes
  , getCompression_factor
  , setCompression_factor
  , tiffRepresentationUsingCompression_factor
  , tiffRepresentationOfImageRepsInArray
  , tiffRepresentationOfImageRepsInArray_usingCompression_factor
  , getTIFFCompressionTypes_count
  , localizedNameForTIFFCompressionType
  , canBeCompressedUsing
  , colorizeByMappingGray_toColor_blackMapping_whiteMapping
  , initForIncrementalLoad
  , incrementalLoadFromData_complete
  , setColor_atX_y
  , colorAtX_y
  , getPixel_atX_y
  , setPixel_atX_y
  , bitmapImageRepByConvertingToColorSpace_renderingIntent
  , bitmapImageRepByRetaggingWithColorSpace
  , representationOfImageRepsInArray_usingType_properties
  , representationUsingType_properties
  , setProperty_withValue
  , valueForProperty
  , bitmapData
  , planar
  , samplesPerPixel
  , bitsPerPixel
  , bytesPerRow
  , bytesPerPlane
  , numberOfPlanes
  , bitmapFormat
  , tiffRepresentation
  , cgImage
  , colorSpace
  , initWithFocusedViewRectSelector
  , initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsPerPixelSelector
  , initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixelSelector
  , initWithCGImageSelector
  , initWithCIImageSelector
  , imageRepsWithDataSelector
  , imageRepWithDataSelector
  , initWithDataSelector
  , getBitmapDataPlanesSelector
  , getCompression_factorSelector
  , setCompression_factorSelector
  , tiffRepresentationUsingCompression_factorSelector
  , tiffRepresentationOfImageRepsInArraySelector
  , tiffRepresentationOfImageRepsInArray_usingCompression_factorSelector
  , getTIFFCompressionTypes_countSelector
  , localizedNameForTIFFCompressionTypeSelector
  , canBeCompressedUsingSelector
  , colorizeByMappingGray_toColor_blackMapping_whiteMappingSelector
  , initForIncrementalLoadSelector
  , incrementalLoadFromData_completeSelector
  , setColor_atX_ySelector
  , colorAtX_ySelector
  , getPixel_atX_ySelector
  , setPixel_atX_ySelector
  , bitmapImageRepByConvertingToColorSpace_renderingIntentSelector
  , bitmapImageRepByRetaggingWithColorSpaceSelector
  , representationOfImageRepsInArray_usingType_propertiesSelector
  , representationUsingType_propertiesSelector
  , setProperty_withValueSelector
  , valueForPropertySelector
  , bitmapDataSelector
  , planarSelector
  , samplesPerPixelSelector
  , bitsPerPixelSelector
  , bytesPerRowSelector
  , bytesPerPlaneSelector
  , numberOfPlanesSelector
  , bitmapFormatSelector
  , tiffRepresentationSelector
  , cgImageSelector
  , colorSpaceSelector

  -- * Enum types
  , NSBitmapFormat(NSBitmapFormat)
  , pattern NSBitmapFormatAlphaFirst
  , pattern NSBitmapFormatAlphaNonpremultiplied
  , pattern NSBitmapFormatFloatingPointSamples
  , pattern NSBitmapFormatSixteenBitLittleEndian
  , pattern NSBitmapFormatThirtyTwoBitLittleEndian
  , pattern NSBitmapFormatSixteenBitBigEndian
  , pattern NSBitmapFormatThirtyTwoBitBigEndian
  , NSBitmapImageFileType(NSBitmapImageFileType)
  , pattern NSBitmapImageFileTypeTIFF
  , pattern NSBitmapImageFileTypeBMP
  , pattern NSBitmapImageFileTypeGIF
  , pattern NSBitmapImageFileTypeJPEG
  , pattern NSBitmapImageFileTypePNG
  , pattern NSBitmapImageFileTypeJPEG2000
  , NSColorRenderingIntent(NSColorRenderingIntent)
  , pattern NSColorRenderingIntentDefault
  , pattern NSColorRenderingIntentAbsoluteColorimetric
  , pattern NSColorRenderingIntentRelativeColorimetric
  , pattern NSColorRenderingIntentPerceptual
  , pattern NSColorRenderingIntentSaturation
  , NSTIFFCompression(NSTIFFCompression)
  , pattern NSTIFFCompressionNone
  , pattern NSTIFFCompressionCCITTFAX3
  , pattern NSTIFFCompressionCCITTFAX4
  , pattern NSTIFFCompressionLZW
  , pattern NSTIFFCompressionJPEG
  , pattern NSTIFFCompressionNEXT
  , pattern NSTIFFCompressionPackBits
  , pattern NSTIFFCompressionOldJPEG

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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithFocusedViewRect:@
initWithFocusedViewRect :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> NSRect -> IO (Id NSBitmapImageRep)
initWithFocusedViewRect nsBitmapImageRep  rect =
    sendMsg nsBitmapImageRep (mkSelector "initWithFocusedViewRect:") (retPtr retVoid) [argNSRect rect] >>= ownedObject . castPtr

-- | @- initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bytesPerRow:bitsPerPixel:@
initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsPerPixel :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSString colorSpaceName) => nsBitmapImageRep -> Ptr (Ptr CUChar) -> CLong -> CLong -> CLong -> CLong -> Bool -> Bool -> colorSpaceName -> CLong -> CLong -> IO (Id NSBitmapImageRep)
initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsPerPixel nsBitmapImageRep  planes width height bps spp alpha isPlanar colorSpaceName rBytes pBits =
  withObjCPtr colorSpaceName $ \raw_colorSpaceName ->
      sendMsg nsBitmapImageRep (mkSelector "initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bytesPerRow:bitsPerPixel:") (retPtr retVoid) [argPtr planes, argCLong width, argCLong height, argCLong bps, argCLong spp, argCULong (if alpha then 1 else 0), argCULong (if isPlanar then 1 else 0), argPtr (castPtr raw_colorSpaceName :: Ptr ()), argCLong rBytes, argCLong pBits] >>= ownedObject . castPtr

-- | @- initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bitmapFormat:bytesPerRow:bitsPerPixel:@
initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixel :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSString colorSpaceName) => nsBitmapImageRep -> Ptr (Ptr CUChar) -> CLong -> CLong -> CLong -> CLong -> Bool -> Bool -> colorSpaceName -> NSBitmapFormat -> CLong -> CLong -> IO (Id NSBitmapImageRep)
initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixel nsBitmapImageRep  planes width height bps spp alpha isPlanar colorSpaceName bitmapFormat rBytes pBits =
  withObjCPtr colorSpaceName $ \raw_colorSpaceName ->
      sendMsg nsBitmapImageRep (mkSelector "initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bitmapFormat:bytesPerRow:bitsPerPixel:") (retPtr retVoid) [argPtr planes, argCLong width, argCLong height, argCLong bps, argCLong spp, argCULong (if alpha then 1 else 0), argCULong (if isPlanar then 1 else 0), argPtr (castPtr raw_colorSpaceName :: Ptr ()), argCULong (coerce bitmapFormat), argCLong rBytes, argCLong pBits] >>= ownedObject . castPtr

-- | @- initWithCGImage:@
initWithCGImage :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> Ptr () -> IO (Id NSBitmapImageRep)
initWithCGImage nsBitmapImageRep  cgImage =
    sendMsg nsBitmapImageRep (mkSelector "initWithCGImage:") (retPtr retVoid) [argPtr cgImage] >>= ownedObject . castPtr

-- | @- initWithCIImage:@
initWithCIImage :: (IsNSBitmapImageRep nsBitmapImageRep, IsCIImage ciImage) => nsBitmapImageRep -> ciImage -> IO (Id NSBitmapImageRep)
initWithCIImage nsBitmapImageRep  ciImage =
  withObjCPtr ciImage $ \raw_ciImage ->
      sendMsg nsBitmapImageRep (mkSelector "initWithCIImage:") (retPtr retVoid) [argPtr (castPtr raw_ciImage :: Ptr ())] >>= ownedObject . castPtr

-- | @+ imageRepsWithData:@
imageRepsWithData :: IsNSData data_ => data_ -> IO (Id NSArray)
imageRepsWithData data_ =
  do
    cls' <- getRequiredClass "NSBitmapImageRep"
    withObjCPtr data_ $ \raw_data_ ->
      sendClassMsg cls' (mkSelector "imageRepsWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageRepWithData:@
imageRepWithData :: IsNSData data_ => data_ -> IO (Id NSBitmapImageRep)
imageRepWithData data_ =
  do
    cls' <- getRequiredClass "NSBitmapImageRep"
    withObjCPtr data_ $ \raw_data_ ->
      sendClassMsg cls' (mkSelector "imageRepWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithData:@
initWithData :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSData data_) => nsBitmapImageRep -> data_ -> IO (Id NSBitmapImageRep)
initWithData nsBitmapImageRep  data_ =
  withObjCPtr data_ $ \raw_data_ ->
      sendMsg nsBitmapImageRep (mkSelector "initWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- getBitmapDataPlanes:@
getBitmapDataPlanes :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> Ptr (Ptr CUChar) -> IO ()
getBitmapDataPlanes nsBitmapImageRep  data_ =
    sendMsg nsBitmapImageRep (mkSelector "getBitmapDataPlanes:") retVoid [argPtr data_]

-- | @- getCompression:factor:@
getCompression_factor :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> Ptr NSTIFFCompression -> Ptr CFloat -> IO ()
getCompression_factor nsBitmapImageRep  compression factor =
    sendMsg nsBitmapImageRep (mkSelector "getCompression:factor:") retVoid [argPtr compression, argPtr factor]

-- | @- setCompression:factor:@
setCompression_factor :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> NSTIFFCompression -> CFloat -> IO ()
setCompression_factor nsBitmapImageRep  compression factor =
    sendMsg nsBitmapImageRep (mkSelector "setCompression:factor:") retVoid [argCULong (coerce compression), argCFloat factor]

-- | @- TIFFRepresentationUsingCompression:factor:@
tiffRepresentationUsingCompression_factor :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> NSTIFFCompression -> CFloat -> IO (Id NSData)
tiffRepresentationUsingCompression_factor nsBitmapImageRep  comp factor =
    sendMsg nsBitmapImageRep (mkSelector "TIFFRepresentationUsingCompression:factor:") (retPtr retVoid) [argCULong (coerce comp), argCFloat factor] >>= retainedObject . castPtr

-- | @+ TIFFRepresentationOfImageRepsInArray:@
tiffRepresentationOfImageRepsInArray :: IsNSArray array => array -> IO (Id NSData)
tiffRepresentationOfImageRepsInArray array =
  do
    cls' <- getRequiredClass "NSBitmapImageRep"
    withObjCPtr array $ \raw_array ->
      sendClassMsg cls' (mkSelector "TIFFRepresentationOfImageRepsInArray:") (retPtr retVoid) [argPtr (castPtr raw_array :: Ptr ())] >>= retainedObject . castPtr

-- | @+ TIFFRepresentationOfImageRepsInArray:usingCompression:factor:@
tiffRepresentationOfImageRepsInArray_usingCompression_factor :: IsNSArray array => array -> NSTIFFCompression -> CFloat -> IO (Id NSData)
tiffRepresentationOfImageRepsInArray_usingCompression_factor array comp factor =
  do
    cls' <- getRequiredClass "NSBitmapImageRep"
    withObjCPtr array $ \raw_array ->
      sendClassMsg cls' (mkSelector "TIFFRepresentationOfImageRepsInArray:usingCompression:factor:") (retPtr retVoid) [argPtr (castPtr raw_array :: Ptr ()), argCULong (coerce comp), argCFloat factor] >>= retainedObject . castPtr

-- | @+ getTIFFCompressionTypes:count:@
getTIFFCompressionTypes_count :: Const (Ptr NSTIFFCompression) -> Ptr CLong -> IO ()
getTIFFCompressionTypes_count list numTypes =
  do
    cls' <- getRequiredClass "NSBitmapImageRep"
    sendClassMsg cls' (mkSelector "getTIFFCompressionTypes:count:") retVoid [argPtr (unConst list), argPtr numTypes]

-- | @+ localizedNameForTIFFCompressionType:@
localizedNameForTIFFCompressionType :: NSTIFFCompression -> IO (Id NSString)
localizedNameForTIFFCompressionType compression =
  do
    cls' <- getRequiredClass "NSBitmapImageRep"
    sendClassMsg cls' (mkSelector "localizedNameForTIFFCompressionType:") (retPtr retVoid) [argCULong (coerce compression)] >>= retainedObject . castPtr

-- | @- canBeCompressedUsing:@
canBeCompressedUsing :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> NSTIFFCompression -> IO Bool
canBeCompressedUsing nsBitmapImageRep  compression =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBitmapImageRep (mkSelector "canBeCompressedUsing:") retCULong [argCULong (coerce compression)]

-- | @- colorizeByMappingGray:toColor:blackMapping:whiteMapping:@
colorizeByMappingGray_toColor_blackMapping_whiteMapping :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSColor midPointColor, IsNSColor shadowColor, IsNSColor lightColor) => nsBitmapImageRep -> CDouble -> midPointColor -> shadowColor -> lightColor -> IO ()
colorizeByMappingGray_toColor_blackMapping_whiteMapping nsBitmapImageRep  midPoint midPointColor shadowColor lightColor =
  withObjCPtr midPointColor $ \raw_midPointColor ->
    withObjCPtr shadowColor $ \raw_shadowColor ->
      withObjCPtr lightColor $ \raw_lightColor ->
          sendMsg nsBitmapImageRep (mkSelector "colorizeByMappingGray:toColor:blackMapping:whiteMapping:") retVoid [argCDouble midPoint, argPtr (castPtr raw_midPointColor :: Ptr ()), argPtr (castPtr raw_shadowColor :: Ptr ()), argPtr (castPtr raw_lightColor :: Ptr ())]

-- | @- initForIncrementalLoad@
initForIncrementalLoad :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO (Id NSBitmapImageRep)
initForIncrementalLoad nsBitmapImageRep  =
    sendMsg nsBitmapImageRep (mkSelector "initForIncrementalLoad") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- incrementalLoadFromData:complete:@
incrementalLoadFromData_complete :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSData data_) => nsBitmapImageRep -> data_ -> Bool -> IO CLong
incrementalLoadFromData_complete nsBitmapImageRep  data_ complete =
  withObjCPtr data_ $ \raw_data_ ->
      sendMsg nsBitmapImageRep (mkSelector "incrementalLoadFromData:complete:") retCLong [argPtr (castPtr raw_data_ :: Ptr ()), argCULong (if complete then 1 else 0)]

-- | @- setColor:atX:y:@
setColor_atX_y :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSColor color) => nsBitmapImageRep -> color -> CLong -> CLong -> IO ()
setColor_atX_y nsBitmapImageRep  color x y =
  withObjCPtr color $ \raw_color ->
      sendMsg nsBitmapImageRep (mkSelector "setColor:atX:y:") retVoid [argPtr (castPtr raw_color :: Ptr ()), argCLong x, argCLong y]

-- | @- colorAtX:y:@
colorAtX_y :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> CLong -> CLong -> IO (Id NSColor)
colorAtX_y nsBitmapImageRep  x y =
    sendMsg nsBitmapImageRep (mkSelector "colorAtX:y:") (retPtr retVoid) [argCLong x, argCLong y] >>= retainedObject . castPtr

-- | @- getPixel:atX:y:@
getPixel_atX_y :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> Ptr CULong -> CLong -> CLong -> IO ()
getPixel_atX_y nsBitmapImageRep  p x y =
    sendMsg nsBitmapImageRep (mkSelector "getPixel:atX:y:") retVoid [argPtr p, argCLong x, argCLong y]

-- | @- setPixel:atX:y:@
setPixel_atX_y :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> Ptr CULong -> CLong -> CLong -> IO ()
setPixel_atX_y nsBitmapImageRep  p x y =
    sendMsg nsBitmapImageRep (mkSelector "setPixel:atX:y:") retVoid [argPtr p, argCLong x, argCLong y]

-- | @- bitmapImageRepByConvertingToColorSpace:renderingIntent:@
bitmapImageRepByConvertingToColorSpace_renderingIntent :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSColorSpace targetSpace) => nsBitmapImageRep -> targetSpace -> NSColorRenderingIntent -> IO (Id NSBitmapImageRep)
bitmapImageRepByConvertingToColorSpace_renderingIntent nsBitmapImageRep  targetSpace renderingIntent =
  withObjCPtr targetSpace $ \raw_targetSpace ->
      sendMsg nsBitmapImageRep (mkSelector "bitmapImageRepByConvertingToColorSpace:renderingIntent:") (retPtr retVoid) [argPtr (castPtr raw_targetSpace :: Ptr ()), argCLong (coerce renderingIntent)] >>= retainedObject . castPtr

-- | @- bitmapImageRepByRetaggingWithColorSpace:@
bitmapImageRepByRetaggingWithColorSpace :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSColorSpace newSpace) => nsBitmapImageRep -> newSpace -> IO (Id NSBitmapImageRep)
bitmapImageRepByRetaggingWithColorSpace nsBitmapImageRep  newSpace =
  withObjCPtr newSpace $ \raw_newSpace ->
      sendMsg nsBitmapImageRep (mkSelector "bitmapImageRepByRetaggingWithColorSpace:") (retPtr retVoid) [argPtr (castPtr raw_newSpace :: Ptr ())] >>= retainedObject . castPtr

-- | @+ representationOfImageRepsInArray:usingType:properties:@
representationOfImageRepsInArray_usingType_properties :: (IsNSArray imageReps, IsNSDictionary properties) => imageReps -> NSBitmapImageFileType -> properties -> IO (Id NSData)
representationOfImageRepsInArray_usingType_properties imageReps storageType properties =
  do
    cls' <- getRequiredClass "NSBitmapImageRep"
    withObjCPtr imageReps $ \raw_imageReps ->
      withObjCPtr properties $ \raw_properties ->
        sendClassMsg cls' (mkSelector "representationOfImageRepsInArray:usingType:properties:") (retPtr retVoid) [argPtr (castPtr raw_imageReps :: Ptr ()), argCULong (coerce storageType), argPtr (castPtr raw_properties :: Ptr ())] >>= retainedObject . castPtr

-- | @- representationUsingType:properties:@
representationUsingType_properties :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSDictionary properties) => nsBitmapImageRep -> NSBitmapImageFileType -> properties -> IO (Id NSData)
representationUsingType_properties nsBitmapImageRep  storageType properties =
  withObjCPtr properties $ \raw_properties ->
      sendMsg nsBitmapImageRep (mkSelector "representationUsingType:properties:") (retPtr retVoid) [argCULong (coerce storageType), argPtr (castPtr raw_properties :: Ptr ())] >>= retainedObject . castPtr

-- | @- setProperty:withValue:@
setProperty_withValue :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSString property) => nsBitmapImageRep -> property -> RawId -> IO ()
setProperty_withValue nsBitmapImageRep  property value =
  withObjCPtr property $ \raw_property ->
      sendMsg nsBitmapImageRep (mkSelector "setProperty:withValue:") retVoid [argPtr (castPtr raw_property :: Ptr ()), argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- valueForProperty:@
valueForProperty :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSString property) => nsBitmapImageRep -> property -> IO RawId
valueForProperty nsBitmapImageRep  property =
  withObjCPtr property $ \raw_property ->
      fmap (RawId . castPtr) $ sendMsg nsBitmapImageRep (mkSelector "valueForProperty:") (retPtr retVoid) [argPtr (castPtr raw_property :: Ptr ())]

-- | @- bitmapData@
bitmapData :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO RawId
bitmapData nsBitmapImageRep  =
    fmap (RawId . castPtr) $ sendMsg nsBitmapImageRep (mkSelector "bitmapData") (retPtr retVoid) []

-- | @- planar@
planar :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO Bool
planar nsBitmapImageRep  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBitmapImageRep (mkSelector "planar") retCULong []

-- | @- samplesPerPixel@
samplesPerPixel :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO CLong
samplesPerPixel nsBitmapImageRep  =
    sendMsg nsBitmapImageRep (mkSelector "samplesPerPixel") retCLong []

-- | @- bitsPerPixel@
bitsPerPixel :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO CLong
bitsPerPixel nsBitmapImageRep  =
    sendMsg nsBitmapImageRep (mkSelector "bitsPerPixel") retCLong []

-- | @- bytesPerRow@
bytesPerRow :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO CLong
bytesPerRow nsBitmapImageRep  =
    sendMsg nsBitmapImageRep (mkSelector "bytesPerRow") retCLong []

-- | @- bytesPerPlane@
bytesPerPlane :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO CLong
bytesPerPlane nsBitmapImageRep  =
    sendMsg nsBitmapImageRep (mkSelector "bytesPerPlane") retCLong []

-- | @- numberOfPlanes@
numberOfPlanes :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO CLong
numberOfPlanes nsBitmapImageRep  =
    sendMsg nsBitmapImageRep (mkSelector "numberOfPlanes") retCLong []

-- | @- bitmapFormat@
bitmapFormat :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO NSBitmapFormat
bitmapFormat nsBitmapImageRep  =
    fmap (coerce :: CULong -> NSBitmapFormat) $ sendMsg nsBitmapImageRep (mkSelector "bitmapFormat") retCULong []

-- | @- TIFFRepresentation@
tiffRepresentation :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO (Id NSData)
tiffRepresentation nsBitmapImageRep  =
    sendMsg nsBitmapImageRep (mkSelector "TIFFRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- CGImage@
cgImage :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO (Ptr ())
cgImage nsBitmapImageRep  =
    fmap castPtr $ sendMsg nsBitmapImageRep (mkSelector "CGImage") (retPtr retVoid) []

-- | @- colorSpace@
colorSpace :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO (Id NSColorSpace)
colorSpace nsBitmapImageRep  =
    sendMsg nsBitmapImageRep (mkSelector "colorSpace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFocusedViewRect:@
initWithFocusedViewRectSelector :: Selector
initWithFocusedViewRectSelector = mkSelector "initWithFocusedViewRect:"

-- | @Selector@ for @initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bytesPerRow:bitsPerPixel:@
initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsPerPixelSelector :: Selector
initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsPerPixelSelector = mkSelector "initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bytesPerRow:bitsPerPixel:"

-- | @Selector@ for @initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bitmapFormat:bytesPerRow:bitsPerPixel:@
initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixelSelector :: Selector
initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixelSelector = mkSelector "initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bitmapFormat:bytesPerRow:bitsPerPixel:"

-- | @Selector@ for @initWithCGImage:@
initWithCGImageSelector :: Selector
initWithCGImageSelector = mkSelector "initWithCGImage:"

-- | @Selector@ for @initWithCIImage:@
initWithCIImageSelector :: Selector
initWithCIImageSelector = mkSelector "initWithCIImage:"

-- | @Selector@ for @imageRepsWithData:@
imageRepsWithDataSelector :: Selector
imageRepsWithDataSelector = mkSelector "imageRepsWithData:"

-- | @Selector@ for @imageRepWithData:@
imageRepWithDataSelector :: Selector
imageRepWithDataSelector = mkSelector "imageRepWithData:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @getBitmapDataPlanes:@
getBitmapDataPlanesSelector :: Selector
getBitmapDataPlanesSelector = mkSelector "getBitmapDataPlanes:"

-- | @Selector@ for @getCompression:factor:@
getCompression_factorSelector :: Selector
getCompression_factorSelector = mkSelector "getCompression:factor:"

-- | @Selector@ for @setCompression:factor:@
setCompression_factorSelector :: Selector
setCompression_factorSelector = mkSelector "setCompression:factor:"

-- | @Selector@ for @TIFFRepresentationUsingCompression:factor:@
tiffRepresentationUsingCompression_factorSelector :: Selector
tiffRepresentationUsingCompression_factorSelector = mkSelector "TIFFRepresentationUsingCompression:factor:"

-- | @Selector@ for @TIFFRepresentationOfImageRepsInArray:@
tiffRepresentationOfImageRepsInArraySelector :: Selector
tiffRepresentationOfImageRepsInArraySelector = mkSelector "TIFFRepresentationOfImageRepsInArray:"

-- | @Selector@ for @TIFFRepresentationOfImageRepsInArray:usingCompression:factor:@
tiffRepresentationOfImageRepsInArray_usingCompression_factorSelector :: Selector
tiffRepresentationOfImageRepsInArray_usingCompression_factorSelector = mkSelector "TIFFRepresentationOfImageRepsInArray:usingCompression:factor:"

-- | @Selector@ for @getTIFFCompressionTypes:count:@
getTIFFCompressionTypes_countSelector :: Selector
getTIFFCompressionTypes_countSelector = mkSelector "getTIFFCompressionTypes:count:"

-- | @Selector@ for @localizedNameForTIFFCompressionType:@
localizedNameForTIFFCompressionTypeSelector :: Selector
localizedNameForTIFFCompressionTypeSelector = mkSelector "localizedNameForTIFFCompressionType:"

-- | @Selector@ for @canBeCompressedUsing:@
canBeCompressedUsingSelector :: Selector
canBeCompressedUsingSelector = mkSelector "canBeCompressedUsing:"

-- | @Selector@ for @colorizeByMappingGray:toColor:blackMapping:whiteMapping:@
colorizeByMappingGray_toColor_blackMapping_whiteMappingSelector :: Selector
colorizeByMappingGray_toColor_blackMapping_whiteMappingSelector = mkSelector "colorizeByMappingGray:toColor:blackMapping:whiteMapping:"

-- | @Selector@ for @initForIncrementalLoad@
initForIncrementalLoadSelector :: Selector
initForIncrementalLoadSelector = mkSelector "initForIncrementalLoad"

-- | @Selector@ for @incrementalLoadFromData:complete:@
incrementalLoadFromData_completeSelector :: Selector
incrementalLoadFromData_completeSelector = mkSelector "incrementalLoadFromData:complete:"

-- | @Selector@ for @setColor:atX:y:@
setColor_atX_ySelector :: Selector
setColor_atX_ySelector = mkSelector "setColor:atX:y:"

-- | @Selector@ for @colorAtX:y:@
colorAtX_ySelector :: Selector
colorAtX_ySelector = mkSelector "colorAtX:y:"

-- | @Selector@ for @getPixel:atX:y:@
getPixel_atX_ySelector :: Selector
getPixel_atX_ySelector = mkSelector "getPixel:atX:y:"

-- | @Selector@ for @setPixel:atX:y:@
setPixel_atX_ySelector :: Selector
setPixel_atX_ySelector = mkSelector "setPixel:atX:y:"

-- | @Selector@ for @bitmapImageRepByConvertingToColorSpace:renderingIntent:@
bitmapImageRepByConvertingToColorSpace_renderingIntentSelector :: Selector
bitmapImageRepByConvertingToColorSpace_renderingIntentSelector = mkSelector "bitmapImageRepByConvertingToColorSpace:renderingIntent:"

-- | @Selector@ for @bitmapImageRepByRetaggingWithColorSpace:@
bitmapImageRepByRetaggingWithColorSpaceSelector :: Selector
bitmapImageRepByRetaggingWithColorSpaceSelector = mkSelector "bitmapImageRepByRetaggingWithColorSpace:"

-- | @Selector@ for @representationOfImageRepsInArray:usingType:properties:@
representationOfImageRepsInArray_usingType_propertiesSelector :: Selector
representationOfImageRepsInArray_usingType_propertiesSelector = mkSelector "representationOfImageRepsInArray:usingType:properties:"

-- | @Selector@ for @representationUsingType:properties:@
representationUsingType_propertiesSelector :: Selector
representationUsingType_propertiesSelector = mkSelector "representationUsingType:properties:"

-- | @Selector@ for @setProperty:withValue:@
setProperty_withValueSelector :: Selector
setProperty_withValueSelector = mkSelector "setProperty:withValue:"

-- | @Selector@ for @valueForProperty:@
valueForPropertySelector :: Selector
valueForPropertySelector = mkSelector "valueForProperty:"

-- | @Selector@ for @bitmapData@
bitmapDataSelector :: Selector
bitmapDataSelector = mkSelector "bitmapData"

-- | @Selector@ for @planar@
planarSelector :: Selector
planarSelector = mkSelector "planar"

-- | @Selector@ for @samplesPerPixel@
samplesPerPixelSelector :: Selector
samplesPerPixelSelector = mkSelector "samplesPerPixel"

-- | @Selector@ for @bitsPerPixel@
bitsPerPixelSelector :: Selector
bitsPerPixelSelector = mkSelector "bitsPerPixel"

-- | @Selector@ for @bytesPerRow@
bytesPerRowSelector :: Selector
bytesPerRowSelector = mkSelector "bytesPerRow"

-- | @Selector@ for @bytesPerPlane@
bytesPerPlaneSelector :: Selector
bytesPerPlaneSelector = mkSelector "bytesPerPlane"

-- | @Selector@ for @numberOfPlanes@
numberOfPlanesSelector :: Selector
numberOfPlanesSelector = mkSelector "numberOfPlanes"

-- | @Selector@ for @bitmapFormat@
bitmapFormatSelector :: Selector
bitmapFormatSelector = mkSelector "bitmapFormat"

-- | @Selector@ for @TIFFRepresentation@
tiffRepresentationSelector :: Selector
tiffRepresentationSelector = mkSelector "TIFFRepresentation"

-- | @Selector@ for @CGImage@
cgImageSelector :: Selector
cgImageSelector = mkSelector "CGImage"

-- | @Selector@ for @colorSpace@
colorSpaceSelector :: Selector
colorSpaceSelector = mkSelector "colorSpace"

