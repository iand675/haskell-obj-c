{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , bitmapDataSelector
  , bitmapFormatSelector
  , bitmapImageRepByConvertingToColorSpace_renderingIntentSelector
  , bitmapImageRepByRetaggingWithColorSpaceSelector
  , bitsPerPixelSelector
  , bytesPerPlaneSelector
  , bytesPerRowSelector
  , canBeCompressedUsingSelector
  , cgImageSelector
  , colorAtX_ySelector
  , colorSpaceSelector
  , colorizeByMappingGray_toColor_blackMapping_whiteMappingSelector
  , getBitmapDataPlanesSelector
  , getCompression_factorSelector
  , getPixel_atX_ySelector
  , getTIFFCompressionTypes_countSelector
  , imageRepWithDataSelector
  , imageRepsWithDataSelector
  , incrementalLoadFromData_completeSelector
  , initForIncrementalLoadSelector
  , initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixelSelector
  , initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsPerPixelSelector
  , initWithCGImageSelector
  , initWithCIImageSelector
  , initWithDataSelector
  , initWithFocusedViewRectSelector
  , localizedNameForTIFFCompressionTypeSelector
  , numberOfPlanesSelector
  , planarSelector
  , representationOfImageRepsInArray_usingType_propertiesSelector
  , representationUsingType_propertiesSelector
  , samplesPerPixelSelector
  , setColor_atX_ySelector
  , setCompression_factorSelector
  , setPixel_atX_ySelector
  , setProperty_withValueSelector
  , tiffRepresentationOfImageRepsInArraySelector
  , tiffRepresentationOfImageRepsInArray_usingCompression_factorSelector
  , tiffRepresentationSelector
  , tiffRepresentationUsingCompression_factorSelector
  , valueForPropertySelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithFocusedViewRect:@
initWithFocusedViewRect :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> NSRect -> IO (Id NSBitmapImageRep)
initWithFocusedViewRect nsBitmapImageRep rect =
  sendOwnedMessage nsBitmapImageRep initWithFocusedViewRectSelector rect

-- | @- initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bytesPerRow:bitsPerPixel:@
initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsPerPixel :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSString colorSpaceName) => nsBitmapImageRep -> Ptr (Ptr CUChar) -> CLong -> CLong -> CLong -> CLong -> Bool -> Bool -> colorSpaceName -> CLong -> CLong -> IO (Id NSBitmapImageRep)
initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsPerPixel nsBitmapImageRep planes width height bps spp alpha isPlanar colorSpaceName rBytes pBits =
  sendOwnedMessage nsBitmapImageRep initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsPerPixelSelector planes width height bps spp alpha isPlanar (toNSString colorSpaceName) rBytes pBits

-- | @- initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bitmapFormat:bytesPerRow:bitsPerPixel:@
initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixel :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSString colorSpaceName) => nsBitmapImageRep -> Ptr (Ptr CUChar) -> CLong -> CLong -> CLong -> CLong -> Bool -> Bool -> colorSpaceName -> NSBitmapFormat -> CLong -> CLong -> IO (Id NSBitmapImageRep)
initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixel nsBitmapImageRep planes width height bps spp alpha isPlanar colorSpaceName bitmapFormat rBytes pBits =
  sendOwnedMessage nsBitmapImageRep initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixelSelector planes width height bps spp alpha isPlanar (toNSString colorSpaceName) bitmapFormat rBytes pBits

-- | @- initWithCGImage:@
initWithCGImage :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> Ptr () -> IO (Id NSBitmapImageRep)
initWithCGImage nsBitmapImageRep cgImage =
  sendOwnedMessage nsBitmapImageRep initWithCGImageSelector cgImage

-- | @- initWithCIImage:@
initWithCIImage :: (IsNSBitmapImageRep nsBitmapImageRep, IsCIImage ciImage) => nsBitmapImageRep -> ciImage -> IO (Id NSBitmapImageRep)
initWithCIImage nsBitmapImageRep ciImage =
  sendOwnedMessage nsBitmapImageRep initWithCIImageSelector (toCIImage ciImage)

-- | @+ imageRepsWithData:@
imageRepsWithData :: IsNSData data_ => data_ -> IO (Id NSArray)
imageRepsWithData data_ =
  do
    cls' <- getRequiredClass "NSBitmapImageRep"
    sendClassMessage cls' imageRepsWithDataSelector (toNSData data_)

-- | @+ imageRepWithData:@
imageRepWithData :: IsNSData data_ => data_ -> IO (Id NSBitmapImageRep)
imageRepWithData data_ =
  do
    cls' <- getRequiredClass "NSBitmapImageRep"
    sendClassMessage cls' imageRepWithDataSelector (toNSData data_)

-- | @- initWithData:@
initWithData :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSData data_) => nsBitmapImageRep -> data_ -> IO (Id NSBitmapImageRep)
initWithData nsBitmapImageRep data_ =
  sendOwnedMessage nsBitmapImageRep initWithDataSelector (toNSData data_)

-- | @- getBitmapDataPlanes:@
getBitmapDataPlanes :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> Ptr (Ptr CUChar) -> IO ()
getBitmapDataPlanes nsBitmapImageRep data_ =
  sendMessage nsBitmapImageRep getBitmapDataPlanesSelector data_

-- | @- getCompression:factor:@
getCompression_factor :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> Ptr NSTIFFCompression -> Ptr CFloat -> IO ()
getCompression_factor nsBitmapImageRep compression factor =
  sendMessage nsBitmapImageRep getCompression_factorSelector compression factor

-- | @- setCompression:factor:@
setCompression_factor :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> NSTIFFCompression -> CFloat -> IO ()
setCompression_factor nsBitmapImageRep compression factor =
  sendMessage nsBitmapImageRep setCompression_factorSelector compression factor

-- | @- TIFFRepresentationUsingCompression:factor:@
tiffRepresentationUsingCompression_factor :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> NSTIFFCompression -> CFloat -> IO (Id NSData)
tiffRepresentationUsingCompression_factor nsBitmapImageRep comp factor =
  sendMessage nsBitmapImageRep tiffRepresentationUsingCompression_factorSelector comp factor

-- | @+ TIFFRepresentationOfImageRepsInArray:@
tiffRepresentationOfImageRepsInArray :: IsNSArray array => array -> IO (Id NSData)
tiffRepresentationOfImageRepsInArray array =
  do
    cls' <- getRequiredClass "NSBitmapImageRep"
    sendClassMessage cls' tiffRepresentationOfImageRepsInArraySelector (toNSArray array)

-- | @+ TIFFRepresentationOfImageRepsInArray:usingCompression:factor:@
tiffRepresentationOfImageRepsInArray_usingCompression_factor :: IsNSArray array => array -> NSTIFFCompression -> CFloat -> IO (Id NSData)
tiffRepresentationOfImageRepsInArray_usingCompression_factor array comp factor =
  do
    cls' <- getRequiredClass "NSBitmapImageRep"
    sendClassMessage cls' tiffRepresentationOfImageRepsInArray_usingCompression_factorSelector (toNSArray array) comp factor

-- | @+ getTIFFCompressionTypes:count:@
getTIFFCompressionTypes_count :: Const (Ptr NSTIFFCompression) -> Ptr CLong -> IO ()
getTIFFCompressionTypes_count list numTypes =
  do
    cls' <- getRequiredClass "NSBitmapImageRep"
    sendClassMessage cls' getTIFFCompressionTypes_countSelector list numTypes

-- | @+ localizedNameForTIFFCompressionType:@
localizedNameForTIFFCompressionType :: NSTIFFCompression -> IO (Id NSString)
localizedNameForTIFFCompressionType compression =
  do
    cls' <- getRequiredClass "NSBitmapImageRep"
    sendClassMessage cls' localizedNameForTIFFCompressionTypeSelector compression

-- | @- canBeCompressedUsing:@
canBeCompressedUsing :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> NSTIFFCompression -> IO Bool
canBeCompressedUsing nsBitmapImageRep compression =
  sendMessage nsBitmapImageRep canBeCompressedUsingSelector compression

-- | @- colorizeByMappingGray:toColor:blackMapping:whiteMapping:@
colorizeByMappingGray_toColor_blackMapping_whiteMapping :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSColor midPointColor, IsNSColor shadowColor, IsNSColor lightColor) => nsBitmapImageRep -> CDouble -> midPointColor -> shadowColor -> lightColor -> IO ()
colorizeByMappingGray_toColor_blackMapping_whiteMapping nsBitmapImageRep midPoint midPointColor shadowColor lightColor =
  sendMessage nsBitmapImageRep colorizeByMappingGray_toColor_blackMapping_whiteMappingSelector midPoint (toNSColor midPointColor) (toNSColor shadowColor) (toNSColor lightColor)

-- | @- initForIncrementalLoad@
initForIncrementalLoad :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO (Id NSBitmapImageRep)
initForIncrementalLoad nsBitmapImageRep =
  sendOwnedMessage nsBitmapImageRep initForIncrementalLoadSelector

-- | @- incrementalLoadFromData:complete:@
incrementalLoadFromData_complete :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSData data_) => nsBitmapImageRep -> data_ -> Bool -> IO CLong
incrementalLoadFromData_complete nsBitmapImageRep data_ complete =
  sendMessage nsBitmapImageRep incrementalLoadFromData_completeSelector (toNSData data_) complete

-- | @- setColor:atX:y:@
setColor_atX_y :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSColor color) => nsBitmapImageRep -> color -> CLong -> CLong -> IO ()
setColor_atX_y nsBitmapImageRep color x y =
  sendMessage nsBitmapImageRep setColor_atX_ySelector (toNSColor color) x y

-- | @- colorAtX:y:@
colorAtX_y :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> CLong -> CLong -> IO (Id NSColor)
colorAtX_y nsBitmapImageRep x y =
  sendMessage nsBitmapImageRep colorAtX_ySelector x y

-- | @- getPixel:atX:y:@
getPixel_atX_y :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> Ptr CULong -> CLong -> CLong -> IO ()
getPixel_atX_y nsBitmapImageRep p x y =
  sendMessage nsBitmapImageRep getPixel_atX_ySelector p x y

-- | @- setPixel:atX:y:@
setPixel_atX_y :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> Ptr CULong -> CLong -> CLong -> IO ()
setPixel_atX_y nsBitmapImageRep p x y =
  sendMessage nsBitmapImageRep setPixel_atX_ySelector p x y

-- | @- bitmapImageRepByConvertingToColorSpace:renderingIntent:@
bitmapImageRepByConvertingToColorSpace_renderingIntent :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSColorSpace targetSpace) => nsBitmapImageRep -> targetSpace -> NSColorRenderingIntent -> IO (Id NSBitmapImageRep)
bitmapImageRepByConvertingToColorSpace_renderingIntent nsBitmapImageRep targetSpace renderingIntent =
  sendMessage nsBitmapImageRep bitmapImageRepByConvertingToColorSpace_renderingIntentSelector (toNSColorSpace targetSpace) renderingIntent

-- | @- bitmapImageRepByRetaggingWithColorSpace:@
bitmapImageRepByRetaggingWithColorSpace :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSColorSpace newSpace) => nsBitmapImageRep -> newSpace -> IO (Id NSBitmapImageRep)
bitmapImageRepByRetaggingWithColorSpace nsBitmapImageRep newSpace =
  sendMessage nsBitmapImageRep bitmapImageRepByRetaggingWithColorSpaceSelector (toNSColorSpace newSpace)

-- | @+ representationOfImageRepsInArray:usingType:properties:@
representationOfImageRepsInArray_usingType_properties :: (IsNSArray imageReps, IsNSDictionary properties) => imageReps -> NSBitmapImageFileType -> properties -> IO (Id NSData)
representationOfImageRepsInArray_usingType_properties imageReps storageType properties =
  do
    cls' <- getRequiredClass "NSBitmapImageRep"
    sendClassMessage cls' representationOfImageRepsInArray_usingType_propertiesSelector (toNSArray imageReps) storageType (toNSDictionary properties)

-- | @- representationUsingType:properties:@
representationUsingType_properties :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSDictionary properties) => nsBitmapImageRep -> NSBitmapImageFileType -> properties -> IO (Id NSData)
representationUsingType_properties nsBitmapImageRep storageType properties =
  sendMessage nsBitmapImageRep representationUsingType_propertiesSelector storageType (toNSDictionary properties)

-- | @- setProperty:withValue:@
setProperty_withValue :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSString property) => nsBitmapImageRep -> property -> RawId -> IO ()
setProperty_withValue nsBitmapImageRep property value =
  sendMessage nsBitmapImageRep setProperty_withValueSelector (toNSString property) value

-- | @- valueForProperty:@
valueForProperty :: (IsNSBitmapImageRep nsBitmapImageRep, IsNSString property) => nsBitmapImageRep -> property -> IO RawId
valueForProperty nsBitmapImageRep property =
  sendMessage nsBitmapImageRep valueForPropertySelector (toNSString property)

-- | @- bitmapData@
bitmapData :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO RawId
bitmapData nsBitmapImageRep =
  sendMessage nsBitmapImageRep bitmapDataSelector

-- | @- planar@
planar :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO Bool
planar nsBitmapImageRep =
  sendMessage nsBitmapImageRep planarSelector

-- | @- samplesPerPixel@
samplesPerPixel :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO CLong
samplesPerPixel nsBitmapImageRep =
  sendMessage nsBitmapImageRep samplesPerPixelSelector

-- | @- bitsPerPixel@
bitsPerPixel :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO CLong
bitsPerPixel nsBitmapImageRep =
  sendMessage nsBitmapImageRep bitsPerPixelSelector

-- | @- bytesPerRow@
bytesPerRow :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO CLong
bytesPerRow nsBitmapImageRep =
  sendMessage nsBitmapImageRep bytesPerRowSelector

-- | @- bytesPerPlane@
bytesPerPlane :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO CLong
bytesPerPlane nsBitmapImageRep =
  sendMessage nsBitmapImageRep bytesPerPlaneSelector

-- | @- numberOfPlanes@
numberOfPlanes :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO CLong
numberOfPlanes nsBitmapImageRep =
  sendMessage nsBitmapImageRep numberOfPlanesSelector

-- | @- bitmapFormat@
bitmapFormat :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO NSBitmapFormat
bitmapFormat nsBitmapImageRep =
  sendMessage nsBitmapImageRep bitmapFormatSelector

-- | @- TIFFRepresentation@
tiffRepresentation :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO (Id NSData)
tiffRepresentation nsBitmapImageRep =
  sendMessage nsBitmapImageRep tiffRepresentationSelector

-- | @- CGImage@
cgImage :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO (Ptr ())
cgImage nsBitmapImageRep =
  sendMessage nsBitmapImageRep cgImageSelector

-- | @- colorSpace@
colorSpace :: IsNSBitmapImageRep nsBitmapImageRep => nsBitmapImageRep -> IO (Id NSColorSpace)
colorSpace nsBitmapImageRep =
  sendMessage nsBitmapImageRep colorSpaceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFocusedViewRect:@
initWithFocusedViewRectSelector :: Selector '[NSRect] (Id NSBitmapImageRep)
initWithFocusedViewRectSelector = mkSelector "initWithFocusedViewRect:"

-- | @Selector@ for @initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bytesPerRow:bitsPerPixel:@
initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsPerPixelSelector :: Selector '[Ptr (Ptr CUChar), CLong, CLong, CLong, CLong, Bool, Bool, Id NSString, CLong, CLong] (Id NSBitmapImageRep)
initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsPerPixelSelector = mkSelector "initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bytesPerRow:bitsPerPixel:"

-- | @Selector@ for @initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bitmapFormat:bytesPerRow:bitsPerPixel:@
initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixelSelector :: Selector '[Ptr (Ptr CUChar), CLong, CLong, CLong, CLong, Bool, Bool, Id NSString, NSBitmapFormat, CLong, CLong] (Id NSBitmapImageRep)
initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixelSelector = mkSelector "initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bitmapFormat:bytesPerRow:bitsPerPixel:"

-- | @Selector@ for @initWithCGImage:@
initWithCGImageSelector :: Selector '[Ptr ()] (Id NSBitmapImageRep)
initWithCGImageSelector = mkSelector "initWithCGImage:"

-- | @Selector@ for @initWithCIImage:@
initWithCIImageSelector :: Selector '[Id CIImage] (Id NSBitmapImageRep)
initWithCIImageSelector = mkSelector "initWithCIImage:"

-- | @Selector@ for @imageRepsWithData:@
imageRepsWithDataSelector :: Selector '[Id NSData] (Id NSArray)
imageRepsWithDataSelector = mkSelector "imageRepsWithData:"

-- | @Selector@ for @imageRepWithData:@
imageRepWithDataSelector :: Selector '[Id NSData] (Id NSBitmapImageRep)
imageRepWithDataSelector = mkSelector "imageRepWithData:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector '[Id NSData] (Id NSBitmapImageRep)
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @getBitmapDataPlanes:@
getBitmapDataPlanesSelector :: Selector '[Ptr (Ptr CUChar)] ()
getBitmapDataPlanesSelector = mkSelector "getBitmapDataPlanes:"

-- | @Selector@ for @getCompression:factor:@
getCompression_factorSelector :: Selector '[Ptr NSTIFFCompression, Ptr CFloat] ()
getCompression_factorSelector = mkSelector "getCompression:factor:"

-- | @Selector@ for @setCompression:factor:@
setCompression_factorSelector :: Selector '[NSTIFFCompression, CFloat] ()
setCompression_factorSelector = mkSelector "setCompression:factor:"

-- | @Selector@ for @TIFFRepresentationUsingCompression:factor:@
tiffRepresentationUsingCompression_factorSelector :: Selector '[NSTIFFCompression, CFloat] (Id NSData)
tiffRepresentationUsingCompression_factorSelector = mkSelector "TIFFRepresentationUsingCompression:factor:"

-- | @Selector@ for @TIFFRepresentationOfImageRepsInArray:@
tiffRepresentationOfImageRepsInArraySelector :: Selector '[Id NSArray] (Id NSData)
tiffRepresentationOfImageRepsInArraySelector = mkSelector "TIFFRepresentationOfImageRepsInArray:"

-- | @Selector@ for @TIFFRepresentationOfImageRepsInArray:usingCompression:factor:@
tiffRepresentationOfImageRepsInArray_usingCompression_factorSelector :: Selector '[Id NSArray, NSTIFFCompression, CFloat] (Id NSData)
tiffRepresentationOfImageRepsInArray_usingCompression_factorSelector = mkSelector "TIFFRepresentationOfImageRepsInArray:usingCompression:factor:"

-- | @Selector@ for @getTIFFCompressionTypes:count:@
getTIFFCompressionTypes_countSelector :: Selector '[Const (Ptr NSTIFFCompression), Ptr CLong] ()
getTIFFCompressionTypes_countSelector = mkSelector "getTIFFCompressionTypes:count:"

-- | @Selector@ for @localizedNameForTIFFCompressionType:@
localizedNameForTIFFCompressionTypeSelector :: Selector '[NSTIFFCompression] (Id NSString)
localizedNameForTIFFCompressionTypeSelector = mkSelector "localizedNameForTIFFCompressionType:"

-- | @Selector@ for @canBeCompressedUsing:@
canBeCompressedUsingSelector :: Selector '[NSTIFFCompression] Bool
canBeCompressedUsingSelector = mkSelector "canBeCompressedUsing:"

-- | @Selector@ for @colorizeByMappingGray:toColor:blackMapping:whiteMapping:@
colorizeByMappingGray_toColor_blackMapping_whiteMappingSelector :: Selector '[CDouble, Id NSColor, Id NSColor, Id NSColor] ()
colorizeByMappingGray_toColor_blackMapping_whiteMappingSelector = mkSelector "colorizeByMappingGray:toColor:blackMapping:whiteMapping:"

-- | @Selector@ for @initForIncrementalLoad@
initForIncrementalLoadSelector :: Selector '[] (Id NSBitmapImageRep)
initForIncrementalLoadSelector = mkSelector "initForIncrementalLoad"

-- | @Selector@ for @incrementalLoadFromData:complete:@
incrementalLoadFromData_completeSelector :: Selector '[Id NSData, Bool] CLong
incrementalLoadFromData_completeSelector = mkSelector "incrementalLoadFromData:complete:"

-- | @Selector@ for @setColor:atX:y:@
setColor_atX_ySelector :: Selector '[Id NSColor, CLong, CLong] ()
setColor_atX_ySelector = mkSelector "setColor:atX:y:"

-- | @Selector@ for @colorAtX:y:@
colorAtX_ySelector :: Selector '[CLong, CLong] (Id NSColor)
colorAtX_ySelector = mkSelector "colorAtX:y:"

-- | @Selector@ for @getPixel:atX:y:@
getPixel_atX_ySelector :: Selector '[Ptr CULong, CLong, CLong] ()
getPixel_atX_ySelector = mkSelector "getPixel:atX:y:"

-- | @Selector@ for @setPixel:atX:y:@
setPixel_atX_ySelector :: Selector '[Ptr CULong, CLong, CLong] ()
setPixel_atX_ySelector = mkSelector "setPixel:atX:y:"

-- | @Selector@ for @bitmapImageRepByConvertingToColorSpace:renderingIntent:@
bitmapImageRepByConvertingToColorSpace_renderingIntentSelector :: Selector '[Id NSColorSpace, NSColorRenderingIntent] (Id NSBitmapImageRep)
bitmapImageRepByConvertingToColorSpace_renderingIntentSelector = mkSelector "bitmapImageRepByConvertingToColorSpace:renderingIntent:"

-- | @Selector@ for @bitmapImageRepByRetaggingWithColorSpace:@
bitmapImageRepByRetaggingWithColorSpaceSelector :: Selector '[Id NSColorSpace] (Id NSBitmapImageRep)
bitmapImageRepByRetaggingWithColorSpaceSelector = mkSelector "bitmapImageRepByRetaggingWithColorSpace:"

-- | @Selector@ for @representationOfImageRepsInArray:usingType:properties:@
representationOfImageRepsInArray_usingType_propertiesSelector :: Selector '[Id NSArray, NSBitmapImageFileType, Id NSDictionary] (Id NSData)
representationOfImageRepsInArray_usingType_propertiesSelector = mkSelector "representationOfImageRepsInArray:usingType:properties:"

-- | @Selector@ for @representationUsingType:properties:@
representationUsingType_propertiesSelector :: Selector '[NSBitmapImageFileType, Id NSDictionary] (Id NSData)
representationUsingType_propertiesSelector = mkSelector "representationUsingType:properties:"

-- | @Selector@ for @setProperty:withValue:@
setProperty_withValueSelector :: Selector '[Id NSString, RawId] ()
setProperty_withValueSelector = mkSelector "setProperty:withValue:"

-- | @Selector@ for @valueForProperty:@
valueForPropertySelector :: Selector '[Id NSString] RawId
valueForPropertySelector = mkSelector "valueForProperty:"

-- | @Selector@ for @bitmapData@
bitmapDataSelector :: Selector '[] RawId
bitmapDataSelector = mkSelector "bitmapData"

-- | @Selector@ for @planar@
planarSelector :: Selector '[] Bool
planarSelector = mkSelector "planar"

-- | @Selector@ for @samplesPerPixel@
samplesPerPixelSelector :: Selector '[] CLong
samplesPerPixelSelector = mkSelector "samplesPerPixel"

-- | @Selector@ for @bitsPerPixel@
bitsPerPixelSelector :: Selector '[] CLong
bitsPerPixelSelector = mkSelector "bitsPerPixel"

-- | @Selector@ for @bytesPerRow@
bytesPerRowSelector :: Selector '[] CLong
bytesPerRowSelector = mkSelector "bytesPerRow"

-- | @Selector@ for @bytesPerPlane@
bytesPerPlaneSelector :: Selector '[] CLong
bytesPerPlaneSelector = mkSelector "bytesPerPlane"

-- | @Selector@ for @numberOfPlanes@
numberOfPlanesSelector :: Selector '[] CLong
numberOfPlanesSelector = mkSelector "numberOfPlanes"

-- | @Selector@ for @bitmapFormat@
bitmapFormatSelector :: Selector '[] NSBitmapFormat
bitmapFormatSelector = mkSelector "bitmapFormat"

-- | @Selector@ for @TIFFRepresentation@
tiffRepresentationSelector :: Selector '[] (Id NSData)
tiffRepresentationSelector = mkSelector "TIFFRepresentation"

-- | @Selector@ for @CGImage@
cgImageSelector :: Selector '[] (Ptr ())
cgImageSelector = mkSelector "CGImage"

-- | @Selector@ for @colorSpace@
colorSpaceSelector :: Selector '[] (Id NSColorSpace)
colorSpaceSelector = mkSelector "colorSpace"

