{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSImageRep@.
module ObjC.AppKit.NSImageRep
  ( NSImageRep
  , IsNSImageRep(..)
  , init_
  , initWithCoder
  , draw
  , drawAtPoint
  , drawInRect
  , drawInRect_fromRect_operation_fraction_respectFlipped_hints
  , registerImageRepClass
  , unregisterImageRepClass
  , imageRepClassForFileType
  , imageRepClassForPasteboardType
  , imageRepClassForType
  , imageRepClassForData
  , canInitWithData
  , imageUnfilteredFileTypes
  , imageUnfilteredPasteboardTypes
  , imageFileTypes
  , imagePasteboardTypes
  , canInitWithPasteboard
  , imageRepsWithContentsOfFile
  , imageRepWithContentsOfFile
  , imageRepsWithContentsOfURL
  , imageRepWithContentsOfURL
  , imageRepsWithPasteboard
  , imageRepWithPasteboard
  , cgImageForProposedRect_context_hints
  , size
  , setSize
  , alpha
  , setAlpha
  , opaque
  , setOpaque
  , colorSpaceName
  , setColorSpaceName
  , bitsPerSample
  , setBitsPerSample
  , pixelsWide
  , setPixelsWide
  , pixelsHigh
  , setPixelsHigh
  , layoutDirection
  , setLayoutDirection
  , registeredImageRepClasses
  , imageUnfilteredTypes
  , imageTypes
  , alphaSelector
  , bitsPerSampleSelector
  , canInitWithDataSelector
  , canInitWithPasteboardSelector
  , cgImageForProposedRect_context_hintsSelector
  , colorSpaceNameSelector
  , drawAtPointSelector
  , drawInRectSelector
  , drawInRect_fromRect_operation_fraction_respectFlipped_hintsSelector
  , drawSelector
  , imageFileTypesSelector
  , imagePasteboardTypesSelector
  , imageRepClassForDataSelector
  , imageRepClassForFileTypeSelector
  , imageRepClassForPasteboardTypeSelector
  , imageRepClassForTypeSelector
  , imageRepWithContentsOfFileSelector
  , imageRepWithContentsOfURLSelector
  , imageRepWithPasteboardSelector
  , imageRepsWithContentsOfFileSelector
  , imageRepsWithContentsOfURLSelector
  , imageRepsWithPasteboardSelector
  , imageTypesSelector
  , imageUnfilteredFileTypesSelector
  , imageUnfilteredPasteboardTypesSelector
  , imageUnfilteredTypesSelector
  , initSelector
  , initWithCoderSelector
  , layoutDirectionSelector
  , opaqueSelector
  , pixelsHighSelector
  , pixelsWideSelector
  , registerImageRepClassSelector
  , registeredImageRepClassesSelector
  , setAlphaSelector
  , setBitsPerSampleSelector
  , setColorSpaceNameSelector
  , setLayoutDirectionSelector
  , setOpaqueSelector
  , setPixelsHighSelector
  , setPixelsWideSelector
  , setSizeSelector
  , sizeSelector
  , unregisterImageRepClassSelector

  -- * Enum types
  , NSCompositingOperation(NSCompositingOperation)
  , pattern NSCompositingOperationClear
  , pattern NSCompositingOperationCopy
  , pattern NSCompositingOperationSourceOver
  , pattern NSCompositingOperationSourceIn
  , pattern NSCompositingOperationSourceOut
  , pattern NSCompositingOperationSourceAtop
  , pattern NSCompositingOperationDestinationOver
  , pattern NSCompositingOperationDestinationIn
  , pattern NSCompositingOperationDestinationOut
  , pattern NSCompositingOperationDestinationAtop
  , pattern NSCompositingOperationXOR
  , pattern NSCompositingOperationPlusDarker
  , pattern NSCompositingOperationHighlight
  , pattern NSCompositingOperationPlusLighter
  , pattern NSCompositingOperationMultiply
  , pattern NSCompositingOperationScreen
  , pattern NSCompositingOperationOverlay
  , pattern NSCompositingOperationDarken
  , pattern NSCompositingOperationLighten
  , pattern NSCompositingOperationColorDodge
  , pattern NSCompositingOperationColorBurn
  , pattern NSCompositingOperationSoftLight
  , pattern NSCompositingOperationHardLight
  , pattern NSCompositingOperationDifference
  , pattern NSCompositingOperationExclusion
  , pattern NSCompositingOperationHue
  , pattern NSCompositingOperationSaturation
  , pattern NSCompositingOperationColor
  , pattern NSCompositingOperationLuminosity
  , NSImageLayoutDirection(NSImageLayoutDirection)
  , pattern NSImageLayoutDirectionUnspecified
  , pattern NSImageLayoutDirectionLeftToRight
  , pattern NSImageLayoutDirectionRightToLeft

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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSImageRep nsImageRep => nsImageRep -> IO (Id NSImageRep)
init_ nsImageRep =
  sendOwnedMessage nsImageRep initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsNSImageRep nsImageRep, IsNSCoder coder) => nsImageRep -> coder -> IO (Id NSImageRep)
initWithCoder nsImageRep coder =
  sendOwnedMessage nsImageRep initWithCoderSelector (toNSCoder coder)

-- | @- draw@
draw :: IsNSImageRep nsImageRep => nsImageRep -> IO Bool
draw nsImageRep =
  sendMessage nsImageRep drawSelector

-- | @- drawAtPoint:@
drawAtPoint :: IsNSImageRep nsImageRep => nsImageRep -> NSPoint -> IO Bool
drawAtPoint nsImageRep point =
  sendMessage nsImageRep drawAtPointSelector point

-- | @- drawInRect:@
drawInRect :: IsNSImageRep nsImageRep => nsImageRep -> NSRect -> IO Bool
drawInRect nsImageRep rect =
  sendMessage nsImageRep drawInRectSelector rect

-- | @- drawInRect:fromRect:operation:fraction:respectFlipped:hints:@
drawInRect_fromRect_operation_fraction_respectFlipped_hints :: (IsNSImageRep nsImageRep, IsNSDictionary hints) => nsImageRep -> NSRect -> NSRect -> NSCompositingOperation -> CDouble -> Bool -> hints -> IO Bool
drawInRect_fromRect_operation_fraction_respectFlipped_hints nsImageRep dstSpacePortionRect srcSpacePortionRect op requestedAlpha respectContextIsFlipped hints =
  sendMessage nsImageRep drawInRect_fromRect_operation_fraction_respectFlipped_hintsSelector dstSpacePortionRect srcSpacePortionRect op requestedAlpha respectContextIsFlipped (toNSDictionary hints)

-- | @+ registerImageRepClass:@
registerImageRepClass :: Class -> IO ()
registerImageRepClass imageRepClass =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' registerImageRepClassSelector imageRepClass

-- | @+ unregisterImageRepClass:@
unregisterImageRepClass :: Class -> IO ()
unregisterImageRepClass imageRepClass =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' unregisterImageRepClassSelector imageRepClass

-- | @+ imageRepClassForFileType:@
imageRepClassForFileType :: IsNSString type_ => type_ -> IO Class
imageRepClassForFileType type_ =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' imageRepClassForFileTypeSelector (toNSString type_)

-- | @+ imageRepClassForPasteboardType:@
imageRepClassForPasteboardType :: IsNSString type_ => type_ -> IO Class
imageRepClassForPasteboardType type_ =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' imageRepClassForPasteboardTypeSelector (toNSString type_)

-- | @+ imageRepClassForType:@
imageRepClassForType :: IsNSString type_ => type_ -> IO Class
imageRepClassForType type_ =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' imageRepClassForTypeSelector (toNSString type_)

-- | @+ imageRepClassForData:@
imageRepClassForData :: IsNSData data_ => data_ -> IO Class
imageRepClassForData data_ =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' imageRepClassForDataSelector (toNSData data_)

-- | @+ canInitWithData:@
canInitWithData :: IsNSData data_ => data_ -> IO Bool
canInitWithData data_ =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' canInitWithDataSelector (toNSData data_)

-- | @+ imageUnfilteredFileTypes@
imageUnfilteredFileTypes :: IO (Id NSArray)
imageUnfilteredFileTypes  =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' imageUnfilteredFileTypesSelector

-- | @+ imageUnfilteredPasteboardTypes@
imageUnfilteredPasteboardTypes :: IO (Id NSArray)
imageUnfilteredPasteboardTypes  =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' imageUnfilteredPasteboardTypesSelector

-- | @+ imageFileTypes@
imageFileTypes :: IO (Id NSArray)
imageFileTypes  =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' imageFileTypesSelector

-- | @+ imagePasteboardTypes@
imagePasteboardTypes :: IO (Id NSArray)
imagePasteboardTypes  =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' imagePasteboardTypesSelector

-- | @+ canInitWithPasteboard:@
canInitWithPasteboard :: IsNSPasteboard pasteboard => pasteboard -> IO Bool
canInitWithPasteboard pasteboard =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' canInitWithPasteboardSelector (toNSPasteboard pasteboard)

-- | @+ imageRepsWithContentsOfFile:@
imageRepsWithContentsOfFile :: IsNSString filename => filename -> IO (Id NSArray)
imageRepsWithContentsOfFile filename =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' imageRepsWithContentsOfFileSelector (toNSString filename)

-- | @+ imageRepWithContentsOfFile:@
imageRepWithContentsOfFile :: IsNSString filename => filename -> IO (Id NSImageRep)
imageRepWithContentsOfFile filename =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' imageRepWithContentsOfFileSelector (toNSString filename)

-- | @+ imageRepsWithContentsOfURL:@
imageRepsWithContentsOfURL :: IsNSURL url => url -> IO (Id NSArray)
imageRepsWithContentsOfURL url =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' imageRepsWithContentsOfURLSelector (toNSURL url)

-- | @+ imageRepWithContentsOfURL:@
imageRepWithContentsOfURL :: IsNSURL url => url -> IO (Id NSImageRep)
imageRepWithContentsOfURL url =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' imageRepWithContentsOfURLSelector (toNSURL url)

-- | @+ imageRepsWithPasteboard:@
imageRepsWithPasteboard :: IsNSPasteboard pasteboard => pasteboard -> IO (Id NSArray)
imageRepsWithPasteboard pasteboard =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' imageRepsWithPasteboardSelector (toNSPasteboard pasteboard)

-- | @+ imageRepWithPasteboard:@
imageRepWithPasteboard :: IsNSPasteboard pasteboard => pasteboard -> IO (Id NSImageRep)
imageRepWithPasteboard pasteboard =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' imageRepWithPasteboardSelector (toNSPasteboard pasteboard)

-- | @- CGImageForProposedRect:context:hints:@
cgImageForProposedRect_context_hints :: (IsNSImageRep nsImageRep, IsNSGraphicsContext context, IsNSDictionary hints) => nsImageRep -> Ptr NSRect -> context -> hints -> IO (Ptr ())
cgImageForProposedRect_context_hints nsImageRep proposedDestRect context hints =
  sendMessage nsImageRep cgImageForProposedRect_context_hintsSelector proposedDestRect (toNSGraphicsContext context) (toNSDictionary hints)

-- | @- size@
size :: IsNSImageRep nsImageRep => nsImageRep -> IO NSSize
size nsImageRep =
  sendMessage nsImageRep sizeSelector

-- | @- setSize:@
setSize :: IsNSImageRep nsImageRep => nsImageRep -> NSSize -> IO ()
setSize nsImageRep value =
  sendMessage nsImageRep setSizeSelector value

-- | @- alpha@
alpha :: IsNSImageRep nsImageRep => nsImageRep -> IO Bool
alpha nsImageRep =
  sendMessage nsImageRep alphaSelector

-- | @- setAlpha:@
setAlpha :: IsNSImageRep nsImageRep => nsImageRep -> Bool -> IO ()
setAlpha nsImageRep value =
  sendMessage nsImageRep setAlphaSelector value

-- | @- opaque@
opaque :: IsNSImageRep nsImageRep => nsImageRep -> IO Bool
opaque nsImageRep =
  sendMessage nsImageRep opaqueSelector

-- | @- setOpaque:@
setOpaque :: IsNSImageRep nsImageRep => nsImageRep -> Bool -> IO ()
setOpaque nsImageRep value =
  sendMessage nsImageRep setOpaqueSelector value

-- | @- colorSpaceName@
colorSpaceName :: IsNSImageRep nsImageRep => nsImageRep -> IO (Id NSString)
colorSpaceName nsImageRep =
  sendMessage nsImageRep colorSpaceNameSelector

-- | @- setColorSpaceName:@
setColorSpaceName :: (IsNSImageRep nsImageRep, IsNSString value) => nsImageRep -> value -> IO ()
setColorSpaceName nsImageRep value =
  sendMessage nsImageRep setColorSpaceNameSelector (toNSString value)

-- | @- bitsPerSample@
bitsPerSample :: IsNSImageRep nsImageRep => nsImageRep -> IO CLong
bitsPerSample nsImageRep =
  sendMessage nsImageRep bitsPerSampleSelector

-- | @- setBitsPerSample:@
setBitsPerSample :: IsNSImageRep nsImageRep => nsImageRep -> CLong -> IO ()
setBitsPerSample nsImageRep value =
  sendMessage nsImageRep setBitsPerSampleSelector value

-- | @- pixelsWide@
pixelsWide :: IsNSImageRep nsImageRep => nsImageRep -> IO CLong
pixelsWide nsImageRep =
  sendMessage nsImageRep pixelsWideSelector

-- | @- setPixelsWide:@
setPixelsWide :: IsNSImageRep nsImageRep => nsImageRep -> CLong -> IO ()
setPixelsWide nsImageRep value =
  sendMessage nsImageRep setPixelsWideSelector value

-- | @- pixelsHigh@
pixelsHigh :: IsNSImageRep nsImageRep => nsImageRep -> IO CLong
pixelsHigh nsImageRep =
  sendMessage nsImageRep pixelsHighSelector

-- | @- setPixelsHigh:@
setPixelsHigh :: IsNSImageRep nsImageRep => nsImageRep -> CLong -> IO ()
setPixelsHigh nsImageRep value =
  sendMessage nsImageRep setPixelsHighSelector value

-- | @- layoutDirection@
layoutDirection :: IsNSImageRep nsImageRep => nsImageRep -> IO NSImageLayoutDirection
layoutDirection nsImageRep =
  sendMessage nsImageRep layoutDirectionSelector

-- | @- setLayoutDirection:@
setLayoutDirection :: IsNSImageRep nsImageRep => nsImageRep -> NSImageLayoutDirection -> IO ()
setLayoutDirection nsImageRep value =
  sendMessage nsImageRep setLayoutDirectionSelector value

-- | @+ registeredImageRepClasses@
registeredImageRepClasses :: IO (Id NSArray)
registeredImageRepClasses  =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' registeredImageRepClassesSelector

-- | @+ imageUnfilteredTypes@
imageUnfilteredTypes :: IO (Id NSArray)
imageUnfilteredTypes  =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' imageUnfilteredTypesSelector

-- | @+ imageTypes@
imageTypes :: IO (Id NSArray)
imageTypes  =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMessage cls' imageTypesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSImageRep)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSImageRep)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @draw@
drawSelector :: Selector '[] Bool
drawSelector = mkSelector "draw"

-- | @Selector@ for @drawAtPoint:@
drawAtPointSelector :: Selector '[NSPoint] Bool
drawAtPointSelector = mkSelector "drawAtPoint:"

-- | @Selector@ for @drawInRect:@
drawInRectSelector :: Selector '[NSRect] Bool
drawInRectSelector = mkSelector "drawInRect:"

-- | @Selector@ for @drawInRect:fromRect:operation:fraction:respectFlipped:hints:@
drawInRect_fromRect_operation_fraction_respectFlipped_hintsSelector :: Selector '[NSRect, NSRect, NSCompositingOperation, CDouble, Bool, Id NSDictionary] Bool
drawInRect_fromRect_operation_fraction_respectFlipped_hintsSelector = mkSelector "drawInRect:fromRect:operation:fraction:respectFlipped:hints:"

-- | @Selector@ for @registerImageRepClass:@
registerImageRepClassSelector :: Selector '[Class] ()
registerImageRepClassSelector = mkSelector "registerImageRepClass:"

-- | @Selector@ for @unregisterImageRepClass:@
unregisterImageRepClassSelector :: Selector '[Class] ()
unregisterImageRepClassSelector = mkSelector "unregisterImageRepClass:"

-- | @Selector@ for @imageRepClassForFileType:@
imageRepClassForFileTypeSelector :: Selector '[Id NSString] Class
imageRepClassForFileTypeSelector = mkSelector "imageRepClassForFileType:"

-- | @Selector@ for @imageRepClassForPasteboardType:@
imageRepClassForPasteboardTypeSelector :: Selector '[Id NSString] Class
imageRepClassForPasteboardTypeSelector = mkSelector "imageRepClassForPasteboardType:"

-- | @Selector@ for @imageRepClassForType:@
imageRepClassForTypeSelector :: Selector '[Id NSString] Class
imageRepClassForTypeSelector = mkSelector "imageRepClassForType:"

-- | @Selector@ for @imageRepClassForData:@
imageRepClassForDataSelector :: Selector '[Id NSData] Class
imageRepClassForDataSelector = mkSelector "imageRepClassForData:"

-- | @Selector@ for @canInitWithData:@
canInitWithDataSelector :: Selector '[Id NSData] Bool
canInitWithDataSelector = mkSelector "canInitWithData:"

-- | @Selector@ for @imageUnfilteredFileTypes@
imageUnfilteredFileTypesSelector :: Selector '[] (Id NSArray)
imageUnfilteredFileTypesSelector = mkSelector "imageUnfilteredFileTypes"

-- | @Selector@ for @imageUnfilteredPasteboardTypes@
imageUnfilteredPasteboardTypesSelector :: Selector '[] (Id NSArray)
imageUnfilteredPasteboardTypesSelector = mkSelector "imageUnfilteredPasteboardTypes"

-- | @Selector@ for @imageFileTypes@
imageFileTypesSelector :: Selector '[] (Id NSArray)
imageFileTypesSelector = mkSelector "imageFileTypes"

-- | @Selector@ for @imagePasteboardTypes@
imagePasteboardTypesSelector :: Selector '[] (Id NSArray)
imagePasteboardTypesSelector = mkSelector "imagePasteboardTypes"

-- | @Selector@ for @canInitWithPasteboard:@
canInitWithPasteboardSelector :: Selector '[Id NSPasteboard] Bool
canInitWithPasteboardSelector = mkSelector "canInitWithPasteboard:"

-- | @Selector@ for @imageRepsWithContentsOfFile:@
imageRepsWithContentsOfFileSelector :: Selector '[Id NSString] (Id NSArray)
imageRepsWithContentsOfFileSelector = mkSelector "imageRepsWithContentsOfFile:"

-- | @Selector@ for @imageRepWithContentsOfFile:@
imageRepWithContentsOfFileSelector :: Selector '[Id NSString] (Id NSImageRep)
imageRepWithContentsOfFileSelector = mkSelector "imageRepWithContentsOfFile:"

-- | @Selector@ for @imageRepsWithContentsOfURL:@
imageRepsWithContentsOfURLSelector :: Selector '[Id NSURL] (Id NSArray)
imageRepsWithContentsOfURLSelector = mkSelector "imageRepsWithContentsOfURL:"

-- | @Selector@ for @imageRepWithContentsOfURL:@
imageRepWithContentsOfURLSelector :: Selector '[Id NSURL] (Id NSImageRep)
imageRepWithContentsOfURLSelector = mkSelector "imageRepWithContentsOfURL:"

-- | @Selector@ for @imageRepsWithPasteboard:@
imageRepsWithPasteboardSelector :: Selector '[Id NSPasteboard] (Id NSArray)
imageRepsWithPasteboardSelector = mkSelector "imageRepsWithPasteboard:"

-- | @Selector@ for @imageRepWithPasteboard:@
imageRepWithPasteboardSelector :: Selector '[Id NSPasteboard] (Id NSImageRep)
imageRepWithPasteboardSelector = mkSelector "imageRepWithPasteboard:"

-- | @Selector@ for @CGImageForProposedRect:context:hints:@
cgImageForProposedRect_context_hintsSelector :: Selector '[Ptr NSRect, Id NSGraphicsContext, Id NSDictionary] (Ptr ())
cgImageForProposedRect_context_hintsSelector = mkSelector "CGImageForProposedRect:context:hints:"

-- | @Selector@ for @size@
sizeSelector :: Selector '[] NSSize
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector '[NSSize] ()
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector '[] Bool
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector '[Bool] ()
setAlphaSelector = mkSelector "setAlpha:"

-- | @Selector@ for @opaque@
opaqueSelector :: Selector '[] Bool
opaqueSelector = mkSelector "opaque"

-- | @Selector@ for @setOpaque:@
setOpaqueSelector :: Selector '[Bool] ()
setOpaqueSelector = mkSelector "setOpaque:"

-- | @Selector@ for @colorSpaceName@
colorSpaceNameSelector :: Selector '[] (Id NSString)
colorSpaceNameSelector = mkSelector "colorSpaceName"

-- | @Selector@ for @setColorSpaceName:@
setColorSpaceNameSelector :: Selector '[Id NSString] ()
setColorSpaceNameSelector = mkSelector "setColorSpaceName:"

-- | @Selector@ for @bitsPerSample@
bitsPerSampleSelector :: Selector '[] CLong
bitsPerSampleSelector = mkSelector "bitsPerSample"

-- | @Selector@ for @setBitsPerSample:@
setBitsPerSampleSelector :: Selector '[CLong] ()
setBitsPerSampleSelector = mkSelector "setBitsPerSample:"

-- | @Selector@ for @pixelsWide@
pixelsWideSelector :: Selector '[] CLong
pixelsWideSelector = mkSelector "pixelsWide"

-- | @Selector@ for @setPixelsWide:@
setPixelsWideSelector :: Selector '[CLong] ()
setPixelsWideSelector = mkSelector "setPixelsWide:"

-- | @Selector@ for @pixelsHigh@
pixelsHighSelector :: Selector '[] CLong
pixelsHighSelector = mkSelector "pixelsHigh"

-- | @Selector@ for @setPixelsHigh:@
setPixelsHighSelector :: Selector '[CLong] ()
setPixelsHighSelector = mkSelector "setPixelsHigh:"

-- | @Selector@ for @layoutDirection@
layoutDirectionSelector :: Selector '[] NSImageLayoutDirection
layoutDirectionSelector = mkSelector "layoutDirection"

-- | @Selector@ for @setLayoutDirection:@
setLayoutDirectionSelector :: Selector '[NSImageLayoutDirection] ()
setLayoutDirectionSelector = mkSelector "setLayoutDirection:"

-- | @Selector@ for @registeredImageRepClasses@
registeredImageRepClassesSelector :: Selector '[] (Id NSArray)
registeredImageRepClassesSelector = mkSelector "registeredImageRepClasses"

-- | @Selector@ for @imageUnfilteredTypes@
imageUnfilteredTypesSelector :: Selector '[] (Id NSArray)
imageUnfilteredTypesSelector = mkSelector "imageUnfilteredTypes"

-- | @Selector@ for @imageTypes@
imageTypesSelector :: Selector '[] (Id NSArray)
imageTypesSelector = mkSelector "imageTypes"

