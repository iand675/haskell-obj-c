{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithCoderSelector
  , drawSelector
  , drawAtPointSelector
  , drawInRectSelector
  , drawInRect_fromRect_operation_fraction_respectFlipped_hintsSelector
  , registerImageRepClassSelector
  , unregisterImageRepClassSelector
  , imageRepClassForFileTypeSelector
  , imageRepClassForPasteboardTypeSelector
  , imageRepClassForTypeSelector
  , imageRepClassForDataSelector
  , canInitWithDataSelector
  , imageUnfilteredFileTypesSelector
  , imageUnfilteredPasteboardTypesSelector
  , imageFileTypesSelector
  , imagePasteboardTypesSelector
  , canInitWithPasteboardSelector
  , imageRepsWithContentsOfFileSelector
  , imageRepWithContentsOfFileSelector
  , imageRepsWithContentsOfURLSelector
  , imageRepWithContentsOfURLSelector
  , imageRepsWithPasteboardSelector
  , imageRepWithPasteboardSelector
  , cgImageForProposedRect_context_hintsSelector
  , sizeSelector
  , setSizeSelector
  , alphaSelector
  , setAlphaSelector
  , opaqueSelector
  , setOpaqueSelector
  , colorSpaceNameSelector
  , setColorSpaceNameSelector
  , bitsPerSampleSelector
  , setBitsPerSampleSelector
  , pixelsWideSelector
  , setPixelsWideSelector
  , pixelsHighSelector
  , setPixelsHighSelector
  , layoutDirectionSelector
  , setLayoutDirectionSelector
  , registeredImageRepClassesSelector
  , imageUnfilteredTypesSelector
  , imageTypesSelector

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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSImageRep nsImageRep => nsImageRep -> IO (Id NSImageRep)
init_ nsImageRep  =
    sendMsg nsImageRep (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSImageRep nsImageRep, IsNSCoder coder) => nsImageRep -> coder -> IO (Id NSImageRep)
initWithCoder nsImageRep  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg nsImageRep (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- draw@
draw :: IsNSImageRep nsImageRep => nsImageRep -> IO Bool
draw nsImageRep  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImageRep (mkSelector "draw") retCULong []

-- | @- drawAtPoint:@
drawAtPoint :: IsNSImageRep nsImageRep => nsImageRep -> NSPoint -> IO Bool
drawAtPoint nsImageRep  point =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImageRep (mkSelector "drawAtPoint:") retCULong [argNSPoint point]

-- | @- drawInRect:@
drawInRect :: IsNSImageRep nsImageRep => nsImageRep -> NSRect -> IO Bool
drawInRect nsImageRep  rect =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImageRep (mkSelector "drawInRect:") retCULong [argNSRect rect]

-- | @- drawInRect:fromRect:operation:fraction:respectFlipped:hints:@
drawInRect_fromRect_operation_fraction_respectFlipped_hints :: (IsNSImageRep nsImageRep, IsNSDictionary hints) => nsImageRep -> NSRect -> NSRect -> NSCompositingOperation -> CDouble -> Bool -> hints -> IO Bool
drawInRect_fromRect_operation_fraction_respectFlipped_hints nsImageRep  dstSpacePortionRect srcSpacePortionRect op requestedAlpha respectContextIsFlipped hints =
  withObjCPtr hints $ \raw_hints ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImageRep (mkSelector "drawInRect:fromRect:operation:fraction:respectFlipped:hints:") retCULong [argNSRect dstSpacePortionRect, argNSRect srcSpacePortionRect, argCULong (coerce op), argCDouble requestedAlpha, argCULong (if respectContextIsFlipped then 1 else 0), argPtr (castPtr raw_hints :: Ptr ())]

-- | @+ registerImageRepClass:@
registerImageRepClass :: Class -> IO ()
registerImageRepClass imageRepClass =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMsg cls' (mkSelector "registerImageRepClass:") retVoid [argPtr (unClass imageRepClass)]

-- | @+ unregisterImageRepClass:@
unregisterImageRepClass :: Class -> IO ()
unregisterImageRepClass imageRepClass =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMsg cls' (mkSelector "unregisterImageRepClass:") retVoid [argPtr (unClass imageRepClass)]

-- | @+ imageRepClassForFileType:@
imageRepClassForFileType :: IsNSString type_ => type_ -> IO Class
imageRepClassForFileType type_ =
  do
    cls' <- getRequiredClass "NSImageRep"
    withObjCPtr type_ $ \raw_type_ ->
      fmap (Class . castPtr) $ sendClassMsg cls' (mkSelector "imageRepClassForFileType:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ())]

-- | @+ imageRepClassForPasteboardType:@
imageRepClassForPasteboardType :: IsNSString type_ => type_ -> IO Class
imageRepClassForPasteboardType type_ =
  do
    cls' <- getRequiredClass "NSImageRep"
    withObjCPtr type_ $ \raw_type_ ->
      fmap (Class . castPtr) $ sendClassMsg cls' (mkSelector "imageRepClassForPasteboardType:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ())]

-- | @+ imageRepClassForType:@
imageRepClassForType :: IsNSString type_ => type_ -> IO Class
imageRepClassForType type_ =
  do
    cls' <- getRequiredClass "NSImageRep"
    withObjCPtr type_ $ \raw_type_ ->
      fmap (Class . castPtr) $ sendClassMsg cls' (mkSelector "imageRepClassForType:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ())]

-- | @+ imageRepClassForData:@
imageRepClassForData :: IsNSData data_ => data_ -> IO Class
imageRepClassForData data_ =
  do
    cls' <- getRequiredClass "NSImageRep"
    withObjCPtr data_ $ \raw_data_ ->
      fmap (Class . castPtr) $ sendClassMsg cls' (mkSelector "imageRepClassForData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())]

-- | @+ canInitWithData:@
canInitWithData :: IsNSData data_ => data_ -> IO Bool
canInitWithData data_ =
  do
    cls' <- getRequiredClass "NSImageRep"
    withObjCPtr data_ $ \raw_data_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canInitWithData:") retCULong [argPtr (castPtr raw_data_ :: Ptr ())]

-- | @+ imageUnfilteredFileTypes@
imageUnfilteredFileTypes :: IO (Id NSArray)
imageUnfilteredFileTypes  =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMsg cls' (mkSelector "imageUnfilteredFileTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ imageUnfilteredPasteboardTypes@
imageUnfilteredPasteboardTypes :: IO (Id NSArray)
imageUnfilteredPasteboardTypes  =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMsg cls' (mkSelector "imageUnfilteredPasteboardTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ imageFileTypes@
imageFileTypes :: IO (Id NSArray)
imageFileTypes  =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMsg cls' (mkSelector "imageFileTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ imagePasteboardTypes@
imagePasteboardTypes :: IO (Id NSArray)
imagePasteboardTypes  =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMsg cls' (mkSelector "imagePasteboardTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ canInitWithPasteboard:@
canInitWithPasteboard :: IsNSPasteboard pasteboard => pasteboard -> IO Bool
canInitWithPasteboard pasteboard =
  do
    cls' <- getRequiredClass "NSImageRep"
    withObjCPtr pasteboard $ \raw_pasteboard ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canInitWithPasteboard:") retCULong [argPtr (castPtr raw_pasteboard :: Ptr ())]

-- | @+ imageRepsWithContentsOfFile:@
imageRepsWithContentsOfFile :: IsNSString filename => filename -> IO (Id NSArray)
imageRepsWithContentsOfFile filename =
  do
    cls' <- getRequiredClass "NSImageRep"
    withObjCPtr filename $ \raw_filename ->
      sendClassMsg cls' (mkSelector "imageRepsWithContentsOfFile:") (retPtr retVoid) [argPtr (castPtr raw_filename :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageRepWithContentsOfFile:@
imageRepWithContentsOfFile :: IsNSString filename => filename -> IO (Id NSImageRep)
imageRepWithContentsOfFile filename =
  do
    cls' <- getRequiredClass "NSImageRep"
    withObjCPtr filename $ \raw_filename ->
      sendClassMsg cls' (mkSelector "imageRepWithContentsOfFile:") (retPtr retVoid) [argPtr (castPtr raw_filename :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageRepsWithContentsOfURL:@
imageRepsWithContentsOfURL :: IsNSURL url => url -> IO (Id NSArray)
imageRepsWithContentsOfURL url =
  do
    cls' <- getRequiredClass "NSImageRep"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "imageRepsWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageRepWithContentsOfURL:@
imageRepWithContentsOfURL :: IsNSURL url => url -> IO (Id NSImageRep)
imageRepWithContentsOfURL url =
  do
    cls' <- getRequiredClass "NSImageRep"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "imageRepWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageRepsWithPasteboard:@
imageRepsWithPasteboard :: IsNSPasteboard pasteboard => pasteboard -> IO (Id NSArray)
imageRepsWithPasteboard pasteboard =
  do
    cls' <- getRequiredClass "NSImageRep"
    withObjCPtr pasteboard $ \raw_pasteboard ->
      sendClassMsg cls' (mkSelector "imageRepsWithPasteboard:") (retPtr retVoid) [argPtr (castPtr raw_pasteboard :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageRepWithPasteboard:@
imageRepWithPasteboard :: IsNSPasteboard pasteboard => pasteboard -> IO (Id NSImageRep)
imageRepWithPasteboard pasteboard =
  do
    cls' <- getRequiredClass "NSImageRep"
    withObjCPtr pasteboard $ \raw_pasteboard ->
      sendClassMsg cls' (mkSelector "imageRepWithPasteboard:") (retPtr retVoid) [argPtr (castPtr raw_pasteboard :: Ptr ())] >>= retainedObject . castPtr

-- | @- CGImageForProposedRect:context:hints:@
cgImageForProposedRect_context_hints :: (IsNSImageRep nsImageRep, IsNSGraphicsContext context, IsNSDictionary hints) => nsImageRep -> Ptr NSRect -> context -> hints -> IO (Ptr ())
cgImageForProposedRect_context_hints nsImageRep  proposedDestRect context hints =
  withObjCPtr context $ \raw_context ->
    withObjCPtr hints $ \raw_hints ->
        fmap castPtr $ sendMsg nsImageRep (mkSelector "CGImageForProposedRect:context:hints:") (retPtr retVoid) [argPtr proposedDestRect, argPtr (castPtr raw_context :: Ptr ()), argPtr (castPtr raw_hints :: Ptr ())]

-- | @- size@
size :: IsNSImageRep nsImageRep => nsImageRep -> IO NSSize
size nsImageRep  =
    sendMsgStret nsImageRep (mkSelector "size") retNSSize []

-- | @- setSize:@
setSize :: IsNSImageRep nsImageRep => nsImageRep -> NSSize -> IO ()
setSize nsImageRep  value =
    sendMsg nsImageRep (mkSelector "setSize:") retVoid [argNSSize value]

-- | @- alpha@
alpha :: IsNSImageRep nsImageRep => nsImageRep -> IO Bool
alpha nsImageRep  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImageRep (mkSelector "alpha") retCULong []

-- | @- setAlpha:@
setAlpha :: IsNSImageRep nsImageRep => nsImageRep -> Bool -> IO ()
setAlpha nsImageRep  value =
    sendMsg nsImageRep (mkSelector "setAlpha:") retVoid [argCULong (if value then 1 else 0)]

-- | @- opaque@
opaque :: IsNSImageRep nsImageRep => nsImageRep -> IO Bool
opaque nsImageRep  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImageRep (mkSelector "opaque") retCULong []

-- | @- setOpaque:@
setOpaque :: IsNSImageRep nsImageRep => nsImageRep -> Bool -> IO ()
setOpaque nsImageRep  value =
    sendMsg nsImageRep (mkSelector "setOpaque:") retVoid [argCULong (if value then 1 else 0)]

-- | @- colorSpaceName@
colorSpaceName :: IsNSImageRep nsImageRep => nsImageRep -> IO (Id NSString)
colorSpaceName nsImageRep  =
    sendMsg nsImageRep (mkSelector "colorSpaceName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColorSpaceName:@
setColorSpaceName :: (IsNSImageRep nsImageRep, IsNSString value) => nsImageRep -> value -> IO ()
setColorSpaceName nsImageRep  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsImageRep (mkSelector "setColorSpaceName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bitsPerSample@
bitsPerSample :: IsNSImageRep nsImageRep => nsImageRep -> IO CLong
bitsPerSample nsImageRep  =
    sendMsg nsImageRep (mkSelector "bitsPerSample") retCLong []

-- | @- setBitsPerSample:@
setBitsPerSample :: IsNSImageRep nsImageRep => nsImageRep -> CLong -> IO ()
setBitsPerSample nsImageRep  value =
    sendMsg nsImageRep (mkSelector "setBitsPerSample:") retVoid [argCLong value]

-- | @- pixelsWide@
pixelsWide :: IsNSImageRep nsImageRep => nsImageRep -> IO CLong
pixelsWide nsImageRep  =
    sendMsg nsImageRep (mkSelector "pixelsWide") retCLong []

-- | @- setPixelsWide:@
setPixelsWide :: IsNSImageRep nsImageRep => nsImageRep -> CLong -> IO ()
setPixelsWide nsImageRep  value =
    sendMsg nsImageRep (mkSelector "setPixelsWide:") retVoid [argCLong value]

-- | @- pixelsHigh@
pixelsHigh :: IsNSImageRep nsImageRep => nsImageRep -> IO CLong
pixelsHigh nsImageRep  =
    sendMsg nsImageRep (mkSelector "pixelsHigh") retCLong []

-- | @- setPixelsHigh:@
setPixelsHigh :: IsNSImageRep nsImageRep => nsImageRep -> CLong -> IO ()
setPixelsHigh nsImageRep  value =
    sendMsg nsImageRep (mkSelector "setPixelsHigh:") retVoid [argCLong value]

-- | @- layoutDirection@
layoutDirection :: IsNSImageRep nsImageRep => nsImageRep -> IO NSImageLayoutDirection
layoutDirection nsImageRep  =
    fmap (coerce :: CLong -> NSImageLayoutDirection) $ sendMsg nsImageRep (mkSelector "layoutDirection") retCLong []

-- | @- setLayoutDirection:@
setLayoutDirection :: IsNSImageRep nsImageRep => nsImageRep -> NSImageLayoutDirection -> IO ()
setLayoutDirection nsImageRep  value =
    sendMsg nsImageRep (mkSelector "setLayoutDirection:") retVoid [argCLong (coerce value)]

-- | @+ registeredImageRepClasses@
registeredImageRepClasses :: IO (Id NSArray)
registeredImageRepClasses  =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMsg cls' (mkSelector "registeredImageRepClasses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ imageUnfilteredTypes@
imageUnfilteredTypes :: IO (Id NSArray)
imageUnfilteredTypes  =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMsg cls' (mkSelector "imageUnfilteredTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ imageTypes@
imageTypes :: IO (Id NSArray)
imageTypes  =
  do
    cls' <- getRequiredClass "NSImageRep"
    sendClassMsg cls' (mkSelector "imageTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @draw@
drawSelector :: Selector
drawSelector = mkSelector "draw"

-- | @Selector@ for @drawAtPoint:@
drawAtPointSelector :: Selector
drawAtPointSelector = mkSelector "drawAtPoint:"

-- | @Selector@ for @drawInRect:@
drawInRectSelector :: Selector
drawInRectSelector = mkSelector "drawInRect:"

-- | @Selector@ for @drawInRect:fromRect:operation:fraction:respectFlipped:hints:@
drawInRect_fromRect_operation_fraction_respectFlipped_hintsSelector :: Selector
drawInRect_fromRect_operation_fraction_respectFlipped_hintsSelector = mkSelector "drawInRect:fromRect:operation:fraction:respectFlipped:hints:"

-- | @Selector@ for @registerImageRepClass:@
registerImageRepClassSelector :: Selector
registerImageRepClassSelector = mkSelector "registerImageRepClass:"

-- | @Selector@ for @unregisterImageRepClass:@
unregisterImageRepClassSelector :: Selector
unregisterImageRepClassSelector = mkSelector "unregisterImageRepClass:"

-- | @Selector@ for @imageRepClassForFileType:@
imageRepClassForFileTypeSelector :: Selector
imageRepClassForFileTypeSelector = mkSelector "imageRepClassForFileType:"

-- | @Selector@ for @imageRepClassForPasteboardType:@
imageRepClassForPasteboardTypeSelector :: Selector
imageRepClassForPasteboardTypeSelector = mkSelector "imageRepClassForPasteboardType:"

-- | @Selector@ for @imageRepClassForType:@
imageRepClassForTypeSelector :: Selector
imageRepClassForTypeSelector = mkSelector "imageRepClassForType:"

-- | @Selector@ for @imageRepClassForData:@
imageRepClassForDataSelector :: Selector
imageRepClassForDataSelector = mkSelector "imageRepClassForData:"

-- | @Selector@ for @canInitWithData:@
canInitWithDataSelector :: Selector
canInitWithDataSelector = mkSelector "canInitWithData:"

-- | @Selector@ for @imageUnfilteredFileTypes@
imageUnfilteredFileTypesSelector :: Selector
imageUnfilteredFileTypesSelector = mkSelector "imageUnfilteredFileTypes"

-- | @Selector@ for @imageUnfilteredPasteboardTypes@
imageUnfilteredPasteboardTypesSelector :: Selector
imageUnfilteredPasteboardTypesSelector = mkSelector "imageUnfilteredPasteboardTypes"

-- | @Selector@ for @imageFileTypes@
imageFileTypesSelector :: Selector
imageFileTypesSelector = mkSelector "imageFileTypes"

-- | @Selector@ for @imagePasteboardTypes@
imagePasteboardTypesSelector :: Selector
imagePasteboardTypesSelector = mkSelector "imagePasteboardTypes"

-- | @Selector@ for @canInitWithPasteboard:@
canInitWithPasteboardSelector :: Selector
canInitWithPasteboardSelector = mkSelector "canInitWithPasteboard:"

-- | @Selector@ for @imageRepsWithContentsOfFile:@
imageRepsWithContentsOfFileSelector :: Selector
imageRepsWithContentsOfFileSelector = mkSelector "imageRepsWithContentsOfFile:"

-- | @Selector@ for @imageRepWithContentsOfFile:@
imageRepWithContentsOfFileSelector :: Selector
imageRepWithContentsOfFileSelector = mkSelector "imageRepWithContentsOfFile:"

-- | @Selector@ for @imageRepsWithContentsOfURL:@
imageRepsWithContentsOfURLSelector :: Selector
imageRepsWithContentsOfURLSelector = mkSelector "imageRepsWithContentsOfURL:"

-- | @Selector@ for @imageRepWithContentsOfURL:@
imageRepWithContentsOfURLSelector :: Selector
imageRepWithContentsOfURLSelector = mkSelector "imageRepWithContentsOfURL:"

-- | @Selector@ for @imageRepsWithPasteboard:@
imageRepsWithPasteboardSelector :: Selector
imageRepsWithPasteboardSelector = mkSelector "imageRepsWithPasteboard:"

-- | @Selector@ for @imageRepWithPasteboard:@
imageRepWithPasteboardSelector :: Selector
imageRepWithPasteboardSelector = mkSelector "imageRepWithPasteboard:"

-- | @Selector@ for @CGImageForProposedRect:context:hints:@
cgImageForProposedRect_context_hintsSelector :: Selector
cgImageForProposedRect_context_hintsSelector = mkSelector "CGImageForProposedRect:context:hints:"

-- | @Selector@ for @size@
sizeSelector :: Selector
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector
setAlphaSelector = mkSelector "setAlpha:"

-- | @Selector@ for @opaque@
opaqueSelector :: Selector
opaqueSelector = mkSelector "opaque"

-- | @Selector@ for @setOpaque:@
setOpaqueSelector :: Selector
setOpaqueSelector = mkSelector "setOpaque:"

-- | @Selector@ for @colorSpaceName@
colorSpaceNameSelector :: Selector
colorSpaceNameSelector = mkSelector "colorSpaceName"

-- | @Selector@ for @setColorSpaceName:@
setColorSpaceNameSelector :: Selector
setColorSpaceNameSelector = mkSelector "setColorSpaceName:"

-- | @Selector@ for @bitsPerSample@
bitsPerSampleSelector :: Selector
bitsPerSampleSelector = mkSelector "bitsPerSample"

-- | @Selector@ for @setBitsPerSample:@
setBitsPerSampleSelector :: Selector
setBitsPerSampleSelector = mkSelector "setBitsPerSample:"

-- | @Selector@ for @pixelsWide@
pixelsWideSelector :: Selector
pixelsWideSelector = mkSelector "pixelsWide"

-- | @Selector@ for @setPixelsWide:@
setPixelsWideSelector :: Selector
setPixelsWideSelector = mkSelector "setPixelsWide:"

-- | @Selector@ for @pixelsHigh@
pixelsHighSelector :: Selector
pixelsHighSelector = mkSelector "pixelsHigh"

-- | @Selector@ for @setPixelsHigh:@
setPixelsHighSelector :: Selector
setPixelsHighSelector = mkSelector "setPixelsHigh:"

-- | @Selector@ for @layoutDirection@
layoutDirectionSelector :: Selector
layoutDirectionSelector = mkSelector "layoutDirection"

-- | @Selector@ for @setLayoutDirection:@
setLayoutDirectionSelector :: Selector
setLayoutDirectionSelector = mkSelector "setLayoutDirection:"

-- | @Selector@ for @registeredImageRepClasses@
registeredImageRepClassesSelector :: Selector
registeredImageRepClassesSelector = mkSelector "registeredImageRepClasses"

-- | @Selector@ for @imageUnfilteredTypes@
imageUnfilteredTypesSelector :: Selector
imageUnfilteredTypesSelector = mkSelector "imageUnfilteredTypes"

-- | @Selector@ for @imageTypes@
imageTypesSelector :: Selector
imageTypesSelector = mkSelector "imageTypes"

