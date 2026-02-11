{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSImage@.
module ObjC.AppKit.NSImage
  ( NSImage
  , IsNSImage(..)
  , imageNamed
  , imageWithSystemSymbolName_accessibilityDescription
  , imageWithSystemSymbolName_variableValue_accessibilityDescription
  , imageWithSymbolName_variableValue
  , imageWithSymbolName_bundle_variableValue
  , initWithSize
  , initWithCoder
  , initWithData
  , initWithContentsOfFile
  , initWithContentsOfURL
  , initByReferencingFile
  , initByReferencingURL
  , initWithPasteboard
  , initWithDataIgnoringOrientation
  , imageWithSize_flipped_drawingHandler
  , setName
  , name
  , drawAtPoint_fromRect_operation_fraction
  , drawInRect_fromRect_operation_fraction
  , drawInRect_fromRect_operation_fraction_respectFlipped_hints
  , drawRepresentation_inRect
  , drawInRect
  , recache
  , tiffRepresentationUsingCompression_factor
  , addRepresentations
  , addRepresentation
  , removeRepresentation
  , canInitWithPasteboard
  , initWithCGImage_size
  , cgImageForProposedRect_context_hints
  , bestRepresentationForRect_context_hints
  , hitTestRect_withImageDestinationRect_context_hints_flipped
  , recommendedLayerContentsScale
  , layerContentsForContentsScale
  , imageWithSymbolConfiguration
  , imageWithLocale
  , imageUnfilteredFileTypes
  , imageUnfilteredPasteboardTypes
  , imageFileTypes
  , imagePasteboardTypes
  , initWithIconRef
  , bestRepresentationForDevice
  , lockFocus
  , lockFocusFlipped
  , unlockFocus
  , setFlipped
  , isFlipped
  , setScalesWhenResized
  , scalesWhenResized
  , setDataRetained
  , isDataRetained
  , setCachedSeparately
  , isCachedSeparately
  , setCacheDepthMatchesImageDepth
  , cacheDepthMatchesImageDepth
  , dissolveToPoint_fraction
  , dissolveToPoint_fromRect_fraction
  , compositeToPoint_operation
  , compositeToPoint_fromRect_operation
  , compositeToPoint_operation_fraction
  , compositeToPoint_fromRect_operation_fraction
  , lockFocusOnRepresentation
  , cancelIncrementalLoad
  , size
  , setSize
  , backgroundColor
  , setBackgroundColor
  , usesEPSOnResolutionMismatch
  , setUsesEPSOnResolutionMismatch
  , prefersColorMatch
  , setPrefersColorMatch
  , matchesOnMultipleResolution
  , setMatchesOnMultipleResolution
  , matchesOnlyOnBestFittingAxis
  , setMatchesOnlyOnBestFittingAxis
  , tiffRepresentation
  , representations
  , valid
  , cacheMode
  , setCacheMode
  , alignmentRect
  , setAlignmentRect
  , template
  , setTemplate
  , capInsets
  , setCapInsets
  , resizingMode
  , setResizingMode
  , imageNamedSelector
  , imageWithSystemSymbolName_accessibilityDescriptionSelector
  , imageWithSystemSymbolName_variableValue_accessibilityDescriptionSelector
  , imageWithSymbolName_variableValueSelector
  , imageWithSymbolName_bundle_variableValueSelector
  , initWithSizeSelector
  , initWithCoderSelector
  , initWithDataSelector
  , initWithContentsOfFileSelector
  , initWithContentsOfURLSelector
  , initByReferencingFileSelector
  , initByReferencingURLSelector
  , initWithPasteboardSelector
  , initWithDataIgnoringOrientationSelector
  , imageWithSize_flipped_drawingHandlerSelector
  , setNameSelector
  , nameSelector
  , drawAtPoint_fromRect_operation_fractionSelector
  , drawInRect_fromRect_operation_fractionSelector
  , drawInRect_fromRect_operation_fraction_respectFlipped_hintsSelector
  , drawRepresentation_inRectSelector
  , drawInRectSelector
  , recacheSelector
  , tiffRepresentationUsingCompression_factorSelector
  , addRepresentationsSelector
  , addRepresentationSelector
  , removeRepresentationSelector
  , canInitWithPasteboardSelector
  , initWithCGImage_sizeSelector
  , cgImageForProposedRect_context_hintsSelector
  , bestRepresentationForRect_context_hintsSelector
  , hitTestRect_withImageDestinationRect_context_hints_flippedSelector
  , recommendedLayerContentsScaleSelector
  , layerContentsForContentsScaleSelector
  , imageWithSymbolConfigurationSelector
  , imageWithLocaleSelector
  , imageUnfilteredFileTypesSelector
  , imageUnfilteredPasteboardTypesSelector
  , imageFileTypesSelector
  , imagePasteboardTypesSelector
  , initWithIconRefSelector
  , bestRepresentationForDeviceSelector
  , lockFocusSelector
  , lockFocusFlippedSelector
  , unlockFocusSelector
  , setFlippedSelector
  , isFlippedSelector
  , setScalesWhenResizedSelector
  , scalesWhenResizedSelector
  , setDataRetainedSelector
  , isDataRetainedSelector
  , setCachedSeparatelySelector
  , isCachedSeparatelySelector
  , setCacheDepthMatchesImageDepthSelector
  , cacheDepthMatchesImageDepthSelector
  , dissolveToPoint_fractionSelector
  , dissolveToPoint_fromRect_fractionSelector
  , compositeToPoint_operationSelector
  , compositeToPoint_fromRect_operationSelector
  , compositeToPoint_operation_fractionSelector
  , compositeToPoint_fromRect_operation_fractionSelector
  , lockFocusOnRepresentationSelector
  , cancelIncrementalLoadSelector
  , sizeSelector
  , setSizeSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , usesEPSOnResolutionMismatchSelector
  , setUsesEPSOnResolutionMismatchSelector
  , prefersColorMatchSelector
  , setPrefersColorMatchSelector
  , matchesOnMultipleResolutionSelector
  , setMatchesOnMultipleResolutionSelector
  , matchesOnlyOnBestFittingAxisSelector
  , setMatchesOnlyOnBestFittingAxisSelector
  , tiffRepresentationSelector
  , representationsSelector
  , validSelector
  , cacheModeSelector
  , setCacheModeSelector
  , alignmentRectSelector
  , setAlignmentRectSelector
  , templateSelector
  , setTemplateSelector
  , capInsetsSelector
  , setCapInsetsSelector
  , resizingModeSelector
  , setResizingModeSelector

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
  , NSImageCacheMode(NSImageCacheMode)
  , pattern NSImageCacheDefault
  , pattern NSImageCacheAlways
  , pattern NSImageCacheBySize
  , pattern NSImageCacheNever
  , NSImageResizingMode(NSImageResizingMode)
  , pattern NSImageResizingModeTile
  , pattern NSImageResizingModeStretch
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
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ imageNamed:@
imageNamed :: IsNSString name => name -> IO (Id NSImage)
imageNamed name =
  do
    cls' <- getRequiredClass "NSImage"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "imageNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a system symbol image with the specified name and value
--
-- @name@ — A name from the system’s SF Symbols catalog
--
-- @description@ — The image’s accessibility description. This description is used automatically by interface elements that display images. Like all accessibility descriptions, use a short localized string that does not include the name of the interface element. For instance, “delete” rather than “delete button”.
--
-- ObjC selector: @+ imageWithSystemSymbolName:accessibilityDescription:@
imageWithSystemSymbolName_accessibilityDescription :: (IsNSString name, IsNSString description) => name -> description -> IO (Id NSImage)
imageWithSystemSymbolName_accessibilityDescription name description =
  do
    cls' <- getRequiredClass "NSImage"
    withObjCPtr name $ \raw_name ->
      withObjCPtr description $ \raw_description ->
        sendClassMsg cls' (mkSelector "imageWithSystemSymbolName:accessibilityDescription:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_description :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a system symbol image with the specified name and value. The @value@ argument is only accommodated if the symbol supports variable rendering.
--
-- @name@ — A name from the system’s SF Symbols catalog
--
-- @value@ — The value represented by the symbol. The value should be between 0 and 1 inclusive ([0,1]).
--
-- @description@ — The image’s accessibility description. This description is used automatically by interface elements that display images. Like all accessibility descriptions, use a short localized string that does not include the name of the interface element. For instance, “delete” rather than “delete button”.
--
-- Note: Values less than 0 or greater than 1 will be clamped to 0 and 1, respectively.
--
-- ObjC selector: @+ imageWithSystemSymbolName:variableValue:accessibilityDescription:@
imageWithSystemSymbolName_variableValue_accessibilityDescription :: (IsNSString name, IsNSString description) => name -> CDouble -> description -> IO (Id NSImage)
imageWithSystemSymbolName_variableValue_accessibilityDescription name value description =
  do
    cls' <- getRequiredClass "NSImage"
    withObjCPtr name $ \raw_name ->
      withObjCPtr description $ \raw_description ->
        sendClassMsg cls' (mkSelector "imageWithSystemSymbolName:variableValue:accessibilityDescription:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCDouble (fromIntegral value), argPtr (castPtr raw_description :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a symbol image with the specified name and value. The @value@ argument is only accommodated if the symbol supports variable rendering.
--
-- @name@ — A name of a symbol image file in the main bundle’s catalog
--
-- @value@ — The value represented by the symbol. The value should be between 0 and 1 inclusive ([0,1]).
--
-- Note: Values less than 0 or greater than 1 will be clamped to 0 and 1, respectively.
--
-- ObjC selector: @+ imageWithSymbolName:variableValue:@
imageWithSymbolName_variableValue :: IsNSString name => name -> CDouble -> IO (Id NSImage)
imageWithSymbolName_variableValue name value =
  do
    cls' <- getRequiredClass "NSImage"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "imageWithSymbolName:variableValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCDouble (fromIntegral value)] >>= retainedObject . castPtr

-- | Creates a symbol image with the specified name and value. The @value@ argument is only accommodated if the symbol supports variable rendering.
--
-- @name@ — A name of a symbol image file in the main bundle’s catalog
--
-- @bundle@ — The bundle containing the image file or asset catalog. Specify @nil@ to search the app’s main bundle.
--
-- @value@ — The value represented by the symbol. The value should be between 0 and 1 inclusive ([0,1]).
--
-- Note: Values less than 0 or greater than 1 will be clamped to 0 and 1, respectively.
--
-- ObjC selector: @+ imageWithSymbolName:bundle:variableValue:@
imageWithSymbolName_bundle_variableValue :: (IsNSString name, IsNSBundle bundle) => name -> bundle -> CDouble -> IO (Id NSImage)
imageWithSymbolName_bundle_variableValue name bundle value =
  do
    cls' <- getRequiredClass "NSImage"
    withObjCPtr name $ \raw_name ->
      withObjCPtr bundle $ \raw_bundle ->
        sendClassMsg cls' (mkSelector "imageWithSymbolName:bundle:variableValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_bundle :: Ptr ()), argCDouble (fromIntegral value)] >>= retainedObject . castPtr

-- | @- initWithSize:@
initWithSize :: IsNSImage nsImage => nsImage -> NSSize -> IO (Id NSImage)
initWithSize nsImage  size =
  sendMsg nsImage (mkSelector "initWithSize:") (retPtr retVoid) [argNSSize size] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSImage nsImage, IsNSCoder coder) => nsImage -> coder -> IO (Id NSImage)
initWithCoder nsImage  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsImage (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithData:@
initWithData :: (IsNSImage nsImage, IsNSData data_) => nsImage -> data_ -> IO (Id NSImage)
initWithData nsImage  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg nsImage (mkSelector "initWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContentsOfFile:@
initWithContentsOfFile :: (IsNSImage nsImage, IsNSString fileName) => nsImage -> fileName -> IO (Id NSImage)
initWithContentsOfFile nsImage  fileName =
withObjCPtr fileName $ \raw_fileName ->
    sendMsg nsImage (mkSelector "initWithContentsOfFile:") (retPtr retVoid) [argPtr (castPtr raw_fileName :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSImage nsImage, IsNSURL url) => nsImage -> url -> IO (Id NSImage)
initWithContentsOfURL nsImage  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsImage (mkSelector "initWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- initByReferencingFile:@
initByReferencingFile :: (IsNSImage nsImage, IsNSString fileName) => nsImage -> fileName -> IO (Id NSImage)
initByReferencingFile nsImage  fileName =
withObjCPtr fileName $ \raw_fileName ->
    sendMsg nsImage (mkSelector "initByReferencingFile:") (retPtr retVoid) [argPtr (castPtr raw_fileName :: Ptr ())] >>= ownedObject . castPtr

-- | @- initByReferencingURL:@
initByReferencingURL :: (IsNSImage nsImage, IsNSURL url) => nsImage -> url -> IO (Id NSImage)
initByReferencingURL nsImage  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsImage (mkSelector "initByReferencingURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithPasteboard:@
initWithPasteboard :: (IsNSImage nsImage, IsNSPasteboard pasteboard) => nsImage -> pasteboard -> IO (Id NSImage)
initWithPasteboard nsImage  pasteboard =
withObjCPtr pasteboard $ \raw_pasteboard ->
    sendMsg nsImage (mkSelector "initWithPasteboard:") (retPtr retVoid) [argPtr (castPtr raw_pasteboard :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDataIgnoringOrientation:@
initWithDataIgnoringOrientation :: (IsNSImage nsImage, IsNSData data_) => nsImage -> data_ -> IO (Id NSImage)
initWithDataIgnoringOrientation nsImage  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg nsImage (mkSelector "initWithDataIgnoringOrientation:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @+ imageWithSize:flipped:drawingHandler:@
imageWithSize_flipped_drawingHandler :: NSSize -> Bool -> Ptr () -> IO (Id NSImage)
imageWithSize_flipped_drawingHandler size drawingHandlerShouldBeCalledWithFlippedContext drawingHandler =
  do
    cls' <- getRequiredClass "NSImage"
    sendClassMsg cls' (mkSelector "imageWithSize:flipped:drawingHandler:") (retPtr retVoid) [argNSSize size, argCULong (if drawingHandlerShouldBeCalledWithFlippedContext then 1 else 0), argPtr (castPtr drawingHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsNSImage nsImage, IsNSString string) => nsImage -> string -> IO Bool
setName nsImage  string =
withObjCPtr string $ \raw_string ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImage (mkSelector "setName:") retCULong [argPtr (castPtr raw_string :: Ptr ())]

-- | @- name@
name :: IsNSImage nsImage => nsImage -> IO (Id NSString)
name nsImage  =
  sendMsg nsImage (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- drawAtPoint:fromRect:operation:fraction:@
drawAtPoint_fromRect_operation_fraction :: IsNSImage nsImage => nsImage -> NSPoint -> NSRect -> NSCompositingOperation -> CDouble -> IO ()
drawAtPoint_fromRect_operation_fraction nsImage  point fromRect op delta =
  sendMsg nsImage (mkSelector "drawAtPoint:fromRect:operation:fraction:") retVoid [argNSPoint point, argNSRect fromRect, argCULong (coerce op), argCDouble (fromIntegral delta)]

-- | @- drawInRect:fromRect:operation:fraction:@
drawInRect_fromRect_operation_fraction :: IsNSImage nsImage => nsImage -> NSRect -> NSRect -> NSCompositingOperation -> CDouble -> IO ()
drawInRect_fromRect_operation_fraction nsImage  rect fromRect op delta =
  sendMsg nsImage (mkSelector "drawInRect:fromRect:operation:fraction:") retVoid [argNSRect rect, argNSRect fromRect, argCULong (coerce op), argCDouble (fromIntegral delta)]

-- | @- drawInRect:fromRect:operation:fraction:respectFlipped:hints:@
drawInRect_fromRect_operation_fraction_respectFlipped_hints :: (IsNSImage nsImage, IsNSDictionary hints) => nsImage -> NSRect -> NSRect -> NSCompositingOperation -> CDouble -> Bool -> hints -> IO ()
drawInRect_fromRect_operation_fraction_respectFlipped_hints nsImage  dstSpacePortionRect srcSpacePortionRect op requestedAlpha respectContextIsFlipped hints =
withObjCPtr hints $ \raw_hints ->
    sendMsg nsImage (mkSelector "drawInRect:fromRect:operation:fraction:respectFlipped:hints:") retVoid [argNSRect dstSpacePortionRect, argNSRect srcSpacePortionRect, argCULong (coerce op), argCDouble (fromIntegral requestedAlpha), argCULong (if respectContextIsFlipped then 1 else 0), argPtr (castPtr raw_hints :: Ptr ())]

-- | @- drawRepresentation:inRect:@
drawRepresentation_inRect :: (IsNSImage nsImage, IsNSImageRep imageRep) => nsImage -> imageRep -> NSRect -> IO Bool
drawRepresentation_inRect nsImage  imageRep rect =
withObjCPtr imageRep $ \raw_imageRep ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImage (mkSelector "drawRepresentation:inRect:") retCULong [argPtr (castPtr raw_imageRep :: Ptr ()), argNSRect rect]

-- | @- drawInRect:@
drawInRect :: IsNSImage nsImage => nsImage -> NSRect -> IO ()
drawInRect nsImage  rect =
  sendMsg nsImage (mkSelector "drawInRect:") retVoid [argNSRect rect]

-- | @- recache@
recache :: IsNSImage nsImage => nsImage -> IO ()
recache nsImage  =
  sendMsg nsImage (mkSelector "recache") retVoid []

-- | @- TIFFRepresentationUsingCompression:factor:@
tiffRepresentationUsingCompression_factor :: IsNSImage nsImage => nsImage -> NSTIFFCompression -> CFloat -> IO (Id NSData)
tiffRepresentationUsingCompression_factor nsImage  comp factor =
  sendMsg nsImage (mkSelector "TIFFRepresentationUsingCompression:factor:") (retPtr retVoid) [argCULong (coerce comp), argCFloat (fromIntegral factor)] >>= retainedObject . castPtr

-- | @- addRepresentations:@
addRepresentations :: (IsNSImage nsImage, IsNSArray imageReps) => nsImage -> imageReps -> IO ()
addRepresentations nsImage  imageReps =
withObjCPtr imageReps $ \raw_imageReps ->
    sendMsg nsImage (mkSelector "addRepresentations:") retVoid [argPtr (castPtr raw_imageReps :: Ptr ())]

-- | @- addRepresentation:@
addRepresentation :: (IsNSImage nsImage, IsNSImageRep imageRep) => nsImage -> imageRep -> IO ()
addRepresentation nsImage  imageRep =
withObjCPtr imageRep $ \raw_imageRep ->
    sendMsg nsImage (mkSelector "addRepresentation:") retVoid [argPtr (castPtr raw_imageRep :: Ptr ())]

-- | @- removeRepresentation:@
removeRepresentation :: (IsNSImage nsImage, IsNSImageRep imageRep) => nsImage -> imageRep -> IO ()
removeRepresentation nsImage  imageRep =
withObjCPtr imageRep $ \raw_imageRep ->
    sendMsg nsImage (mkSelector "removeRepresentation:") retVoid [argPtr (castPtr raw_imageRep :: Ptr ())]

-- | @+ canInitWithPasteboard:@
canInitWithPasteboard :: IsNSPasteboard pasteboard => pasteboard -> IO Bool
canInitWithPasteboard pasteboard =
  do
    cls' <- getRequiredClass "NSImage"
    withObjCPtr pasteboard $ \raw_pasteboard ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canInitWithPasteboard:") retCULong [argPtr (castPtr raw_pasteboard :: Ptr ())]

-- | @- initWithCGImage:size:@
initWithCGImage_size :: IsNSImage nsImage => nsImage -> Ptr () -> NSSize -> IO (Id NSImage)
initWithCGImage_size nsImage  cgImage size =
  sendMsg nsImage (mkSelector "initWithCGImage:size:") (retPtr retVoid) [argPtr cgImage, argNSSize size] >>= ownedObject . castPtr

-- | @- CGImageForProposedRect:context:hints:@
cgImageForProposedRect_context_hints :: (IsNSImage nsImage, IsNSGraphicsContext referenceContext, IsNSDictionary hints) => nsImage -> Ptr NSRect -> referenceContext -> hints -> IO (Ptr ())
cgImageForProposedRect_context_hints nsImage  proposedDestRect referenceContext hints =
withObjCPtr referenceContext $ \raw_referenceContext ->
  withObjCPtr hints $ \raw_hints ->
      fmap castPtr $ sendMsg nsImage (mkSelector "CGImageForProposedRect:context:hints:") (retPtr retVoid) [argPtr proposedDestRect, argPtr (castPtr raw_referenceContext :: Ptr ()), argPtr (castPtr raw_hints :: Ptr ())]

-- | @- bestRepresentationForRect:context:hints:@
bestRepresentationForRect_context_hints :: (IsNSImage nsImage, IsNSGraphicsContext referenceContext, IsNSDictionary hints) => nsImage -> NSRect -> referenceContext -> hints -> IO (Id NSImageRep)
bestRepresentationForRect_context_hints nsImage  rect referenceContext hints =
withObjCPtr referenceContext $ \raw_referenceContext ->
  withObjCPtr hints $ \raw_hints ->
      sendMsg nsImage (mkSelector "bestRepresentationForRect:context:hints:") (retPtr retVoid) [argNSRect rect, argPtr (castPtr raw_referenceContext :: Ptr ()), argPtr (castPtr raw_hints :: Ptr ())] >>= retainedObject . castPtr

-- | @- hitTestRect:withImageDestinationRect:context:hints:flipped:@
hitTestRect_withImageDestinationRect_context_hints_flipped :: (IsNSImage nsImage, IsNSGraphicsContext context, IsNSDictionary hints) => nsImage -> NSRect -> NSRect -> context -> hints -> Bool -> IO Bool
hitTestRect_withImageDestinationRect_context_hints_flipped nsImage  testRectDestSpace imageRectDestSpace context hints flipped =
withObjCPtr context $ \raw_context ->
  withObjCPtr hints $ \raw_hints ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImage (mkSelector "hitTestRect:withImageDestinationRect:context:hints:flipped:") retCULong [argNSRect testRectDestSpace, argNSRect imageRectDestSpace, argPtr (castPtr raw_context :: Ptr ()), argPtr (castPtr raw_hints :: Ptr ()), argCULong (if flipped then 1 else 0)]

-- | @- recommendedLayerContentsScale:@
recommendedLayerContentsScale :: IsNSImage nsImage => nsImage -> CDouble -> IO CDouble
recommendedLayerContentsScale nsImage  preferredContentsScale =
  sendMsg nsImage (mkSelector "recommendedLayerContentsScale:") retCDouble [argCDouble (fromIntegral preferredContentsScale)]

-- | @- layerContentsForContentsScale:@
layerContentsForContentsScale :: IsNSImage nsImage => nsImage -> CDouble -> IO RawId
layerContentsForContentsScale nsImage  layerContentsScale =
  fmap (RawId . castPtr) $ sendMsg nsImage (mkSelector "layerContentsForContentsScale:") (retPtr retVoid) [argCDouble (fromIntegral layerContentsScale)]

-- | @- imageWithSymbolConfiguration:@
imageWithSymbolConfiguration :: (IsNSImage nsImage, IsNSImageSymbolConfiguration configuration) => nsImage -> configuration -> IO (Id NSImage)
imageWithSymbolConfiguration nsImage  configuration =
withObjCPtr configuration $ \raw_configuration ->
    sendMsg nsImage (mkSelector "imageWithSymbolConfiguration:") (retPtr retVoid) [argPtr (castPtr raw_configuration :: Ptr ())] >>= retainedObject . castPtr

-- | Creates and returns a new image with the specified locale. If the receiver contains locale-sensitive representations, the returned image will prefer to draw using representations appropriate for the specified locale. If locale is @nil@, the returned image uses the default behavior of choosing representations appropriate for the system’s currently-configured locale.
--
-- ObjC selector: @- imageWithLocale:@
imageWithLocale :: (IsNSImage nsImage, IsNSLocale locale) => nsImage -> locale -> IO (Id NSImage)
imageWithLocale nsImage  locale =
withObjCPtr locale $ \raw_locale ->
    sendMsg nsImage (mkSelector "imageWithLocale:") (retPtr retVoid) [argPtr (castPtr raw_locale :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageUnfilteredFileTypes@
imageUnfilteredFileTypes :: IO (Id NSArray)
imageUnfilteredFileTypes  =
  do
    cls' <- getRequiredClass "NSImage"
    sendClassMsg cls' (mkSelector "imageUnfilteredFileTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ imageUnfilteredPasteboardTypes@
imageUnfilteredPasteboardTypes :: IO (Id NSArray)
imageUnfilteredPasteboardTypes  =
  do
    cls' <- getRequiredClass "NSImage"
    sendClassMsg cls' (mkSelector "imageUnfilteredPasteboardTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ imageFileTypes@
imageFileTypes :: IO (Id NSArray)
imageFileTypes  =
  do
    cls' <- getRequiredClass "NSImage"
    sendClassMsg cls' (mkSelector "imageFileTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ imagePasteboardTypes@
imagePasteboardTypes :: IO (Id NSArray)
imagePasteboardTypes  =
  do
    cls' <- getRequiredClass "NSImage"
    sendClassMsg cls' (mkSelector "imagePasteboardTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- initWithIconRef:@
initWithIconRef :: IsNSImage nsImage => nsImage -> Ptr () -> IO (Id NSImage)
initWithIconRef nsImage  iconRef =
  sendMsg nsImage (mkSelector "initWithIconRef:") (retPtr retVoid) [argPtr iconRef] >>= ownedObject . castPtr

-- | @- bestRepresentationForDevice:@
bestRepresentationForDevice :: (IsNSImage nsImage, IsNSDictionary deviceDescription) => nsImage -> deviceDescription -> IO (Id NSImageRep)
bestRepresentationForDevice nsImage  deviceDescription =
withObjCPtr deviceDescription $ \raw_deviceDescription ->
    sendMsg nsImage (mkSelector "bestRepresentationForDevice:") (retPtr retVoid) [argPtr (castPtr raw_deviceDescription :: Ptr ())] >>= retainedObject . castPtr

-- | @- lockFocus@
lockFocus :: IsNSImage nsImage => nsImage -> IO ()
lockFocus nsImage  =
  sendMsg nsImage (mkSelector "lockFocus") retVoid []

-- | @- lockFocusFlipped:@
lockFocusFlipped :: IsNSImage nsImage => nsImage -> Bool -> IO ()
lockFocusFlipped nsImage  flipped =
  sendMsg nsImage (mkSelector "lockFocusFlipped:") retVoid [argCULong (if flipped then 1 else 0)]

-- | @- unlockFocus@
unlockFocus :: IsNSImage nsImage => nsImage -> IO ()
unlockFocus nsImage  =
  sendMsg nsImage (mkSelector "unlockFocus") retVoid []

-- | @- setFlipped:@
setFlipped :: IsNSImage nsImage => nsImage -> Bool -> IO ()
setFlipped nsImage  flag =
  sendMsg nsImage (mkSelector "setFlipped:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- isFlipped@
isFlipped :: IsNSImage nsImage => nsImage -> IO Bool
isFlipped nsImage  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImage (mkSelector "isFlipped") retCULong []

-- | @- setScalesWhenResized:@
setScalesWhenResized :: IsNSImage nsImage => nsImage -> Bool -> IO ()
setScalesWhenResized nsImage  flag =
  sendMsg nsImage (mkSelector "setScalesWhenResized:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- scalesWhenResized@
scalesWhenResized :: IsNSImage nsImage => nsImage -> IO Bool
scalesWhenResized nsImage  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImage (mkSelector "scalesWhenResized") retCULong []

-- | @- setDataRetained:@
setDataRetained :: IsNSImage nsImage => nsImage -> Bool -> IO ()
setDataRetained nsImage  flag =
  sendMsg nsImage (mkSelector "setDataRetained:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- isDataRetained@
isDataRetained :: IsNSImage nsImage => nsImage -> IO Bool
isDataRetained nsImage  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImage (mkSelector "isDataRetained") retCULong []

-- | @- setCachedSeparately:@
setCachedSeparately :: IsNSImage nsImage => nsImage -> Bool -> IO ()
setCachedSeparately nsImage  flag =
  sendMsg nsImage (mkSelector "setCachedSeparately:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- isCachedSeparately@
isCachedSeparately :: IsNSImage nsImage => nsImage -> IO Bool
isCachedSeparately nsImage  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImage (mkSelector "isCachedSeparately") retCULong []

-- | @- setCacheDepthMatchesImageDepth:@
setCacheDepthMatchesImageDepth :: IsNSImage nsImage => nsImage -> Bool -> IO ()
setCacheDepthMatchesImageDepth nsImage  flag =
  sendMsg nsImage (mkSelector "setCacheDepthMatchesImageDepth:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- cacheDepthMatchesImageDepth@
cacheDepthMatchesImageDepth :: IsNSImage nsImage => nsImage -> IO Bool
cacheDepthMatchesImageDepth nsImage  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImage (mkSelector "cacheDepthMatchesImageDepth") retCULong []

-- | @- dissolveToPoint:fraction:@
dissolveToPoint_fraction :: IsNSImage nsImage => nsImage -> NSPoint -> CDouble -> IO ()
dissolveToPoint_fraction nsImage  point fraction =
  sendMsg nsImage (mkSelector "dissolveToPoint:fraction:") retVoid [argNSPoint point, argCDouble (fromIntegral fraction)]

-- | @- dissolveToPoint:fromRect:fraction:@
dissolveToPoint_fromRect_fraction :: IsNSImage nsImage => nsImage -> NSPoint -> NSRect -> CDouble -> IO ()
dissolveToPoint_fromRect_fraction nsImage  point rect fraction =
  sendMsg nsImage (mkSelector "dissolveToPoint:fromRect:fraction:") retVoid [argNSPoint point, argNSRect rect, argCDouble (fromIntegral fraction)]

-- | @- compositeToPoint:operation:@
compositeToPoint_operation :: IsNSImage nsImage => nsImage -> NSPoint -> NSCompositingOperation -> IO ()
compositeToPoint_operation nsImage  point operation =
  sendMsg nsImage (mkSelector "compositeToPoint:operation:") retVoid [argNSPoint point, argCULong (coerce operation)]

-- | @- compositeToPoint:fromRect:operation:@
compositeToPoint_fromRect_operation :: IsNSImage nsImage => nsImage -> NSPoint -> NSRect -> NSCompositingOperation -> IO ()
compositeToPoint_fromRect_operation nsImage  point rect operation =
  sendMsg nsImage (mkSelector "compositeToPoint:fromRect:operation:") retVoid [argNSPoint point, argNSRect rect, argCULong (coerce operation)]

-- | @- compositeToPoint:operation:fraction:@
compositeToPoint_operation_fraction :: IsNSImage nsImage => nsImage -> NSPoint -> NSCompositingOperation -> CDouble -> IO ()
compositeToPoint_operation_fraction nsImage  point operation fraction =
  sendMsg nsImage (mkSelector "compositeToPoint:operation:fraction:") retVoid [argNSPoint point, argCULong (coerce operation), argCDouble (fromIntegral fraction)]

-- | @- compositeToPoint:fromRect:operation:fraction:@
compositeToPoint_fromRect_operation_fraction :: IsNSImage nsImage => nsImage -> NSPoint -> NSRect -> NSCompositingOperation -> CDouble -> IO ()
compositeToPoint_fromRect_operation_fraction nsImage  point rect operation fraction =
  sendMsg nsImage (mkSelector "compositeToPoint:fromRect:operation:fraction:") retVoid [argNSPoint point, argNSRect rect, argCULong (coerce operation), argCDouble (fromIntegral fraction)]

-- | @- lockFocusOnRepresentation:@
lockFocusOnRepresentation :: (IsNSImage nsImage, IsNSImageRep imageRepresentation) => nsImage -> imageRepresentation -> IO ()
lockFocusOnRepresentation nsImage  imageRepresentation =
withObjCPtr imageRepresentation $ \raw_imageRepresentation ->
    sendMsg nsImage (mkSelector "lockFocusOnRepresentation:") retVoid [argPtr (castPtr raw_imageRepresentation :: Ptr ())]

-- | @- cancelIncrementalLoad@
cancelIncrementalLoad :: IsNSImage nsImage => nsImage -> IO ()
cancelIncrementalLoad nsImage  =
  sendMsg nsImage (mkSelector "cancelIncrementalLoad") retVoid []

-- | @- size@
size :: IsNSImage nsImage => nsImage -> IO NSSize
size nsImage  =
  sendMsgStret nsImage (mkSelector "size") retNSSize []

-- | @- setSize:@
setSize :: IsNSImage nsImage => nsImage -> NSSize -> IO ()
setSize nsImage  value =
  sendMsg nsImage (mkSelector "setSize:") retVoid [argNSSize value]

-- | @- backgroundColor@
backgroundColor :: IsNSImage nsImage => nsImage -> IO (Id NSColor)
backgroundColor nsImage  =
  sendMsg nsImage (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSImage nsImage, IsNSColor value) => nsImage -> value -> IO ()
setBackgroundColor nsImage  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsImage (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- usesEPSOnResolutionMismatch@
usesEPSOnResolutionMismatch :: IsNSImage nsImage => nsImage -> IO Bool
usesEPSOnResolutionMismatch nsImage  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImage (mkSelector "usesEPSOnResolutionMismatch") retCULong []

-- | @- setUsesEPSOnResolutionMismatch:@
setUsesEPSOnResolutionMismatch :: IsNSImage nsImage => nsImage -> Bool -> IO ()
setUsesEPSOnResolutionMismatch nsImage  value =
  sendMsg nsImage (mkSelector "setUsesEPSOnResolutionMismatch:") retVoid [argCULong (if value then 1 else 0)]

-- | @- prefersColorMatch@
prefersColorMatch :: IsNSImage nsImage => nsImage -> IO Bool
prefersColorMatch nsImage  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImage (mkSelector "prefersColorMatch") retCULong []

-- | @- setPrefersColorMatch:@
setPrefersColorMatch :: IsNSImage nsImage => nsImage -> Bool -> IO ()
setPrefersColorMatch nsImage  value =
  sendMsg nsImage (mkSelector "setPrefersColorMatch:") retVoid [argCULong (if value then 1 else 0)]

-- | @- matchesOnMultipleResolution@
matchesOnMultipleResolution :: IsNSImage nsImage => nsImage -> IO Bool
matchesOnMultipleResolution nsImage  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImage (mkSelector "matchesOnMultipleResolution") retCULong []

-- | @- setMatchesOnMultipleResolution:@
setMatchesOnMultipleResolution :: IsNSImage nsImage => nsImage -> Bool -> IO ()
setMatchesOnMultipleResolution nsImage  value =
  sendMsg nsImage (mkSelector "setMatchesOnMultipleResolution:") retVoid [argCULong (if value then 1 else 0)]

-- | @- matchesOnlyOnBestFittingAxis@
matchesOnlyOnBestFittingAxis :: IsNSImage nsImage => nsImage -> IO Bool
matchesOnlyOnBestFittingAxis nsImage  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImage (mkSelector "matchesOnlyOnBestFittingAxis") retCULong []

-- | @- setMatchesOnlyOnBestFittingAxis:@
setMatchesOnlyOnBestFittingAxis :: IsNSImage nsImage => nsImage -> Bool -> IO ()
setMatchesOnlyOnBestFittingAxis nsImage  value =
  sendMsg nsImage (mkSelector "setMatchesOnlyOnBestFittingAxis:") retVoid [argCULong (if value then 1 else 0)]

-- | @- TIFFRepresentation@
tiffRepresentation :: IsNSImage nsImage => nsImage -> IO (Id NSData)
tiffRepresentation nsImage  =
  sendMsg nsImage (mkSelector "TIFFRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- representations@
representations :: IsNSImage nsImage => nsImage -> IO (Id NSArray)
representations nsImage  =
  sendMsg nsImage (mkSelector "representations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- valid@
valid :: IsNSImage nsImage => nsImage -> IO Bool
valid nsImage  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImage (mkSelector "valid") retCULong []

-- | @- cacheMode@
cacheMode :: IsNSImage nsImage => nsImage -> IO NSImageCacheMode
cacheMode nsImage  =
  fmap (coerce :: CULong -> NSImageCacheMode) $ sendMsg nsImage (mkSelector "cacheMode") retCULong []

-- | @- setCacheMode:@
setCacheMode :: IsNSImage nsImage => nsImage -> NSImageCacheMode -> IO ()
setCacheMode nsImage  value =
  sendMsg nsImage (mkSelector "setCacheMode:") retVoid [argCULong (coerce value)]

-- | @- alignmentRect@
alignmentRect :: IsNSImage nsImage => nsImage -> IO NSRect
alignmentRect nsImage  =
  sendMsgStret nsImage (mkSelector "alignmentRect") retNSRect []

-- | @- setAlignmentRect:@
setAlignmentRect :: IsNSImage nsImage => nsImage -> NSRect -> IO ()
setAlignmentRect nsImage  value =
  sendMsg nsImage (mkSelector "setAlignmentRect:") retVoid [argNSRect value]

-- | @- template@
template :: IsNSImage nsImage => nsImage -> IO Bool
template nsImage  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImage (mkSelector "template") retCULong []

-- | @- setTemplate:@
setTemplate :: IsNSImage nsImage => nsImage -> Bool -> IO ()
setTemplate nsImage  value =
  sendMsg nsImage (mkSelector "setTemplate:") retVoid [argCULong (if value then 1 else 0)]

-- | @- capInsets@
capInsets :: IsNSImage nsImage => nsImage -> IO NSEdgeInsets
capInsets nsImage  =
  sendMsgStret nsImage (mkSelector "capInsets") retNSEdgeInsets []

-- | @- setCapInsets:@
setCapInsets :: IsNSImage nsImage => nsImage -> NSEdgeInsets -> IO ()
setCapInsets nsImage  value =
  sendMsg nsImage (mkSelector "setCapInsets:") retVoid [argNSEdgeInsets value]

-- | @- resizingMode@
resizingMode :: IsNSImage nsImage => nsImage -> IO NSImageResizingMode
resizingMode nsImage  =
  fmap (coerce :: CLong -> NSImageResizingMode) $ sendMsg nsImage (mkSelector "resizingMode") retCLong []

-- | @- setResizingMode:@
setResizingMode :: IsNSImage nsImage => nsImage -> NSImageResizingMode -> IO ()
setResizingMode nsImage  value =
  sendMsg nsImage (mkSelector "setResizingMode:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageNamed:@
imageNamedSelector :: Selector
imageNamedSelector = mkSelector "imageNamed:"

-- | @Selector@ for @imageWithSystemSymbolName:accessibilityDescription:@
imageWithSystemSymbolName_accessibilityDescriptionSelector :: Selector
imageWithSystemSymbolName_accessibilityDescriptionSelector = mkSelector "imageWithSystemSymbolName:accessibilityDescription:"

-- | @Selector@ for @imageWithSystemSymbolName:variableValue:accessibilityDescription:@
imageWithSystemSymbolName_variableValue_accessibilityDescriptionSelector :: Selector
imageWithSystemSymbolName_variableValue_accessibilityDescriptionSelector = mkSelector "imageWithSystemSymbolName:variableValue:accessibilityDescription:"

-- | @Selector@ for @imageWithSymbolName:variableValue:@
imageWithSymbolName_variableValueSelector :: Selector
imageWithSymbolName_variableValueSelector = mkSelector "imageWithSymbolName:variableValue:"

-- | @Selector@ for @imageWithSymbolName:bundle:variableValue:@
imageWithSymbolName_bundle_variableValueSelector :: Selector
imageWithSymbolName_bundle_variableValueSelector = mkSelector "imageWithSymbolName:bundle:variableValue:"

-- | @Selector@ for @initWithSize:@
initWithSizeSelector :: Selector
initWithSizeSelector = mkSelector "initWithSize:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @initWithContentsOfFile:@
initWithContentsOfFileSelector :: Selector
initWithContentsOfFileSelector = mkSelector "initWithContentsOfFile:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @initByReferencingFile:@
initByReferencingFileSelector :: Selector
initByReferencingFileSelector = mkSelector "initByReferencingFile:"

-- | @Selector@ for @initByReferencingURL:@
initByReferencingURLSelector :: Selector
initByReferencingURLSelector = mkSelector "initByReferencingURL:"

-- | @Selector@ for @initWithPasteboard:@
initWithPasteboardSelector :: Selector
initWithPasteboardSelector = mkSelector "initWithPasteboard:"

-- | @Selector@ for @initWithDataIgnoringOrientation:@
initWithDataIgnoringOrientationSelector :: Selector
initWithDataIgnoringOrientationSelector = mkSelector "initWithDataIgnoringOrientation:"

-- | @Selector@ for @imageWithSize:flipped:drawingHandler:@
imageWithSize_flipped_drawingHandlerSelector :: Selector
imageWithSize_flipped_drawingHandlerSelector = mkSelector "imageWithSize:flipped:drawingHandler:"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @drawAtPoint:fromRect:operation:fraction:@
drawAtPoint_fromRect_operation_fractionSelector :: Selector
drawAtPoint_fromRect_operation_fractionSelector = mkSelector "drawAtPoint:fromRect:operation:fraction:"

-- | @Selector@ for @drawInRect:fromRect:operation:fraction:@
drawInRect_fromRect_operation_fractionSelector :: Selector
drawInRect_fromRect_operation_fractionSelector = mkSelector "drawInRect:fromRect:operation:fraction:"

-- | @Selector@ for @drawInRect:fromRect:operation:fraction:respectFlipped:hints:@
drawInRect_fromRect_operation_fraction_respectFlipped_hintsSelector :: Selector
drawInRect_fromRect_operation_fraction_respectFlipped_hintsSelector = mkSelector "drawInRect:fromRect:operation:fraction:respectFlipped:hints:"

-- | @Selector@ for @drawRepresentation:inRect:@
drawRepresentation_inRectSelector :: Selector
drawRepresentation_inRectSelector = mkSelector "drawRepresentation:inRect:"

-- | @Selector@ for @drawInRect:@
drawInRectSelector :: Selector
drawInRectSelector = mkSelector "drawInRect:"

-- | @Selector@ for @recache@
recacheSelector :: Selector
recacheSelector = mkSelector "recache"

-- | @Selector@ for @TIFFRepresentationUsingCompression:factor:@
tiffRepresentationUsingCompression_factorSelector :: Selector
tiffRepresentationUsingCompression_factorSelector = mkSelector "TIFFRepresentationUsingCompression:factor:"

-- | @Selector@ for @addRepresentations:@
addRepresentationsSelector :: Selector
addRepresentationsSelector = mkSelector "addRepresentations:"

-- | @Selector@ for @addRepresentation:@
addRepresentationSelector :: Selector
addRepresentationSelector = mkSelector "addRepresentation:"

-- | @Selector@ for @removeRepresentation:@
removeRepresentationSelector :: Selector
removeRepresentationSelector = mkSelector "removeRepresentation:"

-- | @Selector@ for @canInitWithPasteboard:@
canInitWithPasteboardSelector :: Selector
canInitWithPasteboardSelector = mkSelector "canInitWithPasteboard:"

-- | @Selector@ for @initWithCGImage:size:@
initWithCGImage_sizeSelector :: Selector
initWithCGImage_sizeSelector = mkSelector "initWithCGImage:size:"

-- | @Selector@ for @CGImageForProposedRect:context:hints:@
cgImageForProposedRect_context_hintsSelector :: Selector
cgImageForProposedRect_context_hintsSelector = mkSelector "CGImageForProposedRect:context:hints:"

-- | @Selector@ for @bestRepresentationForRect:context:hints:@
bestRepresentationForRect_context_hintsSelector :: Selector
bestRepresentationForRect_context_hintsSelector = mkSelector "bestRepresentationForRect:context:hints:"

-- | @Selector@ for @hitTestRect:withImageDestinationRect:context:hints:flipped:@
hitTestRect_withImageDestinationRect_context_hints_flippedSelector :: Selector
hitTestRect_withImageDestinationRect_context_hints_flippedSelector = mkSelector "hitTestRect:withImageDestinationRect:context:hints:flipped:"

-- | @Selector@ for @recommendedLayerContentsScale:@
recommendedLayerContentsScaleSelector :: Selector
recommendedLayerContentsScaleSelector = mkSelector "recommendedLayerContentsScale:"

-- | @Selector@ for @layerContentsForContentsScale:@
layerContentsForContentsScaleSelector :: Selector
layerContentsForContentsScaleSelector = mkSelector "layerContentsForContentsScale:"

-- | @Selector@ for @imageWithSymbolConfiguration:@
imageWithSymbolConfigurationSelector :: Selector
imageWithSymbolConfigurationSelector = mkSelector "imageWithSymbolConfiguration:"

-- | @Selector@ for @imageWithLocale:@
imageWithLocaleSelector :: Selector
imageWithLocaleSelector = mkSelector "imageWithLocale:"

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

-- | @Selector@ for @initWithIconRef:@
initWithIconRefSelector :: Selector
initWithIconRefSelector = mkSelector "initWithIconRef:"

-- | @Selector@ for @bestRepresentationForDevice:@
bestRepresentationForDeviceSelector :: Selector
bestRepresentationForDeviceSelector = mkSelector "bestRepresentationForDevice:"

-- | @Selector@ for @lockFocus@
lockFocusSelector :: Selector
lockFocusSelector = mkSelector "lockFocus"

-- | @Selector@ for @lockFocusFlipped:@
lockFocusFlippedSelector :: Selector
lockFocusFlippedSelector = mkSelector "lockFocusFlipped:"

-- | @Selector@ for @unlockFocus@
unlockFocusSelector :: Selector
unlockFocusSelector = mkSelector "unlockFocus"

-- | @Selector@ for @setFlipped:@
setFlippedSelector :: Selector
setFlippedSelector = mkSelector "setFlipped:"

-- | @Selector@ for @isFlipped@
isFlippedSelector :: Selector
isFlippedSelector = mkSelector "isFlipped"

-- | @Selector@ for @setScalesWhenResized:@
setScalesWhenResizedSelector :: Selector
setScalesWhenResizedSelector = mkSelector "setScalesWhenResized:"

-- | @Selector@ for @scalesWhenResized@
scalesWhenResizedSelector :: Selector
scalesWhenResizedSelector = mkSelector "scalesWhenResized"

-- | @Selector@ for @setDataRetained:@
setDataRetainedSelector :: Selector
setDataRetainedSelector = mkSelector "setDataRetained:"

-- | @Selector@ for @isDataRetained@
isDataRetainedSelector :: Selector
isDataRetainedSelector = mkSelector "isDataRetained"

-- | @Selector@ for @setCachedSeparately:@
setCachedSeparatelySelector :: Selector
setCachedSeparatelySelector = mkSelector "setCachedSeparately:"

-- | @Selector@ for @isCachedSeparately@
isCachedSeparatelySelector :: Selector
isCachedSeparatelySelector = mkSelector "isCachedSeparately"

-- | @Selector@ for @setCacheDepthMatchesImageDepth:@
setCacheDepthMatchesImageDepthSelector :: Selector
setCacheDepthMatchesImageDepthSelector = mkSelector "setCacheDepthMatchesImageDepth:"

-- | @Selector@ for @cacheDepthMatchesImageDepth@
cacheDepthMatchesImageDepthSelector :: Selector
cacheDepthMatchesImageDepthSelector = mkSelector "cacheDepthMatchesImageDepth"

-- | @Selector@ for @dissolveToPoint:fraction:@
dissolveToPoint_fractionSelector :: Selector
dissolveToPoint_fractionSelector = mkSelector "dissolveToPoint:fraction:"

-- | @Selector@ for @dissolveToPoint:fromRect:fraction:@
dissolveToPoint_fromRect_fractionSelector :: Selector
dissolveToPoint_fromRect_fractionSelector = mkSelector "dissolveToPoint:fromRect:fraction:"

-- | @Selector@ for @compositeToPoint:operation:@
compositeToPoint_operationSelector :: Selector
compositeToPoint_operationSelector = mkSelector "compositeToPoint:operation:"

-- | @Selector@ for @compositeToPoint:fromRect:operation:@
compositeToPoint_fromRect_operationSelector :: Selector
compositeToPoint_fromRect_operationSelector = mkSelector "compositeToPoint:fromRect:operation:"

-- | @Selector@ for @compositeToPoint:operation:fraction:@
compositeToPoint_operation_fractionSelector :: Selector
compositeToPoint_operation_fractionSelector = mkSelector "compositeToPoint:operation:fraction:"

-- | @Selector@ for @compositeToPoint:fromRect:operation:fraction:@
compositeToPoint_fromRect_operation_fractionSelector :: Selector
compositeToPoint_fromRect_operation_fractionSelector = mkSelector "compositeToPoint:fromRect:operation:fraction:"

-- | @Selector@ for @lockFocusOnRepresentation:@
lockFocusOnRepresentationSelector :: Selector
lockFocusOnRepresentationSelector = mkSelector "lockFocusOnRepresentation:"

-- | @Selector@ for @cancelIncrementalLoad@
cancelIncrementalLoadSelector :: Selector
cancelIncrementalLoadSelector = mkSelector "cancelIncrementalLoad"

-- | @Selector@ for @size@
sizeSelector :: Selector
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @usesEPSOnResolutionMismatch@
usesEPSOnResolutionMismatchSelector :: Selector
usesEPSOnResolutionMismatchSelector = mkSelector "usesEPSOnResolutionMismatch"

-- | @Selector@ for @setUsesEPSOnResolutionMismatch:@
setUsesEPSOnResolutionMismatchSelector :: Selector
setUsesEPSOnResolutionMismatchSelector = mkSelector "setUsesEPSOnResolutionMismatch:"

-- | @Selector@ for @prefersColorMatch@
prefersColorMatchSelector :: Selector
prefersColorMatchSelector = mkSelector "prefersColorMatch"

-- | @Selector@ for @setPrefersColorMatch:@
setPrefersColorMatchSelector :: Selector
setPrefersColorMatchSelector = mkSelector "setPrefersColorMatch:"

-- | @Selector@ for @matchesOnMultipleResolution@
matchesOnMultipleResolutionSelector :: Selector
matchesOnMultipleResolutionSelector = mkSelector "matchesOnMultipleResolution"

-- | @Selector@ for @setMatchesOnMultipleResolution:@
setMatchesOnMultipleResolutionSelector :: Selector
setMatchesOnMultipleResolutionSelector = mkSelector "setMatchesOnMultipleResolution:"

-- | @Selector@ for @matchesOnlyOnBestFittingAxis@
matchesOnlyOnBestFittingAxisSelector :: Selector
matchesOnlyOnBestFittingAxisSelector = mkSelector "matchesOnlyOnBestFittingAxis"

-- | @Selector@ for @setMatchesOnlyOnBestFittingAxis:@
setMatchesOnlyOnBestFittingAxisSelector :: Selector
setMatchesOnlyOnBestFittingAxisSelector = mkSelector "setMatchesOnlyOnBestFittingAxis:"

-- | @Selector@ for @TIFFRepresentation@
tiffRepresentationSelector :: Selector
tiffRepresentationSelector = mkSelector "TIFFRepresentation"

-- | @Selector@ for @representations@
representationsSelector :: Selector
representationsSelector = mkSelector "representations"

-- | @Selector@ for @valid@
validSelector :: Selector
validSelector = mkSelector "valid"

-- | @Selector@ for @cacheMode@
cacheModeSelector :: Selector
cacheModeSelector = mkSelector "cacheMode"

-- | @Selector@ for @setCacheMode:@
setCacheModeSelector :: Selector
setCacheModeSelector = mkSelector "setCacheMode:"

-- | @Selector@ for @alignmentRect@
alignmentRectSelector :: Selector
alignmentRectSelector = mkSelector "alignmentRect"

-- | @Selector@ for @setAlignmentRect:@
setAlignmentRectSelector :: Selector
setAlignmentRectSelector = mkSelector "setAlignmentRect:"

-- | @Selector@ for @template@
templateSelector :: Selector
templateSelector = mkSelector "template"

-- | @Selector@ for @setTemplate:@
setTemplateSelector :: Selector
setTemplateSelector = mkSelector "setTemplate:"

-- | @Selector@ for @capInsets@
capInsetsSelector :: Selector
capInsetsSelector = mkSelector "capInsets"

-- | @Selector@ for @setCapInsets:@
setCapInsetsSelector :: Selector
setCapInsetsSelector = mkSelector "setCapInsets:"

-- | @Selector@ for @resizingMode@
resizingModeSelector :: Selector
resizingModeSelector = mkSelector "resizingMode"

-- | @Selector@ for @setResizingMode:@
setResizingModeSelector :: Selector
setResizingModeSelector = mkSelector "setResizingMode:"

