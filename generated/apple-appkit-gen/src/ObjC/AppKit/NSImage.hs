{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , imageTypes
  , imageUnfilteredTypes
  , cacheMode
  , setCacheMode
  , alignmentRect
  , setAlignmentRect
  , template
  , setTemplate
  , accessibilityDescription
  , setAccessibilityDescription
  , capInsets
  , setCapInsets
  , resizingMode
  , setResizingMode
  , symbolConfiguration
  , locale
  , accessibilityDescriptionSelector
  , addRepresentationSelector
  , addRepresentationsSelector
  , alignmentRectSelector
  , backgroundColorSelector
  , bestRepresentationForDeviceSelector
  , bestRepresentationForRect_context_hintsSelector
  , cacheDepthMatchesImageDepthSelector
  , cacheModeSelector
  , canInitWithPasteboardSelector
  , cancelIncrementalLoadSelector
  , capInsetsSelector
  , cgImageForProposedRect_context_hintsSelector
  , compositeToPoint_fromRect_operationSelector
  , compositeToPoint_fromRect_operation_fractionSelector
  , compositeToPoint_operationSelector
  , compositeToPoint_operation_fractionSelector
  , delegateSelector
  , dissolveToPoint_fractionSelector
  , dissolveToPoint_fromRect_fractionSelector
  , drawAtPoint_fromRect_operation_fractionSelector
  , drawInRectSelector
  , drawInRect_fromRect_operation_fractionSelector
  , drawInRect_fromRect_operation_fraction_respectFlipped_hintsSelector
  , drawRepresentation_inRectSelector
  , hitTestRect_withImageDestinationRect_context_hints_flippedSelector
  , imageFileTypesSelector
  , imageNamedSelector
  , imagePasteboardTypesSelector
  , imageTypesSelector
  , imageUnfilteredFileTypesSelector
  , imageUnfilteredPasteboardTypesSelector
  , imageUnfilteredTypesSelector
  , imageWithLocaleSelector
  , imageWithSize_flipped_drawingHandlerSelector
  , imageWithSymbolConfigurationSelector
  , imageWithSymbolName_bundle_variableValueSelector
  , imageWithSymbolName_variableValueSelector
  , imageWithSystemSymbolName_accessibilityDescriptionSelector
  , imageWithSystemSymbolName_variableValue_accessibilityDescriptionSelector
  , initByReferencingFileSelector
  , initByReferencingURLSelector
  , initWithCGImage_sizeSelector
  , initWithCoderSelector
  , initWithContentsOfFileSelector
  , initWithContentsOfURLSelector
  , initWithDataIgnoringOrientationSelector
  , initWithDataSelector
  , initWithIconRefSelector
  , initWithPasteboardSelector
  , initWithSizeSelector
  , isCachedSeparatelySelector
  , isDataRetainedSelector
  , isFlippedSelector
  , layerContentsForContentsScaleSelector
  , localeSelector
  , lockFocusFlippedSelector
  , lockFocusOnRepresentationSelector
  , lockFocusSelector
  , matchesOnMultipleResolutionSelector
  , matchesOnlyOnBestFittingAxisSelector
  , nameSelector
  , prefersColorMatchSelector
  , recacheSelector
  , recommendedLayerContentsScaleSelector
  , removeRepresentationSelector
  , representationsSelector
  , resizingModeSelector
  , scalesWhenResizedSelector
  , setAccessibilityDescriptionSelector
  , setAlignmentRectSelector
  , setBackgroundColorSelector
  , setCacheDepthMatchesImageDepthSelector
  , setCacheModeSelector
  , setCachedSeparatelySelector
  , setCapInsetsSelector
  , setDataRetainedSelector
  , setDelegateSelector
  , setFlippedSelector
  , setMatchesOnMultipleResolutionSelector
  , setMatchesOnlyOnBestFittingAxisSelector
  , setNameSelector
  , setPrefersColorMatchSelector
  , setResizingModeSelector
  , setScalesWhenResizedSelector
  , setSizeSelector
  , setTemplateSelector
  , setUsesEPSOnResolutionMismatchSelector
  , sizeSelector
  , symbolConfigurationSelector
  , templateSelector
  , tiffRepresentationSelector
  , tiffRepresentationUsingCompression_factorSelector
  , unlockFocusSelector
  , usesEPSOnResolutionMismatchSelector
  , validSelector

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

-- | @+ imageNamed:@
imageNamed :: IsNSString name => name -> IO (Id NSImage)
imageNamed name =
  do
    cls' <- getRequiredClass "NSImage"
    sendClassMessage cls' imageNamedSelector (toNSString name)

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
    sendClassMessage cls' imageWithSystemSymbolName_accessibilityDescriptionSelector (toNSString name) (toNSString description)

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
    sendClassMessage cls' imageWithSystemSymbolName_variableValue_accessibilityDescriptionSelector (toNSString name) value (toNSString description)

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
    sendClassMessage cls' imageWithSymbolName_variableValueSelector (toNSString name) value

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
    sendClassMessage cls' imageWithSymbolName_bundle_variableValueSelector (toNSString name) (toNSBundle bundle) value

-- | @- initWithSize:@
initWithSize :: IsNSImage nsImage => nsImage -> NSSize -> IO (Id NSImage)
initWithSize nsImage size =
  sendOwnedMessage nsImage initWithSizeSelector size

-- | @- initWithCoder:@
initWithCoder :: (IsNSImage nsImage, IsNSCoder coder) => nsImage -> coder -> IO (Id NSImage)
initWithCoder nsImage coder =
  sendOwnedMessage nsImage initWithCoderSelector (toNSCoder coder)

-- | @- initWithData:@
initWithData :: (IsNSImage nsImage, IsNSData data_) => nsImage -> data_ -> IO (Id NSImage)
initWithData nsImage data_ =
  sendOwnedMessage nsImage initWithDataSelector (toNSData data_)

-- | @- initWithContentsOfFile:@
initWithContentsOfFile :: (IsNSImage nsImage, IsNSString fileName) => nsImage -> fileName -> IO (Id NSImage)
initWithContentsOfFile nsImage fileName =
  sendOwnedMessage nsImage initWithContentsOfFileSelector (toNSString fileName)

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSImage nsImage, IsNSURL url) => nsImage -> url -> IO (Id NSImage)
initWithContentsOfURL nsImage url =
  sendOwnedMessage nsImage initWithContentsOfURLSelector (toNSURL url)

-- | @- initByReferencingFile:@
initByReferencingFile :: (IsNSImage nsImage, IsNSString fileName) => nsImage -> fileName -> IO (Id NSImage)
initByReferencingFile nsImage fileName =
  sendOwnedMessage nsImage initByReferencingFileSelector (toNSString fileName)

-- | @- initByReferencingURL:@
initByReferencingURL :: (IsNSImage nsImage, IsNSURL url) => nsImage -> url -> IO (Id NSImage)
initByReferencingURL nsImage url =
  sendOwnedMessage nsImage initByReferencingURLSelector (toNSURL url)

-- | @- initWithPasteboard:@
initWithPasteboard :: (IsNSImage nsImage, IsNSPasteboard pasteboard) => nsImage -> pasteboard -> IO (Id NSImage)
initWithPasteboard nsImage pasteboard =
  sendOwnedMessage nsImage initWithPasteboardSelector (toNSPasteboard pasteboard)

-- | @- initWithDataIgnoringOrientation:@
initWithDataIgnoringOrientation :: (IsNSImage nsImage, IsNSData data_) => nsImage -> data_ -> IO (Id NSImage)
initWithDataIgnoringOrientation nsImage data_ =
  sendOwnedMessage nsImage initWithDataIgnoringOrientationSelector (toNSData data_)

-- | @+ imageWithSize:flipped:drawingHandler:@
imageWithSize_flipped_drawingHandler :: NSSize -> Bool -> Ptr () -> IO (Id NSImage)
imageWithSize_flipped_drawingHandler size drawingHandlerShouldBeCalledWithFlippedContext drawingHandler =
  do
    cls' <- getRequiredClass "NSImage"
    sendClassMessage cls' imageWithSize_flipped_drawingHandlerSelector size drawingHandlerShouldBeCalledWithFlippedContext drawingHandler

-- | @- setName:@
setName :: (IsNSImage nsImage, IsNSString string) => nsImage -> string -> IO Bool
setName nsImage string =
  sendMessage nsImage setNameSelector (toNSString string)

-- | @- name@
name :: IsNSImage nsImage => nsImage -> IO (Id NSString)
name nsImage =
  sendMessage nsImage nameSelector

-- | @- drawAtPoint:fromRect:operation:fraction:@
drawAtPoint_fromRect_operation_fraction :: IsNSImage nsImage => nsImage -> NSPoint -> NSRect -> NSCompositingOperation -> CDouble -> IO ()
drawAtPoint_fromRect_operation_fraction nsImage point fromRect op delta =
  sendMessage nsImage drawAtPoint_fromRect_operation_fractionSelector point fromRect op delta

-- | @- drawInRect:fromRect:operation:fraction:@
drawInRect_fromRect_operation_fraction :: IsNSImage nsImage => nsImage -> NSRect -> NSRect -> NSCompositingOperation -> CDouble -> IO ()
drawInRect_fromRect_operation_fraction nsImage rect fromRect op delta =
  sendMessage nsImage drawInRect_fromRect_operation_fractionSelector rect fromRect op delta

-- | @- drawInRect:fromRect:operation:fraction:respectFlipped:hints:@
drawInRect_fromRect_operation_fraction_respectFlipped_hints :: (IsNSImage nsImage, IsNSDictionary hints) => nsImage -> NSRect -> NSRect -> NSCompositingOperation -> CDouble -> Bool -> hints -> IO ()
drawInRect_fromRect_operation_fraction_respectFlipped_hints nsImage dstSpacePortionRect srcSpacePortionRect op requestedAlpha respectContextIsFlipped hints =
  sendMessage nsImage drawInRect_fromRect_operation_fraction_respectFlipped_hintsSelector dstSpacePortionRect srcSpacePortionRect op requestedAlpha respectContextIsFlipped (toNSDictionary hints)

-- | @- drawRepresentation:inRect:@
drawRepresentation_inRect :: (IsNSImage nsImage, IsNSImageRep imageRep) => nsImage -> imageRep -> NSRect -> IO Bool
drawRepresentation_inRect nsImage imageRep rect =
  sendMessage nsImage drawRepresentation_inRectSelector (toNSImageRep imageRep) rect

-- | @- drawInRect:@
drawInRect :: IsNSImage nsImage => nsImage -> NSRect -> IO ()
drawInRect nsImage rect =
  sendMessage nsImage drawInRectSelector rect

-- | @- recache@
recache :: IsNSImage nsImage => nsImage -> IO ()
recache nsImage =
  sendMessage nsImage recacheSelector

-- | @- TIFFRepresentationUsingCompression:factor:@
tiffRepresentationUsingCompression_factor :: IsNSImage nsImage => nsImage -> NSTIFFCompression -> CFloat -> IO (Id NSData)
tiffRepresentationUsingCompression_factor nsImage comp factor =
  sendMessage nsImage tiffRepresentationUsingCompression_factorSelector comp factor

-- | @- addRepresentations:@
addRepresentations :: (IsNSImage nsImage, IsNSArray imageReps) => nsImage -> imageReps -> IO ()
addRepresentations nsImage imageReps =
  sendMessage nsImage addRepresentationsSelector (toNSArray imageReps)

-- | @- addRepresentation:@
addRepresentation :: (IsNSImage nsImage, IsNSImageRep imageRep) => nsImage -> imageRep -> IO ()
addRepresentation nsImage imageRep =
  sendMessage nsImage addRepresentationSelector (toNSImageRep imageRep)

-- | @- removeRepresentation:@
removeRepresentation :: (IsNSImage nsImage, IsNSImageRep imageRep) => nsImage -> imageRep -> IO ()
removeRepresentation nsImage imageRep =
  sendMessage nsImage removeRepresentationSelector (toNSImageRep imageRep)

-- | @+ canInitWithPasteboard:@
canInitWithPasteboard :: IsNSPasteboard pasteboard => pasteboard -> IO Bool
canInitWithPasteboard pasteboard =
  do
    cls' <- getRequiredClass "NSImage"
    sendClassMessage cls' canInitWithPasteboardSelector (toNSPasteboard pasteboard)

-- | @- initWithCGImage:size:@
initWithCGImage_size :: IsNSImage nsImage => nsImage -> Ptr () -> NSSize -> IO (Id NSImage)
initWithCGImage_size nsImage cgImage size =
  sendOwnedMessage nsImage initWithCGImage_sizeSelector cgImage size

-- | @- CGImageForProposedRect:context:hints:@
cgImageForProposedRect_context_hints :: (IsNSImage nsImage, IsNSGraphicsContext referenceContext, IsNSDictionary hints) => nsImage -> Ptr NSRect -> referenceContext -> hints -> IO (Ptr ())
cgImageForProposedRect_context_hints nsImage proposedDestRect referenceContext hints =
  sendMessage nsImage cgImageForProposedRect_context_hintsSelector proposedDestRect (toNSGraphicsContext referenceContext) (toNSDictionary hints)

-- | @- bestRepresentationForRect:context:hints:@
bestRepresentationForRect_context_hints :: (IsNSImage nsImage, IsNSGraphicsContext referenceContext, IsNSDictionary hints) => nsImage -> NSRect -> referenceContext -> hints -> IO (Id NSImageRep)
bestRepresentationForRect_context_hints nsImage rect referenceContext hints =
  sendMessage nsImage bestRepresentationForRect_context_hintsSelector rect (toNSGraphicsContext referenceContext) (toNSDictionary hints)

-- | @- hitTestRect:withImageDestinationRect:context:hints:flipped:@
hitTestRect_withImageDestinationRect_context_hints_flipped :: (IsNSImage nsImage, IsNSGraphicsContext context, IsNSDictionary hints) => nsImage -> NSRect -> NSRect -> context -> hints -> Bool -> IO Bool
hitTestRect_withImageDestinationRect_context_hints_flipped nsImage testRectDestSpace imageRectDestSpace context hints flipped =
  sendMessage nsImage hitTestRect_withImageDestinationRect_context_hints_flippedSelector testRectDestSpace imageRectDestSpace (toNSGraphicsContext context) (toNSDictionary hints) flipped

-- | @- recommendedLayerContentsScale:@
recommendedLayerContentsScale :: IsNSImage nsImage => nsImage -> CDouble -> IO CDouble
recommendedLayerContentsScale nsImage preferredContentsScale =
  sendMessage nsImage recommendedLayerContentsScaleSelector preferredContentsScale

-- | @- layerContentsForContentsScale:@
layerContentsForContentsScale :: IsNSImage nsImage => nsImage -> CDouble -> IO RawId
layerContentsForContentsScale nsImage layerContentsScale =
  sendMessage nsImage layerContentsForContentsScaleSelector layerContentsScale

-- | @- imageWithSymbolConfiguration:@
imageWithSymbolConfiguration :: (IsNSImage nsImage, IsNSImageSymbolConfiguration configuration) => nsImage -> configuration -> IO (Id NSImage)
imageWithSymbolConfiguration nsImage configuration =
  sendMessage nsImage imageWithSymbolConfigurationSelector (toNSImageSymbolConfiguration configuration)

-- | Creates and returns a new image with the specified locale. If the receiver contains locale-sensitive representations, the returned image will prefer to draw using representations appropriate for the specified locale. If locale is @nil@, the returned image uses the default behavior of choosing representations appropriate for the system’s currently-configured locale.
--
-- ObjC selector: @- imageWithLocale:@
imageWithLocale :: (IsNSImage nsImage, IsNSLocale locale) => nsImage -> locale -> IO (Id NSImage)
imageWithLocale nsImage locale =
  sendMessage nsImage imageWithLocaleSelector (toNSLocale locale)

-- | @+ imageUnfilteredFileTypes@
imageUnfilteredFileTypes :: IO (Id NSArray)
imageUnfilteredFileTypes  =
  do
    cls' <- getRequiredClass "NSImage"
    sendClassMessage cls' imageUnfilteredFileTypesSelector

-- | @+ imageUnfilteredPasteboardTypes@
imageUnfilteredPasteboardTypes :: IO (Id NSArray)
imageUnfilteredPasteboardTypes  =
  do
    cls' <- getRequiredClass "NSImage"
    sendClassMessage cls' imageUnfilteredPasteboardTypesSelector

-- | @+ imageFileTypes@
imageFileTypes :: IO (Id NSArray)
imageFileTypes  =
  do
    cls' <- getRequiredClass "NSImage"
    sendClassMessage cls' imageFileTypesSelector

-- | @+ imagePasteboardTypes@
imagePasteboardTypes :: IO (Id NSArray)
imagePasteboardTypes  =
  do
    cls' <- getRequiredClass "NSImage"
    sendClassMessage cls' imagePasteboardTypesSelector

-- | @- initWithIconRef:@
initWithIconRef :: IsNSImage nsImage => nsImage -> Ptr () -> IO (Id NSImage)
initWithIconRef nsImage iconRef =
  sendOwnedMessage nsImage initWithIconRefSelector iconRef

-- | @- bestRepresentationForDevice:@
bestRepresentationForDevice :: (IsNSImage nsImage, IsNSDictionary deviceDescription) => nsImage -> deviceDescription -> IO (Id NSImageRep)
bestRepresentationForDevice nsImage deviceDescription =
  sendMessage nsImage bestRepresentationForDeviceSelector (toNSDictionary deviceDescription)

-- | @- lockFocus@
lockFocus :: IsNSImage nsImage => nsImage -> IO ()
lockFocus nsImage =
  sendMessage nsImage lockFocusSelector

-- | @- lockFocusFlipped:@
lockFocusFlipped :: IsNSImage nsImage => nsImage -> Bool -> IO ()
lockFocusFlipped nsImage flipped =
  sendMessage nsImage lockFocusFlippedSelector flipped

-- | @- unlockFocus@
unlockFocus :: IsNSImage nsImage => nsImage -> IO ()
unlockFocus nsImage =
  sendMessage nsImage unlockFocusSelector

-- | @- setFlipped:@
setFlipped :: IsNSImage nsImage => nsImage -> Bool -> IO ()
setFlipped nsImage flag =
  sendMessage nsImage setFlippedSelector flag

-- | @- isFlipped@
isFlipped :: IsNSImage nsImage => nsImage -> IO Bool
isFlipped nsImage =
  sendMessage nsImage isFlippedSelector

-- | @- setScalesWhenResized:@
setScalesWhenResized :: IsNSImage nsImage => nsImage -> Bool -> IO ()
setScalesWhenResized nsImage flag =
  sendMessage nsImage setScalesWhenResizedSelector flag

-- | @- scalesWhenResized@
scalesWhenResized :: IsNSImage nsImage => nsImage -> IO Bool
scalesWhenResized nsImage =
  sendMessage nsImage scalesWhenResizedSelector

-- | @- setDataRetained:@
setDataRetained :: IsNSImage nsImage => nsImage -> Bool -> IO ()
setDataRetained nsImage flag =
  sendMessage nsImage setDataRetainedSelector flag

-- | @- isDataRetained@
isDataRetained :: IsNSImage nsImage => nsImage -> IO Bool
isDataRetained nsImage =
  sendMessage nsImage isDataRetainedSelector

-- | @- setCachedSeparately:@
setCachedSeparately :: IsNSImage nsImage => nsImage -> Bool -> IO ()
setCachedSeparately nsImage flag =
  sendMessage nsImage setCachedSeparatelySelector flag

-- | @- isCachedSeparately@
isCachedSeparately :: IsNSImage nsImage => nsImage -> IO Bool
isCachedSeparately nsImage =
  sendMessage nsImage isCachedSeparatelySelector

-- | @- setCacheDepthMatchesImageDepth:@
setCacheDepthMatchesImageDepth :: IsNSImage nsImage => nsImage -> Bool -> IO ()
setCacheDepthMatchesImageDepth nsImage flag =
  sendMessage nsImage setCacheDepthMatchesImageDepthSelector flag

-- | @- cacheDepthMatchesImageDepth@
cacheDepthMatchesImageDepth :: IsNSImage nsImage => nsImage -> IO Bool
cacheDepthMatchesImageDepth nsImage =
  sendMessage nsImage cacheDepthMatchesImageDepthSelector

-- | @- dissolveToPoint:fraction:@
dissolveToPoint_fraction :: IsNSImage nsImage => nsImage -> NSPoint -> CDouble -> IO ()
dissolveToPoint_fraction nsImage point fraction =
  sendMessage nsImage dissolveToPoint_fractionSelector point fraction

-- | @- dissolveToPoint:fromRect:fraction:@
dissolveToPoint_fromRect_fraction :: IsNSImage nsImage => nsImage -> NSPoint -> NSRect -> CDouble -> IO ()
dissolveToPoint_fromRect_fraction nsImage point rect fraction =
  sendMessage nsImage dissolveToPoint_fromRect_fractionSelector point rect fraction

-- | @- compositeToPoint:operation:@
compositeToPoint_operation :: IsNSImage nsImage => nsImage -> NSPoint -> NSCompositingOperation -> IO ()
compositeToPoint_operation nsImage point operation =
  sendMessage nsImage compositeToPoint_operationSelector point operation

-- | @- compositeToPoint:fromRect:operation:@
compositeToPoint_fromRect_operation :: IsNSImage nsImage => nsImage -> NSPoint -> NSRect -> NSCompositingOperation -> IO ()
compositeToPoint_fromRect_operation nsImage point rect operation =
  sendMessage nsImage compositeToPoint_fromRect_operationSelector point rect operation

-- | @- compositeToPoint:operation:fraction:@
compositeToPoint_operation_fraction :: IsNSImage nsImage => nsImage -> NSPoint -> NSCompositingOperation -> CDouble -> IO ()
compositeToPoint_operation_fraction nsImage point operation fraction =
  sendMessage nsImage compositeToPoint_operation_fractionSelector point operation fraction

-- | @- compositeToPoint:fromRect:operation:fraction:@
compositeToPoint_fromRect_operation_fraction :: IsNSImage nsImage => nsImage -> NSPoint -> NSRect -> NSCompositingOperation -> CDouble -> IO ()
compositeToPoint_fromRect_operation_fraction nsImage point rect operation fraction =
  sendMessage nsImage compositeToPoint_fromRect_operation_fractionSelector point rect operation fraction

-- | @- lockFocusOnRepresentation:@
lockFocusOnRepresentation :: (IsNSImage nsImage, IsNSImageRep imageRepresentation) => nsImage -> imageRepresentation -> IO ()
lockFocusOnRepresentation nsImage imageRepresentation =
  sendMessage nsImage lockFocusOnRepresentationSelector (toNSImageRep imageRepresentation)

-- | @- cancelIncrementalLoad@
cancelIncrementalLoad :: IsNSImage nsImage => nsImage -> IO ()
cancelIncrementalLoad nsImage =
  sendMessage nsImage cancelIncrementalLoadSelector

-- | @- size@
size :: IsNSImage nsImage => nsImage -> IO NSSize
size nsImage =
  sendMessage nsImage sizeSelector

-- | @- setSize:@
setSize :: IsNSImage nsImage => nsImage -> NSSize -> IO ()
setSize nsImage value =
  sendMessage nsImage setSizeSelector value

-- | @- backgroundColor@
backgroundColor :: IsNSImage nsImage => nsImage -> IO (Id NSColor)
backgroundColor nsImage =
  sendMessage nsImage backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSImage nsImage, IsNSColor value) => nsImage -> value -> IO ()
setBackgroundColor nsImage value =
  sendMessage nsImage setBackgroundColorSelector (toNSColor value)

-- | @- usesEPSOnResolutionMismatch@
usesEPSOnResolutionMismatch :: IsNSImage nsImage => nsImage -> IO Bool
usesEPSOnResolutionMismatch nsImage =
  sendMessage nsImage usesEPSOnResolutionMismatchSelector

-- | @- setUsesEPSOnResolutionMismatch:@
setUsesEPSOnResolutionMismatch :: IsNSImage nsImage => nsImage -> Bool -> IO ()
setUsesEPSOnResolutionMismatch nsImage value =
  sendMessage nsImage setUsesEPSOnResolutionMismatchSelector value

-- | @- prefersColorMatch@
prefersColorMatch :: IsNSImage nsImage => nsImage -> IO Bool
prefersColorMatch nsImage =
  sendMessage nsImage prefersColorMatchSelector

-- | @- setPrefersColorMatch:@
setPrefersColorMatch :: IsNSImage nsImage => nsImage -> Bool -> IO ()
setPrefersColorMatch nsImage value =
  sendMessage nsImage setPrefersColorMatchSelector value

-- | @- matchesOnMultipleResolution@
matchesOnMultipleResolution :: IsNSImage nsImage => nsImage -> IO Bool
matchesOnMultipleResolution nsImage =
  sendMessage nsImage matchesOnMultipleResolutionSelector

-- | @- setMatchesOnMultipleResolution:@
setMatchesOnMultipleResolution :: IsNSImage nsImage => nsImage -> Bool -> IO ()
setMatchesOnMultipleResolution nsImage value =
  sendMessage nsImage setMatchesOnMultipleResolutionSelector value

-- | @- matchesOnlyOnBestFittingAxis@
matchesOnlyOnBestFittingAxis :: IsNSImage nsImage => nsImage -> IO Bool
matchesOnlyOnBestFittingAxis nsImage =
  sendMessage nsImage matchesOnlyOnBestFittingAxisSelector

-- | @- setMatchesOnlyOnBestFittingAxis:@
setMatchesOnlyOnBestFittingAxis :: IsNSImage nsImage => nsImage -> Bool -> IO ()
setMatchesOnlyOnBestFittingAxis nsImage value =
  sendMessage nsImage setMatchesOnlyOnBestFittingAxisSelector value

-- | @- TIFFRepresentation@
tiffRepresentation :: IsNSImage nsImage => nsImage -> IO (Id NSData)
tiffRepresentation nsImage =
  sendMessage nsImage tiffRepresentationSelector

-- | @- representations@
representations :: IsNSImage nsImage => nsImage -> IO (Id NSArray)
representations nsImage =
  sendMessage nsImage representationsSelector

-- | @- valid@
valid :: IsNSImage nsImage => nsImage -> IO Bool
valid nsImage =
  sendMessage nsImage validSelector

-- | @- delegate@
delegate :: IsNSImage nsImage => nsImage -> IO RawId
delegate nsImage =
  sendMessage nsImage delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSImage nsImage => nsImage -> RawId -> IO ()
setDelegate nsImage value =
  sendMessage nsImage setDelegateSelector value

-- | @+ imageTypes@
imageTypes :: IO (Id NSArray)
imageTypes  =
  do
    cls' <- getRequiredClass "NSImage"
    sendClassMessage cls' imageTypesSelector

-- | @+ imageUnfilteredTypes@
imageUnfilteredTypes :: IO (Id NSArray)
imageUnfilteredTypes  =
  do
    cls' <- getRequiredClass "NSImage"
    sendClassMessage cls' imageUnfilteredTypesSelector

-- | @- cacheMode@
cacheMode :: IsNSImage nsImage => nsImage -> IO NSImageCacheMode
cacheMode nsImage =
  sendMessage nsImage cacheModeSelector

-- | @- setCacheMode:@
setCacheMode :: IsNSImage nsImage => nsImage -> NSImageCacheMode -> IO ()
setCacheMode nsImage value =
  sendMessage nsImage setCacheModeSelector value

-- | @- alignmentRect@
alignmentRect :: IsNSImage nsImage => nsImage -> IO NSRect
alignmentRect nsImage =
  sendMessage nsImage alignmentRectSelector

-- | @- setAlignmentRect:@
setAlignmentRect :: IsNSImage nsImage => nsImage -> NSRect -> IO ()
setAlignmentRect nsImage value =
  sendMessage nsImage setAlignmentRectSelector value

-- | @- template@
template :: IsNSImage nsImage => nsImage -> IO Bool
template nsImage =
  sendMessage nsImage templateSelector

-- | @- setTemplate:@
setTemplate :: IsNSImage nsImage => nsImage -> Bool -> IO ()
setTemplate nsImage value =
  sendMessage nsImage setTemplateSelector value

-- | @- accessibilityDescription@
accessibilityDescription :: IsNSImage nsImage => nsImage -> IO (Id NSString)
accessibilityDescription nsImage =
  sendMessage nsImage accessibilityDescriptionSelector

-- | @- setAccessibilityDescription:@
setAccessibilityDescription :: (IsNSImage nsImage, IsNSString value) => nsImage -> value -> IO ()
setAccessibilityDescription nsImage value =
  sendMessage nsImage setAccessibilityDescriptionSelector (toNSString value)

-- | @- capInsets@
capInsets :: IsNSImage nsImage => nsImage -> IO NSEdgeInsets
capInsets nsImage =
  sendMessage nsImage capInsetsSelector

-- | @- setCapInsets:@
setCapInsets :: IsNSImage nsImage => nsImage -> NSEdgeInsets -> IO ()
setCapInsets nsImage value =
  sendMessage nsImage setCapInsetsSelector value

-- | @- resizingMode@
resizingMode :: IsNSImage nsImage => nsImage -> IO NSImageResizingMode
resizingMode nsImage =
  sendMessage nsImage resizingModeSelector

-- | @- setResizingMode:@
setResizingMode :: IsNSImage nsImage => nsImage -> NSImageResizingMode -> IO ()
setResizingMode nsImage value =
  sendMessage nsImage setResizingModeSelector value

-- | @- symbolConfiguration@
symbolConfiguration :: IsNSImage nsImage => nsImage -> IO (Id NSImageSymbolConfiguration)
symbolConfiguration nsImage =
  sendMessage nsImage symbolConfigurationSelector

-- | The image’s preferred locale for resolving representations, if one has been specified using @-imageWithLocale:@. Otherwise, @nil@.
--
-- ObjC selector: @- locale@
locale :: IsNSImage nsImage => nsImage -> IO (Id NSLocale)
locale nsImage =
  sendMessage nsImage localeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageNamed:@
imageNamedSelector :: Selector '[Id NSString] (Id NSImage)
imageNamedSelector = mkSelector "imageNamed:"

-- | @Selector@ for @imageWithSystemSymbolName:accessibilityDescription:@
imageWithSystemSymbolName_accessibilityDescriptionSelector :: Selector '[Id NSString, Id NSString] (Id NSImage)
imageWithSystemSymbolName_accessibilityDescriptionSelector = mkSelector "imageWithSystemSymbolName:accessibilityDescription:"

-- | @Selector@ for @imageWithSystemSymbolName:variableValue:accessibilityDescription:@
imageWithSystemSymbolName_variableValue_accessibilityDescriptionSelector :: Selector '[Id NSString, CDouble, Id NSString] (Id NSImage)
imageWithSystemSymbolName_variableValue_accessibilityDescriptionSelector = mkSelector "imageWithSystemSymbolName:variableValue:accessibilityDescription:"

-- | @Selector@ for @imageWithSymbolName:variableValue:@
imageWithSymbolName_variableValueSelector :: Selector '[Id NSString, CDouble] (Id NSImage)
imageWithSymbolName_variableValueSelector = mkSelector "imageWithSymbolName:variableValue:"

-- | @Selector@ for @imageWithSymbolName:bundle:variableValue:@
imageWithSymbolName_bundle_variableValueSelector :: Selector '[Id NSString, Id NSBundle, CDouble] (Id NSImage)
imageWithSymbolName_bundle_variableValueSelector = mkSelector "imageWithSymbolName:bundle:variableValue:"

-- | @Selector@ for @initWithSize:@
initWithSizeSelector :: Selector '[NSSize] (Id NSImage)
initWithSizeSelector = mkSelector "initWithSize:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSImage)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector '[Id NSData] (Id NSImage)
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @initWithContentsOfFile:@
initWithContentsOfFileSelector :: Selector '[Id NSString] (Id NSImage)
initWithContentsOfFileSelector = mkSelector "initWithContentsOfFile:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector '[Id NSURL] (Id NSImage)
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @initByReferencingFile:@
initByReferencingFileSelector :: Selector '[Id NSString] (Id NSImage)
initByReferencingFileSelector = mkSelector "initByReferencingFile:"

-- | @Selector@ for @initByReferencingURL:@
initByReferencingURLSelector :: Selector '[Id NSURL] (Id NSImage)
initByReferencingURLSelector = mkSelector "initByReferencingURL:"

-- | @Selector@ for @initWithPasteboard:@
initWithPasteboardSelector :: Selector '[Id NSPasteboard] (Id NSImage)
initWithPasteboardSelector = mkSelector "initWithPasteboard:"

-- | @Selector@ for @initWithDataIgnoringOrientation:@
initWithDataIgnoringOrientationSelector :: Selector '[Id NSData] (Id NSImage)
initWithDataIgnoringOrientationSelector = mkSelector "initWithDataIgnoringOrientation:"

-- | @Selector@ for @imageWithSize:flipped:drawingHandler:@
imageWithSize_flipped_drawingHandlerSelector :: Selector '[NSSize, Bool, Ptr ()] (Id NSImage)
imageWithSize_flipped_drawingHandlerSelector = mkSelector "imageWithSize:flipped:drawingHandler:"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] Bool
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @drawAtPoint:fromRect:operation:fraction:@
drawAtPoint_fromRect_operation_fractionSelector :: Selector '[NSPoint, NSRect, NSCompositingOperation, CDouble] ()
drawAtPoint_fromRect_operation_fractionSelector = mkSelector "drawAtPoint:fromRect:operation:fraction:"

-- | @Selector@ for @drawInRect:fromRect:operation:fraction:@
drawInRect_fromRect_operation_fractionSelector :: Selector '[NSRect, NSRect, NSCompositingOperation, CDouble] ()
drawInRect_fromRect_operation_fractionSelector = mkSelector "drawInRect:fromRect:operation:fraction:"

-- | @Selector@ for @drawInRect:fromRect:operation:fraction:respectFlipped:hints:@
drawInRect_fromRect_operation_fraction_respectFlipped_hintsSelector :: Selector '[NSRect, NSRect, NSCompositingOperation, CDouble, Bool, Id NSDictionary] ()
drawInRect_fromRect_operation_fraction_respectFlipped_hintsSelector = mkSelector "drawInRect:fromRect:operation:fraction:respectFlipped:hints:"

-- | @Selector@ for @drawRepresentation:inRect:@
drawRepresentation_inRectSelector :: Selector '[Id NSImageRep, NSRect] Bool
drawRepresentation_inRectSelector = mkSelector "drawRepresentation:inRect:"

-- | @Selector@ for @drawInRect:@
drawInRectSelector :: Selector '[NSRect] ()
drawInRectSelector = mkSelector "drawInRect:"

-- | @Selector@ for @recache@
recacheSelector :: Selector '[] ()
recacheSelector = mkSelector "recache"

-- | @Selector@ for @TIFFRepresentationUsingCompression:factor:@
tiffRepresentationUsingCompression_factorSelector :: Selector '[NSTIFFCompression, CFloat] (Id NSData)
tiffRepresentationUsingCompression_factorSelector = mkSelector "TIFFRepresentationUsingCompression:factor:"

-- | @Selector@ for @addRepresentations:@
addRepresentationsSelector :: Selector '[Id NSArray] ()
addRepresentationsSelector = mkSelector "addRepresentations:"

-- | @Selector@ for @addRepresentation:@
addRepresentationSelector :: Selector '[Id NSImageRep] ()
addRepresentationSelector = mkSelector "addRepresentation:"

-- | @Selector@ for @removeRepresentation:@
removeRepresentationSelector :: Selector '[Id NSImageRep] ()
removeRepresentationSelector = mkSelector "removeRepresentation:"

-- | @Selector@ for @canInitWithPasteboard:@
canInitWithPasteboardSelector :: Selector '[Id NSPasteboard] Bool
canInitWithPasteboardSelector = mkSelector "canInitWithPasteboard:"

-- | @Selector@ for @initWithCGImage:size:@
initWithCGImage_sizeSelector :: Selector '[Ptr (), NSSize] (Id NSImage)
initWithCGImage_sizeSelector = mkSelector "initWithCGImage:size:"

-- | @Selector@ for @CGImageForProposedRect:context:hints:@
cgImageForProposedRect_context_hintsSelector :: Selector '[Ptr NSRect, Id NSGraphicsContext, Id NSDictionary] (Ptr ())
cgImageForProposedRect_context_hintsSelector = mkSelector "CGImageForProposedRect:context:hints:"

-- | @Selector@ for @bestRepresentationForRect:context:hints:@
bestRepresentationForRect_context_hintsSelector :: Selector '[NSRect, Id NSGraphicsContext, Id NSDictionary] (Id NSImageRep)
bestRepresentationForRect_context_hintsSelector = mkSelector "bestRepresentationForRect:context:hints:"

-- | @Selector@ for @hitTestRect:withImageDestinationRect:context:hints:flipped:@
hitTestRect_withImageDestinationRect_context_hints_flippedSelector :: Selector '[NSRect, NSRect, Id NSGraphicsContext, Id NSDictionary, Bool] Bool
hitTestRect_withImageDestinationRect_context_hints_flippedSelector = mkSelector "hitTestRect:withImageDestinationRect:context:hints:flipped:"

-- | @Selector@ for @recommendedLayerContentsScale:@
recommendedLayerContentsScaleSelector :: Selector '[CDouble] CDouble
recommendedLayerContentsScaleSelector = mkSelector "recommendedLayerContentsScale:"

-- | @Selector@ for @layerContentsForContentsScale:@
layerContentsForContentsScaleSelector :: Selector '[CDouble] RawId
layerContentsForContentsScaleSelector = mkSelector "layerContentsForContentsScale:"

-- | @Selector@ for @imageWithSymbolConfiguration:@
imageWithSymbolConfigurationSelector :: Selector '[Id NSImageSymbolConfiguration] (Id NSImage)
imageWithSymbolConfigurationSelector = mkSelector "imageWithSymbolConfiguration:"

-- | @Selector@ for @imageWithLocale:@
imageWithLocaleSelector :: Selector '[Id NSLocale] (Id NSImage)
imageWithLocaleSelector = mkSelector "imageWithLocale:"

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

-- | @Selector@ for @initWithIconRef:@
initWithIconRefSelector :: Selector '[Ptr ()] (Id NSImage)
initWithIconRefSelector = mkSelector "initWithIconRef:"

-- | @Selector@ for @bestRepresentationForDevice:@
bestRepresentationForDeviceSelector :: Selector '[Id NSDictionary] (Id NSImageRep)
bestRepresentationForDeviceSelector = mkSelector "bestRepresentationForDevice:"

-- | @Selector@ for @lockFocus@
lockFocusSelector :: Selector '[] ()
lockFocusSelector = mkSelector "lockFocus"

-- | @Selector@ for @lockFocusFlipped:@
lockFocusFlippedSelector :: Selector '[Bool] ()
lockFocusFlippedSelector = mkSelector "lockFocusFlipped:"

-- | @Selector@ for @unlockFocus@
unlockFocusSelector :: Selector '[] ()
unlockFocusSelector = mkSelector "unlockFocus"

-- | @Selector@ for @setFlipped:@
setFlippedSelector :: Selector '[Bool] ()
setFlippedSelector = mkSelector "setFlipped:"

-- | @Selector@ for @isFlipped@
isFlippedSelector :: Selector '[] Bool
isFlippedSelector = mkSelector "isFlipped"

-- | @Selector@ for @setScalesWhenResized:@
setScalesWhenResizedSelector :: Selector '[Bool] ()
setScalesWhenResizedSelector = mkSelector "setScalesWhenResized:"

-- | @Selector@ for @scalesWhenResized@
scalesWhenResizedSelector :: Selector '[] Bool
scalesWhenResizedSelector = mkSelector "scalesWhenResized"

-- | @Selector@ for @setDataRetained:@
setDataRetainedSelector :: Selector '[Bool] ()
setDataRetainedSelector = mkSelector "setDataRetained:"

-- | @Selector@ for @isDataRetained@
isDataRetainedSelector :: Selector '[] Bool
isDataRetainedSelector = mkSelector "isDataRetained"

-- | @Selector@ for @setCachedSeparately:@
setCachedSeparatelySelector :: Selector '[Bool] ()
setCachedSeparatelySelector = mkSelector "setCachedSeparately:"

-- | @Selector@ for @isCachedSeparately@
isCachedSeparatelySelector :: Selector '[] Bool
isCachedSeparatelySelector = mkSelector "isCachedSeparately"

-- | @Selector@ for @setCacheDepthMatchesImageDepth:@
setCacheDepthMatchesImageDepthSelector :: Selector '[Bool] ()
setCacheDepthMatchesImageDepthSelector = mkSelector "setCacheDepthMatchesImageDepth:"

-- | @Selector@ for @cacheDepthMatchesImageDepth@
cacheDepthMatchesImageDepthSelector :: Selector '[] Bool
cacheDepthMatchesImageDepthSelector = mkSelector "cacheDepthMatchesImageDepth"

-- | @Selector@ for @dissolveToPoint:fraction:@
dissolveToPoint_fractionSelector :: Selector '[NSPoint, CDouble] ()
dissolveToPoint_fractionSelector = mkSelector "dissolveToPoint:fraction:"

-- | @Selector@ for @dissolveToPoint:fromRect:fraction:@
dissolveToPoint_fromRect_fractionSelector :: Selector '[NSPoint, NSRect, CDouble] ()
dissolveToPoint_fromRect_fractionSelector = mkSelector "dissolveToPoint:fromRect:fraction:"

-- | @Selector@ for @compositeToPoint:operation:@
compositeToPoint_operationSelector :: Selector '[NSPoint, NSCompositingOperation] ()
compositeToPoint_operationSelector = mkSelector "compositeToPoint:operation:"

-- | @Selector@ for @compositeToPoint:fromRect:operation:@
compositeToPoint_fromRect_operationSelector :: Selector '[NSPoint, NSRect, NSCompositingOperation] ()
compositeToPoint_fromRect_operationSelector = mkSelector "compositeToPoint:fromRect:operation:"

-- | @Selector@ for @compositeToPoint:operation:fraction:@
compositeToPoint_operation_fractionSelector :: Selector '[NSPoint, NSCompositingOperation, CDouble] ()
compositeToPoint_operation_fractionSelector = mkSelector "compositeToPoint:operation:fraction:"

-- | @Selector@ for @compositeToPoint:fromRect:operation:fraction:@
compositeToPoint_fromRect_operation_fractionSelector :: Selector '[NSPoint, NSRect, NSCompositingOperation, CDouble] ()
compositeToPoint_fromRect_operation_fractionSelector = mkSelector "compositeToPoint:fromRect:operation:fraction:"

-- | @Selector@ for @lockFocusOnRepresentation:@
lockFocusOnRepresentationSelector :: Selector '[Id NSImageRep] ()
lockFocusOnRepresentationSelector = mkSelector "lockFocusOnRepresentation:"

-- | @Selector@ for @cancelIncrementalLoad@
cancelIncrementalLoadSelector :: Selector '[] ()
cancelIncrementalLoadSelector = mkSelector "cancelIncrementalLoad"

-- | @Selector@ for @size@
sizeSelector :: Selector '[] NSSize
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector '[NSSize] ()
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @usesEPSOnResolutionMismatch@
usesEPSOnResolutionMismatchSelector :: Selector '[] Bool
usesEPSOnResolutionMismatchSelector = mkSelector "usesEPSOnResolutionMismatch"

-- | @Selector@ for @setUsesEPSOnResolutionMismatch:@
setUsesEPSOnResolutionMismatchSelector :: Selector '[Bool] ()
setUsesEPSOnResolutionMismatchSelector = mkSelector "setUsesEPSOnResolutionMismatch:"

-- | @Selector@ for @prefersColorMatch@
prefersColorMatchSelector :: Selector '[] Bool
prefersColorMatchSelector = mkSelector "prefersColorMatch"

-- | @Selector@ for @setPrefersColorMatch:@
setPrefersColorMatchSelector :: Selector '[Bool] ()
setPrefersColorMatchSelector = mkSelector "setPrefersColorMatch:"

-- | @Selector@ for @matchesOnMultipleResolution@
matchesOnMultipleResolutionSelector :: Selector '[] Bool
matchesOnMultipleResolutionSelector = mkSelector "matchesOnMultipleResolution"

-- | @Selector@ for @setMatchesOnMultipleResolution:@
setMatchesOnMultipleResolutionSelector :: Selector '[Bool] ()
setMatchesOnMultipleResolutionSelector = mkSelector "setMatchesOnMultipleResolution:"

-- | @Selector@ for @matchesOnlyOnBestFittingAxis@
matchesOnlyOnBestFittingAxisSelector :: Selector '[] Bool
matchesOnlyOnBestFittingAxisSelector = mkSelector "matchesOnlyOnBestFittingAxis"

-- | @Selector@ for @setMatchesOnlyOnBestFittingAxis:@
setMatchesOnlyOnBestFittingAxisSelector :: Selector '[Bool] ()
setMatchesOnlyOnBestFittingAxisSelector = mkSelector "setMatchesOnlyOnBestFittingAxis:"

-- | @Selector@ for @TIFFRepresentation@
tiffRepresentationSelector :: Selector '[] (Id NSData)
tiffRepresentationSelector = mkSelector "TIFFRepresentation"

-- | @Selector@ for @representations@
representationsSelector :: Selector '[] (Id NSArray)
representationsSelector = mkSelector "representations"

-- | @Selector@ for @valid@
validSelector :: Selector '[] Bool
validSelector = mkSelector "valid"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @imageTypes@
imageTypesSelector :: Selector '[] (Id NSArray)
imageTypesSelector = mkSelector "imageTypes"

-- | @Selector@ for @imageUnfilteredTypes@
imageUnfilteredTypesSelector :: Selector '[] (Id NSArray)
imageUnfilteredTypesSelector = mkSelector "imageUnfilteredTypes"

-- | @Selector@ for @cacheMode@
cacheModeSelector :: Selector '[] NSImageCacheMode
cacheModeSelector = mkSelector "cacheMode"

-- | @Selector@ for @setCacheMode:@
setCacheModeSelector :: Selector '[NSImageCacheMode] ()
setCacheModeSelector = mkSelector "setCacheMode:"

-- | @Selector@ for @alignmentRect@
alignmentRectSelector :: Selector '[] NSRect
alignmentRectSelector = mkSelector "alignmentRect"

-- | @Selector@ for @setAlignmentRect:@
setAlignmentRectSelector :: Selector '[NSRect] ()
setAlignmentRectSelector = mkSelector "setAlignmentRect:"

-- | @Selector@ for @template@
templateSelector :: Selector '[] Bool
templateSelector = mkSelector "template"

-- | @Selector@ for @setTemplate:@
setTemplateSelector :: Selector '[Bool] ()
setTemplateSelector = mkSelector "setTemplate:"

-- | @Selector@ for @accessibilityDescription@
accessibilityDescriptionSelector :: Selector '[] (Id NSString)
accessibilityDescriptionSelector = mkSelector "accessibilityDescription"

-- | @Selector@ for @setAccessibilityDescription:@
setAccessibilityDescriptionSelector :: Selector '[Id NSString] ()
setAccessibilityDescriptionSelector = mkSelector "setAccessibilityDescription:"

-- | @Selector@ for @capInsets@
capInsetsSelector :: Selector '[] NSEdgeInsets
capInsetsSelector = mkSelector "capInsets"

-- | @Selector@ for @setCapInsets:@
setCapInsetsSelector :: Selector '[NSEdgeInsets] ()
setCapInsetsSelector = mkSelector "setCapInsets:"

-- | @Selector@ for @resizingMode@
resizingModeSelector :: Selector '[] NSImageResizingMode
resizingModeSelector = mkSelector "resizingMode"

-- | @Selector@ for @setResizingMode:@
setResizingModeSelector :: Selector '[NSImageResizingMode] ()
setResizingModeSelector = mkSelector "setResizingMode:"

-- | @Selector@ for @symbolConfiguration@
symbolConfigurationSelector :: Selector '[] (Id NSImageSymbolConfiguration)
symbolConfigurationSelector = mkSelector "symbolConfiguration"

-- | @Selector@ for @locale@
localeSelector :: Selector '[] (Id NSLocale)
localeSelector = mkSelector "locale"

