{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFPage@.
module ObjC.PDFKit.PDFPage
  ( PDFPage
  , IsPDFPage(..)
  , init_
  , initWithImage_options
  , initWithImage
  , boundsForBox
  , setBounds_forBox
  , addAnnotation
  , removeAnnotation
  , annotationAtPoint
  , drawWithBox_toContext
  , transformContext_forBox
  , thumbnailOfSize_forBox
  , characterBoundsAtIndex
  , characterIndexAtPoint
  , selectionForRect
  , selectionForWordAtPoint
  , selectionForLineAtPoint
  , selectionFromPoint_toPoint
  , selectionForRange
  , drawWithBox
  , transformContextForBox
  , document
  , pageRef
  , label
  , rotation
  , setRotation
  , annotations
  , displaysAnnotations
  , setDisplaysAnnotations
  , numberOfCharacters
  , string
  , attributedString
  , dataRepresentation
  , addAnnotationSelector
  , annotationAtPointSelector
  , annotationsSelector
  , attributedStringSelector
  , boundsForBoxSelector
  , characterBoundsAtIndexSelector
  , characterIndexAtPointSelector
  , dataRepresentationSelector
  , displaysAnnotationsSelector
  , documentSelector
  , drawWithBoxSelector
  , drawWithBox_toContextSelector
  , initSelector
  , initWithImageSelector
  , initWithImage_optionsSelector
  , labelSelector
  , numberOfCharactersSelector
  , pageRefSelector
  , removeAnnotationSelector
  , rotationSelector
  , selectionForLineAtPointSelector
  , selectionForRangeSelector
  , selectionForRectSelector
  , selectionForWordAtPointSelector
  , selectionFromPoint_toPointSelector
  , setBounds_forBoxSelector
  , setDisplaysAnnotationsSelector
  , setRotationSelector
  , stringSelector
  , thumbnailOfSize_forBoxSelector
  , transformContextForBoxSelector
  , transformContext_forBoxSelector

  -- * Enum types
  , PDFDisplayBox(PDFDisplayBox)
  , pattern KPDFDisplayBoxMediaBox
  , pattern KPDFDisplayBoxCropBox
  , pattern KPDFDisplayBoxBleedBox
  , pattern KPDFDisplayBoxTrimBox
  , pattern KPDFDisplayBoxArtBox

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PDFKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.PDFKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPDFPage pdfPage => pdfPage -> IO (Id PDFPage)
init_ pdfPage =
  sendOwnedMessage pdfPage initSelector

-- | @- initWithImage:options:@
initWithImage_options :: (IsPDFPage pdfPage, IsNSImage image, IsNSDictionary options) => pdfPage -> image -> options -> IO (Id PDFPage)
initWithImage_options pdfPage image options =
  sendOwnedMessage pdfPage initWithImage_optionsSelector (toNSImage image) (toNSDictionary options)

-- | @- initWithImage:@
initWithImage :: (IsPDFPage pdfPage, IsNSImage image) => pdfPage -> image -> IO (Id PDFPage)
initWithImage pdfPage image =
  sendOwnedMessage pdfPage initWithImageSelector (toNSImage image)

-- | @- boundsForBox:@
boundsForBox :: IsPDFPage pdfPage => pdfPage -> PDFDisplayBox -> IO NSRect
boundsForBox pdfPage box =
  sendMessage pdfPage boundsForBoxSelector box

-- | @- setBounds:forBox:@
setBounds_forBox :: IsPDFPage pdfPage => pdfPage -> NSRect -> PDFDisplayBox -> IO ()
setBounds_forBox pdfPage bounds box =
  sendMessage pdfPage setBounds_forBoxSelector bounds box

-- | @- addAnnotation:@
addAnnotation :: (IsPDFPage pdfPage, IsPDFAnnotation annotation) => pdfPage -> annotation -> IO ()
addAnnotation pdfPage annotation =
  sendMessage pdfPage addAnnotationSelector (toPDFAnnotation annotation)

-- | @- removeAnnotation:@
removeAnnotation :: (IsPDFPage pdfPage, IsPDFAnnotation annotation) => pdfPage -> annotation -> IO ()
removeAnnotation pdfPage annotation =
  sendMessage pdfPage removeAnnotationSelector (toPDFAnnotation annotation)

-- | @- annotationAtPoint:@
annotationAtPoint :: IsPDFPage pdfPage => pdfPage -> NSPoint -> IO (Id PDFAnnotation)
annotationAtPoint pdfPage point =
  sendMessage pdfPage annotationAtPointSelector point

-- | @- drawWithBox:toContext:@
drawWithBox_toContext :: IsPDFPage pdfPage => pdfPage -> PDFDisplayBox -> Ptr () -> IO ()
drawWithBox_toContext pdfPage box context =
  sendMessage pdfPage drawWithBox_toContextSelector box context

-- | @- transformContext:forBox:@
transformContext_forBox :: IsPDFPage pdfPage => pdfPage -> Ptr () -> PDFDisplayBox -> IO ()
transformContext_forBox pdfPage context box =
  sendMessage pdfPage transformContext_forBoxSelector context box

-- | @- thumbnailOfSize:forBox:@
thumbnailOfSize_forBox :: IsPDFPage pdfPage => pdfPage -> NSSize -> PDFDisplayBox -> IO (Id NSImage)
thumbnailOfSize_forBox pdfPage size box =
  sendMessage pdfPage thumbnailOfSize_forBoxSelector size box

-- | @- characterBoundsAtIndex:@
characterBoundsAtIndex :: IsPDFPage pdfPage => pdfPage -> CLong -> IO NSRect
characterBoundsAtIndex pdfPage index =
  sendMessage pdfPage characterBoundsAtIndexSelector index

-- | @- characterIndexAtPoint:@
characterIndexAtPoint :: IsPDFPage pdfPage => pdfPage -> NSPoint -> IO CLong
characterIndexAtPoint pdfPage point =
  sendMessage pdfPage characterIndexAtPointSelector point

-- | @- selectionForRect:@
selectionForRect :: IsPDFPage pdfPage => pdfPage -> NSRect -> IO (Id PDFSelection)
selectionForRect pdfPage rect =
  sendMessage pdfPage selectionForRectSelector rect

-- | @- selectionForWordAtPoint:@
selectionForWordAtPoint :: IsPDFPage pdfPage => pdfPage -> NSPoint -> IO (Id PDFSelection)
selectionForWordAtPoint pdfPage point =
  sendMessage pdfPage selectionForWordAtPointSelector point

-- | @- selectionForLineAtPoint:@
selectionForLineAtPoint :: IsPDFPage pdfPage => pdfPage -> NSPoint -> IO (Id PDFSelection)
selectionForLineAtPoint pdfPage point =
  sendMessage pdfPage selectionForLineAtPointSelector point

-- | @- selectionFromPoint:toPoint:@
selectionFromPoint_toPoint :: IsPDFPage pdfPage => pdfPage -> NSPoint -> NSPoint -> IO (Id PDFSelection)
selectionFromPoint_toPoint pdfPage startPoint endPoint =
  sendMessage pdfPage selectionFromPoint_toPointSelector startPoint endPoint

-- | @- selectionForRange:@
selectionForRange :: IsPDFPage pdfPage => pdfPage -> NSRange -> IO (Id PDFSelection)
selectionForRange pdfPage range =
  sendMessage pdfPage selectionForRangeSelector range

-- | @- drawWithBox:@
drawWithBox :: IsPDFPage pdfPage => pdfPage -> PDFDisplayBox -> IO ()
drawWithBox pdfPage box =
  sendMessage pdfPage drawWithBoxSelector box

-- | @- transformContextForBox:@
transformContextForBox :: IsPDFPage pdfPage => pdfPage -> PDFDisplayBox -> IO ()
transformContextForBox pdfPage box =
  sendMessage pdfPage transformContextForBoxSelector box

-- | @- document@
document :: IsPDFPage pdfPage => pdfPage -> IO (Id PDFDocument)
document pdfPage =
  sendMessage pdfPage documentSelector

-- | @- pageRef@
pageRef :: IsPDFPage pdfPage => pdfPage -> IO (Ptr ())
pageRef pdfPage =
  sendMessage pdfPage pageRefSelector

-- | @- label@
label :: IsPDFPage pdfPage => pdfPage -> IO (Id NSString)
label pdfPage =
  sendMessage pdfPage labelSelector

-- | @- rotation@
rotation :: IsPDFPage pdfPage => pdfPage -> IO CLong
rotation pdfPage =
  sendMessage pdfPage rotationSelector

-- | @- setRotation:@
setRotation :: IsPDFPage pdfPage => pdfPage -> CLong -> IO ()
setRotation pdfPage value =
  sendMessage pdfPage setRotationSelector value

-- | @- annotations@
annotations :: IsPDFPage pdfPage => pdfPage -> IO (Id NSArray)
annotations pdfPage =
  sendMessage pdfPage annotationsSelector

-- | @- displaysAnnotations@
displaysAnnotations :: IsPDFPage pdfPage => pdfPage -> IO Bool
displaysAnnotations pdfPage =
  sendMessage pdfPage displaysAnnotationsSelector

-- | @- setDisplaysAnnotations:@
setDisplaysAnnotations :: IsPDFPage pdfPage => pdfPage -> Bool -> IO ()
setDisplaysAnnotations pdfPage value =
  sendMessage pdfPage setDisplaysAnnotationsSelector value

-- | @- numberOfCharacters@
numberOfCharacters :: IsPDFPage pdfPage => pdfPage -> IO CULong
numberOfCharacters pdfPage =
  sendMessage pdfPage numberOfCharactersSelector

-- | @- string@
string :: IsPDFPage pdfPage => pdfPage -> IO (Id NSString)
string pdfPage =
  sendMessage pdfPage stringSelector

-- | @- attributedString@
attributedString :: IsPDFPage pdfPage => pdfPage -> IO (Id NSAttributedString)
attributedString pdfPage =
  sendMessage pdfPage attributedStringSelector

-- | @- dataRepresentation@
dataRepresentation :: IsPDFPage pdfPage => pdfPage -> IO (Id NSData)
dataRepresentation pdfPage =
  sendMessage pdfPage dataRepresentationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PDFPage)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithImage:options:@
initWithImage_optionsSelector :: Selector '[Id NSImage, Id NSDictionary] (Id PDFPage)
initWithImage_optionsSelector = mkSelector "initWithImage:options:"

-- | @Selector@ for @initWithImage:@
initWithImageSelector :: Selector '[Id NSImage] (Id PDFPage)
initWithImageSelector = mkSelector "initWithImage:"

-- | @Selector@ for @boundsForBox:@
boundsForBoxSelector :: Selector '[PDFDisplayBox] NSRect
boundsForBoxSelector = mkSelector "boundsForBox:"

-- | @Selector@ for @setBounds:forBox:@
setBounds_forBoxSelector :: Selector '[NSRect, PDFDisplayBox] ()
setBounds_forBoxSelector = mkSelector "setBounds:forBox:"

-- | @Selector@ for @addAnnotation:@
addAnnotationSelector :: Selector '[Id PDFAnnotation] ()
addAnnotationSelector = mkSelector "addAnnotation:"

-- | @Selector@ for @removeAnnotation:@
removeAnnotationSelector :: Selector '[Id PDFAnnotation] ()
removeAnnotationSelector = mkSelector "removeAnnotation:"

-- | @Selector@ for @annotationAtPoint:@
annotationAtPointSelector :: Selector '[NSPoint] (Id PDFAnnotation)
annotationAtPointSelector = mkSelector "annotationAtPoint:"

-- | @Selector@ for @drawWithBox:toContext:@
drawWithBox_toContextSelector :: Selector '[PDFDisplayBox, Ptr ()] ()
drawWithBox_toContextSelector = mkSelector "drawWithBox:toContext:"

-- | @Selector@ for @transformContext:forBox:@
transformContext_forBoxSelector :: Selector '[Ptr (), PDFDisplayBox] ()
transformContext_forBoxSelector = mkSelector "transformContext:forBox:"

-- | @Selector@ for @thumbnailOfSize:forBox:@
thumbnailOfSize_forBoxSelector :: Selector '[NSSize, PDFDisplayBox] (Id NSImage)
thumbnailOfSize_forBoxSelector = mkSelector "thumbnailOfSize:forBox:"

-- | @Selector@ for @characterBoundsAtIndex:@
characterBoundsAtIndexSelector :: Selector '[CLong] NSRect
characterBoundsAtIndexSelector = mkSelector "characterBoundsAtIndex:"

-- | @Selector@ for @characterIndexAtPoint:@
characterIndexAtPointSelector :: Selector '[NSPoint] CLong
characterIndexAtPointSelector = mkSelector "characterIndexAtPoint:"

-- | @Selector@ for @selectionForRect:@
selectionForRectSelector :: Selector '[NSRect] (Id PDFSelection)
selectionForRectSelector = mkSelector "selectionForRect:"

-- | @Selector@ for @selectionForWordAtPoint:@
selectionForWordAtPointSelector :: Selector '[NSPoint] (Id PDFSelection)
selectionForWordAtPointSelector = mkSelector "selectionForWordAtPoint:"

-- | @Selector@ for @selectionForLineAtPoint:@
selectionForLineAtPointSelector :: Selector '[NSPoint] (Id PDFSelection)
selectionForLineAtPointSelector = mkSelector "selectionForLineAtPoint:"

-- | @Selector@ for @selectionFromPoint:toPoint:@
selectionFromPoint_toPointSelector :: Selector '[NSPoint, NSPoint] (Id PDFSelection)
selectionFromPoint_toPointSelector = mkSelector "selectionFromPoint:toPoint:"

-- | @Selector@ for @selectionForRange:@
selectionForRangeSelector :: Selector '[NSRange] (Id PDFSelection)
selectionForRangeSelector = mkSelector "selectionForRange:"

-- | @Selector@ for @drawWithBox:@
drawWithBoxSelector :: Selector '[PDFDisplayBox] ()
drawWithBoxSelector = mkSelector "drawWithBox:"

-- | @Selector@ for @transformContextForBox:@
transformContextForBoxSelector :: Selector '[PDFDisplayBox] ()
transformContextForBoxSelector = mkSelector "transformContextForBox:"

-- | @Selector@ for @document@
documentSelector :: Selector '[] (Id PDFDocument)
documentSelector = mkSelector "document"

-- | @Selector@ for @pageRef@
pageRefSelector :: Selector '[] (Ptr ())
pageRefSelector = mkSelector "pageRef"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @rotation@
rotationSelector :: Selector '[] CLong
rotationSelector = mkSelector "rotation"

-- | @Selector@ for @setRotation:@
setRotationSelector :: Selector '[CLong] ()
setRotationSelector = mkSelector "setRotation:"

-- | @Selector@ for @annotations@
annotationsSelector :: Selector '[] (Id NSArray)
annotationsSelector = mkSelector "annotations"

-- | @Selector@ for @displaysAnnotations@
displaysAnnotationsSelector :: Selector '[] Bool
displaysAnnotationsSelector = mkSelector "displaysAnnotations"

-- | @Selector@ for @setDisplaysAnnotations:@
setDisplaysAnnotationsSelector :: Selector '[Bool] ()
setDisplaysAnnotationsSelector = mkSelector "setDisplaysAnnotations:"

-- | @Selector@ for @numberOfCharacters@
numberOfCharactersSelector :: Selector '[] CULong
numberOfCharactersSelector = mkSelector "numberOfCharacters"

-- | @Selector@ for @string@
stringSelector :: Selector '[] (Id NSString)
stringSelector = mkSelector "string"

-- | @Selector@ for @attributedString@
attributedStringSelector :: Selector '[] (Id NSAttributedString)
attributedStringSelector = mkSelector "attributedString"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector '[] (Id NSData)
dataRepresentationSelector = mkSelector "dataRepresentation"

