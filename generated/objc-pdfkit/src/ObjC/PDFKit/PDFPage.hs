{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithImage_optionsSelector
  , initWithImageSelector
  , boundsForBoxSelector
  , setBounds_forBoxSelector
  , addAnnotationSelector
  , removeAnnotationSelector
  , annotationAtPointSelector
  , drawWithBox_toContextSelector
  , transformContext_forBoxSelector
  , thumbnailOfSize_forBoxSelector
  , characterBoundsAtIndexSelector
  , characterIndexAtPointSelector
  , selectionForRectSelector
  , selectionForWordAtPointSelector
  , selectionForLineAtPointSelector
  , selectionFromPoint_toPointSelector
  , selectionForRangeSelector
  , drawWithBoxSelector
  , transformContextForBoxSelector
  , documentSelector
  , pageRefSelector
  , labelSelector
  , rotationSelector
  , setRotationSelector
  , annotationsSelector
  , displaysAnnotationsSelector
  , setDisplaysAnnotationsSelector
  , numberOfCharactersSelector
  , stringSelector
  , attributedStringSelector
  , dataRepresentationSelector

  -- * Enum types
  , PDFDisplayBox(PDFDisplayBox)
  , pattern KPDFDisplayBoxMediaBox
  , pattern KPDFDisplayBoxCropBox
  , pattern KPDFDisplayBoxBleedBox
  , pattern KPDFDisplayBoxTrimBox
  , pattern KPDFDisplayBoxArtBox

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

import ObjC.PDFKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.PDFKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPDFPage pdfPage => pdfPage -> IO (Id PDFPage)
init_ pdfPage  =
  sendMsg pdfPage (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithImage:options:@
initWithImage_options :: (IsPDFPage pdfPage, IsNSImage image, IsNSDictionary options) => pdfPage -> image -> options -> IO (Id PDFPage)
initWithImage_options pdfPage  image options =
withObjCPtr image $ \raw_image ->
  withObjCPtr options $ \raw_options ->
      sendMsg pdfPage (mkSelector "initWithImage:options:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithImage:@
initWithImage :: (IsPDFPage pdfPage, IsNSImage image) => pdfPage -> image -> IO (Id PDFPage)
initWithImage pdfPage  image =
withObjCPtr image $ \raw_image ->
    sendMsg pdfPage (mkSelector "initWithImage:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ())] >>= ownedObject . castPtr

-- | @- boundsForBox:@
boundsForBox :: IsPDFPage pdfPage => pdfPage -> PDFDisplayBox -> IO NSRect
boundsForBox pdfPage  box =
  sendMsgStret pdfPage (mkSelector "boundsForBox:") retNSRect [argCLong (coerce box)]

-- | @- setBounds:forBox:@
setBounds_forBox :: IsPDFPage pdfPage => pdfPage -> NSRect -> PDFDisplayBox -> IO ()
setBounds_forBox pdfPage  bounds box =
  sendMsg pdfPage (mkSelector "setBounds:forBox:") retVoid [argNSRect bounds, argCLong (coerce box)]

-- | @- addAnnotation:@
addAnnotation :: (IsPDFPage pdfPage, IsPDFAnnotation annotation) => pdfPage -> annotation -> IO ()
addAnnotation pdfPage  annotation =
withObjCPtr annotation $ \raw_annotation ->
    sendMsg pdfPage (mkSelector "addAnnotation:") retVoid [argPtr (castPtr raw_annotation :: Ptr ())]

-- | @- removeAnnotation:@
removeAnnotation :: (IsPDFPage pdfPage, IsPDFAnnotation annotation) => pdfPage -> annotation -> IO ()
removeAnnotation pdfPage  annotation =
withObjCPtr annotation $ \raw_annotation ->
    sendMsg pdfPage (mkSelector "removeAnnotation:") retVoid [argPtr (castPtr raw_annotation :: Ptr ())]

-- | @- annotationAtPoint:@
annotationAtPoint :: IsPDFPage pdfPage => pdfPage -> NSPoint -> IO (Id PDFAnnotation)
annotationAtPoint pdfPage  point =
  sendMsg pdfPage (mkSelector "annotationAtPoint:") (retPtr retVoid) [argNSPoint point] >>= retainedObject . castPtr

-- | @- drawWithBox:toContext:@
drawWithBox_toContext :: IsPDFPage pdfPage => pdfPage -> PDFDisplayBox -> Ptr () -> IO ()
drawWithBox_toContext pdfPage  box context =
  sendMsg pdfPage (mkSelector "drawWithBox:toContext:") retVoid [argCLong (coerce box), argPtr context]

-- | @- transformContext:forBox:@
transformContext_forBox :: IsPDFPage pdfPage => pdfPage -> Ptr () -> PDFDisplayBox -> IO ()
transformContext_forBox pdfPage  context box =
  sendMsg pdfPage (mkSelector "transformContext:forBox:") retVoid [argPtr context, argCLong (coerce box)]

-- | @- thumbnailOfSize:forBox:@
thumbnailOfSize_forBox :: IsPDFPage pdfPage => pdfPage -> NSSize -> PDFDisplayBox -> IO (Id NSImage)
thumbnailOfSize_forBox pdfPage  size box =
  sendMsg pdfPage (mkSelector "thumbnailOfSize:forBox:") (retPtr retVoid) [argNSSize size, argCLong (coerce box)] >>= retainedObject . castPtr

-- | @- characterBoundsAtIndex:@
characterBoundsAtIndex :: IsPDFPage pdfPage => pdfPage -> CLong -> IO NSRect
characterBoundsAtIndex pdfPage  index =
  sendMsgStret pdfPage (mkSelector "characterBoundsAtIndex:") retNSRect [argCLong (fromIntegral index)]

-- | @- characterIndexAtPoint:@
characterIndexAtPoint :: IsPDFPage pdfPage => pdfPage -> NSPoint -> IO CLong
characterIndexAtPoint pdfPage  point =
  sendMsg pdfPage (mkSelector "characterIndexAtPoint:") retCLong [argNSPoint point]

-- | @- selectionForRect:@
selectionForRect :: IsPDFPage pdfPage => pdfPage -> NSRect -> IO (Id PDFSelection)
selectionForRect pdfPage  rect =
  sendMsg pdfPage (mkSelector "selectionForRect:") (retPtr retVoid) [argNSRect rect] >>= retainedObject . castPtr

-- | @- selectionForWordAtPoint:@
selectionForWordAtPoint :: IsPDFPage pdfPage => pdfPage -> NSPoint -> IO (Id PDFSelection)
selectionForWordAtPoint pdfPage  point =
  sendMsg pdfPage (mkSelector "selectionForWordAtPoint:") (retPtr retVoid) [argNSPoint point] >>= retainedObject . castPtr

-- | @- selectionForLineAtPoint:@
selectionForLineAtPoint :: IsPDFPage pdfPage => pdfPage -> NSPoint -> IO (Id PDFSelection)
selectionForLineAtPoint pdfPage  point =
  sendMsg pdfPage (mkSelector "selectionForLineAtPoint:") (retPtr retVoid) [argNSPoint point] >>= retainedObject . castPtr

-- | @- selectionFromPoint:toPoint:@
selectionFromPoint_toPoint :: IsPDFPage pdfPage => pdfPage -> NSPoint -> NSPoint -> IO (Id PDFSelection)
selectionFromPoint_toPoint pdfPage  startPoint endPoint =
  sendMsg pdfPage (mkSelector "selectionFromPoint:toPoint:") (retPtr retVoid) [argNSPoint startPoint, argNSPoint endPoint] >>= retainedObject . castPtr

-- | @- selectionForRange:@
selectionForRange :: IsPDFPage pdfPage => pdfPage -> NSRange -> IO (Id PDFSelection)
selectionForRange pdfPage  range =
  sendMsg pdfPage (mkSelector "selectionForRange:") (retPtr retVoid) [argNSRange range] >>= retainedObject . castPtr

-- | @- drawWithBox:@
drawWithBox :: IsPDFPage pdfPage => pdfPage -> PDFDisplayBox -> IO ()
drawWithBox pdfPage  box =
  sendMsg pdfPage (mkSelector "drawWithBox:") retVoid [argCLong (coerce box)]

-- | @- transformContextForBox:@
transformContextForBox :: IsPDFPage pdfPage => pdfPage -> PDFDisplayBox -> IO ()
transformContextForBox pdfPage  box =
  sendMsg pdfPage (mkSelector "transformContextForBox:") retVoid [argCLong (coerce box)]

-- | @- document@
document :: IsPDFPage pdfPage => pdfPage -> IO (Id PDFDocument)
document pdfPage  =
  sendMsg pdfPage (mkSelector "document") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pageRef@
pageRef :: IsPDFPage pdfPage => pdfPage -> IO (Ptr ())
pageRef pdfPage  =
  fmap castPtr $ sendMsg pdfPage (mkSelector "pageRef") (retPtr retVoid) []

-- | @- label@
label :: IsPDFPage pdfPage => pdfPage -> IO (Id NSString)
label pdfPage  =
  sendMsg pdfPage (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rotation@
rotation :: IsPDFPage pdfPage => pdfPage -> IO CLong
rotation pdfPage  =
  sendMsg pdfPage (mkSelector "rotation") retCLong []

-- | @- setRotation:@
setRotation :: IsPDFPage pdfPage => pdfPage -> CLong -> IO ()
setRotation pdfPage  value =
  sendMsg pdfPage (mkSelector "setRotation:") retVoid [argCLong (fromIntegral value)]

-- | @- annotations@
annotations :: IsPDFPage pdfPage => pdfPage -> IO (Id NSArray)
annotations pdfPage  =
  sendMsg pdfPage (mkSelector "annotations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- displaysAnnotations@
displaysAnnotations :: IsPDFPage pdfPage => pdfPage -> IO Bool
displaysAnnotations pdfPage  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfPage (mkSelector "displaysAnnotations") retCULong []

-- | @- setDisplaysAnnotations:@
setDisplaysAnnotations :: IsPDFPage pdfPage => pdfPage -> Bool -> IO ()
setDisplaysAnnotations pdfPage  value =
  sendMsg pdfPage (mkSelector "setDisplaysAnnotations:") retVoid [argCULong (if value then 1 else 0)]

-- | @- numberOfCharacters@
numberOfCharacters :: IsPDFPage pdfPage => pdfPage -> IO CULong
numberOfCharacters pdfPage  =
  sendMsg pdfPage (mkSelector "numberOfCharacters") retCULong []

-- | @- string@
string :: IsPDFPage pdfPage => pdfPage -> IO (Id NSString)
string pdfPage  =
  sendMsg pdfPage (mkSelector "string") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attributedString@
attributedString :: IsPDFPage pdfPage => pdfPage -> IO (Id NSAttributedString)
attributedString pdfPage  =
  sendMsg pdfPage (mkSelector "attributedString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dataRepresentation@
dataRepresentation :: IsPDFPage pdfPage => pdfPage -> IO (Id NSData)
dataRepresentation pdfPage  =
  sendMsg pdfPage (mkSelector "dataRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithImage:options:@
initWithImage_optionsSelector :: Selector
initWithImage_optionsSelector = mkSelector "initWithImage:options:"

-- | @Selector@ for @initWithImage:@
initWithImageSelector :: Selector
initWithImageSelector = mkSelector "initWithImage:"

-- | @Selector@ for @boundsForBox:@
boundsForBoxSelector :: Selector
boundsForBoxSelector = mkSelector "boundsForBox:"

-- | @Selector@ for @setBounds:forBox:@
setBounds_forBoxSelector :: Selector
setBounds_forBoxSelector = mkSelector "setBounds:forBox:"

-- | @Selector@ for @addAnnotation:@
addAnnotationSelector :: Selector
addAnnotationSelector = mkSelector "addAnnotation:"

-- | @Selector@ for @removeAnnotation:@
removeAnnotationSelector :: Selector
removeAnnotationSelector = mkSelector "removeAnnotation:"

-- | @Selector@ for @annotationAtPoint:@
annotationAtPointSelector :: Selector
annotationAtPointSelector = mkSelector "annotationAtPoint:"

-- | @Selector@ for @drawWithBox:toContext:@
drawWithBox_toContextSelector :: Selector
drawWithBox_toContextSelector = mkSelector "drawWithBox:toContext:"

-- | @Selector@ for @transformContext:forBox:@
transformContext_forBoxSelector :: Selector
transformContext_forBoxSelector = mkSelector "transformContext:forBox:"

-- | @Selector@ for @thumbnailOfSize:forBox:@
thumbnailOfSize_forBoxSelector :: Selector
thumbnailOfSize_forBoxSelector = mkSelector "thumbnailOfSize:forBox:"

-- | @Selector@ for @characterBoundsAtIndex:@
characterBoundsAtIndexSelector :: Selector
characterBoundsAtIndexSelector = mkSelector "characterBoundsAtIndex:"

-- | @Selector@ for @characterIndexAtPoint:@
characterIndexAtPointSelector :: Selector
characterIndexAtPointSelector = mkSelector "characterIndexAtPoint:"

-- | @Selector@ for @selectionForRect:@
selectionForRectSelector :: Selector
selectionForRectSelector = mkSelector "selectionForRect:"

-- | @Selector@ for @selectionForWordAtPoint:@
selectionForWordAtPointSelector :: Selector
selectionForWordAtPointSelector = mkSelector "selectionForWordAtPoint:"

-- | @Selector@ for @selectionForLineAtPoint:@
selectionForLineAtPointSelector :: Selector
selectionForLineAtPointSelector = mkSelector "selectionForLineAtPoint:"

-- | @Selector@ for @selectionFromPoint:toPoint:@
selectionFromPoint_toPointSelector :: Selector
selectionFromPoint_toPointSelector = mkSelector "selectionFromPoint:toPoint:"

-- | @Selector@ for @selectionForRange:@
selectionForRangeSelector :: Selector
selectionForRangeSelector = mkSelector "selectionForRange:"

-- | @Selector@ for @drawWithBox:@
drawWithBoxSelector :: Selector
drawWithBoxSelector = mkSelector "drawWithBox:"

-- | @Selector@ for @transformContextForBox:@
transformContextForBoxSelector :: Selector
transformContextForBoxSelector = mkSelector "transformContextForBox:"

-- | @Selector@ for @document@
documentSelector :: Selector
documentSelector = mkSelector "document"

-- | @Selector@ for @pageRef@
pageRefSelector :: Selector
pageRefSelector = mkSelector "pageRef"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @rotation@
rotationSelector :: Selector
rotationSelector = mkSelector "rotation"

-- | @Selector@ for @setRotation:@
setRotationSelector :: Selector
setRotationSelector = mkSelector "setRotation:"

-- | @Selector@ for @annotations@
annotationsSelector :: Selector
annotationsSelector = mkSelector "annotations"

-- | @Selector@ for @displaysAnnotations@
displaysAnnotationsSelector :: Selector
displaysAnnotationsSelector = mkSelector "displaysAnnotations"

-- | @Selector@ for @setDisplaysAnnotations:@
setDisplaysAnnotationsSelector :: Selector
setDisplaysAnnotationsSelector = mkSelector "setDisplaysAnnotations:"

-- | @Selector@ for @numberOfCharacters@
numberOfCharactersSelector :: Selector
numberOfCharactersSelector = mkSelector "numberOfCharacters"

-- | @Selector@ for @string@
stringSelector :: Selector
stringSelector = mkSelector "string"

-- | @Selector@ for @attributedString@
attributedStringSelector :: Selector
attributedStringSelector = mkSelector "attributedString"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector
dataRepresentationSelector = mkSelector "dataRepresentation"

