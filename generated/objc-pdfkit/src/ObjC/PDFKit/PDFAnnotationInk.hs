{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFAnnotationInk@.
module ObjC.PDFKit.PDFAnnotationInk
  ( PDFAnnotationInk
  , IsPDFAnnotationInk(..)
  , paths
  , addBezierPath
  , removeBezierPath
  , pathsSelector
  , addBezierPathSelector
  , removeBezierPathSelector


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

import ObjC.PDFKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- paths@
paths :: IsPDFAnnotationInk pdfAnnotationInk => pdfAnnotationInk -> IO (Id NSArray)
paths pdfAnnotationInk  =
  sendMsg pdfAnnotationInk (mkSelector "paths") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- addBezierPath:@
addBezierPath :: (IsPDFAnnotationInk pdfAnnotationInk, IsNSBezierPath path) => pdfAnnotationInk -> path -> IO ()
addBezierPath pdfAnnotationInk  path =
withObjCPtr path $ \raw_path ->
    sendMsg pdfAnnotationInk (mkSelector "addBezierPath:") retVoid [argPtr (castPtr raw_path :: Ptr ())]

-- | @- removeBezierPath:@
removeBezierPath :: (IsPDFAnnotationInk pdfAnnotationInk, IsNSBezierPath path) => pdfAnnotationInk -> path -> IO ()
removeBezierPath pdfAnnotationInk  path =
withObjCPtr path $ \raw_path ->
    sendMsg pdfAnnotationInk (mkSelector "removeBezierPath:") retVoid [argPtr (castPtr raw_path :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @paths@
pathsSelector :: Selector
pathsSelector = mkSelector "paths"

-- | @Selector@ for @addBezierPath:@
addBezierPathSelector :: Selector
addBezierPathSelector = mkSelector "addBezierPath:"

-- | @Selector@ for @removeBezierPath:@
removeBezierPathSelector :: Selector
removeBezierPathSelector = mkSelector "removeBezierPath:"

