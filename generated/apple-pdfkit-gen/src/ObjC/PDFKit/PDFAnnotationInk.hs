{-# LANGUAGE DataKinds #-}
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
  , addBezierPathSelector
  , pathsSelector
  , removeBezierPathSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PDFKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- paths@
paths :: IsPDFAnnotationInk pdfAnnotationInk => pdfAnnotationInk -> IO (Id NSArray)
paths pdfAnnotationInk =
  sendMessage pdfAnnotationInk pathsSelector

-- | @- addBezierPath:@
addBezierPath :: (IsPDFAnnotationInk pdfAnnotationInk, IsNSBezierPath path) => pdfAnnotationInk -> path -> IO ()
addBezierPath pdfAnnotationInk path =
  sendMessage pdfAnnotationInk addBezierPathSelector (toNSBezierPath path)

-- | @- removeBezierPath:@
removeBezierPath :: (IsPDFAnnotationInk pdfAnnotationInk, IsNSBezierPath path) => pdfAnnotationInk -> path -> IO ()
removeBezierPath pdfAnnotationInk path =
  sendMessage pdfAnnotationInk removeBezierPathSelector (toNSBezierPath path)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @paths@
pathsSelector :: Selector '[] (Id NSArray)
pathsSelector = mkSelector "paths"

-- | @Selector@ for @addBezierPath:@
addBezierPathSelector :: Selector '[Id NSBezierPath] ()
addBezierPathSelector = mkSelector "addBezierPath:"

-- | @Selector@ for @removeBezierPath:@
removeBezierPathSelector :: Selector '[Id NSBezierPath] ()
removeBezierPathSelector = mkSelector "removeBezierPath:"

