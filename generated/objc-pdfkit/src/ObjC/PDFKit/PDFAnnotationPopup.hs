{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFAnnotationPopup@.
module ObjC.PDFKit.PDFAnnotationPopup
  ( PDFAnnotationPopup
  , IsPDFAnnotationPopup(..)
  , isOpen
  , setIsOpen
  , isOpenSelector
  , setIsOpenSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- isOpen@
isOpen :: IsPDFAnnotationPopup pdfAnnotationPopup => pdfAnnotationPopup -> IO Bool
isOpen pdfAnnotationPopup  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfAnnotationPopup (mkSelector "isOpen") retCULong []

-- | @- setIsOpen:@
setIsOpen :: IsPDFAnnotationPopup pdfAnnotationPopup => pdfAnnotationPopup -> Bool -> IO ()
setIsOpen pdfAnnotationPopup  isOpen =
  sendMsg pdfAnnotationPopup (mkSelector "setIsOpen:") retVoid [argCULong (if isOpen then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isOpen@
isOpenSelector :: Selector
isOpenSelector = mkSelector "isOpen"

-- | @Selector@ for @setIsOpen:@
setIsOpenSelector :: Selector
setIsOpenSelector = mkSelector "setIsOpen:"

