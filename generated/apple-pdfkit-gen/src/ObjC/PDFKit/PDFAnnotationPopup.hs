{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PDFKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- isOpen@
isOpen :: IsPDFAnnotationPopup pdfAnnotationPopup => pdfAnnotationPopup -> IO Bool
isOpen pdfAnnotationPopup =
  sendMessage pdfAnnotationPopup isOpenSelector

-- | @- setIsOpen:@
setIsOpen :: IsPDFAnnotationPopup pdfAnnotationPopup => pdfAnnotationPopup -> Bool -> IO ()
setIsOpen pdfAnnotationPopup isOpen =
  sendMessage pdfAnnotationPopup setIsOpenSelector isOpen

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isOpen@
isOpenSelector :: Selector '[] Bool
isOpenSelector = mkSelector "isOpen"

-- | @Selector@ for @setIsOpen:@
setIsOpenSelector :: Selector '[Bool] ()
setIsOpenSelector = mkSelector "setIsOpen:"

