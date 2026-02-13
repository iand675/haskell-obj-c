{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFAnnotationStamp@.
module ObjC.PDFKit.PDFAnnotationStamp
  ( PDFAnnotationStamp
  , IsPDFAnnotationStamp(..)
  , name
  , setName
  , nameSelector
  , setNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PDFKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsPDFAnnotationStamp pdfAnnotationStamp => pdfAnnotationStamp -> IO (Id NSString)
name pdfAnnotationStamp =
  sendMessage pdfAnnotationStamp nameSelector

-- | @- setName:@
setName :: (IsPDFAnnotationStamp pdfAnnotationStamp, IsNSString name) => pdfAnnotationStamp -> name -> IO ()
setName pdfAnnotationStamp name =
  sendMessage pdfAnnotationStamp setNameSelector (toNSString name)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

