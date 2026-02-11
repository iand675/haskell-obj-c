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

-- | @- name@
name :: IsPDFAnnotationStamp pdfAnnotationStamp => pdfAnnotationStamp -> IO (Id NSString)
name pdfAnnotationStamp  =
  sendMsg pdfAnnotationStamp (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsPDFAnnotationStamp pdfAnnotationStamp, IsNSString name) => pdfAnnotationStamp -> name -> IO ()
setName pdfAnnotationStamp  name =
withObjCPtr name $ \raw_name ->
    sendMsg pdfAnnotationStamp (mkSelector "setName:") retVoid [argPtr (castPtr raw_name :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

