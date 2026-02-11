{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFActionNamed@.
module ObjC.PDFKit.PDFActionNamed
  ( PDFActionNamed
  , IsPDFActionNamed(..)
  , initWithName
  , name
  , setName
  , initWithNameSelector
  , nameSelector
  , setNameSelector

  -- * Enum types
  , PDFActionNamedName(PDFActionNamedName)
  , pattern KPDFActionNamedNone
  , pattern KPDFActionNamedNextPage
  , pattern KPDFActionNamedPreviousPage
  , pattern KPDFActionNamedFirstPage
  , pattern KPDFActionNamedLastPage
  , pattern KPDFActionNamedGoBack
  , pattern KPDFActionNamedGoForward
  , pattern KPDFActionNamedGoToPage
  , pattern KPDFActionNamedFind
  , pattern KPDFActionNamedPrint
  , pattern KPDFActionNamedZoomIn
  , pattern KPDFActionNamedZoomOut

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
import ObjC.PDFKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithName:@
initWithName :: IsPDFActionNamed pdfActionNamed => pdfActionNamed -> PDFActionNamedName -> IO (Id PDFActionNamed)
initWithName pdfActionNamed  name =
  sendMsg pdfActionNamed (mkSelector "initWithName:") (retPtr retVoid) [argCLong (coerce name)] >>= ownedObject . castPtr

-- | @- name@
name :: IsPDFActionNamed pdfActionNamed => pdfActionNamed -> IO PDFActionNamedName
name pdfActionNamed  =
  fmap (coerce :: CLong -> PDFActionNamedName) $ sendMsg pdfActionNamed (mkSelector "name") retCLong []

-- | @- setName:@
setName :: IsPDFActionNamed pdfActionNamed => pdfActionNamed -> PDFActionNamedName -> IO ()
setName pdfActionNamed  value =
  sendMsg pdfActionNamed (mkSelector "setName:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

