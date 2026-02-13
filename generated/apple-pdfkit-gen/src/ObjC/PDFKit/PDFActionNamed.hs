{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PDFKit.Internal.Classes
import ObjC.PDFKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithName:@
initWithName :: IsPDFActionNamed pdfActionNamed => pdfActionNamed -> PDFActionNamedName -> IO (Id PDFActionNamed)
initWithName pdfActionNamed name =
  sendOwnedMessage pdfActionNamed initWithNameSelector name

-- | @- name@
name :: IsPDFActionNamed pdfActionNamed => pdfActionNamed -> IO PDFActionNamedName
name pdfActionNamed =
  sendMessage pdfActionNamed nameSelector

-- | @- setName:@
setName :: IsPDFActionNamed pdfActionNamed => pdfActionNamed -> PDFActionNamedName -> IO ()
setName pdfActionNamed value =
  sendMessage pdfActionNamed setNameSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector '[PDFActionNamedName] (Id PDFActionNamed)
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] PDFActionNamedName
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[PDFActionNamedName] ()
setNameSelector = mkSelector "setName:"

