{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPDFImageRep@.
module ObjC.AppKit.NSPDFImageRep
  ( NSPDFImageRep
  , IsNSPDFImageRep(..)
  , imageRepWithData
  , initWithData
  , pdfRepresentation
  , bounds
  , currentPage
  , setCurrentPage
  , pageCount
  , boundsSelector
  , currentPageSelector
  , imageRepWithDataSelector
  , initWithDataSelector
  , pageCountSelector
  , pdfRepresentationSelector
  , setCurrentPageSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ imageRepWithData:@
imageRepWithData :: IsNSData pdfData => pdfData -> IO (Id NSPDFImageRep)
imageRepWithData pdfData =
  do
    cls' <- getRequiredClass "NSPDFImageRep"
    sendClassMessage cls' imageRepWithDataSelector (toNSData pdfData)

-- | @- initWithData:@
initWithData :: (IsNSPDFImageRep nspdfImageRep, IsNSData pdfData) => nspdfImageRep -> pdfData -> IO (Id NSPDFImageRep)
initWithData nspdfImageRep pdfData =
  sendOwnedMessage nspdfImageRep initWithDataSelector (toNSData pdfData)

-- | @- PDFRepresentation@
pdfRepresentation :: IsNSPDFImageRep nspdfImageRep => nspdfImageRep -> IO (Id NSData)
pdfRepresentation nspdfImageRep =
  sendMessage nspdfImageRep pdfRepresentationSelector

-- | @- bounds@
bounds :: IsNSPDFImageRep nspdfImageRep => nspdfImageRep -> IO NSRect
bounds nspdfImageRep =
  sendMessage nspdfImageRep boundsSelector

-- | @- currentPage@
currentPage :: IsNSPDFImageRep nspdfImageRep => nspdfImageRep -> IO CLong
currentPage nspdfImageRep =
  sendMessage nspdfImageRep currentPageSelector

-- | @- setCurrentPage:@
setCurrentPage :: IsNSPDFImageRep nspdfImageRep => nspdfImageRep -> CLong -> IO ()
setCurrentPage nspdfImageRep value =
  sendMessage nspdfImageRep setCurrentPageSelector value

-- | @- pageCount@
pageCount :: IsNSPDFImageRep nspdfImageRep => nspdfImageRep -> IO CLong
pageCount nspdfImageRep =
  sendMessage nspdfImageRep pageCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageRepWithData:@
imageRepWithDataSelector :: Selector '[Id NSData] (Id NSPDFImageRep)
imageRepWithDataSelector = mkSelector "imageRepWithData:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector '[Id NSData] (Id NSPDFImageRep)
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @PDFRepresentation@
pdfRepresentationSelector :: Selector '[] (Id NSData)
pdfRepresentationSelector = mkSelector "PDFRepresentation"

-- | @Selector@ for @bounds@
boundsSelector :: Selector '[] NSRect
boundsSelector = mkSelector "bounds"

-- | @Selector@ for @currentPage@
currentPageSelector :: Selector '[] CLong
currentPageSelector = mkSelector "currentPage"

-- | @Selector@ for @setCurrentPage:@
setCurrentPageSelector :: Selector '[CLong] ()
setCurrentPageSelector = mkSelector "setCurrentPage:"

-- | @Selector@ for @pageCount@
pageCountSelector :: Selector '[] CLong
pageCountSelector = mkSelector "pageCount"

