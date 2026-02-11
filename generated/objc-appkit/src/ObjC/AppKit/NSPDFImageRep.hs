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
  , imageRepWithDataSelector
  , initWithDataSelector
  , pdfRepresentationSelector
  , boundsSelector
  , currentPageSelector
  , setCurrentPageSelector
  , pageCountSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ imageRepWithData:@
imageRepWithData :: IsNSData pdfData => pdfData -> IO (Id NSPDFImageRep)
imageRepWithData pdfData =
  do
    cls' <- getRequiredClass "NSPDFImageRep"
    withObjCPtr pdfData $ \raw_pdfData ->
      sendClassMsg cls' (mkSelector "imageRepWithData:") (retPtr retVoid) [argPtr (castPtr raw_pdfData :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithData:@
initWithData :: (IsNSPDFImageRep nspdfImageRep, IsNSData pdfData) => nspdfImageRep -> pdfData -> IO (Id NSPDFImageRep)
initWithData nspdfImageRep  pdfData =
withObjCPtr pdfData $ \raw_pdfData ->
    sendMsg nspdfImageRep (mkSelector "initWithData:") (retPtr retVoid) [argPtr (castPtr raw_pdfData :: Ptr ())] >>= ownedObject . castPtr

-- | @- PDFRepresentation@
pdfRepresentation :: IsNSPDFImageRep nspdfImageRep => nspdfImageRep -> IO (Id NSData)
pdfRepresentation nspdfImageRep  =
  sendMsg nspdfImageRep (mkSelector "PDFRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- bounds@
bounds :: IsNSPDFImageRep nspdfImageRep => nspdfImageRep -> IO NSRect
bounds nspdfImageRep  =
  sendMsgStret nspdfImageRep (mkSelector "bounds") retNSRect []

-- | @- currentPage@
currentPage :: IsNSPDFImageRep nspdfImageRep => nspdfImageRep -> IO CLong
currentPage nspdfImageRep  =
  sendMsg nspdfImageRep (mkSelector "currentPage") retCLong []

-- | @- setCurrentPage:@
setCurrentPage :: IsNSPDFImageRep nspdfImageRep => nspdfImageRep -> CLong -> IO ()
setCurrentPage nspdfImageRep  value =
  sendMsg nspdfImageRep (mkSelector "setCurrentPage:") retVoid [argCLong (fromIntegral value)]

-- | @- pageCount@
pageCount :: IsNSPDFImageRep nspdfImageRep => nspdfImageRep -> IO CLong
pageCount nspdfImageRep  =
  sendMsg nspdfImageRep (mkSelector "pageCount") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageRepWithData:@
imageRepWithDataSelector :: Selector
imageRepWithDataSelector = mkSelector "imageRepWithData:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @PDFRepresentation@
pdfRepresentationSelector :: Selector
pdfRepresentationSelector = mkSelector "PDFRepresentation"

-- | @Selector@ for @bounds@
boundsSelector :: Selector
boundsSelector = mkSelector "bounds"

-- | @Selector@ for @currentPage@
currentPageSelector :: Selector
currentPageSelector = mkSelector "currentPage"

-- | @Selector@ for @setCurrentPage:@
setCurrentPageSelector :: Selector
setCurrentPageSelector = mkSelector "setCurrentPage:"

-- | @Selector@ for @pageCount@
pageCountSelector :: Selector
pageCountSelector = mkSelector "pageCount"

