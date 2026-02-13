{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFDestination@.
module ObjC.PDFKit.PDFDestination
  ( PDFDestination
  , IsPDFDestination(..)
  , initWithPage_atPoint
  , compare_
  , page
  , point
  , zoom
  , setZoom
  , compareSelector
  , initWithPage_atPointSelector
  , pageSelector
  , pointSelector
  , setZoomSelector
  , zoomSelector

  -- * Enum types
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PDFKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithPage:atPoint:@
initWithPage_atPoint :: (IsPDFDestination pdfDestination, IsPDFPage page) => pdfDestination -> page -> NSPoint -> IO (Id PDFDestination)
initWithPage_atPoint pdfDestination page point =
  sendOwnedMessage pdfDestination initWithPage_atPointSelector (toPDFPage page) point

-- | @- compare:@
compare_ :: (IsPDFDestination pdfDestination, IsPDFDestination destination) => pdfDestination -> destination -> IO NSComparisonResult
compare_ pdfDestination destination =
  sendMessage pdfDestination compareSelector (toPDFDestination destination)

-- | @- page@
page :: IsPDFDestination pdfDestination => pdfDestination -> IO (Id PDFPage)
page pdfDestination =
  sendMessage pdfDestination pageSelector

-- | @- point@
point :: IsPDFDestination pdfDestination => pdfDestination -> IO NSPoint
point pdfDestination =
  sendMessage pdfDestination pointSelector

-- | @- zoom@
zoom :: IsPDFDestination pdfDestination => pdfDestination -> IO CDouble
zoom pdfDestination =
  sendMessage pdfDestination zoomSelector

-- | @- setZoom:@
setZoom :: IsPDFDestination pdfDestination => pdfDestination -> CDouble -> IO ()
setZoom pdfDestination value =
  sendMessage pdfDestination setZoomSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPage:atPoint:@
initWithPage_atPointSelector :: Selector '[Id PDFPage, NSPoint] (Id PDFDestination)
initWithPage_atPointSelector = mkSelector "initWithPage:atPoint:"

-- | @Selector@ for @compare:@
compareSelector :: Selector '[Id PDFDestination] NSComparisonResult
compareSelector = mkSelector "compare:"

-- | @Selector@ for @page@
pageSelector :: Selector '[] (Id PDFPage)
pageSelector = mkSelector "page"

-- | @Selector@ for @point@
pointSelector :: Selector '[] NSPoint
pointSelector = mkSelector "point"

-- | @Selector@ for @zoom@
zoomSelector :: Selector '[] CDouble
zoomSelector = mkSelector "zoom"

-- | @Selector@ for @setZoom:@
setZoomSelector :: Selector '[CDouble] ()
setZoomSelector = mkSelector "setZoom:"

