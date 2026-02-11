{-# LANGUAGE PatternSynonyms #-}
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
  , initWithPage_atPointSelector
  , compareSelector
  , pageSelector
  , pointSelector
  , zoomSelector
  , setZoomSelector

  -- * Enum types
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending

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

import ObjC.PDFKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithPage:atPoint:@
initWithPage_atPoint :: (IsPDFDestination pdfDestination, IsPDFPage page) => pdfDestination -> page -> NSPoint -> IO (Id PDFDestination)
initWithPage_atPoint pdfDestination  page point =
withObjCPtr page $ \raw_page ->
    sendMsg pdfDestination (mkSelector "initWithPage:atPoint:") (retPtr retVoid) [argPtr (castPtr raw_page :: Ptr ()), argNSPoint point] >>= ownedObject . castPtr

-- | @- compare:@
compare_ :: (IsPDFDestination pdfDestination, IsPDFDestination destination) => pdfDestination -> destination -> IO NSComparisonResult
compare_ pdfDestination  destination =
withObjCPtr destination $ \raw_destination ->
    fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg pdfDestination (mkSelector "compare:") retCLong [argPtr (castPtr raw_destination :: Ptr ())]

-- | @- page@
page :: IsPDFDestination pdfDestination => pdfDestination -> IO (Id PDFPage)
page pdfDestination  =
  sendMsg pdfDestination (mkSelector "page") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- point@
point :: IsPDFDestination pdfDestination => pdfDestination -> IO NSPoint
point pdfDestination  =
  sendMsgStret pdfDestination (mkSelector "point") retNSPoint []

-- | @- zoom@
zoom :: IsPDFDestination pdfDestination => pdfDestination -> IO CDouble
zoom pdfDestination  =
  sendMsg pdfDestination (mkSelector "zoom") retCDouble []

-- | @- setZoom:@
setZoom :: IsPDFDestination pdfDestination => pdfDestination -> CDouble -> IO ()
setZoom pdfDestination  value =
  sendMsg pdfDestination (mkSelector "setZoom:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPage:atPoint:@
initWithPage_atPointSelector :: Selector
initWithPage_atPointSelector = mkSelector "initWithPage:atPoint:"

-- | @Selector@ for @compare:@
compareSelector :: Selector
compareSelector = mkSelector "compare:"

-- | @Selector@ for @page@
pageSelector :: Selector
pageSelector = mkSelector "page"

-- | @Selector@ for @point@
pointSelector :: Selector
pointSelector = mkSelector "point"

-- | @Selector@ for @zoom@
zoomSelector :: Selector
zoomSelector = mkSelector "zoom"

-- | @Selector@ for @setZoom:@
setZoomSelector :: Selector
setZoomSelector = mkSelector "setZoom:"

