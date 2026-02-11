{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFActionRemoteGoTo@.
module ObjC.PDFKit.PDFActionRemoteGoTo
  ( PDFActionRemoteGoTo
  , IsPDFActionRemoteGoTo(..)
  , initWithPageIndex_atPoint_fileURL
  , pageIndex
  , setPageIndex
  , point
  , setPoint
  , url
  , setURL
  , initWithPageIndex_atPoint_fileURLSelector
  , pageIndexSelector
  , setPageIndexSelector
  , pointSelector
  , setPointSelector
  , urlSelector
  , setURLSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithPageIndex:atPoint:fileURL:@
initWithPageIndex_atPoint_fileURL :: (IsPDFActionRemoteGoTo pdfActionRemoteGoTo, IsNSURL url) => pdfActionRemoteGoTo -> CULong -> NSPoint -> url -> IO (Id PDFActionRemoteGoTo)
initWithPageIndex_atPoint_fileURL pdfActionRemoteGoTo  pageIndex point url =
withObjCPtr url $ \raw_url ->
    sendMsg pdfActionRemoteGoTo (mkSelector "initWithPageIndex:atPoint:fileURL:") (retPtr retVoid) [argCULong (fromIntegral pageIndex), argNSPoint point, argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- pageIndex@
pageIndex :: IsPDFActionRemoteGoTo pdfActionRemoteGoTo => pdfActionRemoteGoTo -> IO CULong
pageIndex pdfActionRemoteGoTo  =
  sendMsg pdfActionRemoteGoTo (mkSelector "pageIndex") retCULong []

-- | @- setPageIndex:@
setPageIndex :: IsPDFActionRemoteGoTo pdfActionRemoteGoTo => pdfActionRemoteGoTo -> CULong -> IO ()
setPageIndex pdfActionRemoteGoTo  value =
  sendMsg pdfActionRemoteGoTo (mkSelector "setPageIndex:") retVoid [argCULong (fromIntegral value)]

-- | @- point@
point :: IsPDFActionRemoteGoTo pdfActionRemoteGoTo => pdfActionRemoteGoTo -> IO NSPoint
point pdfActionRemoteGoTo  =
  sendMsgStret pdfActionRemoteGoTo (mkSelector "point") retNSPoint []

-- | @- setPoint:@
setPoint :: IsPDFActionRemoteGoTo pdfActionRemoteGoTo => pdfActionRemoteGoTo -> NSPoint -> IO ()
setPoint pdfActionRemoteGoTo  value =
  sendMsg pdfActionRemoteGoTo (mkSelector "setPoint:") retVoid [argNSPoint value]

-- | @- URL@
url :: IsPDFActionRemoteGoTo pdfActionRemoteGoTo => pdfActionRemoteGoTo -> IO (Id NSURL)
url pdfActionRemoteGoTo  =
  sendMsg pdfActionRemoteGoTo (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setURL:@
setURL :: (IsPDFActionRemoteGoTo pdfActionRemoteGoTo, IsNSURL value) => pdfActionRemoteGoTo -> value -> IO ()
setURL pdfActionRemoteGoTo  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfActionRemoteGoTo (mkSelector "setURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPageIndex:atPoint:fileURL:@
initWithPageIndex_atPoint_fileURLSelector :: Selector
initWithPageIndex_atPoint_fileURLSelector = mkSelector "initWithPageIndex:atPoint:fileURL:"

-- | @Selector@ for @pageIndex@
pageIndexSelector :: Selector
pageIndexSelector = mkSelector "pageIndex"

-- | @Selector@ for @setPageIndex:@
setPageIndexSelector :: Selector
setPageIndexSelector = mkSelector "setPageIndex:"

-- | @Selector@ for @point@
pointSelector :: Selector
pointSelector = mkSelector "point"

-- | @Selector@ for @setPoint:@
setPointSelector :: Selector
setPointSelector = mkSelector "setPoint:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector
setURLSelector = mkSelector "setURL:"

