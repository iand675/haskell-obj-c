{-# LANGUAGE DataKinds #-}
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
  , pointSelector
  , setPageIndexSelector
  , setPointSelector
  , setURLSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PDFKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- initWithPageIndex:atPoint:fileURL:@
initWithPageIndex_atPoint_fileURL :: (IsPDFActionRemoteGoTo pdfActionRemoteGoTo, IsNSURL url) => pdfActionRemoteGoTo -> CULong -> NSPoint -> url -> IO (Id PDFActionRemoteGoTo)
initWithPageIndex_atPoint_fileURL pdfActionRemoteGoTo pageIndex point url =
  sendOwnedMessage pdfActionRemoteGoTo initWithPageIndex_atPoint_fileURLSelector pageIndex point (toNSURL url)

-- | @- pageIndex@
pageIndex :: IsPDFActionRemoteGoTo pdfActionRemoteGoTo => pdfActionRemoteGoTo -> IO CULong
pageIndex pdfActionRemoteGoTo =
  sendMessage pdfActionRemoteGoTo pageIndexSelector

-- | @- setPageIndex:@
setPageIndex :: IsPDFActionRemoteGoTo pdfActionRemoteGoTo => pdfActionRemoteGoTo -> CULong -> IO ()
setPageIndex pdfActionRemoteGoTo value =
  sendMessage pdfActionRemoteGoTo setPageIndexSelector value

-- | @- point@
point :: IsPDFActionRemoteGoTo pdfActionRemoteGoTo => pdfActionRemoteGoTo -> IO NSPoint
point pdfActionRemoteGoTo =
  sendMessage pdfActionRemoteGoTo pointSelector

-- | @- setPoint:@
setPoint :: IsPDFActionRemoteGoTo pdfActionRemoteGoTo => pdfActionRemoteGoTo -> NSPoint -> IO ()
setPoint pdfActionRemoteGoTo value =
  sendMessage pdfActionRemoteGoTo setPointSelector value

-- | @- URL@
url :: IsPDFActionRemoteGoTo pdfActionRemoteGoTo => pdfActionRemoteGoTo -> IO (Id NSURL)
url pdfActionRemoteGoTo =
  sendMessage pdfActionRemoteGoTo urlSelector

-- | @- setURL:@
setURL :: (IsPDFActionRemoteGoTo pdfActionRemoteGoTo, IsNSURL value) => pdfActionRemoteGoTo -> value -> IO ()
setURL pdfActionRemoteGoTo value =
  sendMessage pdfActionRemoteGoTo setURLSelector (toNSURL value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPageIndex:atPoint:fileURL:@
initWithPageIndex_atPoint_fileURLSelector :: Selector '[CULong, NSPoint, Id NSURL] (Id PDFActionRemoteGoTo)
initWithPageIndex_atPoint_fileURLSelector = mkSelector "initWithPageIndex:atPoint:fileURL:"

-- | @Selector@ for @pageIndex@
pageIndexSelector :: Selector '[] CULong
pageIndexSelector = mkSelector "pageIndex"

-- | @Selector@ for @setPageIndex:@
setPageIndexSelector :: Selector '[CULong] ()
setPageIndexSelector = mkSelector "setPageIndex:"

-- | @Selector@ for @point@
pointSelector :: Selector '[] NSPoint
pointSelector = mkSelector "point"

-- | @Selector@ for @setPoint:@
setPointSelector :: Selector '[NSPoint] ()
setPointSelector = mkSelector "setPoint:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector '[Id NSURL] ()
setURLSelector = mkSelector "setURL:"

