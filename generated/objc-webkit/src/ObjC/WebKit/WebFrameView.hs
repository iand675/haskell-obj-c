{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | WebFrameView
--
-- Generated bindings for @WebFrameView@.
module ObjC.WebKit.WebFrameView
  ( WebFrameView
  , IsWebFrameView(..)
  , printOperationWithPrintInfo
  , printDocumentView
  , webFrame
  , documentView
  , allowsScrolling
  , setAllowsScrolling
  , canPrintHeadersAndFooters
  , documentViewShouldHandlePrint
  , printOperationWithPrintInfoSelector
  , printDocumentViewSelector
  , webFrameSelector
  , documentViewSelector
  , allowsScrollingSelector
  , setAllowsScrollingSelector
  , canPrintHeadersAndFootersSelector
  , documentViewShouldHandlePrintSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | printOperationWithPrintInfo
--
-- Creates a print operation set up to print this frame
--
-- Returns: A newly created print operation object
--
-- ObjC selector: @- printOperationWithPrintInfo:@
printOperationWithPrintInfo :: (IsWebFrameView webFrameView, IsNSPrintInfo printInfo) => webFrameView -> printInfo -> IO (Id NSPrintOperation)
printOperationWithPrintInfo webFrameView  printInfo =
withObjCPtr printInfo $ \raw_printInfo ->
    sendMsg webFrameView (mkSelector "printOperationWithPrintInfo:") (retPtr retVoid) [argPtr (castPtr raw_printInfo :: Ptr ())] >>= retainedObject . castPtr

-- | printDocumentView
--
-- Called by the host application when the WebFrameView returns YES from -documentViewShouldHandlePrint.
--
-- ObjC selector: @- printDocumentView@
printDocumentView :: IsWebFrameView webFrameView => webFrameView -> IO ()
printDocumentView webFrameView  =
  sendMsg webFrameView (mkSelector "printDocumentView") retVoid []

-- | webFrame
--
-- The WebFrame associated with this WebFrameView
--
-- ObjC selector: @- webFrame@
webFrame :: IsWebFrameView webFrameView => webFrameView -> IO (Id WebFrame)
webFrame webFrameView  =
  sendMsg webFrameView (mkSelector "webFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | documentView
--
-- The WebFrameView's document subview
--
-- The subview that renders the WebFrameView's contents
--
-- ObjC selector: @- documentView@
documentView :: IsWebFrameView webFrameView => webFrameView -> IO (Id NSView)
documentView webFrameView  =
  sendMsg webFrameView (mkSelector "documentView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | allowsScrolling
--
-- Whether the WebFrameView allows its document to be scrolled
--
-- ObjC selector: @- allowsScrolling@
allowsScrolling :: IsWebFrameView webFrameView => webFrameView -> IO Bool
allowsScrolling webFrameView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webFrameView (mkSelector "allowsScrolling") retCULong []

-- | allowsScrolling
--
-- Whether the WebFrameView allows its document to be scrolled
--
-- ObjC selector: @- setAllowsScrolling:@
setAllowsScrolling :: IsWebFrameView webFrameView => webFrameView -> Bool -> IO ()
setAllowsScrolling webFrameView  value =
  sendMsg webFrameView (mkSelector "setAllowsScrolling:") retVoid [argCULong (if value then 1 else 0)]

-- | canPrintHeadersAndFooters
--
-- Whether this frame can print headers and footers
--
-- ObjC selector: @- canPrintHeadersAndFooters@
canPrintHeadersAndFooters :: IsWebFrameView webFrameView => webFrameView -> IO Bool
canPrintHeadersAndFooters webFrameView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webFrameView (mkSelector "canPrintHeadersAndFooters") retCULong []

-- | documentViewShouldHandlePrint
--
-- Called by the host application before it initializes and runs a print operation.
--
-- If NO is returned, the host application will abort its print operation and call -printDocumentView on the    WebFrameView.  The document view is then expected to run its own print operation.  If YES is returned, the host     application's print operation will continue as normal.
--
-- ObjC selector: @- documentViewShouldHandlePrint@
documentViewShouldHandlePrint :: IsWebFrameView webFrameView => webFrameView -> IO Bool
documentViewShouldHandlePrint webFrameView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webFrameView (mkSelector "documentViewShouldHandlePrint") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @printOperationWithPrintInfo:@
printOperationWithPrintInfoSelector :: Selector
printOperationWithPrintInfoSelector = mkSelector "printOperationWithPrintInfo:"

-- | @Selector@ for @printDocumentView@
printDocumentViewSelector :: Selector
printDocumentViewSelector = mkSelector "printDocumentView"

-- | @Selector@ for @webFrame@
webFrameSelector :: Selector
webFrameSelector = mkSelector "webFrame"

-- | @Selector@ for @documentView@
documentViewSelector :: Selector
documentViewSelector = mkSelector "documentView"

-- | @Selector@ for @allowsScrolling@
allowsScrollingSelector :: Selector
allowsScrollingSelector = mkSelector "allowsScrolling"

-- | @Selector@ for @setAllowsScrolling:@
setAllowsScrollingSelector :: Selector
setAllowsScrollingSelector = mkSelector "setAllowsScrolling:"

-- | @Selector@ for @canPrintHeadersAndFooters@
canPrintHeadersAndFootersSelector :: Selector
canPrintHeadersAndFootersSelector = mkSelector "canPrintHeadersAndFooters"

-- | @Selector@ for @documentViewShouldHandlePrint@
documentViewShouldHandlePrintSelector :: Selector
documentViewShouldHandlePrintSelector = mkSelector "documentViewShouldHandlePrint"

