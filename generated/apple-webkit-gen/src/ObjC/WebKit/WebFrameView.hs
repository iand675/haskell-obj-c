{-# LANGUAGE DataKinds #-}
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
  , allowsScrollingSelector
  , canPrintHeadersAndFootersSelector
  , documentViewSelector
  , documentViewShouldHandlePrintSelector
  , printDocumentViewSelector
  , printOperationWithPrintInfoSelector
  , setAllowsScrollingSelector
  , webFrameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
printOperationWithPrintInfo webFrameView printInfo =
  sendMessage webFrameView printOperationWithPrintInfoSelector (toNSPrintInfo printInfo)

-- | printDocumentView
--
-- Called by the host application when the WebFrameView returns YES from -documentViewShouldHandlePrint.
--
-- ObjC selector: @- printDocumentView@
printDocumentView :: IsWebFrameView webFrameView => webFrameView -> IO ()
printDocumentView webFrameView =
  sendMessage webFrameView printDocumentViewSelector

-- | webFrame
--
-- The WebFrame associated with this WebFrameView
--
-- ObjC selector: @- webFrame@
webFrame :: IsWebFrameView webFrameView => webFrameView -> IO (Id WebFrame)
webFrame webFrameView =
  sendMessage webFrameView webFrameSelector

-- | documentView
--
-- The WebFrameView's document subview
--
-- The subview that renders the WebFrameView's contents
--
-- ObjC selector: @- documentView@
documentView :: IsWebFrameView webFrameView => webFrameView -> IO (Id NSView)
documentView webFrameView =
  sendMessage webFrameView documentViewSelector

-- | allowsScrolling
--
-- Whether the WebFrameView allows its document to be scrolled
--
-- ObjC selector: @- allowsScrolling@
allowsScrolling :: IsWebFrameView webFrameView => webFrameView -> IO Bool
allowsScrolling webFrameView =
  sendMessage webFrameView allowsScrollingSelector

-- | allowsScrolling
--
-- Whether the WebFrameView allows its document to be scrolled
--
-- ObjC selector: @- setAllowsScrolling:@
setAllowsScrolling :: IsWebFrameView webFrameView => webFrameView -> Bool -> IO ()
setAllowsScrolling webFrameView value =
  sendMessage webFrameView setAllowsScrollingSelector value

-- | canPrintHeadersAndFooters
--
-- Whether this frame can print headers and footers
--
-- ObjC selector: @- canPrintHeadersAndFooters@
canPrintHeadersAndFooters :: IsWebFrameView webFrameView => webFrameView -> IO Bool
canPrintHeadersAndFooters webFrameView =
  sendMessage webFrameView canPrintHeadersAndFootersSelector

-- | documentViewShouldHandlePrint
--
-- Called by the host application before it initializes and runs a print operation.
--
-- If NO is returned, the host application will abort its print operation and call -printDocumentView on the    WebFrameView.  The document view is then expected to run its own print operation.  If YES is returned, the host     application's print operation will continue as normal.
--
-- ObjC selector: @- documentViewShouldHandlePrint@
documentViewShouldHandlePrint :: IsWebFrameView webFrameView => webFrameView -> IO Bool
documentViewShouldHandlePrint webFrameView =
  sendMessage webFrameView documentViewShouldHandlePrintSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @printOperationWithPrintInfo:@
printOperationWithPrintInfoSelector :: Selector '[Id NSPrintInfo] (Id NSPrintOperation)
printOperationWithPrintInfoSelector = mkSelector "printOperationWithPrintInfo:"

-- | @Selector@ for @printDocumentView@
printDocumentViewSelector :: Selector '[] ()
printDocumentViewSelector = mkSelector "printDocumentView"

-- | @Selector@ for @webFrame@
webFrameSelector :: Selector '[] (Id WebFrame)
webFrameSelector = mkSelector "webFrame"

-- | @Selector@ for @documentView@
documentViewSelector :: Selector '[] (Id NSView)
documentViewSelector = mkSelector "documentView"

-- | @Selector@ for @allowsScrolling@
allowsScrollingSelector :: Selector '[] Bool
allowsScrollingSelector = mkSelector "allowsScrolling"

-- | @Selector@ for @setAllowsScrolling:@
setAllowsScrollingSelector :: Selector '[Bool] ()
setAllowsScrollingSelector = mkSelector "setAllowsScrolling:"

-- | @Selector@ for @canPrintHeadersAndFooters@
canPrintHeadersAndFootersSelector :: Selector '[] Bool
canPrintHeadersAndFootersSelector = mkSelector "canPrintHeadersAndFooters"

-- | @Selector@ for @documentViewShouldHandlePrint@
documentViewShouldHandlePrintSelector :: Selector '[] Bool
documentViewShouldHandlePrintSelector = mkSelector "documentViewShouldHandlePrint"

