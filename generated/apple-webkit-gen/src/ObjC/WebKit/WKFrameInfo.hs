{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A WKFrameInfo object contains information about a frame on a webpage.
--
-- An instance of this class is a transient, data-only object; it does not uniquely identify a frame across multiple delegate method calls.
--
-- Generated bindings for @WKFrameInfo@.
module ObjC.WebKit.WKFrameInfo
  ( WKFrameInfo
  , IsWKFrameInfo(..)
  , mainFrame
  , request
  , securityOrigin
  , webView
  , mainFrameSelector
  , requestSelector
  , securityOriginSelector
  , webViewSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A Boolean value indicating whether the frame is the main frame or a subframe.
--
-- ObjC selector: @- mainFrame@
mainFrame :: IsWKFrameInfo wkFrameInfo => wkFrameInfo -> IO Bool
mainFrame wkFrameInfo =
  sendMessage wkFrameInfo mainFrameSelector

-- | The frame's current request.
--
-- ObjC selector: @- request@
request :: IsWKFrameInfo wkFrameInfo => wkFrameInfo -> IO (Id NSURLRequest)
request wkFrameInfo =
  sendMessage wkFrameInfo requestSelector

-- | The frame's current security origin.
--
-- ObjC selector: @- securityOrigin@
securityOrigin :: IsWKFrameInfo wkFrameInfo => wkFrameInfo -> IO (Id WKSecurityOrigin)
securityOrigin wkFrameInfo =
  sendMessage wkFrameInfo securityOriginSelector

-- | The web view of the webpage that contains this frame.
--
-- ObjC selector: @- webView@
webView :: IsWKFrameInfo wkFrameInfo => wkFrameInfo -> IO (Id WKWebView)
webView wkFrameInfo =
  sendMessage wkFrameInfo webViewSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mainFrame@
mainFrameSelector :: Selector '[] Bool
mainFrameSelector = mkSelector "mainFrame"

-- | @Selector@ for @request@
requestSelector :: Selector '[] (Id NSURLRequest)
requestSelector = mkSelector "request"

-- | @Selector@ for @securityOrigin@
securityOriginSelector :: Selector '[] (Id WKSecurityOrigin)
securityOriginSelector = mkSelector "securityOrigin"

-- | @Selector@ for @webView@
webViewSelector :: Selector '[] (Id WKWebView)
webViewSelector = mkSelector "webView"

