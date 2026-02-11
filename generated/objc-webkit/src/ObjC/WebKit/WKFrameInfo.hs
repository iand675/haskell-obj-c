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
import ObjC.Foundation.Internal.Classes

-- | A Boolean value indicating whether the frame is the main frame or a subframe.
--
-- ObjC selector: @- mainFrame@
mainFrame :: IsWKFrameInfo wkFrameInfo => wkFrameInfo -> IO Bool
mainFrame wkFrameInfo  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkFrameInfo (mkSelector "mainFrame") retCULong []

-- | The frame's current request.
--
-- ObjC selector: @- request@
request :: IsWKFrameInfo wkFrameInfo => wkFrameInfo -> IO (Id NSURLRequest)
request wkFrameInfo  =
  sendMsg wkFrameInfo (mkSelector "request") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The frame's current security origin.
--
-- ObjC selector: @- securityOrigin@
securityOrigin :: IsWKFrameInfo wkFrameInfo => wkFrameInfo -> IO (Id WKSecurityOrigin)
securityOrigin wkFrameInfo  =
  sendMsg wkFrameInfo (mkSelector "securityOrigin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The web view of the webpage that contains this frame.
--
-- ObjC selector: @- webView@
webView :: IsWKFrameInfo wkFrameInfo => wkFrameInfo -> IO (Id WKWebView)
webView wkFrameInfo  =
  sendMsg wkFrameInfo (mkSelector "webView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mainFrame@
mainFrameSelector :: Selector
mainFrameSelector = mkSelector "mainFrame"

-- | @Selector@ for @request@
requestSelector :: Selector
requestSelector = mkSelector "request"

-- | @Selector@ for @securityOrigin@
securityOriginSelector :: Selector
securityOriginSelector = mkSelector "securityOrigin"

-- | @Selector@ for @webView@
webViewSelector :: Selector
webViewSelector = mkSelector "webView"

