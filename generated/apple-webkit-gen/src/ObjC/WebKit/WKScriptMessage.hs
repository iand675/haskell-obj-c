{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A WKScriptMessage object contains information about a message sent from a webpage.
--
-- Generated bindings for @WKScriptMessage@.
module ObjC.WebKit.WKScriptMessage
  ( WKScriptMessage
  , IsWKScriptMessage(..)
  , body
  , webView
  , frameInfo
  , name
  , world
  , bodySelector
  , frameInfoSelector
  , nameSelector
  , webViewSelector
  , worldSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The body of the message.
--
-- Allowed types are NSNumber, NSString, NSDate, NSArray, NSDictionary, and NSNull.
--
-- ObjC selector: @- body@
body :: IsWKScriptMessage wkScriptMessage => wkScriptMessage -> IO RawId
body wkScriptMessage =
  sendMessage wkScriptMessage bodySelector

-- | The web view sending the message.
--
-- ObjC selector: @- webView@
webView :: IsWKScriptMessage wkScriptMessage => wkScriptMessage -> IO (Id WKWebView)
webView wkScriptMessage =
  sendMessage wkScriptMessage webViewSelector

-- | The frame sending the message.
--
-- ObjC selector: @- frameInfo@
frameInfo :: IsWKScriptMessage wkScriptMessage => wkScriptMessage -> IO (Id WKFrameInfo)
frameInfo wkScriptMessage =
  sendMessage wkScriptMessage frameInfoSelector

-- | The name of the message handler to which the message is sent.
--
-- ObjC selector: @- name@
name :: IsWKScriptMessage wkScriptMessage => wkScriptMessage -> IO (Id NSString)
name wkScriptMessage =
  sendMessage wkScriptMessage nameSelector

-- | The content world from which the message was sent.
--
-- ObjC selector: @- world@
world :: IsWKScriptMessage wkScriptMessage => wkScriptMessage -> IO (Id WKContentWorld)
world wkScriptMessage =
  sendMessage wkScriptMessage worldSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @body@
bodySelector :: Selector '[] RawId
bodySelector = mkSelector "body"

-- | @Selector@ for @webView@
webViewSelector :: Selector '[] (Id WKWebView)
webViewSelector = mkSelector "webView"

-- | @Selector@ for @frameInfo@
frameInfoSelector :: Selector '[] (Id WKFrameInfo)
frameInfoSelector = mkSelector "frameInfo"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @world@
worldSelector :: Selector '[] (Id WKContentWorld)
worldSelector = mkSelector "world"

