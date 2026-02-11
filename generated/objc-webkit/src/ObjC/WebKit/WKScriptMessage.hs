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
  , webViewSelector
  , frameInfoSelector
  , nameSelector
  , worldSelector


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

-- | The body of the message.
--
-- Allowed types are NSNumber, NSString, NSDate, NSArray, NSDictionary, and NSNull.
--
-- ObjC selector: @- body@
body :: IsWKScriptMessage wkScriptMessage => wkScriptMessage -> IO RawId
body wkScriptMessage  =
  fmap (RawId . castPtr) $ sendMsg wkScriptMessage (mkSelector "body") (retPtr retVoid) []

-- | The web view sending the message.
--
-- ObjC selector: @- webView@
webView :: IsWKScriptMessage wkScriptMessage => wkScriptMessage -> IO (Id WKWebView)
webView wkScriptMessage  =
  sendMsg wkScriptMessage (mkSelector "webView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The frame sending the message.
--
-- ObjC selector: @- frameInfo@
frameInfo :: IsWKScriptMessage wkScriptMessage => wkScriptMessage -> IO (Id WKFrameInfo)
frameInfo wkScriptMessage  =
  sendMsg wkScriptMessage (mkSelector "frameInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name of the message handler to which the message is sent.
--
-- ObjC selector: @- name@
name :: IsWKScriptMessage wkScriptMessage => wkScriptMessage -> IO (Id NSString)
name wkScriptMessage  =
  sendMsg wkScriptMessage (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The content world from which the message was sent.
--
-- ObjC selector: @- world@
world :: IsWKScriptMessage wkScriptMessage => wkScriptMessage -> IO (Id WKContentWorld)
world wkScriptMessage  =
  sendMsg wkScriptMessage (mkSelector "world") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @body@
bodySelector :: Selector
bodySelector = mkSelector "body"

-- | @Selector@ for @webView@
webViewSelector :: Selector
webViewSelector = mkSelector "webView"

-- | @Selector@ for @frameInfo@
frameInfoSelector :: Selector
frameInfoSelector = mkSelector "frameInfo"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @world@
worldSelector :: Selector
worldSelector = mkSelector "world"

