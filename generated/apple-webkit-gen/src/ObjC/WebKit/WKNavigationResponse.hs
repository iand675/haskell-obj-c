{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contains information about a navigation response, used for making policy decisions.
--
-- Generated bindings for @WKNavigationResponse@.
module ObjC.WebKit.WKNavigationResponse
  ( WKNavigationResponse
  , IsWKNavigationResponse(..)
  , forMainFrame
  , response
  , canShowMIMEType
  , canShowMIMETypeSelector
  , forMainFrameSelector
  , responseSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A Boolean value indicating whether the frame being navigated is the main frame.
--
-- ObjC selector: @- forMainFrame@
forMainFrame :: IsWKNavigationResponse wkNavigationResponse => wkNavigationResponse -> IO Bool
forMainFrame wkNavigationResponse =
  sendMessage wkNavigationResponse forMainFrameSelector

-- | The frame's response.
--
-- ObjC selector: @- response@
response :: IsWKNavigationResponse wkNavigationResponse => wkNavigationResponse -> IO (Id NSURLResponse)
response wkNavigationResponse =
  sendMessage wkNavigationResponse responseSelector

-- | A Boolean value indicating whether WebKit can display the response's MIME type natively.
--
-- Allowing a navigation response with a MIME type that can't be shown will cause the navigation to fail.
--
-- ObjC selector: @- canShowMIMEType@
canShowMIMEType :: IsWKNavigationResponse wkNavigationResponse => wkNavigationResponse -> IO Bool
canShowMIMEType wkNavigationResponse =
  sendMessage wkNavigationResponse canShowMIMETypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @forMainFrame@
forMainFrameSelector :: Selector '[] Bool
forMainFrameSelector = mkSelector "forMainFrame"

-- | @Selector@ for @response@
responseSelector :: Selector '[] (Id NSURLResponse)
responseSelector = mkSelector "response"

-- | @Selector@ for @canShowMIMEType@
canShowMIMETypeSelector :: Selector '[] Bool
canShowMIMETypeSelector = mkSelector "canShowMIMEType"

