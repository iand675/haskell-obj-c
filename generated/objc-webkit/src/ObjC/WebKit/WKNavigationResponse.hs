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
  , forMainFrameSelector
  , responseSelector
  , canShowMIMETypeSelector


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

-- | A Boolean value indicating whether the frame being navigated is the main frame.
--
-- ObjC selector: @- forMainFrame@
forMainFrame :: IsWKNavigationResponse wkNavigationResponse => wkNavigationResponse -> IO Bool
forMainFrame wkNavigationResponse  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkNavigationResponse (mkSelector "forMainFrame") retCULong []

-- | The frame's response.
--
-- ObjC selector: @- response@
response :: IsWKNavigationResponse wkNavigationResponse => wkNavigationResponse -> IO (Id NSURLResponse)
response wkNavigationResponse  =
  sendMsg wkNavigationResponse (mkSelector "response") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A Boolean value indicating whether WebKit can display the response's MIME type natively.
--
-- Allowing a navigation response with a MIME type that can't be shown will cause the navigation to fail.
--
-- ObjC selector: @- canShowMIMEType@
canShowMIMEType :: IsWKNavigationResponse wkNavigationResponse => wkNavigationResponse -> IO Bool
canShowMIMEType wkNavigationResponse  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkNavigationResponse (mkSelector "canShowMIMEType") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @forMainFrame@
forMainFrameSelector :: Selector
forMainFrameSelector = mkSelector "forMainFrame"

-- | @Selector@ for @response@
responseSelector :: Selector
responseSelector = mkSelector "response"

-- | @Selector@ for @canShowMIMEType@
canShowMIMETypeSelector :: Selector
canShowMIMETypeSelector = mkSelector "canShowMIMEType"

