{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFilterBrowserFlow
--
-- The NEFilterBrowserFlow class declares the programmatic interface of an object that represents a flow of network data to be filtered, which is originated from NEFilterSource.
--
-- NEFilterBrowserFlow is part of NetworkExtension.framework
--
-- Generated bindings for @NEFilterBrowserFlow@.
module ObjC.NetworkExtension.NEFilterBrowserFlow
  ( NEFilterBrowserFlow
  , IsNEFilterBrowserFlow(..)
  , request
  , response
  , parentURL
  , requestSelector
  , responseSelector
  , parentURLSelector


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

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | request
--
-- The NSURLRequest of the flow. This property is always nil for the control providers.
--
-- ObjC selector: @- request@
request :: IsNEFilterBrowserFlow neFilterBrowserFlow => neFilterBrowserFlow -> IO (Id NSURLRequest)
request neFilterBrowserFlow  =
    sendMsg neFilterBrowserFlow (mkSelector "request") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | response
--
-- The NSURLResponse of the flow. This will be nil until the request is sent to the server and the response headers are received. And this property is always nil for the control providers.
--
-- ObjC selector: @- response@
response :: IsNEFilterBrowserFlow neFilterBrowserFlow => neFilterBrowserFlow -> IO (Id NSURLResponse)
response neFilterBrowserFlow  =
    sendMsg neFilterBrowserFlow (mkSelector "response") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | parentURL
--
-- The parent URL for the current flow which is created to load the sub frames because the flow with the parent URL was allowed. Will be nil if the parent flow does not exist.
--
-- ObjC selector: @- parentURL@
parentURL :: IsNEFilterBrowserFlow neFilterBrowserFlow => neFilterBrowserFlow -> IO (Id NSURL)
parentURL neFilterBrowserFlow  =
    sendMsg neFilterBrowserFlow (mkSelector "parentURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @request@
requestSelector :: Selector
requestSelector = mkSelector "request"

-- | @Selector@ for @response@
responseSelector :: Selector
responseSelector = mkSelector "response"

-- | @Selector@ for @parentURL@
parentURLSelector :: Selector
parentURLSelector = mkSelector "parentURL"

