{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents a web content filter
--
-- Generated bindings for @BEWebContentFilter@.
module ObjC.BrowserEngineKit.BEWebContentFilter
  ( BEWebContentFilter
  , IsBEWebContentFilter(..)
  , evaluateURL_completionHandler
  , allowURL_completionHandler
  , shouldEvaluateURLs
  , allowURL_completionHandlerSelector
  , evaluateURL_completionHandlerSelector
  , shouldEvaluateURLsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BrowserEngineKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Evaluates whether a URL should be blocked and if so, provides a UTF-8 encoded HTML representation of a blocking page.
--
-- @url@ — The URL to be evaluated.
--
-- @completionHandler@ — The completion block to be invoked with result when evaluation is complete. Result is YES if the url should be blocked, and NO if it isn't.
--
-- This method performs a lazy initialization of some objects, so the first call could take longer than subsequent calls.
--
-- ObjC selector: @- evaluateURL:completionHandler:@
evaluateURL_completionHandler :: (IsBEWebContentFilter beWebContentFilter, IsNSURL url) => beWebContentFilter -> url -> Ptr () -> IO ()
evaluateURL_completionHandler beWebContentFilter url completionHandler =
  sendMessage beWebContentFilter evaluateURL_completionHandlerSelector (toNSURL url) completionHandler

-- | Adds blocked URL to built-in web content filter's allowlist.
--
-- @url@ — The URL to be added.
--
-- @completionHandler@ — The completion block to be called when the add operation is complete, with result of the operation. Result is YES if the url is added successfully, and NO if it isn't.
--
-- ObjC selector: @- allowURL:completionHandler:@
allowURL_completionHandler :: (IsBEWebContentFilter beWebContentFilter, IsNSURL url) => beWebContentFilter -> url -> Ptr () -> IO ()
allowURL_completionHandler beWebContentFilter url completionHandler =
  sendMessage beWebContentFilter allowURL_completionHandlerSelector (toNSURL url) completionHandler

-- | Determines whether the built-in web content filter is active.
--
-- YES if the built-in web content filter is active, and NO if it isn't.
--
-- ObjC selector: @+ shouldEvaluateURLs@
shouldEvaluateURLs :: IO Bool
shouldEvaluateURLs  =
  do
    cls' <- getRequiredClass "BEWebContentFilter"
    sendClassMessage cls' shouldEvaluateURLsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @evaluateURL:completionHandler:@
evaluateURL_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] ()
evaluateURL_completionHandlerSelector = mkSelector "evaluateURL:completionHandler:"

-- | @Selector@ for @allowURL:completionHandler:@
allowURL_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] ()
allowURL_completionHandlerSelector = mkSelector "allowURL:completionHandler:"

-- | @Selector@ for @shouldEvaluateURLs@
shouldEvaluateURLsSelector :: Selector '[] Bool
shouldEvaluateURLsSelector = mkSelector "shouldEvaluateURLs"

