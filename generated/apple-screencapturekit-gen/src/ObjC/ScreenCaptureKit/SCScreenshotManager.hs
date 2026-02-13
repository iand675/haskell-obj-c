{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SCScreenshotManager@.
module ObjC.ScreenCaptureKit.SCScreenshotManager
  ( SCScreenshotManager
  , IsSCScreenshotManager(..)
  , init_
  , captureSampleBufferWithFilter_configuration_completionHandler
  , captureImageWithFilter_configuration_completionHandler
  , captureScreenshotWithFilter_configuration_completionHandler
  , captureImageWithFilter_configuration_completionHandlerSelector
  , captureSampleBufferWithFilter_configuration_completionHandlerSelector
  , captureScreenshotWithFilter_configuration_completionHandlerSelector
  , initSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSCScreenshotManager scScreenshotManager => scScreenshotManager -> IO (Id SCScreenshotManager)
init_ scScreenshotManager =
  sendOwnedMessage scScreenshotManager initSelector

-- | captureSampleBufferWithFilter:configuration:completionHandler:
--
-- @contentFilter@ — is the filter containing the content to take a screenshot of
--
-- @config@ — is the stream configuration containing information on how to format the screenshot
--
-- @completionHandler@ — is the handler that will deliver the screenshot to the user
--
-- this method takes a screenshot using the filter and configuration passed in and returns it as a CMSampleBuffer
--
-- ObjC selector: @+ captureSampleBufferWithFilter:configuration:completionHandler:@
captureSampleBufferWithFilter_configuration_completionHandler :: (IsSCContentFilter contentFilter, IsSCStreamConfiguration config) => contentFilter -> config -> Ptr () -> IO ()
captureSampleBufferWithFilter_configuration_completionHandler contentFilter config completionHandler =
  do
    cls' <- getRequiredClass "SCScreenshotManager"
    sendClassMessage cls' captureSampleBufferWithFilter_configuration_completionHandlerSelector (toSCContentFilter contentFilter) (toSCStreamConfiguration config) completionHandler

-- | captureImageWithFilter:configuration:completionHandler:
--
-- @contentFilter@ — is the filter containing the content to take a screenshot of
--
-- @config@ — is the stream configuration containing information on how to format the screenshot
--
-- @completionHandler@ — is the handler that will deliver the screenshot to the user
--
-- this method takes a screenshot using the filter and configuration passed in and returns it as a CGImage in BGRA format if captureDynamicRange is SCCaptureDynamicRangeSDR, in RGhA format if captureDynamicRange is SCCaptureDynamicRangeHDRLocalDisplay/SCCaptureDynamicRangeHDRCanonicalDisplay
--
-- ObjC selector: @+ captureImageWithFilter:configuration:completionHandler:@
captureImageWithFilter_configuration_completionHandler :: (IsSCContentFilter contentFilter, IsSCStreamConfiguration config) => contentFilter -> config -> Ptr () -> IO ()
captureImageWithFilter_configuration_completionHandler contentFilter config completionHandler =
  do
    cls' <- getRequiredClass "SCScreenshotManager"
    sendClassMessage cls' captureImageWithFilter_configuration_completionHandlerSelector (toSCContentFilter contentFilter) (toSCStreamConfiguration config) completionHandler

-- | captureScreenshotWithFilter:configuration:completionHandler:
--
-- @contentFilter@ — is the filter containing the content to take a screenshot of
--
-- @config@ — is the screenshot configuration containing information on how to format the screenshot
--
-- @completionHandler@ — is the handler that will deliver the SCScreenshotOutput object to the client
--
-- this method returns an SCScreenshotOutput object containing CGImages of the screenshot requested by the client
--
-- ObjC selector: @+ captureScreenshotWithFilter:configuration:completionHandler:@
captureScreenshotWithFilter_configuration_completionHandler :: (IsSCContentFilter contentFilter, IsSCScreenshotConfiguration config) => contentFilter -> config -> Ptr () -> IO ()
captureScreenshotWithFilter_configuration_completionHandler contentFilter config completionHandler =
  do
    cls' <- getRequiredClass "SCScreenshotManager"
    sendClassMessage cls' captureScreenshotWithFilter_configuration_completionHandlerSelector (toSCContentFilter contentFilter) (toSCScreenshotConfiguration config) completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SCScreenshotManager)
initSelector = mkSelector "init"

-- | @Selector@ for @captureSampleBufferWithFilter:configuration:completionHandler:@
captureSampleBufferWithFilter_configuration_completionHandlerSelector :: Selector '[Id SCContentFilter, Id SCStreamConfiguration, Ptr ()] ()
captureSampleBufferWithFilter_configuration_completionHandlerSelector = mkSelector "captureSampleBufferWithFilter:configuration:completionHandler:"

-- | @Selector@ for @captureImageWithFilter:configuration:completionHandler:@
captureImageWithFilter_configuration_completionHandlerSelector :: Selector '[Id SCContentFilter, Id SCStreamConfiguration, Ptr ()] ()
captureImageWithFilter_configuration_completionHandlerSelector = mkSelector "captureImageWithFilter:configuration:completionHandler:"

-- | @Selector@ for @captureScreenshotWithFilter:configuration:completionHandler:@
captureScreenshotWithFilter_configuration_completionHandlerSelector :: Selector '[Id SCContentFilter, Id SCScreenshotConfiguration, Ptr ()] ()
captureScreenshotWithFilter_configuration_completionHandlerSelector = mkSelector "captureScreenshotWithFilter:configuration:completionHandler:"

