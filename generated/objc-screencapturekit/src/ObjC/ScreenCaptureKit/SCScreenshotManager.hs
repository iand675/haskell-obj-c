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
  , initSelector
  , captureSampleBufferWithFilter_configuration_completionHandlerSelector
  , captureImageWithFilter_configuration_completionHandlerSelector
  , captureScreenshotWithFilter_configuration_completionHandlerSelector


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

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSCScreenshotManager scScreenshotManager => scScreenshotManager -> IO (Id SCScreenshotManager)
init_ scScreenshotManager  =
  sendMsg scScreenshotManager (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    withObjCPtr contentFilter $ \raw_contentFilter ->
      withObjCPtr config $ \raw_config ->
        sendClassMsg cls' (mkSelector "captureSampleBufferWithFilter:configuration:completionHandler:") retVoid [argPtr (castPtr raw_contentFilter :: Ptr ()), argPtr (castPtr raw_config :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

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
    withObjCPtr contentFilter $ \raw_contentFilter ->
      withObjCPtr config $ \raw_config ->
        sendClassMsg cls' (mkSelector "captureImageWithFilter:configuration:completionHandler:") retVoid [argPtr (castPtr raw_contentFilter :: Ptr ()), argPtr (castPtr raw_config :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

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
    withObjCPtr contentFilter $ \raw_contentFilter ->
      withObjCPtr config $ \raw_config ->
        sendClassMsg cls' (mkSelector "captureScreenshotWithFilter:configuration:completionHandler:") retVoid [argPtr (castPtr raw_contentFilter :: Ptr ()), argPtr (castPtr raw_config :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @captureSampleBufferWithFilter:configuration:completionHandler:@
captureSampleBufferWithFilter_configuration_completionHandlerSelector :: Selector
captureSampleBufferWithFilter_configuration_completionHandlerSelector = mkSelector "captureSampleBufferWithFilter:configuration:completionHandler:"

-- | @Selector@ for @captureImageWithFilter:configuration:completionHandler:@
captureImageWithFilter_configuration_completionHandlerSelector :: Selector
captureImageWithFilter_configuration_completionHandlerSelector = mkSelector "captureImageWithFilter:configuration:completionHandler:"

-- | @Selector@ for @captureScreenshotWithFilter:configuration:completionHandler:@
captureScreenshotWithFilter_configuration_completionHandlerSelector :: Selector
captureScreenshotWithFilter_configuration_completionHandlerSelector = mkSelector "captureScreenshotWithFilter:configuration:completionHandler:"

