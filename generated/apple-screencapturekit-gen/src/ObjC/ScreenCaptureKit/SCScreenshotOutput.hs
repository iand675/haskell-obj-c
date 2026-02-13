{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SCScreenshotOutput@.
module ObjC.ScreenCaptureKit.SCScreenshotOutput
  ( SCScreenshotOutput
  , IsSCScreenshotOutput(..)
  , sdrImage
  , setSdrImage
  , hdrImage
  , setHdrImage
  , fileURL
  , setFileURL
  , fileURLSelector
  , hdrImageSelector
  , sdrImageSelector
  , setFileURLSelector
  , setHdrImageSelector
  , setSdrImageSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | SCScreenshotOutput property that denotes the SDR CGimage.  The output CGImage uses the same color space as the display
--
-- ObjC selector: @- sdrImage@
sdrImage :: IsSCScreenshotOutput scScreenshotOutput => scScreenshotOutput -> IO (Ptr ())
sdrImage scScreenshotOutput =
  sendMessage scScreenshotOutput sdrImageSelector

-- | SCScreenshotOutput property that denotes the SDR CGimage.  The output CGImage uses the same color space as the display
--
-- ObjC selector: @- setSdrImage:@
setSdrImage :: IsSCScreenshotOutput scScreenshotOutput => scScreenshotOutput -> Ptr () -> IO ()
setSdrImage scScreenshotOutput value =
  sendMessage scScreenshotOutput setSdrImageSelector value

-- | SCScreenshotOutput property that denotes the HDR CGimage.  The output CGImage uses the extended sRGB color space.
--
-- ObjC selector: @- hdrImage@
hdrImage :: IsSCScreenshotOutput scScreenshotOutput => scScreenshotOutput -> IO (Ptr ())
hdrImage scScreenshotOutput =
  sendMessage scScreenshotOutput hdrImageSelector

-- | SCScreenshotOutput property that denotes the HDR CGimage.  The output CGImage uses the extended sRGB color space.
--
-- ObjC selector: @- setHdrImage:@
setHdrImage :: IsSCScreenshotOutput scScreenshotOutput => scScreenshotOutput -> Ptr () -> IO ()
setHdrImage scScreenshotOutput value =
  sendMessage scScreenshotOutput setHdrImageSelector value

-- | SCScreenshotOutput property to specify the location where the image was saved.  If a fileURL in the screenshot configuration was not specified, then the fileURL will be nil
--
-- ObjC selector: @- fileURL@
fileURL :: IsSCScreenshotOutput scScreenshotOutput => scScreenshotOutput -> IO (Id NSURL)
fileURL scScreenshotOutput =
  sendMessage scScreenshotOutput fileURLSelector

-- | SCScreenshotOutput property to specify the location where the image was saved.  If a fileURL in the screenshot configuration was not specified, then the fileURL will be nil
--
-- ObjC selector: @- setFileURL:@
setFileURL :: (IsSCScreenshotOutput scScreenshotOutput, IsNSURL value) => scScreenshotOutput -> value -> IO ()
setFileURL scScreenshotOutput value =
  sendMessage scScreenshotOutput setFileURLSelector (toNSURL value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sdrImage@
sdrImageSelector :: Selector '[] (Ptr ())
sdrImageSelector = mkSelector "sdrImage"

-- | @Selector@ for @setSdrImage:@
setSdrImageSelector :: Selector '[Ptr ()] ()
setSdrImageSelector = mkSelector "setSdrImage:"

-- | @Selector@ for @hdrImage@
hdrImageSelector :: Selector '[] (Ptr ())
hdrImageSelector = mkSelector "hdrImage"

-- | @Selector@ for @setHdrImage:@
setHdrImageSelector :: Selector '[Ptr ()] ()
setHdrImageSelector = mkSelector "setHdrImage:"

-- | @Selector@ for @fileURL@
fileURLSelector :: Selector '[] (Id NSURL)
fileURLSelector = mkSelector "fileURL"

-- | @Selector@ for @setFileURL:@
setFileURLSelector :: Selector '[Id NSURL] ()
setFileURLSelector = mkSelector "setFileURL:"

