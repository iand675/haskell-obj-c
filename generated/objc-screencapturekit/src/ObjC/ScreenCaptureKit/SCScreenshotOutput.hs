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
  , sdrImageSelector
  , setSdrImageSelector
  , hdrImageSelector
  , setHdrImageSelector
  , fileURLSelector
  , setFileURLSelector


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

-- | SCScreenshotOutput property that denotes the SDR CGimage.  The output CGImage uses the same color space as the display
--
-- ObjC selector: @- sdrImage@
sdrImage :: IsSCScreenshotOutput scScreenshotOutput => scScreenshotOutput -> IO (Ptr ())
sdrImage scScreenshotOutput  =
  fmap castPtr $ sendMsg scScreenshotOutput (mkSelector "sdrImage") (retPtr retVoid) []

-- | SCScreenshotOutput property that denotes the SDR CGimage.  The output CGImage uses the same color space as the display
--
-- ObjC selector: @- setSdrImage:@
setSdrImage :: IsSCScreenshotOutput scScreenshotOutput => scScreenshotOutput -> Ptr () -> IO ()
setSdrImage scScreenshotOutput  value =
  sendMsg scScreenshotOutput (mkSelector "setSdrImage:") retVoid [argPtr value]

-- | SCScreenshotOutput property that denotes the HDR CGimage.  The output CGImage uses the extended sRGB color space.
--
-- ObjC selector: @- hdrImage@
hdrImage :: IsSCScreenshotOutput scScreenshotOutput => scScreenshotOutput -> IO (Ptr ())
hdrImage scScreenshotOutput  =
  fmap castPtr $ sendMsg scScreenshotOutput (mkSelector "hdrImage") (retPtr retVoid) []

-- | SCScreenshotOutput property that denotes the HDR CGimage.  The output CGImage uses the extended sRGB color space.
--
-- ObjC selector: @- setHdrImage:@
setHdrImage :: IsSCScreenshotOutput scScreenshotOutput => scScreenshotOutput -> Ptr () -> IO ()
setHdrImage scScreenshotOutput  value =
  sendMsg scScreenshotOutput (mkSelector "setHdrImage:") retVoid [argPtr value]

-- | SCScreenshotOutput property to specify the location where the image was saved.  If a fileURL in the screenshot configuration was not specified, then the fileURL will be nil
--
-- ObjC selector: @- fileURL@
fileURL :: IsSCScreenshotOutput scScreenshotOutput => scScreenshotOutput -> IO (Id NSURL)
fileURL scScreenshotOutput  =
  sendMsg scScreenshotOutput (mkSelector "fileURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | SCScreenshotOutput property to specify the location where the image was saved.  If a fileURL in the screenshot configuration was not specified, then the fileURL will be nil
--
-- ObjC selector: @- setFileURL:@
setFileURL :: (IsSCScreenshotOutput scScreenshotOutput, IsNSURL value) => scScreenshotOutput -> value -> IO ()
setFileURL scScreenshotOutput  value =
withObjCPtr value $ \raw_value ->
    sendMsg scScreenshotOutput (mkSelector "setFileURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sdrImage@
sdrImageSelector :: Selector
sdrImageSelector = mkSelector "sdrImage"

-- | @Selector@ for @setSdrImage:@
setSdrImageSelector :: Selector
setSdrImageSelector = mkSelector "setSdrImage:"

-- | @Selector@ for @hdrImage@
hdrImageSelector :: Selector
hdrImageSelector = mkSelector "hdrImage"

-- | @Selector@ for @setHdrImage:@
setHdrImageSelector :: Selector
setHdrImageSelector = mkSelector "setHdrImage:"

-- | @Selector@ for @fileURL@
fileURLSelector :: Selector
fileURLSelector = mkSelector "fileURL"

-- | @Selector@ for @setFileURL:@
setFileURLSelector :: Selector
setFileURLSelector = mkSelector "setFileURL:"

