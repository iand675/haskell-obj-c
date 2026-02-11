{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Information about a Quick Response code detected in a still or video image.
--
-- > Note: In macOS 10.13, iOS 11, and tvOS 11 or later, the Vision framework replaces these classes  for identifying and analyzing image features.  See <doc://com.apple.documentation/documentation/vision/vndetectbarcodesrequest>)
--
-- A QR code is a two-dimensional barcode using the ISO/IEC 18004:2006 standard. The properties of  a CIQRCodeFeature object identify the corners of the barcode in the image perspective and provide  the decoded message.
--
-- To detect QR codes in an image or video, choose ``CIDetectorTypeQRCode`` type when initializing a ``CIDetector`` object.
--
-- Generated bindings for @CIQRCodeFeature@.
module ObjC.CoreImage.CIQRCodeFeature
  ( CIQRCodeFeature
  , IsCIQRCodeFeature(..)
  , messageString
  , messageStringSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The string decoded from the detected barcode.
--
-- ObjC selector: @- messageString@
messageString :: IsCIQRCodeFeature ciqrCodeFeature => ciqrCodeFeature -> IO (Id NSString)
messageString ciqrCodeFeature  =
  sendMsg ciqrCodeFeature (mkSelector "messageString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @messageString@
messageStringSelector :: Selector
messageStringSelector = mkSelector "messageString"

