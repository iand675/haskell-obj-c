{-# LANGUAGE DataKinds #-}
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
  , symbolDescriptor
  , messageStringSelector
  , symbolDescriptorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The string decoded from the detected barcode.
--
-- ObjC selector: @- messageString@
messageString :: IsCIQRCodeFeature ciqrCodeFeature => ciqrCodeFeature -> IO (Id NSString)
messageString ciqrCodeFeature =
  sendMessage ciqrCodeFeature messageStringSelector

-- | An abstract representation of a QR Code symbol.
--
-- The property is a ``CIQRCodeDescriptor`` instance that contains the payload, symbol version,  mask pattern, and error correction level, so the QR Code can be reproduced.
--
-- ObjC selector: @- symbolDescriptor@
symbolDescriptor :: IsCIQRCodeFeature ciqrCodeFeature => ciqrCodeFeature -> IO RawId
symbolDescriptor ciqrCodeFeature =
  sendMessage ciqrCodeFeature symbolDescriptorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @messageString@
messageStringSelector :: Selector '[] (Id NSString)
messageStringSelector = mkSelector "messageString"

-- | @Selector@ for @symbolDescriptor@
symbolDescriptorSelector :: Selector '[] RawId
symbolDescriptorSelector = mkSelector "symbolDescriptor"

