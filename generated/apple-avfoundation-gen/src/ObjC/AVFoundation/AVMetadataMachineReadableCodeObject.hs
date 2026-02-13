{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMetadataMachineReadableCodeObject
--
-- AVMetadataMachineReadableCodeObject is a concrete subclass of AVMetadataObject defining the features of a detected one-dimensional or two-dimensional barcode.
--
-- AVMetadataMachineReadableCodeObject represents a single detected machine readable code in a picture. It is an immutable object describing the features and payload of a barcode.
--
-- On supported platforms, AVCaptureMetadataOutput outputs arrays of detected machine readable code objects. See AVCaptureMetadataOutput.h.
--
-- Generated bindings for @AVMetadataMachineReadableCodeObject@.
module ObjC.AVFoundation.AVMetadataMachineReadableCodeObject
  ( AVMetadataMachineReadableCodeObject
  , IsAVMetadataMachineReadableCodeObject(..)
  , corners
  , stringValue
  , descriptor
  , cornersSelector
  , descriptorSelector
  , stringValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | corners
--
-- The points defining the (X,Y) locations of the corners of the machine-readable code.
--
-- The value of this property is an NSArray of NSDictionaries, each of which has been created from a CGPoint using CGPointCreateDictionaryRepresentation(), representing the coordinates of the corners of the object with respect to the image in which it resides. If the metadata originates from video, the points may be expressed as scalar values from 0. - 1. The points in the corners differ from the bounds rectangle in that bounds is axis-aligned to orientation of the captured image, and the values of the corners reside within the bounds rectangle. The points are arranged in counter-clockwise order (clockwise if the code or image is mirrored), starting with the top-left of the code in its canonical orientation.
--
-- ObjC selector: @- corners@
corners :: IsAVMetadataMachineReadableCodeObject avMetadataMachineReadableCodeObject => avMetadataMachineReadableCodeObject -> IO (Id NSArray)
corners avMetadataMachineReadableCodeObject =
  sendMessage avMetadataMachineReadableCodeObject cornersSelector

-- | stringValue
--
-- Returns the receiver's errorCorrectedData decoded into a human-readable string.
--
-- The value of this property is an NSString created by decoding the binary payload according to the format of the machine readable code. Returns nil if a string representation cannot be created from the payload.
--
-- ObjC selector: @- stringValue@
stringValue :: IsAVMetadataMachineReadableCodeObject avMetadataMachineReadableCodeObject => avMetadataMachineReadableCodeObject -> IO (Id NSString)
stringValue avMetadataMachineReadableCodeObject =
  sendMessage avMetadataMachineReadableCodeObject stringValueSelector

-- | descriptor
--
-- An abstract representation of a machine readable code's symbol attributes.
--
-- The value may be nil if an abstract representation of a machine readable code object is not defined for the code type or could not be detected.
--
-- ObjC selector: @- descriptor@
descriptor :: IsAVMetadataMachineReadableCodeObject avMetadataMachineReadableCodeObject => avMetadataMachineReadableCodeObject -> IO (Id CIBarcodeDescriptor)
descriptor avMetadataMachineReadableCodeObject =
  sendMessage avMetadataMachineReadableCodeObject descriptorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @corners@
cornersSelector :: Selector '[] (Id NSArray)
cornersSelector = mkSelector "corners"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector '[] (Id NSString)
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id CIBarcodeDescriptor)
descriptorSelector = mkSelector "descriptor"

