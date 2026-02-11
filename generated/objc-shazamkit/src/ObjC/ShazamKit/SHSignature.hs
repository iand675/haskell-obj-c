{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains the opaque data and other information for a signature.
--
-- Save your signature to a file and share it with others by writing the data to a file. You can use the saved signatures of reference recordings to populate a custom catalog.
--
-- Check whether your captured query signature is long enough to search for a match by comparing ``duration`` to the ``SHCatalog/minimumQuerySignatureDuration`` and ``SHCatalog/maximumQuerySignatureDuration`` of a catalog.
--
-- Generated bindings for @SHSignature@.
module ObjC.ShazamKit.SHSignature
  ( SHSignature
  , IsSHSignature(..)
  , signatureWithDataRepresentation_error
  , initWithDataRepresentation_error
  , duration
  , dataRepresentation
  , signatureWithDataRepresentation_errorSelector
  , initWithDataRepresentation_errorSelector
  , durationSelector
  , dataRepresentationSelector


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

import ObjC.ShazamKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a signature object from raw data.
--
-- - Parameters:   - dataRepresentation: The raw data for the signature.   - error: The error that occurs; otherwise, @nil@.
--
-- - Returns: A signature if the raw data is a valid signature; otherwise, @nil@.
--
-- ObjC selector: @+ signatureWithDataRepresentation:error:@
signatureWithDataRepresentation_error :: (IsNSData dataRepresentation, IsNSError error_) => dataRepresentation -> error_ -> IO (Id SHSignature)
signatureWithDataRepresentation_error dataRepresentation error_ =
  do
    cls' <- getRequiredClass "SHSignature"
    withObjCPtr dataRepresentation $ \raw_dataRepresentation ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "signatureWithDataRepresentation:error:") (retPtr retVoid) [argPtr (castPtr raw_dataRepresentation :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a signature object from raw data.
--
-- - Parameters:   - dataRepresentation: The raw data for the signature.   - error: The error that occurs; otherwise, @nil@.
--
-- - Returns: A signature if the raw data is a valid signature; otherwise, @nil@.
--
-- ObjC selector: @- initWithDataRepresentation:error:@
initWithDataRepresentation_error :: (IsSHSignature shSignature, IsNSData dataRepresentation, IsNSError error_) => shSignature -> dataRepresentation -> error_ -> IO (Id SHSignature)
initWithDataRepresentation_error shSignature  dataRepresentation error_ =
withObjCPtr dataRepresentation $ \raw_dataRepresentation ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg shSignature (mkSelector "initWithDataRepresentation:error:") (retPtr retVoid) [argPtr (castPtr raw_dataRepresentation :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | The duration of the audio you use to generate the signature.
--
-- Audio that contains periods of silence may result in a duration value that's shorter than the full duration of the original audio track.
--
-- ObjC selector: @- duration@
duration :: IsSHSignature shSignature => shSignature -> IO CDouble
duration shSignature  =
  sendMsg shSignature (mkSelector "duration") retCDouble []

-- | The raw data for the signature.
--
-- ObjC selector: @- dataRepresentation@
dataRepresentation :: IsSHSignature shSignature => shSignature -> IO (Id NSData)
dataRepresentation shSignature  =
  sendMsg shSignature (mkSelector "dataRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @signatureWithDataRepresentation:error:@
signatureWithDataRepresentation_errorSelector :: Selector
signatureWithDataRepresentation_errorSelector = mkSelector "signatureWithDataRepresentation:error:"

-- | @Selector@ for @initWithDataRepresentation:error:@
initWithDataRepresentation_errorSelector :: Selector
initWithDataRepresentation_errorSelector = mkSelector "initWithDataRepresentation:error:"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector
dataRepresentationSelector = mkSelector "dataRepresentation"

