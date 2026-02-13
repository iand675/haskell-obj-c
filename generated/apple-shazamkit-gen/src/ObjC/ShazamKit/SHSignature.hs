{-# LANGUAGE DataKinds #-}
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
  , dataRepresentationSelector
  , durationSelector
  , initWithDataRepresentation_errorSelector
  , signatureWithDataRepresentation_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' signatureWithDataRepresentation_errorSelector (toNSData dataRepresentation) (toNSError error_)

-- | Creates a signature object from raw data.
--
-- - Parameters:   - dataRepresentation: The raw data for the signature.   - error: The error that occurs; otherwise, @nil@.
--
-- - Returns: A signature if the raw data is a valid signature; otherwise, @nil@.
--
-- ObjC selector: @- initWithDataRepresentation:error:@
initWithDataRepresentation_error :: (IsSHSignature shSignature, IsNSData dataRepresentation, IsNSError error_) => shSignature -> dataRepresentation -> error_ -> IO (Id SHSignature)
initWithDataRepresentation_error shSignature dataRepresentation error_ =
  sendOwnedMessage shSignature initWithDataRepresentation_errorSelector (toNSData dataRepresentation) (toNSError error_)

-- | The duration of the audio you use to generate the signature.
--
-- Audio that contains periods of silence may result in a duration value that's shorter than the full duration of the original audio track.
--
-- ObjC selector: @- duration@
duration :: IsSHSignature shSignature => shSignature -> IO CDouble
duration shSignature =
  sendMessage shSignature durationSelector

-- | The raw data for the signature.
--
-- ObjC selector: @- dataRepresentation@
dataRepresentation :: IsSHSignature shSignature => shSignature -> IO (Id NSData)
dataRepresentation shSignature =
  sendMessage shSignature dataRepresentationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @signatureWithDataRepresentation:error:@
signatureWithDataRepresentation_errorSelector :: Selector '[Id NSData, Id NSError] (Id SHSignature)
signatureWithDataRepresentation_errorSelector = mkSelector "signatureWithDataRepresentation:error:"

-- | @Selector@ for @initWithDataRepresentation:error:@
initWithDataRepresentation_errorSelector :: Selector '[Id NSData, Id NSError] (Id SHSignature)
initWithDataRepresentation_errorSelector = mkSelector "initWithDataRepresentation:error:"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector '[] (Id NSData)
dataRepresentationSelector = mkSelector "dataRepresentation"

