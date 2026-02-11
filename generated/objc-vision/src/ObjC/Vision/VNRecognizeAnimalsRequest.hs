{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request that will recognize various animals in an image. The list of animals supported by the recognition algorithm can be queried by  -supportedIdentifiersAndReturnError:
--
-- This request will generate VNRecognizedObjectObservation objects with a defined boundingBox, label and confidence level.
--
-- Generated bindings for @VNRecognizeAnimalsRequest@.
module ObjC.Vision.VNRecognizeAnimalsRequest
  ( VNRecognizeAnimalsRequest
  , IsVNRecognizeAnimalsRequest(..)
  , knownAnimalIdentifiersForRevision_error
  , supportedIdentifiersAndReturnError
  , results
  , knownAnimalIdentifiersForRevision_errorSelector
  , supportedIdentifiersAndReturnErrorSelector
  , resultsSelector


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

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | This class method returns a list of all animals supported by the recognition algorithm
--
-- This request will generate a collection of names for supported animals by current recognition algorithm.
--
-- ObjC selector: @+ knownAnimalIdentifiersForRevision:error:@
knownAnimalIdentifiersForRevision_error :: IsNSError error_ => CULong -> error_ -> IO (Id NSArray)
knownAnimalIdentifiersForRevision_error requestRevision error_ =
  do
    cls' <- getRequiredClass "VNRecognizeAnimalsRequest"
    withObjCPtr error_ $ \raw_error_ ->
      sendClassMsg cls' (mkSelector "knownAnimalIdentifiersForRevision:error:") (retPtr retVoid) [argCULong (fromIntegral requestRevision), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Obtain the collection of identifiers supported by the target request.
--
-- This method will return the collection of all possible classification identifiers that are produced by the target request based on its current state of configuration at the time of the call.
--
-- @error@ — The address of the variable that will be populated with the error if the call fails.
--
-- Returns: The collection of classification identifiers, or nil if a failure occurs.
--
-- ObjC selector: @- supportedIdentifiersAndReturnError:@
supportedIdentifiersAndReturnError :: (IsVNRecognizeAnimalsRequest vnRecognizeAnimalsRequest, IsNSError error_) => vnRecognizeAnimalsRequest -> error_ -> IO (Id NSArray)
supportedIdentifiersAndReturnError vnRecognizeAnimalsRequest  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg vnRecognizeAnimalsRequest (mkSelector "supportedIdentifiersAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | VNRecognizedObjectObservation results.
--
-- ObjC selector: @- results@
results :: IsVNRecognizeAnimalsRequest vnRecognizeAnimalsRequest => vnRecognizeAnimalsRequest -> IO (Id NSArray)
results vnRecognizeAnimalsRequest  =
  sendMsg vnRecognizeAnimalsRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @knownAnimalIdentifiersForRevision:error:@
knownAnimalIdentifiersForRevision_errorSelector :: Selector
knownAnimalIdentifiersForRevision_errorSelector = mkSelector "knownAnimalIdentifiersForRevision:error:"

-- | @Selector@ for @supportedIdentifiersAndReturnError:@
supportedIdentifiersAndReturnErrorSelector :: Selector
supportedIdentifiersAndReturnErrorSelector = mkSelector "supportedIdentifiersAndReturnError:"

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

