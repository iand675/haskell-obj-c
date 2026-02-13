{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request for classifying an image.
--
-- This request will produce a collection of VNClassificationObservation objects which describe an image.
--
-- Generated bindings for @VNClassifyImageRequest@.
module ObjC.Vision.VNClassifyImageRequest
  ( VNClassifyImageRequest
  , IsVNClassifyImageRequest(..)
  , knownClassificationsForRevision_error
  , supportedIdentifiersAndReturnError
  , results
  , knownClassificationsForRevision_errorSelector
  , resultsSelector
  , supportedIdentifiersAndReturnErrorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Obtain the collection of classifications currently recognized by the Vision framework.
--
-- @requestRevision@ — The revision of the request for which classifications should be reported.
--
-- @error@ — The address of the variable that will be populated with the error when the call fails.
--
-- Returns: the collection of classifications for the revision, or nil if an error was encountered.
--
-- ObjC selector: @+ knownClassificationsForRevision:error:@
knownClassificationsForRevision_error :: IsNSError error_ => CULong -> error_ -> IO (Id NSArray)
knownClassificationsForRevision_error requestRevision error_ =
  do
    cls' <- getRequiredClass "VNClassifyImageRequest"
    sendClassMessage cls' knownClassificationsForRevision_errorSelector requestRevision (toNSError error_)

-- | Obtain the collection of identifiers supported by the target request.
--
-- This method will return the collection of all possible classification identifiers that are produced by the target request based on its current state of configuration at the time of the call.
--
-- @error@ — The address of the variable that will be populated with the error if the call fails.
--
-- Returns: The collection of classification identifiers, or nil if a failure occurs.
--
-- ObjC selector: @- supportedIdentifiersAndReturnError:@
supportedIdentifiersAndReturnError :: (IsVNClassifyImageRequest vnClassifyImageRequest, IsNSError error_) => vnClassifyImageRequest -> error_ -> IO (Id NSArray)
supportedIdentifiersAndReturnError vnClassifyImageRequest error_ =
  sendMessage vnClassifyImageRequest supportedIdentifiersAndReturnErrorSelector (toNSError error_)

-- | VNClassificationObservation results.
--
-- ObjC selector: @- results@
results :: IsVNClassifyImageRequest vnClassifyImageRequest => vnClassifyImageRequest -> IO (Id NSArray)
results vnClassifyImageRequest =
  sendMessage vnClassifyImageRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @knownClassificationsForRevision:error:@
knownClassificationsForRevision_errorSelector :: Selector '[CULong, Id NSError] (Id NSArray)
knownClassificationsForRevision_errorSelector = mkSelector "knownClassificationsForRevision:error:"

-- | @Selector@ for @supportedIdentifiersAndReturnError:@
supportedIdentifiersAndReturnErrorSelector :: Selector '[Id NSError] (Id NSArray)
supportedIdentifiersAndReturnErrorSelector = mkSelector "supportedIdentifiersAndReturnError:"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

