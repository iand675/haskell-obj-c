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
    withObjCPtr error_ $ \raw_error_ ->
      sendClassMsg cls' (mkSelector "knownClassificationsForRevision:error:") (retPtr retVoid) [argCULong (fromIntegral requestRevision), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

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
supportedIdentifiersAndReturnError vnClassifyImageRequest  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg vnClassifyImageRequest (mkSelector "supportedIdentifiersAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | VNClassificationObservation results.
--
-- ObjC selector: @- results@
results :: IsVNClassifyImageRequest vnClassifyImageRequest => vnClassifyImageRequest -> IO (Id NSArray)
results vnClassifyImageRequest  =
  sendMsg vnClassifyImageRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @knownClassificationsForRevision:error:@
knownClassificationsForRevision_errorSelector :: Selector
knownClassificationsForRevision_errorSelector = mkSelector "knownClassificationsForRevision:error:"

-- | @Selector@ for @supportedIdentifiersAndReturnError:@
supportedIdentifiersAndReturnErrorSelector :: Selector
supportedIdentifiersAndReturnErrorSelector = mkSelector "supportedIdentifiersAndReturnError:"

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

