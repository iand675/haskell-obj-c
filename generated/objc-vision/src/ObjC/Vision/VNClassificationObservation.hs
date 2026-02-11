{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNClassificationObservation
--
-- VNObservation
--
-- VNClassificationObservation returns the classifcation in form of a string.
--
-- VNClassificationObservation is the observation returned by VNCoreMLRequests that using a model that is a classifier. A classifier produces an arrary (this can be a single entry) of classifications which are labels (identifiers) and confidence scores.
--
-- Generated bindings for @VNClassificationObservation@.
module ObjC.Vision.VNClassificationObservation
  ( VNClassificationObservation
  , IsVNClassificationObservation(..)
  , hasMinimumRecall_forPrecision
  , hasMinimumPrecision_forRecall
  , identifier
  , hasPrecisionRecallCurve
  , hasMinimumRecall_forPrecisionSelector
  , hasMinimumPrecision_forRecallSelector
  , identifierSelector
  , hasPrecisionRecallCurveSelector


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

-- | Determine whether or not the observation's operation point for a specific precision has a minimum recall value.
--
-- @minimumRecall@ — The minimum recall desired for an operation point.
--
-- @precision@ — The precision value used to select the operation point.
--
-- Returns: YES if the recall value for the operation point specified by a precision value has the minimum value; otherwise, NO.
--
-- ObjC selector: @- hasMinimumRecall:forPrecision:@
hasMinimumRecall_forPrecision :: IsVNClassificationObservation vnClassificationObservation => vnClassificationObservation -> CFloat -> CFloat -> IO Bool
hasMinimumRecall_forPrecision vnClassificationObservation  minimumRecall precision =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnClassificationObservation (mkSelector "hasMinimumRecall:forPrecision:") retCULong [argCFloat (fromIntegral minimumRecall), argCFloat (fromIntegral precision)]

-- | Determine whether or not the observation's operation point for a specific recall has a minimum precision value.
--
-- @minimumPrecision@ — The minimum precision desired for an operation point.
--
-- @recall@ — The recall value used to select the operation point.
--
-- Returns: YES if the precision value for the operation point specified by a recall value has the minimum value; otherwise, NO.
--
-- ObjC selector: @- hasMinimumPrecision:forRecall:@
hasMinimumPrecision_forRecall :: IsVNClassificationObservation vnClassificationObservation => vnClassificationObservation -> CFloat -> CFloat -> IO Bool
hasMinimumPrecision_forRecall vnClassificationObservation  minimumPrecision recall =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnClassificationObservation (mkSelector "hasMinimumPrecision:forRecall:") retCULong [argCFloat (fromIntegral minimumPrecision), argCFloat (fromIntegral recall)]

-- | The is the label or identifier of a classification request. An example classification could be a string like 'cat' or 'hotdog'. The string is defined in the model that was used for the classification. Usually these are technical labels that are not localized and not meant to be used directly to be presented to an end user in the UI.
--
-- ObjC selector: @- identifier@
identifier :: IsVNClassificationObservation vnClassificationObservation => vnClassificationObservation -> IO (Id NSString)
identifier vnClassificationObservation  =
  sendMsg vnClassificationObservation (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Determine whether or not precision/recall curves are available with the observation.
--
-- If this property is YES, then all other precision/recall related methods in this addition can be called.
--
-- ObjC selector: @- hasPrecisionRecallCurve@
hasPrecisionRecallCurve :: IsVNClassificationObservation vnClassificationObservation => vnClassificationObservation -> IO Bool
hasPrecisionRecallCurve vnClassificationObservation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnClassificationObservation (mkSelector "hasPrecisionRecallCurve") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hasMinimumRecall:forPrecision:@
hasMinimumRecall_forPrecisionSelector :: Selector
hasMinimumRecall_forPrecisionSelector = mkSelector "hasMinimumRecall:forPrecision:"

-- | @Selector@ for @hasMinimumPrecision:forRecall:@
hasMinimumPrecision_forRecallSelector :: Selector
hasMinimumPrecision_forRecallSelector = mkSelector "hasMinimumPrecision:forRecall:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @hasPrecisionRecallCurve@
hasPrecisionRecallCurveSelector :: Selector
hasPrecisionRecallCurveSelector = mkSelector "hasPrecisionRecallCurve"

