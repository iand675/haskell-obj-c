{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @VNFeaturePrintObservation@.
module ObjC.Vision.VNFeaturePrintObservation
  ( VNFeaturePrintObservation
  , IsVNFeaturePrintObservation(..)
  , computeDistance_toFeaturePrintObservation_error
  , elementType
  , elementCount
  , data_
  , computeDistance_toFeaturePrintObservation_errorSelector
  , elementTypeSelector
  , elementCountSelector
  , dataSelector

  -- * Enum types
  , VNElementType(VNElementType)
  , pattern VNElementTypeUnknown
  , pattern VNElementTypeFloat
  , pattern VNElementTypeDouble

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
import ObjC.Vision.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Computes the distance between two observations.
--
-- The larger the distance the more dissimlar the feature prints are. In case of an error this method returns false with an error describing the error condition, for instance comparing two non-comparable feature prints.
--
-- ObjC selector: @- computeDistance:toFeaturePrintObservation:error:@
computeDistance_toFeaturePrintObservation_error :: (IsVNFeaturePrintObservation vnFeaturePrintObservation, IsVNFeaturePrintObservation featurePrint, IsNSError error_) => vnFeaturePrintObservation -> Ptr CFloat -> featurePrint -> error_ -> IO Bool
computeDistance_toFeaturePrintObservation_error vnFeaturePrintObservation  outDistance featurePrint error_ =
withObjCPtr featurePrint $ \raw_featurePrint ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnFeaturePrintObservation (mkSelector "computeDistance:toFeaturePrintObservation:error:") retCULong [argPtr outDistance, argPtr (castPtr raw_featurePrint :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | The type of each element in the data.
--
-- ObjC selector: @- elementType@
elementType :: IsVNFeaturePrintObservation vnFeaturePrintObservation => vnFeaturePrintObservation -> IO VNElementType
elementType vnFeaturePrintObservation  =
  fmap (coerce :: CULong -> VNElementType) $ sendMsg vnFeaturePrintObservation (mkSelector "elementType") retCULong []

-- | The total number of elements in the data.
--
-- ObjC selector: @- elementCount@
elementCount :: IsVNFeaturePrintObservation vnFeaturePrintObservation => vnFeaturePrintObservation -> IO CULong
elementCount vnFeaturePrintObservation  =
  sendMsg vnFeaturePrintObservation (mkSelector "elementCount") retCULong []

-- | The feature print data.
--
-- ObjC selector: @- data@
data_ :: IsVNFeaturePrintObservation vnFeaturePrintObservation => vnFeaturePrintObservation -> IO (Id NSData)
data_ vnFeaturePrintObservation  =
  sendMsg vnFeaturePrintObservation (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @computeDistance:toFeaturePrintObservation:error:@
computeDistance_toFeaturePrintObservation_errorSelector :: Selector
computeDistance_toFeaturePrintObservation_errorSelector = mkSelector "computeDistance:toFeaturePrintObservation:error:"

-- | @Selector@ for @elementType@
elementTypeSelector :: Selector
elementTypeSelector = mkSelector "elementType"

-- | @Selector@ for @elementCount@
elementCountSelector :: Selector
elementCountSelector = mkSelector "elementCount"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

