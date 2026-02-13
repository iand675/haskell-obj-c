{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , dataSelector
  , elementCountSelector
  , elementTypeSelector

  -- * Enum types
  , VNElementType(VNElementType)
  , pattern VNElementTypeUnknown
  , pattern VNElementTypeFloat
  , pattern VNElementTypeDouble

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
computeDistance_toFeaturePrintObservation_error vnFeaturePrintObservation outDistance featurePrint error_ =
  sendMessage vnFeaturePrintObservation computeDistance_toFeaturePrintObservation_errorSelector outDistance (toVNFeaturePrintObservation featurePrint) (toNSError error_)

-- | The type of each element in the data.
--
-- ObjC selector: @- elementType@
elementType :: IsVNFeaturePrintObservation vnFeaturePrintObservation => vnFeaturePrintObservation -> IO VNElementType
elementType vnFeaturePrintObservation =
  sendMessage vnFeaturePrintObservation elementTypeSelector

-- | The total number of elements in the data.
--
-- ObjC selector: @- elementCount@
elementCount :: IsVNFeaturePrintObservation vnFeaturePrintObservation => vnFeaturePrintObservation -> IO CULong
elementCount vnFeaturePrintObservation =
  sendMessage vnFeaturePrintObservation elementCountSelector

-- | The feature print data.
--
-- ObjC selector: @- data@
data_ :: IsVNFeaturePrintObservation vnFeaturePrintObservation => vnFeaturePrintObservation -> IO (Id NSData)
data_ vnFeaturePrintObservation =
  sendMessage vnFeaturePrintObservation dataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @computeDistance:toFeaturePrintObservation:error:@
computeDistance_toFeaturePrintObservation_errorSelector :: Selector '[Ptr CFloat, Id VNFeaturePrintObservation, Id NSError] Bool
computeDistance_toFeaturePrintObservation_errorSelector = mkSelector "computeDistance:toFeaturePrintObservation:error:"

-- | @Selector@ for @elementType@
elementTypeSelector :: Selector '[] VNElementType
elementTypeSelector = mkSelector "elementType"

-- | @Selector@ for @elementCount@
elementCountSelector :: Selector '[] CULong
elementCountSelector = mkSelector "elementCount"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

