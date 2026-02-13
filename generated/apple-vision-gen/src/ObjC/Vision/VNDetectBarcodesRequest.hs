{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request that detects barcodes in an image.
--
-- This request will return zero or more VNBarcodeObservation objects objects which describe the barcodes detected in an image.
--
-- Generated bindings for @VNDetectBarcodesRequest@.
module ObjC.Vision.VNDetectBarcodesRequest
  ( VNDetectBarcodesRequest
  , IsVNDetectBarcodesRequest(..)
  , supportedSymbologiesAndReturnError
  , supportedSymbologies
  , symbologies
  , setSymbologies
  , coalesceCompositeSymbologies
  , setCoalesceCompositeSymbologies
  , results
  , coalesceCompositeSymbologiesSelector
  , resultsSelector
  , setCoalesceCompositeSymbologiesSelector
  , setSymbologiesSelector
  , supportedSymbologiesAndReturnErrorSelector
  , supportedSymbologiesSelector
  , symbologiesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Obtain the collection of barcode symbologies that can be recognized by the request in its current configuration.
--
-- Calling this method could be a potentially expensive operation.
--
-- Returns: An array of VNBarcodeSymbology objects describing the symbologies recognized by the request in its current configuration.
--
-- ObjC selector: @- supportedSymbologiesAndReturnError:@
supportedSymbologiesAndReturnError :: (IsVNDetectBarcodesRequest vnDetectBarcodesRequest, IsNSError error_) => vnDetectBarcodesRequest -> error_ -> IO (Id NSArray)
supportedSymbologiesAndReturnError vnDetectBarcodesRequest error_ =
  sendMessage vnDetectBarcodesRequest supportedSymbologiesAndReturnErrorSelector (toNSError error_)

-- | Obtain the collection of barcode symbologies currently recognized by the Vision framework.
--
-- Calling this method could be a potentially expensive operation.
--
-- Returns: An array of VNBarcodeSymbology objects describing the symbologies currently supported by the Vision framework.
--
-- ObjC selector: @+ supportedSymbologies@
supportedSymbologies :: IO (Id NSArray)
supportedSymbologies  =
  do
    cls' <- getRequiredClass "VNDetectBarcodesRequest"
    sendClassMessage cls' supportedSymbologiesSelector

-- | The collection of barcode symbologies that are to be detected in the image.  The default is to scan for all possible symbologies. Setting a revision on the request will reset the symbologies to all symbologies for the specified revision.
--
-- ObjC selector: @- symbologies@
symbologies :: IsVNDetectBarcodesRequest vnDetectBarcodesRequest => vnDetectBarcodesRequest -> IO (Id NSArray)
symbologies vnDetectBarcodesRequest =
  sendMessage vnDetectBarcodesRequest symbologiesSelector

-- | The collection of barcode symbologies that are to be detected in the image.  The default is to scan for all possible symbologies. Setting a revision on the request will reset the symbologies to all symbologies for the specified revision.
--
-- ObjC selector: @- setSymbologies:@
setSymbologies :: (IsVNDetectBarcodesRequest vnDetectBarcodesRequest, IsNSArray value) => vnDetectBarcodesRequest -> value -> IO ()
setSymbologies vnDetectBarcodesRequest value =
  sendMessage vnDetectBarcodesRequest setSymbologiesSelector (toNSArray value)

-- | An option to coalesce multiple codes if applicable based on the symbology
--
-- ObjC selector: @- coalesceCompositeSymbologies@
coalesceCompositeSymbologies :: IsVNDetectBarcodesRequest vnDetectBarcodesRequest => vnDetectBarcodesRequest -> IO Bool
coalesceCompositeSymbologies vnDetectBarcodesRequest =
  sendMessage vnDetectBarcodesRequest coalesceCompositeSymbologiesSelector

-- | An option to coalesce multiple codes if applicable based on the symbology
--
-- ObjC selector: @- setCoalesceCompositeSymbologies:@
setCoalesceCompositeSymbologies :: IsVNDetectBarcodesRequest vnDetectBarcodesRequest => vnDetectBarcodesRequest -> Bool -> IO ()
setCoalesceCompositeSymbologies vnDetectBarcodesRequest value =
  sendMessage vnDetectBarcodesRequest setCoalesceCompositeSymbologiesSelector value

-- | VNBarcodeObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectBarcodesRequest vnDetectBarcodesRequest => vnDetectBarcodesRequest -> IO (Id NSArray)
results vnDetectBarcodesRequest =
  sendMessage vnDetectBarcodesRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportedSymbologiesAndReturnError:@
supportedSymbologiesAndReturnErrorSelector :: Selector '[Id NSError] (Id NSArray)
supportedSymbologiesAndReturnErrorSelector = mkSelector "supportedSymbologiesAndReturnError:"

-- | @Selector@ for @supportedSymbologies@
supportedSymbologiesSelector :: Selector '[] (Id NSArray)
supportedSymbologiesSelector = mkSelector "supportedSymbologies"

-- | @Selector@ for @symbologies@
symbologiesSelector :: Selector '[] (Id NSArray)
symbologiesSelector = mkSelector "symbologies"

-- | @Selector@ for @setSymbologies:@
setSymbologiesSelector :: Selector '[Id NSArray] ()
setSymbologiesSelector = mkSelector "setSymbologies:"

-- | @Selector@ for @coalesceCompositeSymbologies@
coalesceCompositeSymbologiesSelector :: Selector '[] Bool
coalesceCompositeSymbologiesSelector = mkSelector "coalesceCompositeSymbologies"

-- | @Selector@ for @setCoalesceCompositeSymbologies:@
setCoalesceCompositeSymbologiesSelector :: Selector '[Bool] ()
setCoalesceCompositeSymbologiesSelector = mkSelector "setCoalesceCompositeSymbologies:"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

