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
  , supportedSymbologiesAndReturnErrorSelector
  , supportedSymbologiesSelector
  , symbologiesSelector
  , setSymbologiesSelector
  , coalesceCompositeSymbologiesSelector
  , setCoalesceCompositeSymbologiesSelector
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

-- | Obtain the collection of barcode symbologies that can be recognized by the request in its current configuration.
--
-- Calling this method could be a potentially expensive operation.
--
-- Returns: An array of VNBarcodeSymbology objects describing the symbologies recognized by the request in its current configuration.
--
-- ObjC selector: @- supportedSymbologiesAndReturnError:@
supportedSymbologiesAndReturnError :: (IsVNDetectBarcodesRequest vnDetectBarcodesRequest, IsNSError error_) => vnDetectBarcodesRequest -> error_ -> IO (Id NSArray)
supportedSymbologiesAndReturnError vnDetectBarcodesRequest  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg vnDetectBarcodesRequest (mkSelector "supportedSymbologiesAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "supportedSymbologies") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The collection of barcode symbologies that are to be detected in the image.  The default is to scan for all possible symbologies. Setting a revision on the request will reset the symbologies to all symbologies for the specified revision.
--
-- ObjC selector: @- symbologies@
symbologies :: IsVNDetectBarcodesRequest vnDetectBarcodesRequest => vnDetectBarcodesRequest -> IO (Id NSArray)
symbologies vnDetectBarcodesRequest  =
  sendMsg vnDetectBarcodesRequest (mkSelector "symbologies") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The collection of barcode symbologies that are to be detected in the image.  The default is to scan for all possible symbologies. Setting a revision on the request will reset the symbologies to all symbologies for the specified revision.
--
-- ObjC selector: @- setSymbologies:@
setSymbologies :: (IsVNDetectBarcodesRequest vnDetectBarcodesRequest, IsNSArray value) => vnDetectBarcodesRequest -> value -> IO ()
setSymbologies vnDetectBarcodesRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg vnDetectBarcodesRequest (mkSelector "setSymbologies:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | An option to coalesce multiple codes if applicable based on the symbology
--
-- ObjC selector: @- coalesceCompositeSymbologies@
coalesceCompositeSymbologies :: IsVNDetectBarcodesRequest vnDetectBarcodesRequest => vnDetectBarcodesRequest -> IO Bool
coalesceCompositeSymbologies vnDetectBarcodesRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnDetectBarcodesRequest (mkSelector "coalesceCompositeSymbologies") retCULong []

-- | An option to coalesce multiple codes if applicable based on the symbology
--
-- ObjC selector: @- setCoalesceCompositeSymbologies:@
setCoalesceCompositeSymbologies :: IsVNDetectBarcodesRequest vnDetectBarcodesRequest => vnDetectBarcodesRequest -> Bool -> IO ()
setCoalesceCompositeSymbologies vnDetectBarcodesRequest  value =
  sendMsg vnDetectBarcodesRequest (mkSelector "setCoalesceCompositeSymbologies:") retVoid [argCULong (if value then 1 else 0)]

-- | VNBarcodeObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectBarcodesRequest vnDetectBarcodesRequest => vnDetectBarcodesRequest -> IO (Id NSArray)
results vnDetectBarcodesRequest  =
  sendMsg vnDetectBarcodesRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportedSymbologiesAndReturnError:@
supportedSymbologiesAndReturnErrorSelector :: Selector
supportedSymbologiesAndReturnErrorSelector = mkSelector "supportedSymbologiesAndReturnError:"

-- | @Selector@ for @supportedSymbologies@
supportedSymbologiesSelector :: Selector
supportedSymbologiesSelector = mkSelector "supportedSymbologies"

-- | @Selector@ for @symbologies@
symbologiesSelector :: Selector
symbologiesSelector = mkSelector "symbologies"

-- | @Selector@ for @setSymbologies:@
setSymbologiesSelector :: Selector
setSymbologiesSelector = mkSelector "setSymbologies:"

-- | @Selector@ for @coalesceCompositeSymbologies@
coalesceCompositeSymbologiesSelector :: Selector
coalesceCompositeSymbologiesSelector = mkSelector "coalesceCompositeSymbologies"

-- | @Selector@ for @setCoalesceCompositeSymbologies:@
setCoalesceCompositeSymbologiesSelector :: Selector
setCoalesceCompositeSymbologiesSelector = mkSelector "setCoalesceCompositeSymbologies:"

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

