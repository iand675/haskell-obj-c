{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKLocalSearchCompleter@.
module ObjC.MapKit.MKLocalSearchCompleter
  ( MKLocalSearchCompleter
  , IsMKLocalSearchCompleter(..)
  , cancel
  , queryFragment
  , setQueryFragment
  , regionPriority
  , setRegionPriority
  , filterType
  , setFilterType
  , resultTypes
  , setResultTypes
  , pointOfInterestFilter
  , setPointOfInterestFilter
  , addressFilter
  , setAddressFilter
  , results
  , searching
  , cancelSelector
  , queryFragmentSelector
  , setQueryFragmentSelector
  , regionPrioritySelector
  , setRegionPrioritySelector
  , filterTypeSelector
  , setFilterTypeSelector
  , resultTypesSelector
  , setResultTypesSelector
  , pointOfInterestFilterSelector
  , setPointOfInterestFilterSelector
  , addressFilterSelector
  , setAddressFilterSelector
  , resultsSelector
  , searchingSelector

  -- * Enum types
  , MKLocalSearchCompleterResultType(MKLocalSearchCompleterResultType)
  , pattern MKLocalSearchCompleterResultTypeAddress
  , pattern MKLocalSearchCompleterResultTypePointOfInterest
  , pattern MKLocalSearchCompleterResultTypeQuery
  , pattern MKLocalSearchCompleterResultTypePhysicalFeature
  , MKLocalSearchRegionPriority(MKLocalSearchRegionPriority)
  , pattern MKLocalSearchRegionPriorityDefault
  , pattern MKLocalSearchRegionPriorityRequired
  , MKSearchCompletionFilterType(MKSearchCompletionFilterType)
  , pattern MKSearchCompletionFilterTypeLocationsAndQueries
  , pattern MKSearchCompletionFilterTypeLocationsOnly

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

import ObjC.MapKit.Internal.Classes
import ObjC.MapKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- cancel@
cancel :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> IO ()
cancel mkLocalSearchCompleter  =
  sendMsg mkLocalSearchCompleter (mkSelector "cancel") retVoid []

-- | @- queryFragment@
queryFragment :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> IO (Id NSString)
queryFragment mkLocalSearchCompleter  =
  sendMsg mkLocalSearchCompleter (mkSelector "queryFragment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setQueryFragment:@
setQueryFragment :: (IsMKLocalSearchCompleter mkLocalSearchCompleter, IsNSString value) => mkLocalSearchCompleter -> value -> IO ()
setQueryFragment mkLocalSearchCompleter  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkLocalSearchCompleter (mkSelector "setQueryFragment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- regionPriority@
regionPriority :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> IO MKLocalSearchRegionPriority
regionPriority mkLocalSearchCompleter  =
  fmap (coerce :: CLong -> MKLocalSearchRegionPriority) $ sendMsg mkLocalSearchCompleter (mkSelector "regionPriority") retCLong []

-- | @- setRegionPriority:@
setRegionPriority :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> MKLocalSearchRegionPriority -> IO ()
setRegionPriority mkLocalSearchCompleter  value =
  sendMsg mkLocalSearchCompleter (mkSelector "setRegionPriority:") retVoid [argCLong (coerce value)]

-- | @- filterType@
filterType :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> IO MKSearchCompletionFilterType
filterType mkLocalSearchCompleter  =
  fmap (coerce :: CLong -> MKSearchCompletionFilterType) $ sendMsg mkLocalSearchCompleter (mkSelector "filterType") retCLong []

-- | @- setFilterType:@
setFilterType :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> MKSearchCompletionFilterType -> IO ()
setFilterType mkLocalSearchCompleter  value =
  sendMsg mkLocalSearchCompleter (mkSelector "setFilterType:") retVoid [argCLong (coerce value)]

-- | @- resultTypes@
resultTypes :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> IO MKLocalSearchCompleterResultType
resultTypes mkLocalSearchCompleter  =
  fmap (coerce :: CULong -> MKLocalSearchCompleterResultType) $ sendMsg mkLocalSearchCompleter (mkSelector "resultTypes") retCULong []

-- | @- setResultTypes:@
setResultTypes :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> MKLocalSearchCompleterResultType -> IO ()
setResultTypes mkLocalSearchCompleter  value =
  sendMsg mkLocalSearchCompleter (mkSelector "setResultTypes:") retVoid [argCULong (coerce value)]

-- | @- pointOfInterestFilter@
pointOfInterestFilter :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> IO (Id MKPointOfInterestFilter)
pointOfInterestFilter mkLocalSearchCompleter  =
  sendMsg mkLocalSearchCompleter (mkSelector "pointOfInterestFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPointOfInterestFilter:@
setPointOfInterestFilter :: (IsMKLocalSearchCompleter mkLocalSearchCompleter, IsMKPointOfInterestFilter value) => mkLocalSearchCompleter -> value -> IO ()
setPointOfInterestFilter mkLocalSearchCompleter  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkLocalSearchCompleter (mkSelector "setPointOfInterestFilter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- addressFilter@
addressFilter :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> IO (Id MKAddressFilter)
addressFilter mkLocalSearchCompleter  =
  sendMsg mkLocalSearchCompleter (mkSelector "addressFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAddressFilter:@
setAddressFilter :: (IsMKLocalSearchCompleter mkLocalSearchCompleter, IsMKAddressFilter value) => mkLocalSearchCompleter -> value -> IO ()
setAddressFilter mkLocalSearchCompleter  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkLocalSearchCompleter (mkSelector "setAddressFilter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- results@
results :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> IO (Id NSArray)
results mkLocalSearchCompleter  =
  sendMsg mkLocalSearchCompleter (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- searching@
searching :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> IO Bool
searching mkLocalSearchCompleter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkLocalSearchCompleter (mkSelector "searching") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @queryFragment@
queryFragmentSelector :: Selector
queryFragmentSelector = mkSelector "queryFragment"

-- | @Selector@ for @setQueryFragment:@
setQueryFragmentSelector :: Selector
setQueryFragmentSelector = mkSelector "setQueryFragment:"

-- | @Selector@ for @regionPriority@
regionPrioritySelector :: Selector
regionPrioritySelector = mkSelector "regionPriority"

-- | @Selector@ for @setRegionPriority:@
setRegionPrioritySelector :: Selector
setRegionPrioritySelector = mkSelector "setRegionPriority:"

-- | @Selector@ for @filterType@
filterTypeSelector :: Selector
filterTypeSelector = mkSelector "filterType"

-- | @Selector@ for @setFilterType:@
setFilterTypeSelector :: Selector
setFilterTypeSelector = mkSelector "setFilterType:"

-- | @Selector@ for @resultTypes@
resultTypesSelector :: Selector
resultTypesSelector = mkSelector "resultTypes"

-- | @Selector@ for @setResultTypes:@
setResultTypesSelector :: Selector
setResultTypesSelector = mkSelector "setResultTypes:"

-- | @Selector@ for @pointOfInterestFilter@
pointOfInterestFilterSelector :: Selector
pointOfInterestFilterSelector = mkSelector "pointOfInterestFilter"

-- | @Selector@ for @setPointOfInterestFilter:@
setPointOfInterestFilterSelector :: Selector
setPointOfInterestFilterSelector = mkSelector "setPointOfInterestFilter:"

-- | @Selector@ for @addressFilter@
addressFilterSelector :: Selector
addressFilterSelector = mkSelector "addressFilter"

-- | @Selector@ for @setAddressFilter:@
setAddressFilterSelector :: Selector
setAddressFilterSelector = mkSelector "setAddressFilter:"

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

-- | @Selector@ for @searching@
searchingSelector :: Selector
searchingSelector = mkSelector "searching"

