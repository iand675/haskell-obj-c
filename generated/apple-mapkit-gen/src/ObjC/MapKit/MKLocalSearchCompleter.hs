{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , results
  , searching
  , addressFilterSelector
  , cancelSelector
  , delegateSelector
  , filterTypeSelector
  , pointOfInterestFilterSelector
  , queryFragmentSelector
  , regionPrioritySelector
  , resultTypesSelector
  , resultsSelector
  , searchingSelector
  , setAddressFilterSelector
  , setDelegateSelector
  , setFilterTypeSelector
  , setPointOfInterestFilterSelector
  , setQueryFragmentSelector
  , setRegionPrioritySelector
  , setResultTypesSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.MapKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- cancel@
cancel :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> IO ()
cancel mkLocalSearchCompleter =
  sendMessage mkLocalSearchCompleter cancelSelector

-- | @- queryFragment@
queryFragment :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> IO (Id NSString)
queryFragment mkLocalSearchCompleter =
  sendMessage mkLocalSearchCompleter queryFragmentSelector

-- | @- setQueryFragment:@
setQueryFragment :: (IsMKLocalSearchCompleter mkLocalSearchCompleter, IsNSString value) => mkLocalSearchCompleter -> value -> IO ()
setQueryFragment mkLocalSearchCompleter value =
  sendMessage mkLocalSearchCompleter setQueryFragmentSelector (toNSString value)

-- | @- regionPriority@
regionPriority :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> IO MKLocalSearchRegionPriority
regionPriority mkLocalSearchCompleter =
  sendMessage mkLocalSearchCompleter regionPrioritySelector

-- | @- setRegionPriority:@
setRegionPriority :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> MKLocalSearchRegionPriority -> IO ()
setRegionPriority mkLocalSearchCompleter value =
  sendMessage mkLocalSearchCompleter setRegionPrioritySelector value

-- | @- filterType@
filterType :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> IO MKSearchCompletionFilterType
filterType mkLocalSearchCompleter =
  sendMessage mkLocalSearchCompleter filterTypeSelector

-- | @- setFilterType:@
setFilterType :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> MKSearchCompletionFilterType -> IO ()
setFilterType mkLocalSearchCompleter value =
  sendMessage mkLocalSearchCompleter setFilterTypeSelector value

-- | @- resultTypes@
resultTypes :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> IO MKLocalSearchCompleterResultType
resultTypes mkLocalSearchCompleter =
  sendMessage mkLocalSearchCompleter resultTypesSelector

-- | @- setResultTypes:@
setResultTypes :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> MKLocalSearchCompleterResultType -> IO ()
setResultTypes mkLocalSearchCompleter value =
  sendMessage mkLocalSearchCompleter setResultTypesSelector value

-- | @- pointOfInterestFilter@
pointOfInterestFilter :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> IO (Id MKPointOfInterestFilter)
pointOfInterestFilter mkLocalSearchCompleter =
  sendMessage mkLocalSearchCompleter pointOfInterestFilterSelector

-- | @- setPointOfInterestFilter:@
setPointOfInterestFilter :: (IsMKLocalSearchCompleter mkLocalSearchCompleter, IsMKPointOfInterestFilter value) => mkLocalSearchCompleter -> value -> IO ()
setPointOfInterestFilter mkLocalSearchCompleter value =
  sendMessage mkLocalSearchCompleter setPointOfInterestFilterSelector (toMKPointOfInterestFilter value)

-- | @- addressFilter@
addressFilter :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> IO (Id MKAddressFilter)
addressFilter mkLocalSearchCompleter =
  sendMessage mkLocalSearchCompleter addressFilterSelector

-- | @- setAddressFilter:@
setAddressFilter :: (IsMKLocalSearchCompleter mkLocalSearchCompleter, IsMKAddressFilter value) => mkLocalSearchCompleter -> value -> IO ()
setAddressFilter mkLocalSearchCompleter value =
  sendMessage mkLocalSearchCompleter setAddressFilterSelector (toMKAddressFilter value)

-- | @- delegate@
delegate :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> IO RawId
delegate mkLocalSearchCompleter =
  sendMessage mkLocalSearchCompleter delegateSelector

-- | @- setDelegate:@
setDelegate :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> RawId -> IO ()
setDelegate mkLocalSearchCompleter value =
  sendMessage mkLocalSearchCompleter setDelegateSelector value

-- | @- results@
results :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> IO (Id NSArray)
results mkLocalSearchCompleter =
  sendMessage mkLocalSearchCompleter resultsSelector

-- | @- searching@
searching :: IsMKLocalSearchCompleter mkLocalSearchCompleter => mkLocalSearchCompleter -> IO Bool
searching mkLocalSearchCompleter =
  sendMessage mkLocalSearchCompleter searchingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @queryFragment@
queryFragmentSelector :: Selector '[] (Id NSString)
queryFragmentSelector = mkSelector "queryFragment"

-- | @Selector@ for @setQueryFragment:@
setQueryFragmentSelector :: Selector '[Id NSString] ()
setQueryFragmentSelector = mkSelector "setQueryFragment:"

-- | @Selector@ for @regionPriority@
regionPrioritySelector :: Selector '[] MKLocalSearchRegionPriority
regionPrioritySelector = mkSelector "regionPriority"

-- | @Selector@ for @setRegionPriority:@
setRegionPrioritySelector :: Selector '[MKLocalSearchRegionPriority] ()
setRegionPrioritySelector = mkSelector "setRegionPriority:"

-- | @Selector@ for @filterType@
filterTypeSelector :: Selector '[] MKSearchCompletionFilterType
filterTypeSelector = mkSelector "filterType"

-- | @Selector@ for @setFilterType:@
setFilterTypeSelector :: Selector '[MKSearchCompletionFilterType] ()
setFilterTypeSelector = mkSelector "setFilterType:"

-- | @Selector@ for @resultTypes@
resultTypesSelector :: Selector '[] MKLocalSearchCompleterResultType
resultTypesSelector = mkSelector "resultTypes"

-- | @Selector@ for @setResultTypes:@
setResultTypesSelector :: Selector '[MKLocalSearchCompleterResultType] ()
setResultTypesSelector = mkSelector "setResultTypes:"

-- | @Selector@ for @pointOfInterestFilter@
pointOfInterestFilterSelector :: Selector '[] (Id MKPointOfInterestFilter)
pointOfInterestFilterSelector = mkSelector "pointOfInterestFilter"

-- | @Selector@ for @setPointOfInterestFilter:@
setPointOfInterestFilterSelector :: Selector '[Id MKPointOfInterestFilter] ()
setPointOfInterestFilterSelector = mkSelector "setPointOfInterestFilter:"

-- | @Selector@ for @addressFilter@
addressFilterSelector :: Selector '[] (Id MKAddressFilter)
addressFilterSelector = mkSelector "addressFilter"

-- | @Selector@ for @setAddressFilter:@
setAddressFilterSelector :: Selector '[Id MKAddressFilter] ()
setAddressFilterSelector = mkSelector "setAddressFilter:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

-- | @Selector@ for @searching@
searchingSelector :: Selector '[] Bool
searchingSelector = mkSelector "searching"

