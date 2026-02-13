{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKLocalSearchRequest@.
module ObjC.MapKit.MKLocalSearchRequest
  ( MKLocalSearchRequest
  , IsMKLocalSearchRequest(..)
  , init_
  , initWithNaturalLanguageQuery
  , initWithCompletion
  , naturalLanguageQuery
  , setNaturalLanguageQuery
  , regionPriority
  , setRegionPriority
  , resultTypes
  , setResultTypes
  , pointOfInterestFilter
  , setPointOfInterestFilter
  , addressFilter
  , setAddressFilter
  , addressFilterSelector
  , initSelector
  , initWithCompletionSelector
  , initWithNaturalLanguageQuerySelector
  , naturalLanguageQuerySelector
  , pointOfInterestFilterSelector
  , regionPrioritySelector
  , resultTypesSelector
  , setAddressFilterSelector
  , setNaturalLanguageQuerySelector
  , setPointOfInterestFilterSelector
  , setRegionPrioritySelector
  , setResultTypesSelector

  -- * Enum types
  , MKLocalSearchRegionPriority(MKLocalSearchRegionPriority)
  , pattern MKLocalSearchRegionPriorityDefault
  , pattern MKLocalSearchRegionPriorityRequired
  , MKLocalSearchResultType(MKLocalSearchResultType)
  , pattern MKLocalSearchResultTypeAddress
  , pattern MKLocalSearchResultTypePointOfInterest
  , pattern MKLocalSearchResultTypePhysicalFeature

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

-- | @- init@
init_ :: IsMKLocalSearchRequest mkLocalSearchRequest => mkLocalSearchRequest -> IO (Id MKLocalSearchRequest)
init_ mkLocalSearchRequest =
  sendOwnedMessage mkLocalSearchRequest initSelector

-- | @- initWithNaturalLanguageQuery:@
initWithNaturalLanguageQuery :: (IsMKLocalSearchRequest mkLocalSearchRequest, IsNSString naturalLanguageQuery) => mkLocalSearchRequest -> naturalLanguageQuery -> IO (Id MKLocalSearchRequest)
initWithNaturalLanguageQuery mkLocalSearchRequest naturalLanguageQuery =
  sendOwnedMessage mkLocalSearchRequest initWithNaturalLanguageQuerySelector (toNSString naturalLanguageQuery)

-- | @- initWithCompletion:@
initWithCompletion :: (IsMKLocalSearchRequest mkLocalSearchRequest, IsMKLocalSearchCompletion completion) => mkLocalSearchRequest -> completion -> IO (Id MKLocalSearchRequest)
initWithCompletion mkLocalSearchRequest completion =
  sendOwnedMessage mkLocalSearchRequest initWithCompletionSelector (toMKLocalSearchCompletion completion)

-- | @- naturalLanguageQuery@
naturalLanguageQuery :: IsMKLocalSearchRequest mkLocalSearchRequest => mkLocalSearchRequest -> IO (Id NSString)
naturalLanguageQuery mkLocalSearchRequest =
  sendMessage mkLocalSearchRequest naturalLanguageQuerySelector

-- | @- setNaturalLanguageQuery:@
setNaturalLanguageQuery :: (IsMKLocalSearchRequest mkLocalSearchRequest, IsNSString value) => mkLocalSearchRequest -> value -> IO ()
setNaturalLanguageQuery mkLocalSearchRequest value =
  sendMessage mkLocalSearchRequest setNaturalLanguageQuerySelector (toNSString value)

-- | @- regionPriority@
regionPriority :: IsMKLocalSearchRequest mkLocalSearchRequest => mkLocalSearchRequest -> IO MKLocalSearchRegionPriority
regionPriority mkLocalSearchRequest =
  sendMessage mkLocalSearchRequest regionPrioritySelector

-- | @- setRegionPriority:@
setRegionPriority :: IsMKLocalSearchRequest mkLocalSearchRequest => mkLocalSearchRequest -> MKLocalSearchRegionPriority -> IO ()
setRegionPriority mkLocalSearchRequest value =
  sendMessage mkLocalSearchRequest setRegionPrioritySelector value

-- | @- resultTypes@
resultTypes :: IsMKLocalSearchRequest mkLocalSearchRequest => mkLocalSearchRequest -> IO MKLocalSearchResultType
resultTypes mkLocalSearchRequest =
  sendMessage mkLocalSearchRequest resultTypesSelector

-- | @- setResultTypes:@
setResultTypes :: IsMKLocalSearchRequest mkLocalSearchRequest => mkLocalSearchRequest -> MKLocalSearchResultType -> IO ()
setResultTypes mkLocalSearchRequest value =
  sendMessage mkLocalSearchRequest setResultTypesSelector value

-- | @- pointOfInterestFilter@
pointOfInterestFilter :: IsMKLocalSearchRequest mkLocalSearchRequest => mkLocalSearchRequest -> IO (Id MKPointOfInterestFilter)
pointOfInterestFilter mkLocalSearchRequest =
  sendMessage mkLocalSearchRequest pointOfInterestFilterSelector

-- | @- setPointOfInterestFilter:@
setPointOfInterestFilter :: (IsMKLocalSearchRequest mkLocalSearchRequest, IsMKPointOfInterestFilter value) => mkLocalSearchRequest -> value -> IO ()
setPointOfInterestFilter mkLocalSearchRequest value =
  sendMessage mkLocalSearchRequest setPointOfInterestFilterSelector (toMKPointOfInterestFilter value)

-- | @- addressFilter@
addressFilter :: IsMKLocalSearchRequest mkLocalSearchRequest => mkLocalSearchRequest -> IO (Id MKAddressFilter)
addressFilter mkLocalSearchRequest =
  sendMessage mkLocalSearchRequest addressFilterSelector

-- | @- setAddressFilter:@
setAddressFilter :: (IsMKLocalSearchRequest mkLocalSearchRequest, IsMKAddressFilter value) => mkLocalSearchRequest -> value -> IO ()
setAddressFilter mkLocalSearchRequest value =
  sendMessage mkLocalSearchRequest setAddressFilterSelector (toMKAddressFilter value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MKLocalSearchRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithNaturalLanguageQuery:@
initWithNaturalLanguageQuerySelector :: Selector '[Id NSString] (Id MKLocalSearchRequest)
initWithNaturalLanguageQuerySelector = mkSelector "initWithNaturalLanguageQuery:"

-- | @Selector@ for @initWithCompletion:@
initWithCompletionSelector :: Selector '[Id MKLocalSearchCompletion] (Id MKLocalSearchRequest)
initWithCompletionSelector = mkSelector "initWithCompletion:"

-- | @Selector@ for @naturalLanguageQuery@
naturalLanguageQuerySelector :: Selector '[] (Id NSString)
naturalLanguageQuerySelector = mkSelector "naturalLanguageQuery"

-- | @Selector@ for @setNaturalLanguageQuery:@
setNaturalLanguageQuerySelector :: Selector '[Id NSString] ()
setNaturalLanguageQuerySelector = mkSelector "setNaturalLanguageQuery:"

-- | @Selector@ for @regionPriority@
regionPrioritySelector :: Selector '[] MKLocalSearchRegionPriority
regionPrioritySelector = mkSelector "regionPriority"

-- | @Selector@ for @setRegionPriority:@
setRegionPrioritySelector :: Selector '[MKLocalSearchRegionPriority] ()
setRegionPrioritySelector = mkSelector "setRegionPriority:"

-- | @Selector@ for @resultTypes@
resultTypesSelector :: Selector '[] MKLocalSearchResultType
resultTypesSelector = mkSelector "resultTypes"

-- | @Selector@ for @setResultTypes:@
setResultTypesSelector :: Selector '[MKLocalSearchResultType] ()
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

