{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithNaturalLanguageQuerySelector
  , initWithCompletionSelector
  , naturalLanguageQuerySelector
  , setNaturalLanguageQuerySelector
  , regionPrioritySelector
  , setRegionPrioritySelector
  , resultTypesSelector
  , setResultTypesSelector
  , pointOfInterestFilterSelector
  , setPointOfInterestFilterSelector
  , addressFilterSelector
  , setAddressFilterSelector

  -- * Enum types
  , MKLocalSearchRegionPriority(MKLocalSearchRegionPriority)
  , pattern MKLocalSearchRegionPriorityDefault
  , pattern MKLocalSearchRegionPriorityRequired
  , MKLocalSearchResultType(MKLocalSearchResultType)
  , pattern MKLocalSearchResultTypeAddress
  , pattern MKLocalSearchResultTypePointOfInterest
  , pattern MKLocalSearchResultTypePhysicalFeature

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

-- | @- init@
init_ :: IsMKLocalSearchRequest mkLocalSearchRequest => mkLocalSearchRequest -> IO (Id MKLocalSearchRequest)
init_ mkLocalSearchRequest  =
  sendMsg mkLocalSearchRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithNaturalLanguageQuery:@
initWithNaturalLanguageQuery :: (IsMKLocalSearchRequest mkLocalSearchRequest, IsNSString naturalLanguageQuery) => mkLocalSearchRequest -> naturalLanguageQuery -> IO (Id MKLocalSearchRequest)
initWithNaturalLanguageQuery mkLocalSearchRequest  naturalLanguageQuery =
withObjCPtr naturalLanguageQuery $ \raw_naturalLanguageQuery ->
    sendMsg mkLocalSearchRequest (mkSelector "initWithNaturalLanguageQuery:") (retPtr retVoid) [argPtr (castPtr raw_naturalLanguageQuery :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCompletion:@
initWithCompletion :: (IsMKLocalSearchRequest mkLocalSearchRequest, IsMKLocalSearchCompletion completion) => mkLocalSearchRequest -> completion -> IO (Id MKLocalSearchRequest)
initWithCompletion mkLocalSearchRequest  completion =
withObjCPtr completion $ \raw_completion ->
    sendMsg mkLocalSearchRequest (mkSelector "initWithCompletion:") (retPtr retVoid) [argPtr (castPtr raw_completion :: Ptr ())] >>= ownedObject . castPtr

-- | @- naturalLanguageQuery@
naturalLanguageQuery :: IsMKLocalSearchRequest mkLocalSearchRequest => mkLocalSearchRequest -> IO (Id NSString)
naturalLanguageQuery mkLocalSearchRequest  =
  sendMsg mkLocalSearchRequest (mkSelector "naturalLanguageQuery") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNaturalLanguageQuery:@
setNaturalLanguageQuery :: (IsMKLocalSearchRequest mkLocalSearchRequest, IsNSString value) => mkLocalSearchRequest -> value -> IO ()
setNaturalLanguageQuery mkLocalSearchRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkLocalSearchRequest (mkSelector "setNaturalLanguageQuery:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- regionPriority@
regionPriority :: IsMKLocalSearchRequest mkLocalSearchRequest => mkLocalSearchRequest -> IO MKLocalSearchRegionPriority
regionPriority mkLocalSearchRequest  =
  fmap (coerce :: CLong -> MKLocalSearchRegionPriority) $ sendMsg mkLocalSearchRequest (mkSelector "regionPriority") retCLong []

-- | @- setRegionPriority:@
setRegionPriority :: IsMKLocalSearchRequest mkLocalSearchRequest => mkLocalSearchRequest -> MKLocalSearchRegionPriority -> IO ()
setRegionPriority mkLocalSearchRequest  value =
  sendMsg mkLocalSearchRequest (mkSelector "setRegionPriority:") retVoid [argCLong (coerce value)]

-- | @- resultTypes@
resultTypes :: IsMKLocalSearchRequest mkLocalSearchRequest => mkLocalSearchRequest -> IO MKLocalSearchResultType
resultTypes mkLocalSearchRequest  =
  fmap (coerce :: CULong -> MKLocalSearchResultType) $ sendMsg mkLocalSearchRequest (mkSelector "resultTypes") retCULong []

-- | @- setResultTypes:@
setResultTypes :: IsMKLocalSearchRequest mkLocalSearchRequest => mkLocalSearchRequest -> MKLocalSearchResultType -> IO ()
setResultTypes mkLocalSearchRequest  value =
  sendMsg mkLocalSearchRequest (mkSelector "setResultTypes:") retVoid [argCULong (coerce value)]

-- | @- pointOfInterestFilter@
pointOfInterestFilter :: IsMKLocalSearchRequest mkLocalSearchRequest => mkLocalSearchRequest -> IO (Id MKPointOfInterestFilter)
pointOfInterestFilter mkLocalSearchRequest  =
  sendMsg mkLocalSearchRequest (mkSelector "pointOfInterestFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPointOfInterestFilter:@
setPointOfInterestFilter :: (IsMKLocalSearchRequest mkLocalSearchRequest, IsMKPointOfInterestFilter value) => mkLocalSearchRequest -> value -> IO ()
setPointOfInterestFilter mkLocalSearchRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkLocalSearchRequest (mkSelector "setPointOfInterestFilter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- addressFilter@
addressFilter :: IsMKLocalSearchRequest mkLocalSearchRequest => mkLocalSearchRequest -> IO (Id MKAddressFilter)
addressFilter mkLocalSearchRequest  =
  sendMsg mkLocalSearchRequest (mkSelector "addressFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAddressFilter:@
setAddressFilter :: (IsMKLocalSearchRequest mkLocalSearchRequest, IsMKAddressFilter value) => mkLocalSearchRequest -> value -> IO ()
setAddressFilter mkLocalSearchRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkLocalSearchRequest (mkSelector "setAddressFilter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithNaturalLanguageQuery:@
initWithNaturalLanguageQuerySelector :: Selector
initWithNaturalLanguageQuerySelector = mkSelector "initWithNaturalLanguageQuery:"

-- | @Selector@ for @initWithCompletion:@
initWithCompletionSelector :: Selector
initWithCompletionSelector = mkSelector "initWithCompletion:"

-- | @Selector@ for @naturalLanguageQuery@
naturalLanguageQuerySelector :: Selector
naturalLanguageQuerySelector = mkSelector "naturalLanguageQuery"

-- | @Selector@ for @setNaturalLanguageQuery:@
setNaturalLanguageQuerySelector :: Selector
setNaturalLanguageQuerySelector = mkSelector "setNaturalLanguageQuery:"

-- | @Selector@ for @regionPriority@
regionPrioritySelector :: Selector
regionPrioritySelector = mkSelector "regionPriority"

-- | @Selector@ for @setRegionPriority:@
setRegionPrioritySelector :: Selector
setRegionPrioritySelector = mkSelector "setRegionPriority:"

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

