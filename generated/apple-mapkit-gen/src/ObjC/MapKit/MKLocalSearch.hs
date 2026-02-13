{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKLocalSearch@.
module ObjC.MapKit.MKLocalSearch
  ( MKLocalSearch
  , IsMKLocalSearch(..)
  , initWithRequest
  , initWithPointsOfInterestRequest
  , startWithCompletionHandler
  , cancel
  , searching
  , cancelSelector
  , initWithPointsOfInterestRequestSelector
  , initWithRequestSelector
  , searchingSelector
  , startWithCompletionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithRequest:@
initWithRequest :: (IsMKLocalSearch mkLocalSearch, IsMKLocalSearchRequest request) => mkLocalSearch -> request -> IO (Id MKLocalSearch)
initWithRequest mkLocalSearch request =
  sendOwnedMessage mkLocalSearch initWithRequestSelector (toMKLocalSearchRequest request)

-- | @- initWithPointsOfInterestRequest:@
initWithPointsOfInterestRequest :: (IsMKLocalSearch mkLocalSearch, IsMKLocalPointsOfInterestRequest request) => mkLocalSearch -> request -> IO (Id MKLocalSearch)
initWithPointsOfInterestRequest mkLocalSearch request =
  sendOwnedMessage mkLocalSearch initWithPointsOfInterestRequestSelector (toMKLocalPointsOfInterestRequest request)

-- | @- startWithCompletionHandler:@
startWithCompletionHandler :: IsMKLocalSearch mkLocalSearch => mkLocalSearch -> Ptr () -> IO ()
startWithCompletionHandler mkLocalSearch completionHandler =
  sendMessage mkLocalSearch startWithCompletionHandlerSelector completionHandler

-- | @- cancel@
cancel :: IsMKLocalSearch mkLocalSearch => mkLocalSearch -> IO ()
cancel mkLocalSearch =
  sendMessage mkLocalSearch cancelSelector

-- | @- searching@
searching :: IsMKLocalSearch mkLocalSearch => mkLocalSearch -> IO Bool
searching mkLocalSearch =
  sendMessage mkLocalSearch searchingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRequest:@
initWithRequestSelector :: Selector '[Id MKLocalSearchRequest] (Id MKLocalSearch)
initWithRequestSelector = mkSelector "initWithRequest:"

-- | @Selector@ for @initWithPointsOfInterestRequest:@
initWithPointsOfInterestRequestSelector :: Selector '[Id MKLocalPointsOfInterestRequest] (Id MKLocalSearch)
initWithPointsOfInterestRequestSelector = mkSelector "initWithPointsOfInterestRequest:"

-- | @Selector@ for @startWithCompletionHandler:@
startWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
startWithCompletionHandlerSelector = mkSelector "startWithCompletionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @searching@
searchingSelector :: Selector '[] Bool
searchingSelector = mkSelector "searching"

