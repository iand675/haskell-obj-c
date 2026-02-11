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
  , initWithRequestSelector
  , initWithPointsOfInterestRequestSelector
  , startWithCompletionHandlerSelector
  , cancelSelector
  , searchingSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithRequest:@
initWithRequest :: (IsMKLocalSearch mkLocalSearch, IsMKLocalSearchRequest request) => mkLocalSearch -> request -> IO (Id MKLocalSearch)
initWithRequest mkLocalSearch  request =
withObjCPtr request $ \raw_request ->
    sendMsg mkLocalSearch (mkSelector "initWithRequest:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithPointsOfInterestRequest:@
initWithPointsOfInterestRequest :: (IsMKLocalSearch mkLocalSearch, IsMKLocalPointsOfInterestRequest request) => mkLocalSearch -> request -> IO (Id MKLocalSearch)
initWithPointsOfInterestRequest mkLocalSearch  request =
withObjCPtr request $ \raw_request ->
    sendMsg mkLocalSearch (mkSelector "initWithPointsOfInterestRequest:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ())] >>= ownedObject . castPtr

-- | @- startWithCompletionHandler:@
startWithCompletionHandler :: IsMKLocalSearch mkLocalSearch => mkLocalSearch -> Ptr () -> IO ()
startWithCompletionHandler mkLocalSearch  completionHandler =
  sendMsg mkLocalSearch (mkSelector "startWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- cancel@
cancel :: IsMKLocalSearch mkLocalSearch => mkLocalSearch -> IO ()
cancel mkLocalSearch  =
  sendMsg mkLocalSearch (mkSelector "cancel") retVoid []

-- | @- searching@
searching :: IsMKLocalSearch mkLocalSearch => mkLocalSearch -> IO Bool
searching mkLocalSearch  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkLocalSearch (mkSelector "searching") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRequest:@
initWithRequestSelector :: Selector
initWithRequestSelector = mkSelector "initWithRequest:"

-- | @Selector@ for @initWithPointsOfInterestRequest:@
initWithPointsOfInterestRequestSelector :: Selector
initWithPointsOfInterestRequestSelector = mkSelector "initWithPointsOfInterestRequest:"

-- | @Selector@ for @startWithCompletionHandler:@
startWithCompletionHandlerSelector :: Selector
startWithCompletionHandlerSelector = mkSelector "startWithCompletionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @searching@
searchingSelector :: Selector
searchingSelector = mkSelector "searching"

