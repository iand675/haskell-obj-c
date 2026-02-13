{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKLookAroundSceneRequest@.
module ObjC.MapKit.MKLookAroundSceneRequest
  ( MKLookAroundSceneRequest
  , IsMKLookAroundSceneRequest(..)
  , new
  , init_
  , initWithMapItem
  , getSceneWithCompletionHandler
  , cancel
  , mapItem
  , cancelled
  , loading
  , cancelSelector
  , cancelledSelector
  , getSceneWithCompletionHandlerSelector
  , initSelector
  , initWithMapItemSelector
  , loadingSelector
  , mapItemSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MKLookAroundSceneRequest)
new  =
  do
    cls' <- getRequiredClass "MKLookAroundSceneRequest"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMKLookAroundSceneRequest mkLookAroundSceneRequest => mkLookAroundSceneRequest -> IO (Id MKLookAroundSceneRequest)
init_ mkLookAroundSceneRequest =
  sendOwnedMessage mkLookAroundSceneRequest initSelector

-- | @- initWithMapItem:@
initWithMapItem :: (IsMKLookAroundSceneRequest mkLookAroundSceneRequest, IsMKMapItem mapItem) => mkLookAroundSceneRequest -> mapItem -> IO (Id MKLookAroundSceneRequest)
initWithMapItem mkLookAroundSceneRequest mapItem =
  sendOwnedMessage mkLookAroundSceneRequest initWithMapItemSelector (toMKMapItem mapItem)

-- | @- getSceneWithCompletionHandler:@
getSceneWithCompletionHandler :: IsMKLookAroundSceneRequest mkLookAroundSceneRequest => mkLookAroundSceneRequest -> Ptr () -> IO ()
getSceneWithCompletionHandler mkLookAroundSceneRequest completionHandler =
  sendMessage mkLookAroundSceneRequest getSceneWithCompletionHandlerSelector completionHandler

-- | @- cancel@
cancel :: IsMKLookAroundSceneRequest mkLookAroundSceneRequest => mkLookAroundSceneRequest -> IO ()
cancel mkLookAroundSceneRequest =
  sendMessage mkLookAroundSceneRequest cancelSelector

-- | @- mapItem@
mapItem :: IsMKLookAroundSceneRequest mkLookAroundSceneRequest => mkLookAroundSceneRequest -> IO (Id MKMapItem)
mapItem mkLookAroundSceneRequest =
  sendMessage mkLookAroundSceneRequest mapItemSelector

-- | @- cancelled@
cancelled :: IsMKLookAroundSceneRequest mkLookAroundSceneRequest => mkLookAroundSceneRequest -> IO Bool
cancelled mkLookAroundSceneRequest =
  sendMessage mkLookAroundSceneRequest cancelledSelector

-- | @- loading@
loading :: IsMKLookAroundSceneRequest mkLookAroundSceneRequest => mkLookAroundSceneRequest -> IO Bool
loading mkLookAroundSceneRequest =
  sendMessage mkLookAroundSceneRequest loadingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MKLookAroundSceneRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MKLookAroundSceneRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithMapItem:@
initWithMapItemSelector :: Selector '[Id MKMapItem] (Id MKLookAroundSceneRequest)
initWithMapItemSelector = mkSelector "initWithMapItem:"

-- | @Selector@ for @getSceneWithCompletionHandler:@
getSceneWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
getSceneWithCompletionHandlerSelector = mkSelector "getSceneWithCompletionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @mapItem@
mapItemSelector :: Selector '[] (Id MKMapItem)
mapItemSelector = mkSelector "mapItem"

-- | @Selector@ for @cancelled@
cancelledSelector :: Selector '[] Bool
cancelledSelector = mkSelector "cancelled"

-- | @Selector@ for @loading@
loadingSelector :: Selector '[] Bool
loadingSelector = mkSelector "loading"

