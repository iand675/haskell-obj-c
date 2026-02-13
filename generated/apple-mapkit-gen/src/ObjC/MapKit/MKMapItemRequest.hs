{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMapItemRequest@.
module ObjC.MapKit.MKMapItemRequest
  ( MKMapItemRequest
  , IsMKMapItemRequest(..)
  , init_
  , new
  , initWithMapItemIdentifier
  , initWithMapFeatureAnnotation
  , getMapItemWithCompletionHandler
  , cancel
  , mapItemIdentifier
  , mapFeatureAnnotation
  , featureAnnotation
  , cancelled
  , loading
  , cancelSelector
  , cancelledSelector
  , featureAnnotationSelector
  , getMapItemWithCompletionHandlerSelector
  , initSelector
  , initWithMapFeatureAnnotationSelector
  , initWithMapItemIdentifierSelector
  , loadingSelector
  , mapFeatureAnnotationSelector
  , mapItemIdentifierSelector
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

-- | @- init@
init_ :: IsMKMapItemRequest mkMapItemRequest => mkMapItemRequest -> IO (Id MKMapItemRequest)
init_ mkMapItemRequest =
  sendOwnedMessage mkMapItemRequest initSelector

-- | @+ new@
new :: IO (Id MKMapItemRequest)
new  =
  do
    cls' <- getRequiredClass "MKMapItemRequest"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithMapItemIdentifier:@
initWithMapItemIdentifier :: (IsMKMapItemRequest mkMapItemRequest, IsMKMapItemIdentifier identifier) => mkMapItemRequest -> identifier -> IO (Id MKMapItemRequest)
initWithMapItemIdentifier mkMapItemRequest identifier =
  sendOwnedMessage mkMapItemRequest initWithMapItemIdentifierSelector (toMKMapItemIdentifier identifier)

-- | @- initWithMapFeatureAnnotation:@
initWithMapFeatureAnnotation :: IsMKMapItemRequest mkMapItemRequest => mkMapItemRequest -> RawId -> IO (Id MKMapItemRequest)
initWithMapFeatureAnnotation mkMapItemRequest mapFeatureAnnotation =
  sendOwnedMessage mkMapItemRequest initWithMapFeatureAnnotationSelector mapFeatureAnnotation

-- | @- getMapItemWithCompletionHandler:@
getMapItemWithCompletionHandler :: IsMKMapItemRequest mkMapItemRequest => mkMapItemRequest -> Ptr () -> IO ()
getMapItemWithCompletionHandler mkMapItemRequest completionHandler =
  sendMessage mkMapItemRequest getMapItemWithCompletionHandlerSelector completionHandler

-- | @- cancel@
cancel :: IsMKMapItemRequest mkMapItemRequest => mkMapItemRequest -> IO ()
cancel mkMapItemRequest =
  sendMessage mkMapItemRequest cancelSelector

-- | @- mapItemIdentifier@
mapItemIdentifier :: IsMKMapItemRequest mkMapItemRequest => mkMapItemRequest -> IO (Id MKMapItemIdentifier)
mapItemIdentifier mkMapItemRequest =
  sendMessage mkMapItemRequest mapItemIdentifierSelector

-- | @- mapFeatureAnnotation@
mapFeatureAnnotation :: IsMKMapItemRequest mkMapItemRequest => mkMapItemRequest -> IO RawId
mapFeatureAnnotation mkMapItemRequest =
  sendMessage mkMapItemRequest mapFeatureAnnotationSelector

-- | @- featureAnnotation@
featureAnnotation :: IsMKMapItemRequest mkMapItemRequest => mkMapItemRequest -> IO RawId
featureAnnotation mkMapItemRequest =
  sendMessage mkMapItemRequest featureAnnotationSelector

-- | @- cancelled@
cancelled :: IsMKMapItemRequest mkMapItemRequest => mkMapItemRequest -> IO Bool
cancelled mkMapItemRequest =
  sendMessage mkMapItemRequest cancelledSelector

-- | @- loading@
loading :: IsMKMapItemRequest mkMapItemRequest => mkMapItemRequest -> IO Bool
loading mkMapItemRequest =
  sendMessage mkMapItemRequest loadingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MKMapItemRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MKMapItemRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithMapItemIdentifier:@
initWithMapItemIdentifierSelector :: Selector '[Id MKMapItemIdentifier] (Id MKMapItemRequest)
initWithMapItemIdentifierSelector = mkSelector "initWithMapItemIdentifier:"

-- | @Selector@ for @initWithMapFeatureAnnotation:@
initWithMapFeatureAnnotationSelector :: Selector '[RawId] (Id MKMapItemRequest)
initWithMapFeatureAnnotationSelector = mkSelector "initWithMapFeatureAnnotation:"

-- | @Selector@ for @getMapItemWithCompletionHandler:@
getMapItemWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
getMapItemWithCompletionHandlerSelector = mkSelector "getMapItemWithCompletionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @mapItemIdentifier@
mapItemIdentifierSelector :: Selector '[] (Id MKMapItemIdentifier)
mapItemIdentifierSelector = mkSelector "mapItemIdentifier"

-- | @Selector@ for @mapFeatureAnnotation@
mapFeatureAnnotationSelector :: Selector '[] RawId
mapFeatureAnnotationSelector = mkSelector "mapFeatureAnnotation"

-- | @Selector@ for @featureAnnotation@
featureAnnotationSelector :: Selector '[] RawId
featureAnnotationSelector = mkSelector "featureAnnotation"

-- | @Selector@ for @cancelled@
cancelledSelector :: Selector '[] Bool
cancelledSelector = mkSelector "cancelled"

-- | @Selector@ for @loading@
loadingSelector :: Selector '[] Bool
loadingSelector = mkSelector "loading"

