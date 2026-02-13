{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMapItemAnnotation@.
module ObjC.MapKit.MKMapItemAnnotation
  ( MKMapItemAnnotation
  , IsMKMapItemAnnotation(..)
  , new
  , init_
  , initWithMapItem
  , mapItem
  , initSelector
  , initWithMapItemSelector
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
new :: IO (Id MKMapItemAnnotation)
new  =
  do
    cls' <- getRequiredClass "MKMapItemAnnotation"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMKMapItemAnnotation mkMapItemAnnotation => mkMapItemAnnotation -> IO (Id MKMapItemAnnotation)
init_ mkMapItemAnnotation =
  sendOwnedMessage mkMapItemAnnotation initSelector

-- | @- initWithMapItem:@
initWithMapItem :: (IsMKMapItemAnnotation mkMapItemAnnotation, IsMKMapItem mapItem) => mkMapItemAnnotation -> mapItem -> IO (Id MKMapItemAnnotation)
initWithMapItem mkMapItemAnnotation mapItem =
  sendOwnedMessage mkMapItemAnnotation initWithMapItemSelector (toMKMapItem mapItem)

-- | @- mapItem@
mapItem :: IsMKMapItemAnnotation mkMapItemAnnotation => mkMapItemAnnotation -> IO (Id MKMapItem)
mapItem mkMapItemAnnotation =
  sendMessage mkMapItemAnnotation mapItemSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MKMapItemAnnotation)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MKMapItemAnnotation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithMapItem:@
initWithMapItemSelector :: Selector '[Id MKMapItem] (Id MKMapItemAnnotation)
initWithMapItemSelector = mkSelector "initWithMapItem:"

-- | @Selector@ for @mapItem@
mapItemSelector :: Selector '[] (Id MKMapItem)
mapItemSelector = mkSelector "mapItem"

