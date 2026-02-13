{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMapItemDetailViewController@.
module ObjC.MapKit.MKMapItemDetailViewController
  ( MKMapItemDetailViewController
  , IsMKMapItemDetailViewController(..)
  , initWithMapItem_displaysMap
  , initWithMapItem
  , mapItem
  , setMapItem
  , delegate
  , setDelegate
  , delegateSelector
  , initWithMapItemSelector
  , initWithMapItem_displaysMapSelector
  , mapItemSelector
  , setDelegateSelector
  , setMapItemSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithMapItem:displaysMap:@
initWithMapItem_displaysMap :: (IsMKMapItemDetailViewController mkMapItemDetailViewController, IsMKMapItem mapItem) => mkMapItemDetailViewController -> mapItem -> Bool -> IO (Id MKMapItemDetailViewController)
initWithMapItem_displaysMap mkMapItemDetailViewController mapItem displaysMap =
  sendOwnedMessage mkMapItemDetailViewController initWithMapItem_displaysMapSelector (toMKMapItem mapItem) displaysMap

-- | @- initWithMapItem:@
initWithMapItem :: (IsMKMapItemDetailViewController mkMapItemDetailViewController, IsMKMapItem mapItem) => mkMapItemDetailViewController -> mapItem -> IO (Id MKMapItemDetailViewController)
initWithMapItem mkMapItemDetailViewController mapItem =
  sendOwnedMessage mkMapItemDetailViewController initWithMapItemSelector (toMKMapItem mapItem)

-- | @- mapItem@
mapItem :: IsMKMapItemDetailViewController mkMapItemDetailViewController => mkMapItemDetailViewController -> IO (Id MKMapItem)
mapItem mkMapItemDetailViewController =
  sendMessage mkMapItemDetailViewController mapItemSelector

-- | @- setMapItem:@
setMapItem :: (IsMKMapItemDetailViewController mkMapItemDetailViewController, IsMKMapItem value) => mkMapItemDetailViewController -> value -> IO ()
setMapItem mkMapItemDetailViewController value =
  sendMessage mkMapItemDetailViewController setMapItemSelector (toMKMapItem value)

-- | @- delegate@
delegate :: IsMKMapItemDetailViewController mkMapItemDetailViewController => mkMapItemDetailViewController -> IO RawId
delegate mkMapItemDetailViewController =
  sendMessage mkMapItemDetailViewController delegateSelector

-- | @- setDelegate:@
setDelegate :: IsMKMapItemDetailViewController mkMapItemDetailViewController => mkMapItemDetailViewController -> RawId -> IO ()
setDelegate mkMapItemDetailViewController value =
  sendMessage mkMapItemDetailViewController setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMapItem:displaysMap:@
initWithMapItem_displaysMapSelector :: Selector '[Id MKMapItem, Bool] (Id MKMapItemDetailViewController)
initWithMapItem_displaysMapSelector = mkSelector "initWithMapItem:displaysMap:"

-- | @Selector@ for @initWithMapItem:@
initWithMapItemSelector :: Selector '[Id MKMapItem] (Id MKMapItemDetailViewController)
initWithMapItemSelector = mkSelector "initWithMapItem:"

-- | @Selector@ for @mapItem@
mapItemSelector :: Selector '[] (Id MKMapItem)
mapItemSelector = mkSelector "mapItem"

-- | @Selector@ for @setMapItem:@
setMapItemSelector :: Selector '[Id MKMapItem] ()
setMapItemSelector = mkSelector "setMapItem:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

