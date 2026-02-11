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
  , initWithMapItem_displaysMapSelector
  , initWithMapItemSelector
  , mapItemSelector
  , setMapItemSelector
  , delegateSelector
  , setDelegateSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithMapItem:displaysMap:@
initWithMapItem_displaysMap :: (IsMKMapItemDetailViewController mkMapItemDetailViewController, IsMKMapItem mapItem) => mkMapItemDetailViewController -> mapItem -> Bool -> IO (Id MKMapItemDetailViewController)
initWithMapItem_displaysMap mkMapItemDetailViewController  mapItem displaysMap =
  withObjCPtr mapItem $ \raw_mapItem ->
      sendMsg mkMapItemDetailViewController (mkSelector "initWithMapItem:displaysMap:") (retPtr retVoid) [argPtr (castPtr raw_mapItem :: Ptr ()), argCULong (if displaysMap then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithMapItem:@
initWithMapItem :: (IsMKMapItemDetailViewController mkMapItemDetailViewController, IsMKMapItem mapItem) => mkMapItemDetailViewController -> mapItem -> IO (Id MKMapItemDetailViewController)
initWithMapItem mkMapItemDetailViewController  mapItem =
  withObjCPtr mapItem $ \raw_mapItem ->
      sendMsg mkMapItemDetailViewController (mkSelector "initWithMapItem:") (retPtr retVoid) [argPtr (castPtr raw_mapItem :: Ptr ())] >>= ownedObject . castPtr

-- | @- mapItem@
mapItem :: IsMKMapItemDetailViewController mkMapItemDetailViewController => mkMapItemDetailViewController -> IO (Id MKMapItem)
mapItem mkMapItemDetailViewController  =
    sendMsg mkMapItemDetailViewController (mkSelector "mapItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMapItem:@
setMapItem :: (IsMKMapItemDetailViewController mkMapItemDetailViewController, IsMKMapItem value) => mkMapItemDetailViewController -> value -> IO ()
setMapItem mkMapItemDetailViewController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mkMapItemDetailViewController (mkSelector "setMapItem:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- delegate@
delegate :: IsMKMapItemDetailViewController mkMapItemDetailViewController => mkMapItemDetailViewController -> IO RawId
delegate mkMapItemDetailViewController  =
    fmap (RawId . castPtr) $ sendMsg mkMapItemDetailViewController (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsMKMapItemDetailViewController mkMapItemDetailViewController => mkMapItemDetailViewController -> RawId -> IO ()
setDelegate mkMapItemDetailViewController  value =
    sendMsg mkMapItemDetailViewController (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMapItem:displaysMap:@
initWithMapItem_displaysMapSelector :: Selector
initWithMapItem_displaysMapSelector = mkSelector "initWithMapItem:displaysMap:"

-- | @Selector@ for @initWithMapItem:@
initWithMapItemSelector :: Selector
initWithMapItemSelector = mkSelector "initWithMapItem:"

-- | @Selector@ for @mapItem@
mapItemSelector :: Selector
mapItemSelector = mkSelector "mapItem"

-- | @Selector@ for @setMapItem:@
setMapItemSelector :: Selector
setMapItemSelector = mkSelector "setMapItem:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

