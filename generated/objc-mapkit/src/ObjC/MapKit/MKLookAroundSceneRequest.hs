{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKLookAroundSceneRequest@.
module ObjC.MapKit.MKLookAroundSceneRequest
  ( MKLookAroundSceneRequest
  , IsMKLookAroundSceneRequest(..)
  , new
  , init_
  , initWithCoordinate
  , initWithMapItem
  , getSceneWithCompletionHandler
  , cancel
  , coordinate
  , mapItem
  , cancelled
  , loading
  , newSelector
  , initSelector
  , initWithCoordinateSelector
  , initWithMapItemSelector
  , getSceneWithCompletionHandlerSelector
  , cancelSelector
  , coordinateSelector
  , mapItemSelector
  , cancelledSelector
  , loadingSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.CoreLocation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MKLookAroundSceneRequest)
new  =
  do
    cls' <- getRequiredClass "MKLookAroundSceneRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMKLookAroundSceneRequest mkLookAroundSceneRequest => mkLookAroundSceneRequest -> IO (Id MKLookAroundSceneRequest)
init_ mkLookAroundSceneRequest  =
  sendMsg mkLookAroundSceneRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoordinate:@
initWithCoordinate :: IsMKLookAroundSceneRequest mkLookAroundSceneRequest => mkLookAroundSceneRequest -> CLLocationCoordinate2D -> IO (Id MKLookAroundSceneRequest)
initWithCoordinate mkLookAroundSceneRequest  coordinate =
  sendMsg mkLookAroundSceneRequest (mkSelector "initWithCoordinate:") (retPtr retVoid) [argCLLocationCoordinate2D coordinate] >>= ownedObject . castPtr

-- | @- initWithMapItem:@
initWithMapItem :: (IsMKLookAroundSceneRequest mkLookAroundSceneRequest, IsMKMapItem mapItem) => mkLookAroundSceneRequest -> mapItem -> IO (Id MKLookAroundSceneRequest)
initWithMapItem mkLookAroundSceneRequest  mapItem =
withObjCPtr mapItem $ \raw_mapItem ->
    sendMsg mkLookAroundSceneRequest (mkSelector "initWithMapItem:") (retPtr retVoid) [argPtr (castPtr raw_mapItem :: Ptr ())] >>= ownedObject . castPtr

-- | @- getSceneWithCompletionHandler:@
getSceneWithCompletionHandler :: IsMKLookAroundSceneRequest mkLookAroundSceneRequest => mkLookAroundSceneRequest -> Ptr () -> IO ()
getSceneWithCompletionHandler mkLookAroundSceneRequest  completionHandler =
  sendMsg mkLookAroundSceneRequest (mkSelector "getSceneWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- cancel@
cancel :: IsMKLookAroundSceneRequest mkLookAroundSceneRequest => mkLookAroundSceneRequest -> IO ()
cancel mkLookAroundSceneRequest  =
  sendMsg mkLookAroundSceneRequest (mkSelector "cancel") retVoid []

-- | @- coordinate@
coordinate :: IsMKLookAroundSceneRequest mkLookAroundSceneRequest => mkLookAroundSceneRequest -> IO CLLocationCoordinate2D
coordinate mkLookAroundSceneRequest  =
  sendMsgStret mkLookAroundSceneRequest (mkSelector "coordinate") retCLLocationCoordinate2D []

-- | @- mapItem@
mapItem :: IsMKLookAroundSceneRequest mkLookAroundSceneRequest => mkLookAroundSceneRequest -> IO (Id MKMapItem)
mapItem mkLookAroundSceneRequest  =
  sendMsg mkLookAroundSceneRequest (mkSelector "mapItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- cancelled@
cancelled :: IsMKLookAroundSceneRequest mkLookAroundSceneRequest => mkLookAroundSceneRequest -> IO Bool
cancelled mkLookAroundSceneRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkLookAroundSceneRequest (mkSelector "cancelled") retCULong []

-- | @- loading@
loading :: IsMKLookAroundSceneRequest mkLookAroundSceneRequest => mkLookAroundSceneRequest -> IO Bool
loading mkLookAroundSceneRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkLookAroundSceneRequest (mkSelector "loading") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoordinate:@
initWithCoordinateSelector :: Selector
initWithCoordinateSelector = mkSelector "initWithCoordinate:"

-- | @Selector@ for @initWithMapItem:@
initWithMapItemSelector :: Selector
initWithMapItemSelector = mkSelector "initWithMapItem:"

-- | @Selector@ for @getSceneWithCompletionHandler:@
getSceneWithCompletionHandlerSelector :: Selector
getSceneWithCompletionHandlerSelector = mkSelector "getSceneWithCompletionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @coordinate@
coordinateSelector :: Selector
coordinateSelector = mkSelector "coordinate"

-- | @Selector@ for @mapItem@
mapItemSelector :: Selector
mapItemSelector = mkSelector "mapItem"

-- | @Selector@ for @cancelled@
cancelledSelector :: Selector
cancelledSelector = mkSelector "cancelled"

-- | @Selector@ for @loading@
loadingSelector :: Selector
loadingSelector = mkSelector "loading"

