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
  , newSelector
  , initSelector
  , initWithMapItemSelector
  , mapItemSelector


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

-- | @+ new@
new :: IO (Id MKMapItemAnnotation)
new  =
  do
    cls' <- getRequiredClass "MKMapItemAnnotation"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMKMapItemAnnotation mkMapItemAnnotation => mkMapItemAnnotation -> IO (Id MKMapItemAnnotation)
init_ mkMapItemAnnotation  =
  sendMsg mkMapItemAnnotation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithMapItem:@
initWithMapItem :: (IsMKMapItemAnnotation mkMapItemAnnotation, IsMKMapItem mapItem) => mkMapItemAnnotation -> mapItem -> IO (Id MKMapItemAnnotation)
initWithMapItem mkMapItemAnnotation  mapItem =
withObjCPtr mapItem $ \raw_mapItem ->
    sendMsg mkMapItemAnnotation (mkSelector "initWithMapItem:") (retPtr retVoid) [argPtr (castPtr raw_mapItem :: Ptr ())] >>= ownedObject . castPtr

-- | @- mapItem@
mapItem :: IsMKMapItemAnnotation mkMapItemAnnotation => mkMapItemAnnotation -> IO (Id MKMapItem)
mapItem mkMapItemAnnotation  =
  sendMsg mkMapItemAnnotation (mkSelector "mapItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithMapItem:@
initWithMapItemSelector :: Selector
initWithMapItemSelector = mkSelector "initWithMapItem:"

-- | @Selector@ for @mapItem@
mapItemSelector :: Selector
mapItemSelector = mkSelector "mapItem"

