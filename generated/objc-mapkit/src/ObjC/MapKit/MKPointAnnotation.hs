{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKPointAnnotation@.
module ObjC.MapKit.MKPointAnnotation
  ( MKPointAnnotation
  , IsMKPointAnnotation(..)
  , init_
  , initWithCoordinate
  , initWithCoordinate_title_subtitle
  , coordinate
  , setCoordinate
  , initSelector
  , initWithCoordinateSelector
  , initWithCoordinate_title_subtitleSelector
  , coordinateSelector
  , setCoordinateSelector


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

-- | @- init@
init_ :: IsMKPointAnnotation mkPointAnnotation => mkPointAnnotation -> IO (Id MKPointAnnotation)
init_ mkPointAnnotation  =
  sendMsg mkPointAnnotation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoordinate:@
initWithCoordinate :: IsMKPointAnnotation mkPointAnnotation => mkPointAnnotation -> CLLocationCoordinate2D -> IO (Id MKPointAnnotation)
initWithCoordinate mkPointAnnotation  coordinate =
  sendMsg mkPointAnnotation (mkSelector "initWithCoordinate:") (retPtr retVoid) [argCLLocationCoordinate2D coordinate] >>= ownedObject . castPtr

-- | @- initWithCoordinate:title:subtitle:@
initWithCoordinate_title_subtitle :: (IsMKPointAnnotation mkPointAnnotation, IsNSString title, IsNSString subtitle) => mkPointAnnotation -> CLLocationCoordinate2D -> title -> subtitle -> IO (Id MKPointAnnotation)
initWithCoordinate_title_subtitle mkPointAnnotation  coordinate title subtitle =
withObjCPtr title $ \raw_title ->
  withObjCPtr subtitle $ \raw_subtitle ->
      sendMsg mkPointAnnotation (mkSelector "initWithCoordinate:title:subtitle:") (retPtr retVoid) [argCLLocationCoordinate2D coordinate, argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_subtitle :: Ptr ())] >>= ownedObject . castPtr

-- | @- coordinate@
coordinate :: IsMKPointAnnotation mkPointAnnotation => mkPointAnnotation -> IO CLLocationCoordinate2D
coordinate mkPointAnnotation  =
  sendMsgStret mkPointAnnotation (mkSelector "coordinate") retCLLocationCoordinate2D []

-- | @- setCoordinate:@
setCoordinate :: IsMKPointAnnotation mkPointAnnotation => mkPointAnnotation -> CLLocationCoordinate2D -> IO ()
setCoordinate mkPointAnnotation  value =
  sendMsg mkPointAnnotation (mkSelector "setCoordinate:") retVoid [argCLLocationCoordinate2D value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoordinate:@
initWithCoordinateSelector :: Selector
initWithCoordinateSelector = mkSelector "initWithCoordinate:"

-- | @Selector@ for @initWithCoordinate:title:subtitle:@
initWithCoordinate_title_subtitleSelector :: Selector
initWithCoordinate_title_subtitleSelector = mkSelector "initWithCoordinate:title:subtitle:"

-- | @Selector@ for @coordinate@
coordinateSelector :: Selector
coordinateSelector = mkSelector "coordinate"

-- | @Selector@ for @setCoordinate:@
setCoordinateSelector :: Selector
setCoordinateSelector = mkSelector "setCoordinate:"

