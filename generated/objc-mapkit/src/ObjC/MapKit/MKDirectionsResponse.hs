{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKDirectionsResponse@.
module ObjC.MapKit.MKDirectionsResponse
  ( MKDirectionsResponse
  , IsMKDirectionsResponse(..)
  , source
  , destination
  , routes
  , sourceSelector
  , destinationSelector
  , routesSelector


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

-- | @- source@
source :: IsMKDirectionsResponse mkDirectionsResponse => mkDirectionsResponse -> IO (Id MKMapItem)
source mkDirectionsResponse  =
  sendMsg mkDirectionsResponse (mkSelector "source") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- destination@
destination :: IsMKDirectionsResponse mkDirectionsResponse => mkDirectionsResponse -> IO (Id MKMapItem)
destination mkDirectionsResponse  =
  sendMsg mkDirectionsResponse (mkSelector "destination") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- routes@
routes :: IsMKDirectionsResponse mkDirectionsResponse => mkDirectionsResponse -> IO (Id NSArray)
routes mkDirectionsResponse  =
  sendMsg mkDirectionsResponse (mkSelector "routes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @source@
sourceSelector :: Selector
sourceSelector = mkSelector "source"

-- | @Selector@ for @destination@
destinationSelector :: Selector
destinationSelector = mkSelector "destination"

-- | @Selector@ for @routes@
routesSelector :: Selector
routesSelector = mkSelector "routes"

