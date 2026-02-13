{-# LANGUAGE DataKinds #-}
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
  , destinationSelector
  , routesSelector
  , sourceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- source@
source :: IsMKDirectionsResponse mkDirectionsResponse => mkDirectionsResponse -> IO (Id MKMapItem)
source mkDirectionsResponse =
  sendMessage mkDirectionsResponse sourceSelector

-- | @- destination@
destination :: IsMKDirectionsResponse mkDirectionsResponse => mkDirectionsResponse -> IO (Id MKMapItem)
destination mkDirectionsResponse =
  sendMessage mkDirectionsResponse destinationSelector

-- | @- routes@
routes :: IsMKDirectionsResponse mkDirectionsResponse => mkDirectionsResponse -> IO (Id NSArray)
routes mkDirectionsResponse =
  sendMessage mkDirectionsResponse routesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @source@
sourceSelector :: Selector '[] (Id MKMapItem)
sourceSelector = mkSelector "source"

-- | @Selector@ for @destination@
destinationSelector :: Selector '[] (Id MKMapItem)
destinationSelector = mkSelector "destination"

-- | @Selector@ for @routes@
routesSelector :: Selector '[] (Id NSArray)
routesSelector = mkSelector "routes"

