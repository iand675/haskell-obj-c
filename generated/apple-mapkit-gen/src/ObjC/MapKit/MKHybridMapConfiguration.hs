{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKHybridMapConfiguration@.
module ObjC.MapKit.MKHybridMapConfiguration
  ( MKHybridMapConfiguration
  , IsMKHybridMapConfiguration(..)
  , init_
  , initWithElevationStyle
  , pointOfInterestFilter
  , setPointOfInterestFilter
  , showsTraffic
  , setShowsTraffic
  , initSelector
  , initWithElevationStyleSelector
  , pointOfInterestFilterSelector
  , setPointOfInterestFilterSelector
  , setShowsTrafficSelector
  , showsTrafficSelector

  -- * Enum types
  , MKMapElevationStyle(MKMapElevationStyle)
  , pattern MKMapElevationStyleFlat
  , pattern MKMapElevationStyleRealistic

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.MapKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMKHybridMapConfiguration mkHybridMapConfiguration => mkHybridMapConfiguration -> IO (Id MKHybridMapConfiguration)
init_ mkHybridMapConfiguration =
  sendOwnedMessage mkHybridMapConfiguration initSelector

-- | @- initWithElevationStyle:@
initWithElevationStyle :: IsMKHybridMapConfiguration mkHybridMapConfiguration => mkHybridMapConfiguration -> MKMapElevationStyle -> IO (Id MKHybridMapConfiguration)
initWithElevationStyle mkHybridMapConfiguration elevationStyle =
  sendOwnedMessage mkHybridMapConfiguration initWithElevationStyleSelector elevationStyle

-- | @- pointOfInterestFilter@
pointOfInterestFilter :: IsMKHybridMapConfiguration mkHybridMapConfiguration => mkHybridMapConfiguration -> IO (Id MKPointOfInterestFilter)
pointOfInterestFilter mkHybridMapConfiguration =
  sendMessage mkHybridMapConfiguration pointOfInterestFilterSelector

-- | @- setPointOfInterestFilter:@
setPointOfInterestFilter :: (IsMKHybridMapConfiguration mkHybridMapConfiguration, IsMKPointOfInterestFilter value) => mkHybridMapConfiguration -> value -> IO ()
setPointOfInterestFilter mkHybridMapConfiguration value =
  sendMessage mkHybridMapConfiguration setPointOfInterestFilterSelector (toMKPointOfInterestFilter value)

-- | @- showsTraffic@
showsTraffic :: IsMKHybridMapConfiguration mkHybridMapConfiguration => mkHybridMapConfiguration -> IO Bool
showsTraffic mkHybridMapConfiguration =
  sendMessage mkHybridMapConfiguration showsTrafficSelector

-- | @- setShowsTraffic:@
setShowsTraffic :: IsMKHybridMapConfiguration mkHybridMapConfiguration => mkHybridMapConfiguration -> Bool -> IO ()
setShowsTraffic mkHybridMapConfiguration value =
  sendMessage mkHybridMapConfiguration setShowsTrafficSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MKHybridMapConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithElevationStyle:@
initWithElevationStyleSelector :: Selector '[MKMapElevationStyle] (Id MKHybridMapConfiguration)
initWithElevationStyleSelector = mkSelector "initWithElevationStyle:"

-- | @Selector@ for @pointOfInterestFilter@
pointOfInterestFilterSelector :: Selector '[] (Id MKPointOfInterestFilter)
pointOfInterestFilterSelector = mkSelector "pointOfInterestFilter"

-- | @Selector@ for @setPointOfInterestFilter:@
setPointOfInterestFilterSelector :: Selector '[Id MKPointOfInterestFilter] ()
setPointOfInterestFilterSelector = mkSelector "setPointOfInterestFilter:"

-- | @Selector@ for @showsTraffic@
showsTrafficSelector :: Selector '[] Bool
showsTrafficSelector = mkSelector "showsTraffic"

-- | @Selector@ for @setShowsTraffic:@
setShowsTrafficSelector :: Selector '[Bool] ()
setShowsTrafficSelector = mkSelector "setShowsTraffic:"

