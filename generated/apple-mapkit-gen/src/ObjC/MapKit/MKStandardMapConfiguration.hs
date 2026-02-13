{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKStandardMapConfiguration@.
module ObjC.MapKit.MKStandardMapConfiguration
  ( MKStandardMapConfiguration
  , IsMKStandardMapConfiguration(..)
  , init_
  , initWithElevationStyle
  , initWithElevationStyle_emphasisStyle
  , initWithEmphasisStyle
  , emphasisStyle
  , setEmphasisStyle
  , pointOfInterestFilter
  , setPointOfInterestFilter
  , showsTraffic
  , setShowsTraffic
  , emphasisStyleSelector
  , initSelector
  , initWithElevationStyleSelector
  , initWithElevationStyle_emphasisStyleSelector
  , initWithEmphasisStyleSelector
  , pointOfInterestFilterSelector
  , setEmphasisStyleSelector
  , setPointOfInterestFilterSelector
  , setShowsTrafficSelector
  , showsTrafficSelector

  -- * Enum types
  , MKMapElevationStyle(MKMapElevationStyle)
  , pattern MKMapElevationStyleFlat
  , pattern MKMapElevationStyleRealistic
  , MKStandardMapEmphasisStyle(MKStandardMapEmphasisStyle)
  , pattern MKStandardMapEmphasisStyleDefault
  , pattern MKStandardMapEmphasisStyleMuted

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
init_ :: IsMKStandardMapConfiguration mkStandardMapConfiguration => mkStandardMapConfiguration -> IO (Id MKStandardMapConfiguration)
init_ mkStandardMapConfiguration =
  sendOwnedMessage mkStandardMapConfiguration initSelector

-- | @- initWithElevationStyle:@
initWithElevationStyle :: IsMKStandardMapConfiguration mkStandardMapConfiguration => mkStandardMapConfiguration -> MKMapElevationStyle -> IO (Id MKStandardMapConfiguration)
initWithElevationStyle mkStandardMapConfiguration elevationStyle =
  sendOwnedMessage mkStandardMapConfiguration initWithElevationStyleSelector elevationStyle

-- | @- initWithElevationStyle:emphasisStyle:@
initWithElevationStyle_emphasisStyle :: IsMKStandardMapConfiguration mkStandardMapConfiguration => mkStandardMapConfiguration -> MKMapElevationStyle -> MKStandardMapEmphasisStyle -> IO (Id MKStandardMapConfiguration)
initWithElevationStyle_emphasisStyle mkStandardMapConfiguration elevationStyle emphasisStyle =
  sendOwnedMessage mkStandardMapConfiguration initWithElevationStyle_emphasisStyleSelector elevationStyle emphasisStyle

-- | @- initWithEmphasisStyle:@
initWithEmphasisStyle :: IsMKStandardMapConfiguration mkStandardMapConfiguration => mkStandardMapConfiguration -> MKStandardMapEmphasisStyle -> IO (Id MKStandardMapConfiguration)
initWithEmphasisStyle mkStandardMapConfiguration emphasisStyle =
  sendOwnedMessage mkStandardMapConfiguration initWithEmphasisStyleSelector emphasisStyle

-- | @- emphasisStyle@
emphasisStyle :: IsMKStandardMapConfiguration mkStandardMapConfiguration => mkStandardMapConfiguration -> IO MKStandardMapEmphasisStyle
emphasisStyle mkStandardMapConfiguration =
  sendMessage mkStandardMapConfiguration emphasisStyleSelector

-- | @- setEmphasisStyle:@
setEmphasisStyle :: IsMKStandardMapConfiguration mkStandardMapConfiguration => mkStandardMapConfiguration -> MKStandardMapEmphasisStyle -> IO ()
setEmphasisStyle mkStandardMapConfiguration value =
  sendMessage mkStandardMapConfiguration setEmphasisStyleSelector value

-- | @- pointOfInterestFilter@
pointOfInterestFilter :: IsMKStandardMapConfiguration mkStandardMapConfiguration => mkStandardMapConfiguration -> IO (Id MKPointOfInterestFilter)
pointOfInterestFilter mkStandardMapConfiguration =
  sendMessage mkStandardMapConfiguration pointOfInterestFilterSelector

-- | @- setPointOfInterestFilter:@
setPointOfInterestFilter :: (IsMKStandardMapConfiguration mkStandardMapConfiguration, IsMKPointOfInterestFilter value) => mkStandardMapConfiguration -> value -> IO ()
setPointOfInterestFilter mkStandardMapConfiguration value =
  sendMessage mkStandardMapConfiguration setPointOfInterestFilterSelector (toMKPointOfInterestFilter value)

-- | @- showsTraffic@
showsTraffic :: IsMKStandardMapConfiguration mkStandardMapConfiguration => mkStandardMapConfiguration -> IO Bool
showsTraffic mkStandardMapConfiguration =
  sendMessage mkStandardMapConfiguration showsTrafficSelector

-- | @- setShowsTraffic:@
setShowsTraffic :: IsMKStandardMapConfiguration mkStandardMapConfiguration => mkStandardMapConfiguration -> Bool -> IO ()
setShowsTraffic mkStandardMapConfiguration value =
  sendMessage mkStandardMapConfiguration setShowsTrafficSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MKStandardMapConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithElevationStyle:@
initWithElevationStyleSelector :: Selector '[MKMapElevationStyle] (Id MKStandardMapConfiguration)
initWithElevationStyleSelector = mkSelector "initWithElevationStyle:"

-- | @Selector@ for @initWithElevationStyle:emphasisStyle:@
initWithElevationStyle_emphasisStyleSelector :: Selector '[MKMapElevationStyle, MKStandardMapEmphasisStyle] (Id MKStandardMapConfiguration)
initWithElevationStyle_emphasisStyleSelector = mkSelector "initWithElevationStyle:emphasisStyle:"

-- | @Selector@ for @initWithEmphasisStyle:@
initWithEmphasisStyleSelector :: Selector '[MKStandardMapEmphasisStyle] (Id MKStandardMapConfiguration)
initWithEmphasisStyleSelector = mkSelector "initWithEmphasisStyle:"

-- | @Selector@ for @emphasisStyle@
emphasisStyleSelector :: Selector '[] MKStandardMapEmphasisStyle
emphasisStyleSelector = mkSelector "emphasisStyle"

-- | @Selector@ for @setEmphasisStyle:@
setEmphasisStyleSelector :: Selector '[MKStandardMapEmphasisStyle] ()
setEmphasisStyleSelector = mkSelector "setEmphasisStyle:"

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

