{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithElevationStyleSelector
  , initWithElevationStyle_emphasisStyleSelector
  , initWithEmphasisStyleSelector
  , emphasisStyleSelector
  , setEmphasisStyleSelector
  , pointOfInterestFilterSelector
  , setPointOfInterestFilterSelector
  , showsTrafficSelector
  , setShowsTrafficSelector

  -- * Enum types
  , MKMapElevationStyle(MKMapElevationStyle)
  , pattern MKMapElevationStyleFlat
  , pattern MKMapElevationStyleRealistic
  , MKStandardMapEmphasisStyle(MKStandardMapEmphasisStyle)
  , pattern MKStandardMapEmphasisStyleDefault
  , pattern MKStandardMapEmphasisStyleMuted

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
import ObjC.MapKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMKStandardMapConfiguration mkStandardMapConfiguration => mkStandardMapConfiguration -> IO (Id MKStandardMapConfiguration)
init_ mkStandardMapConfiguration  =
  sendMsg mkStandardMapConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithElevationStyle:@
initWithElevationStyle :: IsMKStandardMapConfiguration mkStandardMapConfiguration => mkStandardMapConfiguration -> MKMapElevationStyle -> IO (Id MKStandardMapConfiguration)
initWithElevationStyle mkStandardMapConfiguration  elevationStyle =
  sendMsg mkStandardMapConfiguration (mkSelector "initWithElevationStyle:") (retPtr retVoid) [argCLong (coerce elevationStyle)] >>= ownedObject . castPtr

-- | @- initWithElevationStyle:emphasisStyle:@
initWithElevationStyle_emphasisStyle :: IsMKStandardMapConfiguration mkStandardMapConfiguration => mkStandardMapConfiguration -> MKMapElevationStyle -> MKStandardMapEmphasisStyle -> IO (Id MKStandardMapConfiguration)
initWithElevationStyle_emphasisStyle mkStandardMapConfiguration  elevationStyle emphasisStyle =
  sendMsg mkStandardMapConfiguration (mkSelector "initWithElevationStyle:emphasisStyle:") (retPtr retVoid) [argCLong (coerce elevationStyle), argCLong (coerce emphasisStyle)] >>= ownedObject . castPtr

-- | @- initWithEmphasisStyle:@
initWithEmphasisStyle :: IsMKStandardMapConfiguration mkStandardMapConfiguration => mkStandardMapConfiguration -> MKStandardMapEmphasisStyle -> IO (Id MKStandardMapConfiguration)
initWithEmphasisStyle mkStandardMapConfiguration  emphasisStyle =
  sendMsg mkStandardMapConfiguration (mkSelector "initWithEmphasisStyle:") (retPtr retVoid) [argCLong (coerce emphasisStyle)] >>= ownedObject . castPtr

-- | @- emphasisStyle@
emphasisStyle :: IsMKStandardMapConfiguration mkStandardMapConfiguration => mkStandardMapConfiguration -> IO MKStandardMapEmphasisStyle
emphasisStyle mkStandardMapConfiguration  =
  fmap (coerce :: CLong -> MKStandardMapEmphasisStyle) $ sendMsg mkStandardMapConfiguration (mkSelector "emphasisStyle") retCLong []

-- | @- setEmphasisStyle:@
setEmphasisStyle :: IsMKStandardMapConfiguration mkStandardMapConfiguration => mkStandardMapConfiguration -> MKStandardMapEmphasisStyle -> IO ()
setEmphasisStyle mkStandardMapConfiguration  value =
  sendMsg mkStandardMapConfiguration (mkSelector "setEmphasisStyle:") retVoid [argCLong (coerce value)]

-- | @- pointOfInterestFilter@
pointOfInterestFilter :: IsMKStandardMapConfiguration mkStandardMapConfiguration => mkStandardMapConfiguration -> IO (Id MKPointOfInterestFilter)
pointOfInterestFilter mkStandardMapConfiguration  =
  sendMsg mkStandardMapConfiguration (mkSelector "pointOfInterestFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPointOfInterestFilter:@
setPointOfInterestFilter :: (IsMKStandardMapConfiguration mkStandardMapConfiguration, IsMKPointOfInterestFilter value) => mkStandardMapConfiguration -> value -> IO ()
setPointOfInterestFilter mkStandardMapConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkStandardMapConfiguration (mkSelector "setPointOfInterestFilter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- showsTraffic@
showsTraffic :: IsMKStandardMapConfiguration mkStandardMapConfiguration => mkStandardMapConfiguration -> IO Bool
showsTraffic mkStandardMapConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkStandardMapConfiguration (mkSelector "showsTraffic") retCULong []

-- | @- setShowsTraffic:@
setShowsTraffic :: IsMKStandardMapConfiguration mkStandardMapConfiguration => mkStandardMapConfiguration -> Bool -> IO ()
setShowsTraffic mkStandardMapConfiguration  value =
  sendMsg mkStandardMapConfiguration (mkSelector "setShowsTraffic:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithElevationStyle:@
initWithElevationStyleSelector :: Selector
initWithElevationStyleSelector = mkSelector "initWithElevationStyle:"

-- | @Selector@ for @initWithElevationStyle:emphasisStyle:@
initWithElevationStyle_emphasisStyleSelector :: Selector
initWithElevationStyle_emphasisStyleSelector = mkSelector "initWithElevationStyle:emphasisStyle:"

-- | @Selector@ for @initWithEmphasisStyle:@
initWithEmphasisStyleSelector :: Selector
initWithEmphasisStyleSelector = mkSelector "initWithEmphasisStyle:"

-- | @Selector@ for @emphasisStyle@
emphasisStyleSelector :: Selector
emphasisStyleSelector = mkSelector "emphasisStyle"

-- | @Selector@ for @setEmphasisStyle:@
setEmphasisStyleSelector :: Selector
setEmphasisStyleSelector = mkSelector "setEmphasisStyle:"

-- | @Selector@ for @pointOfInterestFilter@
pointOfInterestFilterSelector :: Selector
pointOfInterestFilterSelector = mkSelector "pointOfInterestFilter"

-- | @Selector@ for @setPointOfInterestFilter:@
setPointOfInterestFilterSelector :: Selector
setPointOfInterestFilterSelector = mkSelector "setPointOfInterestFilter:"

-- | @Selector@ for @showsTraffic@
showsTrafficSelector :: Selector
showsTrafficSelector = mkSelector "showsTraffic"

-- | @Selector@ for @setShowsTraffic:@
setShowsTrafficSelector :: Selector
setShowsTrafficSelector = mkSelector "setShowsTraffic:"

