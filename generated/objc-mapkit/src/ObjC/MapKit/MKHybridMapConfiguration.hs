{-# LANGUAGE PatternSynonyms #-}
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
  , showsTrafficSelector
  , setShowsTrafficSelector

  -- * Enum types
  , MKMapElevationStyle(MKMapElevationStyle)
  , pattern MKMapElevationStyleFlat
  , pattern MKMapElevationStyleRealistic

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
init_ :: IsMKHybridMapConfiguration mkHybridMapConfiguration => mkHybridMapConfiguration -> IO (Id MKHybridMapConfiguration)
init_ mkHybridMapConfiguration  =
  sendMsg mkHybridMapConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithElevationStyle:@
initWithElevationStyle :: IsMKHybridMapConfiguration mkHybridMapConfiguration => mkHybridMapConfiguration -> MKMapElevationStyle -> IO (Id MKHybridMapConfiguration)
initWithElevationStyle mkHybridMapConfiguration  elevationStyle =
  sendMsg mkHybridMapConfiguration (mkSelector "initWithElevationStyle:") (retPtr retVoid) [argCLong (coerce elevationStyle)] >>= ownedObject . castPtr

-- | @- pointOfInterestFilter@
pointOfInterestFilter :: IsMKHybridMapConfiguration mkHybridMapConfiguration => mkHybridMapConfiguration -> IO (Id MKPointOfInterestFilter)
pointOfInterestFilter mkHybridMapConfiguration  =
  sendMsg mkHybridMapConfiguration (mkSelector "pointOfInterestFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPointOfInterestFilter:@
setPointOfInterestFilter :: (IsMKHybridMapConfiguration mkHybridMapConfiguration, IsMKPointOfInterestFilter value) => mkHybridMapConfiguration -> value -> IO ()
setPointOfInterestFilter mkHybridMapConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkHybridMapConfiguration (mkSelector "setPointOfInterestFilter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- showsTraffic@
showsTraffic :: IsMKHybridMapConfiguration mkHybridMapConfiguration => mkHybridMapConfiguration -> IO Bool
showsTraffic mkHybridMapConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkHybridMapConfiguration (mkSelector "showsTraffic") retCULong []

-- | @- setShowsTraffic:@
setShowsTraffic :: IsMKHybridMapConfiguration mkHybridMapConfiguration => mkHybridMapConfiguration -> Bool -> IO ()
setShowsTraffic mkHybridMapConfiguration  value =
  sendMsg mkHybridMapConfiguration (mkSelector "setShowsTraffic:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithElevationStyle:@
initWithElevationStyleSelector :: Selector
initWithElevationStyleSelector = mkSelector "initWithElevationStyle:"

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

