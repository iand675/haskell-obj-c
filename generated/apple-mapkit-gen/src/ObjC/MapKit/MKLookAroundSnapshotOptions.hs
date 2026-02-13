{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKLookAroundSnapshotOptions@.
module ObjC.MapKit.MKLookAroundSnapshotOptions
  ( MKLookAroundSnapshotOptions
  , IsMKLookAroundSnapshotOptions(..)
  , pointOfInterestFilter
  , setPointOfInterestFilter
  , pointOfInterestFilterSelector
  , setPointOfInterestFilterSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- pointOfInterestFilter@
pointOfInterestFilter :: IsMKLookAroundSnapshotOptions mkLookAroundSnapshotOptions => mkLookAroundSnapshotOptions -> IO (Id MKPointOfInterestFilter)
pointOfInterestFilter mkLookAroundSnapshotOptions =
  sendMessage mkLookAroundSnapshotOptions pointOfInterestFilterSelector

-- | @- setPointOfInterestFilter:@
setPointOfInterestFilter :: (IsMKLookAroundSnapshotOptions mkLookAroundSnapshotOptions, IsMKPointOfInterestFilter value) => mkLookAroundSnapshotOptions -> value -> IO ()
setPointOfInterestFilter mkLookAroundSnapshotOptions value =
  sendMessage mkLookAroundSnapshotOptions setPointOfInterestFilterSelector (toMKPointOfInterestFilter value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pointOfInterestFilter@
pointOfInterestFilterSelector :: Selector '[] (Id MKPointOfInterestFilter)
pointOfInterestFilterSelector = mkSelector "pointOfInterestFilter"

-- | @Selector@ for @setPointOfInterestFilter:@
setPointOfInterestFilterSelector :: Selector '[Id MKPointOfInterestFilter] ()
setPointOfInterestFilterSelector = mkSelector "setPointOfInterestFilter:"

