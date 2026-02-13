{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKLookAroundViewController@.
module ObjC.MapKit.MKLookAroundViewController
  ( MKLookAroundViewController
  , IsMKLookAroundViewController(..)
  , initWithScene
  , initWithNibName_bundle
  , initWithCoder
  , delegate
  , setDelegate
  , scene
  , setScene
  , navigationEnabled
  , setNavigationEnabled
  , showsRoadLabels
  , setShowsRoadLabels
  , pointOfInterestFilter
  , setPointOfInterestFilter
  , badgePosition
  , setBadgePosition
  , badgePositionSelector
  , delegateSelector
  , initWithCoderSelector
  , initWithNibName_bundleSelector
  , initWithSceneSelector
  , navigationEnabledSelector
  , pointOfInterestFilterSelector
  , sceneSelector
  , setBadgePositionSelector
  , setDelegateSelector
  , setNavigationEnabledSelector
  , setPointOfInterestFilterSelector
  , setSceneSelector
  , setShowsRoadLabelsSelector
  , showsRoadLabelsSelector

  -- * Enum types
  , MKLookAroundBadgePosition(MKLookAroundBadgePosition)
  , pattern MKLookAroundBadgePositionTopLeading
  , pattern MKLookAroundBadgePositionTopTrailing
  , pattern MKLookAroundBadgePositionBottomTrailing

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.MapKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithScene:@
initWithScene :: (IsMKLookAroundViewController mkLookAroundViewController, IsMKLookAroundScene scene) => mkLookAroundViewController -> scene -> IO (Id MKLookAroundViewController)
initWithScene mkLookAroundViewController scene =
  sendOwnedMessage mkLookAroundViewController initWithSceneSelector (toMKLookAroundScene scene)

-- | @- initWithNibName:bundle:@
initWithNibName_bundle :: (IsMKLookAroundViewController mkLookAroundViewController, IsNSString nibNameOrNil, IsNSBundle nibBundleOrNil) => mkLookAroundViewController -> nibNameOrNil -> nibBundleOrNil -> IO (Id MKLookAroundViewController)
initWithNibName_bundle mkLookAroundViewController nibNameOrNil nibBundleOrNil =
  sendOwnedMessage mkLookAroundViewController initWithNibName_bundleSelector (toNSString nibNameOrNil) (toNSBundle nibBundleOrNil)

-- | @- initWithCoder:@
initWithCoder :: (IsMKLookAroundViewController mkLookAroundViewController, IsNSCoder coder) => mkLookAroundViewController -> coder -> IO (Id MKLookAroundViewController)
initWithCoder mkLookAroundViewController coder =
  sendOwnedMessage mkLookAroundViewController initWithCoderSelector (toNSCoder coder)

-- | @- delegate@
delegate :: IsMKLookAroundViewController mkLookAroundViewController => mkLookAroundViewController -> IO RawId
delegate mkLookAroundViewController =
  sendMessage mkLookAroundViewController delegateSelector

-- | @- setDelegate:@
setDelegate :: IsMKLookAroundViewController mkLookAroundViewController => mkLookAroundViewController -> RawId -> IO ()
setDelegate mkLookAroundViewController value =
  sendMessage mkLookAroundViewController setDelegateSelector value

-- | @- scene@
scene :: IsMKLookAroundViewController mkLookAroundViewController => mkLookAroundViewController -> IO (Id MKLookAroundScene)
scene mkLookAroundViewController =
  sendMessage mkLookAroundViewController sceneSelector

-- | @- setScene:@
setScene :: (IsMKLookAroundViewController mkLookAroundViewController, IsMKLookAroundScene value) => mkLookAroundViewController -> value -> IO ()
setScene mkLookAroundViewController value =
  sendMessage mkLookAroundViewController setSceneSelector (toMKLookAroundScene value)

-- | @- navigationEnabled@
navigationEnabled :: IsMKLookAroundViewController mkLookAroundViewController => mkLookAroundViewController -> IO Bool
navigationEnabled mkLookAroundViewController =
  sendMessage mkLookAroundViewController navigationEnabledSelector

-- | @- setNavigationEnabled:@
setNavigationEnabled :: IsMKLookAroundViewController mkLookAroundViewController => mkLookAroundViewController -> Bool -> IO ()
setNavigationEnabled mkLookAroundViewController value =
  sendMessage mkLookAroundViewController setNavigationEnabledSelector value

-- | @- showsRoadLabels@
showsRoadLabels :: IsMKLookAroundViewController mkLookAroundViewController => mkLookAroundViewController -> IO Bool
showsRoadLabels mkLookAroundViewController =
  sendMessage mkLookAroundViewController showsRoadLabelsSelector

-- | @- setShowsRoadLabels:@
setShowsRoadLabels :: IsMKLookAroundViewController mkLookAroundViewController => mkLookAroundViewController -> Bool -> IO ()
setShowsRoadLabels mkLookAroundViewController value =
  sendMessage mkLookAroundViewController setShowsRoadLabelsSelector value

-- | @- pointOfInterestFilter@
pointOfInterestFilter :: IsMKLookAroundViewController mkLookAroundViewController => mkLookAroundViewController -> IO (Id MKPointOfInterestFilter)
pointOfInterestFilter mkLookAroundViewController =
  sendMessage mkLookAroundViewController pointOfInterestFilterSelector

-- | @- setPointOfInterestFilter:@
setPointOfInterestFilter :: (IsMKLookAroundViewController mkLookAroundViewController, IsMKPointOfInterestFilter value) => mkLookAroundViewController -> value -> IO ()
setPointOfInterestFilter mkLookAroundViewController value =
  sendMessage mkLookAroundViewController setPointOfInterestFilterSelector (toMKPointOfInterestFilter value)

-- | @- badgePosition@
badgePosition :: IsMKLookAroundViewController mkLookAroundViewController => mkLookAroundViewController -> IO MKLookAroundBadgePosition
badgePosition mkLookAroundViewController =
  sendMessage mkLookAroundViewController badgePositionSelector

-- | @- setBadgePosition:@
setBadgePosition :: IsMKLookAroundViewController mkLookAroundViewController => mkLookAroundViewController -> MKLookAroundBadgePosition -> IO ()
setBadgePosition mkLookAroundViewController value =
  sendMessage mkLookAroundViewController setBadgePositionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithScene:@
initWithSceneSelector :: Selector '[Id MKLookAroundScene] (Id MKLookAroundViewController)
initWithSceneSelector = mkSelector "initWithScene:"

-- | @Selector@ for @initWithNibName:bundle:@
initWithNibName_bundleSelector :: Selector '[Id NSString, Id NSBundle] (Id MKLookAroundViewController)
initWithNibName_bundleSelector = mkSelector "initWithNibName:bundle:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id MKLookAroundViewController)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @scene@
sceneSelector :: Selector '[] (Id MKLookAroundScene)
sceneSelector = mkSelector "scene"

-- | @Selector@ for @setScene:@
setSceneSelector :: Selector '[Id MKLookAroundScene] ()
setSceneSelector = mkSelector "setScene:"

-- | @Selector@ for @navigationEnabled@
navigationEnabledSelector :: Selector '[] Bool
navigationEnabledSelector = mkSelector "navigationEnabled"

-- | @Selector@ for @setNavigationEnabled:@
setNavigationEnabledSelector :: Selector '[Bool] ()
setNavigationEnabledSelector = mkSelector "setNavigationEnabled:"

-- | @Selector@ for @showsRoadLabels@
showsRoadLabelsSelector :: Selector '[] Bool
showsRoadLabelsSelector = mkSelector "showsRoadLabels"

-- | @Selector@ for @setShowsRoadLabels:@
setShowsRoadLabelsSelector :: Selector '[Bool] ()
setShowsRoadLabelsSelector = mkSelector "setShowsRoadLabels:"

-- | @Selector@ for @pointOfInterestFilter@
pointOfInterestFilterSelector :: Selector '[] (Id MKPointOfInterestFilter)
pointOfInterestFilterSelector = mkSelector "pointOfInterestFilter"

-- | @Selector@ for @setPointOfInterestFilter:@
setPointOfInterestFilterSelector :: Selector '[Id MKPointOfInterestFilter] ()
setPointOfInterestFilterSelector = mkSelector "setPointOfInterestFilter:"

-- | @Selector@ for @badgePosition@
badgePositionSelector :: Selector '[] MKLookAroundBadgePosition
badgePositionSelector = mkSelector "badgePosition"

-- | @Selector@ for @setBadgePosition:@
setBadgePositionSelector :: Selector '[MKLookAroundBadgePosition] ()
setBadgePositionSelector = mkSelector "setBadgePosition:"

