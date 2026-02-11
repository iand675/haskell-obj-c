{-# LANGUAGE PatternSynonyms #-}
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
  , initWithSceneSelector
  , initWithNibName_bundleSelector
  , initWithCoderSelector
  , sceneSelector
  , setSceneSelector
  , navigationEnabledSelector
  , setNavigationEnabledSelector
  , showsRoadLabelsSelector
  , setShowsRoadLabelsSelector
  , pointOfInterestFilterSelector
  , setPointOfInterestFilterSelector
  , badgePositionSelector
  , setBadgePositionSelector

  -- * Enum types
  , MKLookAroundBadgePosition(MKLookAroundBadgePosition)
  , pattern MKLookAroundBadgePositionTopLeading
  , pattern MKLookAroundBadgePositionTopTrailing
  , pattern MKLookAroundBadgePositionBottomTrailing

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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithScene:@
initWithScene :: (IsMKLookAroundViewController mkLookAroundViewController, IsMKLookAroundScene scene) => mkLookAroundViewController -> scene -> IO (Id MKLookAroundViewController)
initWithScene mkLookAroundViewController  scene =
withObjCPtr scene $ \raw_scene ->
    sendMsg mkLookAroundViewController (mkSelector "initWithScene:") (retPtr retVoid) [argPtr (castPtr raw_scene :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithNibName:bundle:@
initWithNibName_bundle :: (IsMKLookAroundViewController mkLookAroundViewController, IsNSString nibNameOrNil, IsNSBundle nibBundleOrNil) => mkLookAroundViewController -> nibNameOrNil -> nibBundleOrNil -> IO (Id MKLookAroundViewController)
initWithNibName_bundle mkLookAroundViewController  nibNameOrNil nibBundleOrNil =
withObjCPtr nibNameOrNil $ \raw_nibNameOrNil ->
  withObjCPtr nibBundleOrNil $ \raw_nibBundleOrNil ->
      sendMsg mkLookAroundViewController (mkSelector "initWithNibName:bundle:") (retPtr retVoid) [argPtr (castPtr raw_nibNameOrNil :: Ptr ()), argPtr (castPtr raw_nibBundleOrNil :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsMKLookAroundViewController mkLookAroundViewController, IsNSCoder coder) => mkLookAroundViewController -> coder -> IO (Id MKLookAroundViewController)
initWithCoder mkLookAroundViewController  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg mkLookAroundViewController (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- scene@
scene :: IsMKLookAroundViewController mkLookAroundViewController => mkLookAroundViewController -> IO (Id MKLookAroundScene)
scene mkLookAroundViewController  =
  sendMsg mkLookAroundViewController (mkSelector "scene") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setScene:@
setScene :: (IsMKLookAroundViewController mkLookAroundViewController, IsMKLookAroundScene value) => mkLookAroundViewController -> value -> IO ()
setScene mkLookAroundViewController  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkLookAroundViewController (mkSelector "setScene:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- navigationEnabled@
navigationEnabled :: IsMKLookAroundViewController mkLookAroundViewController => mkLookAroundViewController -> IO Bool
navigationEnabled mkLookAroundViewController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkLookAroundViewController (mkSelector "navigationEnabled") retCULong []

-- | @- setNavigationEnabled:@
setNavigationEnabled :: IsMKLookAroundViewController mkLookAroundViewController => mkLookAroundViewController -> Bool -> IO ()
setNavigationEnabled mkLookAroundViewController  value =
  sendMsg mkLookAroundViewController (mkSelector "setNavigationEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsRoadLabels@
showsRoadLabels :: IsMKLookAroundViewController mkLookAroundViewController => mkLookAroundViewController -> IO Bool
showsRoadLabels mkLookAroundViewController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkLookAroundViewController (mkSelector "showsRoadLabels") retCULong []

-- | @- setShowsRoadLabels:@
setShowsRoadLabels :: IsMKLookAroundViewController mkLookAroundViewController => mkLookAroundViewController -> Bool -> IO ()
setShowsRoadLabels mkLookAroundViewController  value =
  sendMsg mkLookAroundViewController (mkSelector "setShowsRoadLabels:") retVoid [argCULong (if value then 1 else 0)]

-- | @- pointOfInterestFilter@
pointOfInterestFilter :: IsMKLookAroundViewController mkLookAroundViewController => mkLookAroundViewController -> IO (Id MKPointOfInterestFilter)
pointOfInterestFilter mkLookAroundViewController  =
  sendMsg mkLookAroundViewController (mkSelector "pointOfInterestFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPointOfInterestFilter:@
setPointOfInterestFilter :: (IsMKLookAroundViewController mkLookAroundViewController, IsMKPointOfInterestFilter value) => mkLookAroundViewController -> value -> IO ()
setPointOfInterestFilter mkLookAroundViewController  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkLookAroundViewController (mkSelector "setPointOfInterestFilter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- badgePosition@
badgePosition :: IsMKLookAroundViewController mkLookAroundViewController => mkLookAroundViewController -> IO MKLookAroundBadgePosition
badgePosition mkLookAroundViewController  =
  fmap (coerce :: CLong -> MKLookAroundBadgePosition) $ sendMsg mkLookAroundViewController (mkSelector "badgePosition") retCLong []

-- | @- setBadgePosition:@
setBadgePosition :: IsMKLookAroundViewController mkLookAroundViewController => mkLookAroundViewController -> MKLookAroundBadgePosition -> IO ()
setBadgePosition mkLookAroundViewController  value =
  sendMsg mkLookAroundViewController (mkSelector "setBadgePosition:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithScene:@
initWithSceneSelector :: Selector
initWithSceneSelector = mkSelector "initWithScene:"

-- | @Selector@ for @initWithNibName:bundle:@
initWithNibName_bundleSelector :: Selector
initWithNibName_bundleSelector = mkSelector "initWithNibName:bundle:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @scene@
sceneSelector :: Selector
sceneSelector = mkSelector "scene"

-- | @Selector@ for @setScene:@
setSceneSelector :: Selector
setSceneSelector = mkSelector "setScene:"

-- | @Selector@ for @navigationEnabled@
navigationEnabledSelector :: Selector
navigationEnabledSelector = mkSelector "navigationEnabled"

-- | @Selector@ for @setNavigationEnabled:@
setNavigationEnabledSelector :: Selector
setNavigationEnabledSelector = mkSelector "setNavigationEnabled:"

-- | @Selector@ for @showsRoadLabels@
showsRoadLabelsSelector :: Selector
showsRoadLabelsSelector = mkSelector "showsRoadLabels"

-- | @Selector@ for @setShowsRoadLabels:@
setShowsRoadLabelsSelector :: Selector
setShowsRoadLabelsSelector = mkSelector "setShowsRoadLabels:"

-- | @Selector@ for @pointOfInterestFilter@
pointOfInterestFilterSelector :: Selector
pointOfInterestFilterSelector = mkSelector "pointOfInterestFilter"

-- | @Selector@ for @setPointOfInterestFilter:@
setPointOfInterestFilterSelector :: Selector
setPointOfInterestFilterSelector = mkSelector "setPointOfInterestFilter:"

-- | @Selector@ for @badgePosition@
badgePositionSelector :: Selector
badgePositionSelector = mkSelector "badgePosition"

-- | @Selector@ for @setBadgePosition:@
setBadgePositionSelector :: Selector
setBadgePositionSelector = mkSelector "setBadgePosition:"

