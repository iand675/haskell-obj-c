{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMapView@.
module ObjC.MapKit.MKMapView
  ( MKMapView
  , IsMKMapView(..)
  , setCamera_animated
  , setCameraZoomRange_animated
  , setCameraBoundary_animated
  , setUserTrackingMode_animated
  , addAnnotation
  , addAnnotations
  , removeAnnotation
  , removeAnnotations
  , viewForAnnotation
  , dequeueReusableAnnotationViewWithIdentifier
  , dequeueReusableAnnotationViewWithIdentifier_forAnnotation
  , registerClass_forAnnotationViewWithReuseIdentifier
  , selectAnnotation_animated
  , deselectAnnotation_animated
  , showAnnotations_animated
  , addOverlay_level
  , addOverlays_level
  , removeOverlay
  , removeOverlays
  , insertOverlay_atIndex_level
  , insertOverlay_aboveOverlay
  , insertOverlay_belowOverlay
  , exchangeOverlay_withOverlay
  , overlaysInLevel
  , rendererForOverlay
  , addOverlay
  , addOverlays
  , insertOverlay_atIndex
  , exchangeOverlayAtIndex_withOverlayAtIndex
  , delegate
  , setDelegate
  , mapType
  , setMapType
  , preferredConfiguration
  , setPreferredConfiguration
  , camera
  , setCamera
  , cameraZoomRange
  , setCameraZoomRange
  , cameraBoundary
  , setCameraBoundary
  , zoomEnabled
  , setZoomEnabled
  , scrollEnabled
  , setScrollEnabled
  , rotateEnabled
  , setRotateEnabled
  , pitchEnabled
  , setPitchEnabled
  , showsUserTrackingButton
  , setShowsUserTrackingButton
  , pitchButtonVisibility
  , setPitchButtonVisibility
  , showsPitchControl
  , setShowsPitchControl
  , showsZoomControls
  , setShowsZoomControls
  , showsCompass
  , setShowsCompass
  , showsScale
  , setShowsScale
  , pointOfInterestFilter
  , setPointOfInterestFilter
  , showsPointsOfInterest
  , setShowsPointsOfInterest
  , showsBuildings
  , setShowsBuildings
  , showsTraffic
  , setShowsTraffic
  , showsUserLocation
  , setShowsUserLocation
  , userLocation
  , userTrackingMode
  , setUserTrackingMode
  , userLocationVisible
  , annotations
  , selectedAnnotations
  , setSelectedAnnotations
  , overlays
  , addAnnotationSelector
  , addAnnotationsSelector
  , addOverlaySelector
  , addOverlay_levelSelector
  , addOverlaysSelector
  , addOverlays_levelSelector
  , annotationsSelector
  , cameraBoundarySelector
  , cameraSelector
  , cameraZoomRangeSelector
  , delegateSelector
  , dequeueReusableAnnotationViewWithIdentifierSelector
  , dequeueReusableAnnotationViewWithIdentifier_forAnnotationSelector
  , deselectAnnotation_animatedSelector
  , exchangeOverlayAtIndex_withOverlayAtIndexSelector
  , exchangeOverlay_withOverlaySelector
  , insertOverlay_aboveOverlaySelector
  , insertOverlay_atIndexSelector
  , insertOverlay_atIndex_levelSelector
  , insertOverlay_belowOverlaySelector
  , mapTypeSelector
  , overlaysInLevelSelector
  , overlaysSelector
  , pitchButtonVisibilitySelector
  , pitchEnabledSelector
  , pointOfInterestFilterSelector
  , preferredConfigurationSelector
  , registerClass_forAnnotationViewWithReuseIdentifierSelector
  , removeAnnotationSelector
  , removeAnnotationsSelector
  , removeOverlaySelector
  , removeOverlaysSelector
  , rendererForOverlaySelector
  , rotateEnabledSelector
  , scrollEnabledSelector
  , selectAnnotation_animatedSelector
  , selectedAnnotationsSelector
  , setCameraBoundarySelector
  , setCameraBoundary_animatedSelector
  , setCameraSelector
  , setCameraZoomRangeSelector
  , setCameraZoomRange_animatedSelector
  , setCamera_animatedSelector
  , setDelegateSelector
  , setMapTypeSelector
  , setPitchButtonVisibilitySelector
  , setPitchEnabledSelector
  , setPointOfInterestFilterSelector
  , setPreferredConfigurationSelector
  , setRotateEnabledSelector
  , setScrollEnabledSelector
  , setSelectedAnnotationsSelector
  , setShowsBuildingsSelector
  , setShowsCompassSelector
  , setShowsPitchControlSelector
  , setShowsPointsOfInterestSelector
  , setShowsScaleSelector
  , setShowsTrafficSelector
  , setShowsUserLocationSelector
  , setShowsUserTrackingButtonSelector
  , setShowsZoomControlsSelector
  , setUserTrackingModeSelector
  , setUserTrackingMode_animatedSelector
  , setZoomEnabledSelector
  , showAnnotations_animatedSelector
  , showsBuildingsSelector
  , showsCompassSelector
  , showsPitchControlSelector
  , showsPointsOfInterestSelector
  , showsScaleSelector
  , showsTrafficSelector
  , showsUserLocationSelector
  , showsUserTrackingButtonSelector
  , showsZoomControlsSelector
  , userLocationSelector
  , userLocationVisibleSelector
  , userTrackingModeSelector
  , viewForAnnotationSelector
  , zoomEnabledSelector

  -- * Enum types
  , MKFeatureVisibility(MKFeatureVisibility)
  , pattern MKFeatureVisibilityAdaptive
  , pattern MKFeatureVisibilityHidden
  , pattern MKFeatureVisibilityVisible
  , MKMapType(MKMapType)
  , pattern MKMapTypeStandard
  , pattern MKMapTypeSatellite
  , pattern MKMapTypeHybrid
  , pattern MKMapTypeSatelliteFlyover
  , pattern MKMapTypeHybridFlyover
  , pattern MKMapTypeMutedStandard
  , MKOverlayLevel(MKOverlayLevel)
  , pattern MKOverlayLevelAboveRoads
  , pattern MKOverlayLevelAboveLabels
  , MKUserTrackingMode(MKUserTrackingMode)
  , pattern MKUserTrackingModeNone
  , pattern MKUserTrackingModeFollow
  , pattern MKUserTrackingModeFollowWithHeading

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.MapKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- setCamera:animated:@
setCamera_animated :: (IsMKMapView mkMapView, IsMKMapCamera camera) => mkMapView -> camera -> Bool -> IO ()
setCamera_animated mkMapView camera animated =
  sendMessage mkMapView setCamera_animatedSelector (toMKMapCamera camera) animated

-- | @- setCameraZoomRange:animated:@
setCameraZoomRange_animated :: (IsMKMapView mkMapView, IsMKMapCameraZoomRange cameraZoomRange) => mkMapView -> cameraZoomRange -> Bool -> IO ()
setCameraZoomRange_animated mkMapView cameraZoomRange animated =
  sendMessage mkMapView setCameraZoomRange_animatedSelector (toMKMapCameraZoomRange cameraZoomRange) animated

-- | @- setCameraBoundary:animated:@
setCameraBoundary_animated :: (IsMKMapView mkMapView, IsMKMapCameraBoundary cameraBoundary) => mkMapView -> cameraBoundary -> Bool -> IO ()
setCameraBoundary_animated mkMapView cameraBoundary animated =
  sendMessage mkMapView setCameraBoundary_animatedSelector (toMKMapCameraBoundary cameraBoundary) animated

-- | @- setUserTrackingMode:animated:@
setUserTrackingMode_animated :: IsMKMapView mkMapView => mkMapView -> MKUserTrackingMode -> Bool -> IO ()
setUserTrackingMode_animated mkMapView mode animated =
  sendMessage mkMapView setUserTrackingMode_animatedSelector mode animated

-- | @- addAnnotation:@
addAnnotation :: IsMKMapView mkMapView => mkMapView -> RawId -> IO ()
addAnnotation mkMapView annotation =
  sendMessage mkMapView addAnnotationSelector annotation

-- | @- addAnnotations:@
addAnnotations :: (IsMKMapView mkMapView, IsNSArray annotations) => mkMapView -> annotations -> IO ()
addAnnotations mkMapView annotations =
  sendMessage mkMapView addAnnotationsSelector (toNSArray annotations)

-- | @- removeAnnotation:@
removeAnnotation :: IsMKMapView mkMapView => mkMapView -> RawId -> IO ()
removeAnnotation mkMapView annotation =
  sendMessage mkMapView removeAnnotationSelector annotation

-- | @- removeAnnotations:@
removeAnnotations :: (IsMKMapView mkMapView, IsNSArray annotations) => mkMapView -> annotations -> IO ()
removeAnnotations mkMapView annotations =
  sendMessage mkMapView removeAnnotationsSelector (toNSArray annotations)

-- | @- viewForAnnotation:@
viewForAnnotation :: IsMKMapView mkMapView => mkMapView -> RawId -> IO (Id MKAnnotationView)
viewForAnnotation mkMapView annotation =
  sendMessage mkMapView viewForAnnotationSelector annotation

-- | @- dequeueReusableAnnotationViewWithIdentifier:@
dequeueReusableAnnotationViewWithIdentifier :: (IsMKMapView mkMapView, IsNSString identifier) => mkMapView -> identifier -> IO (Id MKAnnotationView)
dequeueReusableAnnotationViewWithIdentifier mkMapView identifier =
  sendMessage mkMapView dequeueReusableAnnotationViewWithIdentifierSelector (toNSString identifier)

-- | @- dequeueReusableAnnotationViewWithIdentifier:forAnnotation:@
dequeueReusableAnnotationViewWithIdentifier_forAnnotation :: (IsMKMapView mkMapView, IsNSString identifier) => mkMapView -> identifier -> RawId -> IO (Id MKAnnotationView)
dequeueReusableAnnotationViewWithIdentifier_forAnnotation mkMapView identifier annotation =
  sendMessage mkMapView dequeueReusableAnnotationViewWithIdentifier_forAnnotationSelector (toNSString identifier) annotation

-- | @- registerClass:forAnnotationViewWithReuseIdentifier:@
registerClass_forAnnotationViewWithReuseIdentifier :: (IsMKMapView mkMapView, IsNSString identifier) => mkMapView -> Class -> identifier -> IO ()
registerClass_forAnnotationViewWithReuseIdentifier mkMapView viewClass identifier =
  sendMessage mkMapView registerClass_forAnnotationViewWithReuseIdentifierSelector viewClass (toNSString identifier)

-- | @- selectAnnotation:animated:@
selectAnnotation_animated :: IsMKMapView mkMapView => mkMapView -> RawId -> Bool -> IO ()
selectAnnotation_animated mkMapView annotation animated =
  sendMessage mkMapView selectAnnotation_animatedSelector annotation animated

-- | @- deselectAnnotation:animated:@
deselectAnnotation_animated :: IsMKMapView mkMapView => mkMapView -> RawId -> Bool -> IO ()
deselectAnnotation_animated mkMapView annotation animated =
  sendMessage mkMapView deselectAnnotation_animatedSelector annotation animated

-- | @- showAnnotations:animated:@
showAnnotations_animated :: (IsMKMapView mkMapView, IsNSArray annotations) => mkMapView -> annotations -> Bool -> IO ()
showAnnotations_animated mkMapView annotations animated =
  sendMessage mkMapView showAnnotations_animatedSelector (toNSArray annotations) animated

-- | @- addOverlay:level:@
addOverlay_level :: IsMKMapView mkMapView => mkMapView -> RawId -> MKOverlayLevel -> IO ()
addOverlay_level mkMapView overlay level =
  sendMessage mkMapView addOverlay_levelSelector overlay level

-- | @- addOverlays:level:@
addOverlays_level :: (IsMKMapView mkMapView, IsNSArray overlays) => mkMapView -> overlays -> MKOverlayLevel -> IO ()
addOverlays_level mkMapView overlays level =
  sendMessage mkMapView addOverlays_levelSelector (toNSArray overlays) level

-- | @- removeOverlay:@
removeOverlay :: IsMKMapView mkMapView => mkMapView -> RawId -> IO ()
removeOverlay mkMapView overlay =
  sendMessage mkMapView removeOverlaySelector overlay

-- | @- removeOverlays:@
removeOverlays :: (IsMKMapView mkMapView, IsNSArray overlays) => mkMapView -> overlays -> IO ()
removeOverlays mkMapView overlays =
  sendMessage mkMapView removeOverlaysSelector (toNSArray overlays)

-- | @- insertOverlay:atIndex:level:@
insertOverlay_atIndex_level :: IsMKMapView mkMapView => mkMapView -> RawId -> CULong -> MKOverlayLevel -> IO ()
insertOverlay_atIndex_level mkMapView overlay index level =
  sendMessage mkMapView insertOverlay_atIndex_levelSelector overlay index level

-- | @- insertOverlay:aboveOverlay:@
insertOverlay_aboveOverlay :: IsMKMapView mkMapView => mkMapView -> RawId -> RawId -> IO ()
insertOverlay_aboveOverlay mkMapView overlay sibling =
  sendMessage mkMapView insertOverlay_aboveOverlaySelector overlay sibling

-- | @- insertOverlay:belowOverlay:@
insertOverlay_belowOverlay :: IsMKMapView mkMapView => mkMapView -> RawId -> RawId -> IO ()
insertOverlay_belowOverlay mkMapView overlay sibling =
  sendMessage mkMapView insertOverlay_belowOverlaySelector overlay sibling

-- | @- exchangeOverlay:withOverlay:@
exchangeOverlay_withOverlay :: IsMKMapView mkMapView => mkMapView -> RawId -> RawId -> IO ()
exchangeOverlay_withOverlay mkMapView overlay1 overlay2 =
  sendMessage mkMapView exchangeOverlay_withOverlaySelector overlay1 overlay2

-- | @- overlaysInLevel:@
overlaysInLevel :: IsMKMapView mkMapView => mkMapView -> MKOverlayLevel -> IO (Id NSArray)
overlaysInLevel mkMapView level =
  sendMessage mkMapView overlaysInLevelSelector level

-- | @- rendererForOverlay:@
rendererForOverlay :: IsMKMapView mkMapView => mkMapView -> RawId -> IO (Id MKOverlayRenderer)
rendererForOverlay mkMapView overlay =
  sendMessage mkMapView rendererForOverlaySelector overlay

-- | @- addOverlay:@
addOverlay :: IsMKMapView mkMapView => mkMapView -> RawId -> IO ()
addOverlay mkMapView overlay =
  sendMessage mkMapView addOverlaySelector overlay

-- | @- addOverlays:@
addOverlays :: (IsMKMapView mkMapView, IsNSArray overlays) => mkMapView -> overlays -> IO ()
addOverlays mkMapView overlays =
  sendMessage mkMapView addOverlaysSelector (toNSArray overlays)

-- | @- insertOverlay:atIndex:@
insertOverlay_atIndex :: IsMKMapView mkMapView => mkMapView -> RawId -> CULong -> IO ()
insertOverlay_atIndex mkMapView overlay index =
  sendMessage mkMapView insertOverlay_atIndexSelector overlay index

-- | @- exchangeOverlayAtIndex:withOverlayAtIndex:@
exchangeOverlayAtIndex_withOverlayAtIndex :: IsMKMapView mkMapView => mkMapView -> CULong -> CULong -> IO ()
exchangeOverlayAtIndex_withOverlayAtIndex mkMapView index1 index2 =
  sendMessage mkMapView exchangeOverlayAtIndex_withOverlayAtIndexSelector index1 index2

-- | @- delegate@
delegate :: IsMKMapView mkMapView => mkMapView -> IO RawId
delegate mkMapView =
  sendMessage mkMapView delegateSelector

-- | @- setDelegate:@
setDelegate :: IsMKMapView mkMapView => mkMapView -> RawId -> IO ()
setDelegate mkMapView value =
  sendMessage mkMapView setDelegateSelector value

-- | @- mapType@
mapType :: IsMKMapView mkMapView => mkMapView -> IO MKMapType
mapType mkMapView =
  sendMessage mkMapView mapTypeSelector

-- | @- setMapType:@
setMapType :: IsMKMapView mkMapView => mkMapView -> MKMapType -> IO ()
setMapType mkMapView value =
  sendMessage mkMapView setMapTypeSelector value

-- | @- preferredConfiguration@
preferredConfiguration :: IsMKMapView mkMapView => mkMapView -> IO (Id MKMapConfiguration)
preferredConfiguration mkMapView =
  sendMessage mkMapView preferredConfigurationSelector

-- | @- setPreferredConfiguration:@
setPreferredConfiguration :: (IsMKMapView mkMapView, IsMKMapConfiguration value) => mkMapView -> value -> IO ()
setPreferredConfiguration mkMapView value =
  sendMessage mkMapView setPreferredConfigurationSelector (toMKMapConfiguration value)

-- | @- camera@
camera :: IsMKMapView mkMapView => mkMapView -> IO (Id MKMapCamera)
camera mkMapView =
  sendMessage mkMapView cameraSelector

-- | @- setCamera:@
setCamera :: (IsMKMapView mkMapView, IsMKMapCamera value) => mkMapView -> value -> IO ()
setCamera mkMapView value =
  sendMessage mkMapView setCameraSelector (toMKMapCamera value)

-- | @- cameraZoomRange@
cameraZoomRange :: IsMKMapView mkMapView => mkMapView -> IO (Id MKMapCameraZoomRange)
cameraZoomRange mkMapView =
  sendMessage mkMapView cameraZoomRangeSelector

-- | @- setCameraZoomRange:@
setCameraZoomRange :: (IsMKMapView mkMapView, IsMKMapCameraZoomRange value) => mkMapView -> value -> IO ()
setCameraZoomRange mkMapView value =
  sendMessage mkMapView setCameraZoomRangeSelector (toMKMapCameraZoomRange value)

-- | @- cameraBoundary@
cameraBoundary :: IsMKMapView mkMapView => mkMapView -> IO (Id MKMapCameraBoundary)
cameraBoundary mkMapView =
  sendMessage mkMapView cameraBoundarySelector

-- | @- setCameraBoundary:@
setCameraBoundary :: (IsMKMapView mkMapView, IsMKMapCameraBoundary value) => mkMapView -> value -> IO ()
setCameraBoundary mkMapView value =
  sendMessage mkMapView setCameraBoundarySelector (toMKMapCameraBoundary value)

-- | @- zoomEnabled@
zoomEnabled :: IsMKMapView mkMapView => mkMapView -> IO Bool
zoomEnabled mkMapView =
  sendMessage mkMapView zoomEnabledSelector

-- | @- setZoomEnabled:@
setZoomEnabled :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setZoomEnabled mkMapView value =
  sendMessage mkMapView setZoomEnabledSelector value

-- | @- scrollEnabled@
scrollEnabled :: IsMKMapView mkMapView => mkMapView -> IO Bool
scrollEnabled mkMapView =
  sendMessage mkMapView scrollEnabledSelector

-- | @- setScrollEnabled:@
setScrollEnabled :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setScrollEnabled mkMapView value =
  sendMessage mkMapView setScrollEnabledSelector value

-- | @- rotateEnabled@
rotateEnabled :: IsMKMapView mkMapView => mkMapView -> IO Bool
rotateEnabled mkMapView =
  sendMessage mkMapView rotateEnabledSelector

-- | @- setRotateEnabled:@
setRotateEnabled :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setRotateEnabled mkMapView value =
  sendMessage mkMapView setRotateEnabledSelector value

-- | @- pitchEnabled@
pitchEnabled :: IsMKMapView mkMapView => mkMapView -> IO Bool
pitchEnabled mkMapView =
  sendMessage mkMapView pitchEnabledSelector

-- | @- setPitchEnabled:@
setPitchEnabled :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setPitchEnabled mkMapView value =
  sendMessage mkMapView setPitchEnabledSelector value

-- | @- showsUserTrackingButton@
showsUserTrackingButton :: IsMKMapView mkMapView => mkMapView -> IO Bool
showsUserTrackingButton mkMapView =
  sendMessage mkMapView showsUserTrackingButtonSelector

-- | @- setShowsUserTrackingButton:@
setShowsUserTrackingButton :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setShowsUserTrackingButton mkMapView value =
  sendMessage mkMapView setShowsUserTrackingButtonSelector value

-- | @- pitchButtonVisibility@
pitchButtonVisibility :: IsMKMapView mkMapView => mkMapView -> IO MKFeatureVisibility
pitchButtonVisibility mkMapView =
  sendMessage mkMapView pitchButtonVisibilitySelector

-- | @- setPitchButtonVisibility:@
setPitchButtonVisibility :: IsMKMapView mkMapView => mkMapView -> MKFeatureVisibility -> IO ()
setPitchButtonVisibility mkMapView value =
  sendMessage mkMapView setPitchButtonVisibilitySelector value

-- | @- showsPitchControl@
showsPitchControl :: IsMKMapView mkMapView => mkMapView -> IO Bool
showsPitchControl mkMapView =
  sendMessage mkMapView showsPitchControlSelector

-- | @- setShowsPitchControl:@
setShowsPitchControl :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setShowsPitchControl mkMapView value =
  sendMessage mkMapView setShowsPitchControlSelector value

-- | @- showsZoomControls@
showsZoomControls :: IsMKMapView mkMapView => mkMapView -> IO Bool
showsZoomControls mkMapView =
  sendMessage mkMapView showsZoomControlsSelector

-- | @- setShowsZoomControls:@
setShowsZoomControls :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setShowsZoomControls mkMapView value =
  sendMessage mkMapView setShowsZoomControlsSelector value

-- | @- showsCompass@
showsCompass :: IsMKMapView mkMapView => mkMapView -> IO Bool
showsCompass mkMapView =
  sendMessage mkMapView showsCompassSelector

-- | @- setShowsCompass:@
setShowsCompass :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setShowsCompass mkMapView value =
  sendMessage mkMapView setShowsCompassSelector value

-- | @- showsScale@
showsScale :: IsMKMapView mkMapView => mkMapView -> IO Bool
showsScale mkMapView =
  sendMessage mkMapView showsScaleSelector

-- | @- setShowsScale:@
setShowsScale :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setShowsScale mkMapView value =
  sendMessage mkMapView setShowsScaleSelector value

-- | @- pointOfInterestFilter@
pointOfInterestFilter :: IsMKMapView mkMapView => mkMapView -> IO (Id MKPointOfInterestFilter)
pointOfInterestFilter mkMapView =
  sendMessage mkMapView pointOfInterestFilterSelector

-- | @- setPointOfInterestFilter:@
setPointOfInterestFilter :: (IsMKMapView mkMapView, IsMKPointOfInterestFilter value) => mkMapView -> value -> IO ()
setPointOfInterestFilter mkMapView value =
  sendMessage mkMapView setPointOfInterestFilterSelector (toMKPointOfInterestFilter value)

-- | @- showsPointsOfInterest@
showsPointsOfInterest :: IsMKMapView mkMapView => mkMapView -> IO Bool
showsPointsOfInterest mkMapView =
  sendMessage mkMapView showsPointsOfInterestSelector

-- | @- setShowsPointsOfInterest:@
setShowsPointsOfInterest :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setShowsPointsOfInterest mkMapView value =
  sendMessage mkMapView setShowsPointsOfInterestSelector value

-- | @- showsBuildings@
showsBuildings :: IsMKMapView mkMapView => mkMapView -> IO Bool
showsBuildings mkMapView =
  sendMessage mkMapView showsBuildingsSelector

-- | @- setShowsBuildings:@
setShowsBuildings :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setShowsBuildings mkMapView value =
  sendMessage mkMapView setShowsBuildingsSelector value

-- | @- showsTraffic@
showsTraffic :: IsMKMapView mkMapView => mkMapView -> IO Bool
showsTraffic mkMapView =
  sendMessage mkMapView showsTrafficSelector

-- | @- setShowsTraffic:@
setShowsTraffic :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setShowsTraffic mkMapView value =
  sendMessage mkMapView setShowsTrafficSelector value

-- | @- showsUserLocation@
showsUserLocation :: IsMKMapView mkMapView => mkMapView -> IO Bool
showsUserLocation mkMapView =
  sendMessage mkMapView showsUserLocationSelector

-- | @- setShowsUserLocation:@
setShowsUserLocation :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setShowsUserLocation mkMapView value =
  sendMessage mkMapView setShowsUserLocationSelector value

-- | @- userLocation@
userLocation :: IsMKMapView mkMapView => mkMapView -> IO (Id MKUserLocation)
userLocation mkMapView =
  sendMessage mkMapView userLocationSelector

-- | @- userTrackingMode@
userTrackingMode :: IsMKMapView mkMapView => mkMapView -> IO MKUserTrackingMode
userTrackingMode mkMapView =
  sendMessage mkMapView userTrackingModeSelector

-- | @- setUserTrackingMode:@
setUserTrackingMode :: IsMKMapView mkMapView => mkMapView -> MKUserTrackingMode -> IO ()
setUserTrackingMode mkMapView value =
  sendMessage mkMapView setUserTrackingModeSelector value

-- | @- userLocationVisible@
userLocationVisible :: IsMKMapView mkMapView => mkMapView -> IO Bool
userLocationVisible mkMapView =
  sendMessage mkMapView userLocationVisibleSelector

-- | @- annotations@
annotations :: IsMKMapView mkMapView => mkMapView -> IO (Id NSArray)
annotations mkMapView =
  sendMessage mkMapView annotationsSelector

-- | @- selectedAnnotations@
selectedAnnotations :: IsMKMapView mkMapView => mkMapView -> IO (Id NSArray)
selectedAnnotations mkMapView =
  sendMessage mkMapView selectedAnnotationsSelector

-- | @- setSelectedAnnotations:@
setSelectedAnnotations :: (IsMKMapView mkMapView, IsNSArray value) => mkMapView -> value -> IO ()
setSelectedAnnotations mkMapView value =
  sendMessage mkMapView setSelectedAnnotationsSelector (toNSArray value)

-- | @- overlays@
overlays :: IsMKMapView mkMapView => mkMapView -> IO (Id NSArray)
overlays mkMapView =
  sendMessage mkMapView overlaysSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setCamera:animated:@
setCamera_animatedSelector :: Selector '[Id MKMapCamera, Bool] ()
setCamera_animatedSelector = mkSelector "setCamera:animated:"

-- | @Selector@ for @setCameraZoomRange:animated:@
setCameraZoomRange_animatedSelector :: Selector '[Id MKMapCameraZoomRange, Bool] ()
setCameraZoomRange_animatedSelector = mkSelector "setCameraZoomRange:animated:"

-- | @Selector@ for @setCameraBoundary:animated:@
setCameraBoundary_animatedSelector :: Selector '[Id MKMapCameraBoundary, Bool] ()
setCameraBoundary_animatedSelector = mkSelector "setCameraBoundary:animated:"

-- | @Selector@ for @setUserTrackingMode:animated:@
setUserTrackingMode_animatedSelector :: Selector '[MKUserTrackingMode, Bool] ()
setUserTrackingMode_animatedSelector = mkSelector "setUserTrackingMode:animated:"

-- | @Selector@ for @addAnnotation:@
addAnnotationSelector :: Selector '[RawId] ()
addAnnotationSelector = mkSelector "addAnnotation:"

-- | @Selector@ for @addAnnotations:@
addAnnotationsSelector :: Selector '[Id NSArray] ()
addAnnotationsSelector = mkSelector "addAnnotations:"

-- | @Selector@ for @removeAnnotation:@
removeAnnotationSelector :: Selector '[RawId] ()
removeAnnotationSelector = mkSelector "removeAnnotation:"

-- | @Selector@ for @removeAnnotations:@
removeAnnotationsSelector :: Selector '[Id NSArray] ()
removeAnnotationsSelector = mkSelector "removeAnnotations:"

-- | @Selector@ for @viewForAnnotation:@
viewForAnnotationSelector :: Selector '[RawId] (Id MKAnnotationView)
viewForAnnotationSelector = mkSelector "viewForAnnotation:"

-- | @Selector@ for @dequeueReusableAnnotationViewWithIdentifier:@
dequeueReusableAnnotationViewWithIdentifierSelector :: Selector '[Id NSString] (Id MKAnnotationView)
dequeueReusableAnnotationViewWithIdentifierSelector = mkSelector "dequeueReusableAnnotationViewWithIdentifier:"

-- | @Selector@ for @dequeueReusableAnnotationViewWithIdentifier:forAnnotation:@
dequeueReusableAnnotationViewWithIdentifier_forAnnotationSelector :: Selector '[Id NSString, RawId] (Id MKAnnotationView)
dequeueReusableAnnotationViewWithIdentifier_forAnnotationSelector = mkSelector "dequeueReusableAnnotationViewWithIdentifier:forAnnotation:"

-- | @Selector@ for @registerClass:forAnnotationViewWithReuseIdentifier:@
registerClass_forAnnotationViewWithReuseIdentifierSelector :: Selector '[Class, Id NSString] ()
registerClass_forAnnotationViewWithReuseIdentifierSelector = mkSelector "registerClass:forAnnotationViewWithReuseIdentifier:"

-- | @Selector@ for @selectAnnotation:animated:@
selectAnnotation_animatedSelector :: Selector '[RawId, Bool] ()
selectAnnotation_animatedSelector = mkSelector "selectAnnotation:animated:"

-- | @Selector@ for @deselectAnnotation:animated:@
deselectAnnotation_animatedSelector :: Selector '[RawId, Bool] ()
deselectAnnotation_animatedSelector = mkSelector "deselectAnnotation:animated:"

-- | @Selector@ for @showAnnotations:animated:@
showAnnotations_animatedSelector :: Selector '[Id NSArray, Bool] ()
showAnnotations_animatedSelector = mkSelector "showAnnotations:animated:"

-- | @Selector@ for @addOverlay:level:@
addOverlay_levelSelector :: Selector '[RawId, MKOverlayLevel] ()
addOverlay_levelSelector = mkSelector "addOverlay:level:"

-- | @Selector@ for @addOverlays:level:@
addOverlays_levelSelector :: Selector '[Id NSArray, MKOverlayLevel] ()
addOverlays_levelSelector = mkSelector "addOverlays:level:"

-- | @Selector@ for @removeOverlay:@
removeOverlaySelector :: Selector '[RawId] ()
removeOverlaySelector = mkSelector "removeOverlay:"

-- | @Selector@ for @removeOverlays:@
removeOverlaysSelector :: Selector '[Id NSArray] ()
removeOverlaysSelector = mkSelector "removeOverlays:"

-- | @Selector@ for @insertOverlay:atIndex:level:@
insertOverlay_atIndex_levelSelector :: Selector '[RawId, CULong, MKOverlayLevel] ()
insertOverlay_atIndex_levelSelector = mkSelector "insertOverlay:atIndex:level:"

-- | @Selector@ for @insertOverlay:aboveOverlay:@
insertOverlay_aboveOverlaySelector :: Selector '[RawId, RawId] ()
insertOverlay_aboveOverlaySelector = mkSelector "insertOverlay:aboveOverlay:"

-- | @Selector@ for @insertOverlay:belowOverlay:@
insertOverlay_belowOverlaySelector :: Selector '[RawId, RawId] ()
insertOverlay_belowOverlaySelector = mkSelector "insertOverlay:belowOverlay:"

-- | @Selector@ for @exchangeOverlay:withOverlay:@
exchangeOverlay_withOverlaySelector :: Selector '[RawId, RawId] ()
exchangeOverlay_withOverlaySelector = mkSelector "exchangeOverlay:withOverlay:"

-- | @Selector@ for @overlaysInLevel:@
overlaysInLevelSelector :: Selector '[MKOverlayLevel] (Id NSArray)
overlaysInLevelSelector = mkSelector "overlaysInLevel:"

-- | @Selector@ for @rendererForOverlay:@
rendererForOverlaySelector :: Selector '[RawId] (Id MKOverlayRenderer)
rendererForOverlaySelector = mkSelector "rendererForOverlay:"

-- | @Selector@ for @addOverlay:@
addOverlaySelector :: Selector '[RawId] ()
addOverlaySelector = mkSelector "addOverlay:"

-- | @Selector@ for @addOverlays:@
addOverlaysSelector :: Selector '[Id NSArray] ()
addOverlaysSelector = mkSelector "addOverlays:"

-- | @Selector@ for @insertOverlay:atIndex:@
insertOverlay_atIndexSelector :: Selector '[RawId, CULong] ()
insertOverlay_atIndexSelector = mkSelector "insertOverlay:atIndex:"

-- | @Selector@ for @exchangeOverlayAtIndex:withOverlayAtIndex:@
exchangeOverlayAtIndex_withOverlayAtIndexSelector :: Selector '[CULong, CULong] ()
exchangeOverlayAtIndex_withOverlayAtIndexSelector = mkSelector "exchangeOverlayAtIndex:withOverlayAtIndex:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @mapType@
mapTypeSelector :: Selector '[] MKMapType
mapTypeSelector = mkSelector "mapType"

-- | @Selector@ for @setMapType:@
setMapTypeSelector :: Selector '[MKMapType] ()
setMapTypeSelector = mkSelector "setMapType:"

-- | @Selector@ for @preferredConfiguration@
preferredConfigurationSelector :: Selector '[] (Id MKMapConfiguration)
preferredConfigurationSelector = mkSelector "preferredConfiguration"

-- | @Selector@ for @setPreferredConfiguration:@
setPreferredConfigurationSelector :: Selector '[Id MKMapConfiguration] ()
setPreferredConfigurationSelector = mkSelector "setPreferredConfiguration:"

-- | @Selector@ for @camera@
cameraSelector :: Selector '[] (Id MKMapCamera)
cameraSelector = mkSelector "camera"

-- | @Selector@ for @setCamera:@
setCameraSelector :: Selector '[Id MKMapCamera] ()
setCameraSelector = mkSelector "setCamera:"

-- | @Selector@ for @cameraZoomRange@
cameraZoomRangeSelector :: Selector '[] (Id MKMapCameraZoomRange)
cameraZoomRangeSelector = mkSelector "cameraZoomRange"

-- | @Selector@ for @setCameraZoomRange:@
setCameraZoomRangeSelector :: Selector '[Id MKMapCameraZoomRange] ()
setCameraZoomRangeSelector = mkSelector "setCameraZoomRange:"

-- | @Selector@ for @cameraBoundary@
cameraBoundarySelector :: Selector '[] (Id MKMapCameraBoundary)
cameraBoundarySelector = mkSelector "cameraBoundary"

-- | @Selector@ for @setCameraBoundary:@
setCameraBoundarySelector :: Selector '[Id MKMapCameraBoundary] ()
setCameraBoundarySelector = mkSelector "setCameraBoundary:"

-- | @Selector@ for @zoomEnabled@
zoomEnabledSelector :: Selector '[] Bool
zoomEnabledSelector = mkSelector "zoomEnabled"

-- | @Selector@ for @setZoomEnabled:@
setZoomEnabledSelector :: Selector '[Bool] ()
setZoomEnabledSelector = mkSelector "setZoomEnabled:"

-- | @Selector@ for @scrollEnabled@
scrollEnabledSelector :: Selector '[] Bool
scrollEnabledSelector = mkSelector "scrollEnabled"

-- | @Selector@ for @setScrollEnabled:@
setScrollEnabledSelector :: Selector '[Bool] ()
setScrollEnabledSelector = mkSelector "setScrollEnabled:"

-- | @Selector@ for @rotateEnabled@
rotateEnabledSelector :: Selector '[] Bool
rotateEnabledSelector = mkSelector "rotateEnabled"

-- | @Selector@ for @setRotateEnabled:@
setRotateEnabledSelector :: Selector '[Bool] ()
setRotateEnabledSelector = mkSelector "setRotateEnabled:"

-- | @Selector@ for @pitchEnabled@
pitchEnabledSelector :: Selector '[] Bool
pitchEnabledSelector = mkSelector "pitchEnabled"

-- | @Selector@ for @setPitchEnabled:@
setPitchEnabledSelector :: Selector '[Bool] ()
setPitchEnabledSelector = mkSelector "setPitchEnabled:"

-- | @Selector@ for @showsUserTrackingButton@
showsUserTrackingButtonSelector :: Selector '[] Bool
showsUserTrackingButtonSelector = mkSelector "showsUserTrackingButton"

-- | @Selector@ for @setShowsUserTrackingButton:@
setShowsUserTrackingButtonSelector :: Selector '[Bool] ()
setShowsUserTrackingButtonSelector = mkSelector "setShowsUserTrackingButton:"

-- | @Selector@ for @pitchButtonVisibility@
pitchButtonVisibilitySelector :: Selector '[] MKFeatureVisibility
pitchButtonVisibilitySelector = mkSelector "pitchButtonVisibility"

-- | @Selector@ for @setPitchButtonVisibility:@
setPitchButtonVisibilitySelector :: Selector '[MKFeatureVisibility] ()
setPitchButtonVisibilitySelector = mkSelector "setPitchButtonVisibility:"

-- | @Selector@ for @showsPitchControl@
showsPitchControlSelector :: Selector '[] Bool
showsPitchControlSelector = mkSelector "showsPitchControl"

-- | @Selector@ for @setShowsPitchControl:@
setShowsPitchControlSelector :: Selector '[Bool] ()
setShowsPitchControlSelector = mkSelector "setShowsPitchControl:"

-- | @Selector@ for @showsZoomControls@
showsZoomControlsSelector :: Selector '[] Bool
showsZoomControlsSelector = mkSelector "showsZoomControls"

-- | @Selector@ for @setShowsZoomControls:@
setShowsZoomControlsSelector :: Selector '[Bool] ()
setShowsZoomControlsSelector = mkSelector "setShowsZoomControls:"

-- | @Selector@ for @showsCompass@
showsCompassSelector :: Selector '[] Bool
showsCompassSelector = mkSelector "showsCompass"

-- | @Selector@ for @setShowsCompass:@
setShowsCompassSelector :: Selector '[Bool] ()
setShowsCompassSelector = mkSelector "setShowsCompass:"

-- | @Selector@ for @showsScale@
showsScaleSelector :: Selector '[] Bool
showsScaleSelector = mkSelector "showsScale"

-- | @Selector@ for @setShowsScale:@
setShowsScaleSelector :: Selector '[Bool] ()
setShowsScaleSelector = mkSelector "setShowsScale:"

-- | @Selector@ for @pointOfInterestFilter@
pointOfInterestFilterSelector :: Selector '[] (Id MKPointOfInterestFilter)
pointOfInterestFilterSelector = mkSelector "pointOfInterestFilter"

-- | @Selector@ for @setPointOfInterestFilter:@
setPointOfInterestFilterSelector :: Selector '[Id MKPointOfInterestFilter] ()
setPointOfInterestFilterSelector = mkSelector "setPointOfInterestFilter:"

-- | @Selector@ for @showsPointsOfInterest@
showsPointsOfInterestSelector :: Selector '[] Bool
showsPointsOfInterestSelector = mkSelector "showsPointsOfInterest"

-- | @Selector@ for @setShowsPointsOfInterest:@
setShowsPointsOfInterestSelector :: Selector '[Bool] ()
setShowsPointsOfInterestSelector = mkSelector "setShowsPointsOfInterest:"

-- | @Selector@ for @showsBuildings@
showsBuildingsSelector :: Selector '[] Bool
showsBuildingsSelector = mkSelector "showsBuildings"

-- | @Selector@ for @setShowsBuildings:@
setShowsBuildingsSelector :: Selector '[Bool] ()
setShowsBuildingsSelector = mkSelector "setShowsBuildings:"

-- | @Selector@ for @showsTraffic@
showsTrafficSelector :: Selector '[] Bool
showsTrafficSelector = mkSelector "showsTraffic"

-- | @Selector@ for @setShowsTraffic:@
setShowsTrafficSelector :: Selector '[Bool] ()
setShowsTrafficSelector = mkSelector "setShowsTraffic:"

-- | @Selector@ for @showsUserLocation@
showsUserLocationSelector :: Selector '[] Bool
showsUserLocationSelector = mkSelector "showsUserLocation"

-- | @Selector@ for @setShowsUserLocation:@
setShowsUserLocationSelector :: Selector '[Bool] ()
setShowsUserLocationSelector = mkSelector "setShowsUserLocation:"

-- | @Selector@ for @userLocation@
userLocationSelector :: Selector '[] (Id MKUserLocation)
userLocationSelector = mkSelector "userLocation"

-- | @Selector@ for @userTrackingMode@
userTrackingModeSelector :: Selector '[] MKUserTrackingMode
userTrackingModeSelector = mkSelector "userTrackingMode"

-- | @Selector@ for @setUserTrackingMode:@
setUserTrackingModeSelector :: Selector '[MKUserTrackingMode] ()
setUserTrackingModeSelector = mkSelector "setUserTrackingMode:"

-- | @Selector@ for @userLocationVisible@
userLocationVisibleSelector :: Selector '[] Bool
userLocationVisibleSelector = mkSelector "userLocationVisible"

-- | @Selector@ for @annotations@
annotationsSelector :: Selector '[] (Id NSArray)
annotationsSelector = mkSelector "annotations"

-- | @Selector@ for @selectedAnnotations@
selectedAnnotationsSelector :: Selector '[] (Id NSArray)
selectedAnnotationsSelector = mkSelector "selectedAnnotations"

-- | @Selector@ for @setSelectedAnnotations:@
setSelectedAnnotationsSelector :: Selector '[Id NSArray] ()
setSelectedAnnotationsSelector = mkSelector "setSelectedAnnotations:"

-- | @Selector@ for @overlays@
overlaysSelector :: Selector '[] (Id NSArray)
overlaysSelector = mkSelector "overlays"

