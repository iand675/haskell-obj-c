{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMapView@.
module ObjC.MapKit.MKMapView
  ( MKMapView
  , IsMKMapView(..)
  , setCenterCoordinate_animated
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
  , mapType
  , setMapType
  , centerCoordinate
  , setCenterCoordinate
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
  , setCenterCoordinate_animatedSelector
  , setCamera_animatedSelector
  , setCameraZoomRange_animatedSelector
  , setCameraBoundary_animatedSelector
  , setUserTrackingMode_animatedSelector
  , addAnnotationSelector
  , addAnnotationsSelector
  , removeAnnotationSelector
  , removeAnnotationsSelector
  , viewForAnnotationSelector
  , dequeueReusableAnnotationViewWithIdentifierSelector
  , dequeueReusableAnnotationViewWithIdentifier_forAnnotationSelector
  , registerClass_forAnnotationViewWithReuseIdentifierSelector
  , selectAnnotation_animatedSelector
  , deselectAnnotation_animatedSelector
  , showAnnotations_animatedSelector
  , addOverlay_levelSelector
  , addOverlays_levelSelector
  , removeOverlaySelector
  , removeOverlaysSelector
  , insertOverlay_atIndex_levelSelector
  , insertOverlay_aboveOverlaySelector
  , insertOverlay_belowOverlaySelector
  , exchangeOverlay_withOverlaySelector
  , overlaysInLevelSelector
  , rendererForOverlaySelector
  , addOverlaySelector
  , addOverlaysSelector
  , insertOverlay_atIndexSelector
  , exchangeOverlayAtIndex_withOverlayAtIndexSelector
  , mapTypeSelector
  , setMapTypeSelector
  , centerCoordinateSelector
  , setCenterCoordinateSelector
  , zoomEnabledSelector
  , setZoomEnabledSelector
  , scrollEnabledSelector
  , setScrollEnabledSelector
  , rotateEnabledSelector
  , setRotateEnabledSelector
  , pitchEnabledSelector
  , setPitchEnabledSelector
  , showsUserTrackingButtonSelector
  , setShowsUserTrackingButtonSelector
  , pitchButtonVisibilitySelector
  , setPitchButtonVisibilitySelector
  , showsPitchControlSelector
  , setShowsPitchControlSelector
  , showsZoomControlsSelector
  , setShowsZoomControlsSelector
  , showsCompassSelector
  , setShowsCompassSelector
  , showsScaleSelector
  , setShowsScaleSelector
  , showsPointsOfInterestSelector
  , setShowsPointsOfInterestSelector
  , showsBuildingsSelector
  , setShowsBuildingsSelector
  , showsTrafficSelector
  , setShowsTrafficSelector
  , showsUserLocationSelector
  , setShowsUserLocationSelector
  , userLocationSelector
  , userTrackingModeSelector
  , setUserTrackingModeSelector
  , userLocationVisibleSelector

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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.CoreLocation.Internal.Structs
import ObjC.Foundation.Internal.Structs
import ObjC.MapKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- setCenterCoordinate:animated:@
setCenterCoordinate_animated :: IsMKMapView mkMapView => mkMapView -> CLLocationCoordinate2D -> Bool -> IO ()
setCenterCoordinate_animated mkMapView  coordinate animated =
  sendMsg mkMapView (mkSelector "setCenterCoordinate:animated:") retVoid [argCLLocationCoordinate2D coordinate, argCULong (if animated then 1 else 0)]

-- | @- setCamera:animated:@
setCamera_animated :: (IsMKMapView mkMapView, IsMKMapCamera camera) => mkMapView -> camera -> Bool -> IO ()
setCamera_animated mkMapView  camera animated =
withObjCPtr camera $ \raw_camera ->
    sendMsg mkMapView (mkSelector "setCamera:animated:") retVoid [argPtr (castPtr raw_camera :: Ptr ()), argCULong (if animated then 1 else 0)]

-- | @- setCameraZoomRange:animated:@
setCameraZoomRange_animated :: (IsMKMapView mkMapView, IsMKMapCameraZoomRange cameraZoomRange) => mkMapView -> cameraZoomRange -> Bool -> IO ()
setCameraZoomRange_animated mkMapView  cameraZoomRange animated =
withObjCPtr cameraZoomRange $ \raw_cameraZoomRange ->
    sendMsg mkMapView (mkSelector "setCameraZoomRange:animated:") retVoid [argPtr (castPtr raw_cameraZoomRange :: Ptr ()), argCULong (if animated then 1 else 0)]

-- | @- setCameraBoundary:animated:@
setCameraBoundary_animated :: (IsMKMapView mkMapView, IsMKMapCameraBoundary cameraBoundary) => mkMapView -> cameraBoundary -> Bool -> IO ()
setCameraBoundary_animated mkMapView  cameraBoundary animated =
withObjCPtr cameraBoundary $ \raw_cameraBoundary ->
    sendMsg mkMapView (mkSelector "setCameraBoundary:animated:") retVoid [argPtr (castPtr raw_cameraBoundary :: Ptr ()), argCULong (if animated then 1 else 0)]

-- | @- setUserTrackingMode:animated:@
setUserTrackingMode_animated :: IsMKMapView mkMapView => mkMapView -> MKUserTrackingMode -> Bool -> IO ()
setUserTrackingMode_animated mkMapView  mode animated =
  sendMsg mkMapView (mkSelector "setUserTrackingMode:animated:") retVoid [argCLong (coerce mode), argCULong (if animated then 1 else 0)]

-- | @- addAnnotation:@
addAnnotation :: IsMKMapView mkMapView => mkMapView -> RawId -> IO ()
addAnnotation mkMapView  annotation =
  sendMsg mkMapView (mkSelector "addAnnotation:") retVoid [argPtr (castPtr (unRawId annotation) :: Ptr ())]

-- | @- addAnnotations:@
addAnnotations :: (IsMKMapView mkMapView, IsNSArray annotations) => mkMapView -> annotations -> IO ()
addAnnotations mkMapView  annotations =
withObjCPtr annotations $ \raw_annotations ->
    sendMsg mkMapView (mkSelector "addAnnotations:") retVoid [argPtr (castPtr raw_annotations :: Ptr ())]

-- | @- removeAnnotation:@
removeAnnotation :: IsMKMapView mkMapView => mkMapView -> RawId -> IO ()
removeAnnotation mkMapView  annotation =
  sendMsg mkMapView (mkSelector "removeAnnotation:") retVoid [argPtr (castPtr (unRawId annotation) :: Ptr ())]

-- | @- removeAnnotations:@
removeAnnotations :: (IsMKMapView mkMapView, IsNSArray annotations) => mkMapView -> annotations -> IO ()
removeAnnotations mkMapView  annotations =
withObjCPtr annotations $ \raw_annotations ->
    sendMsg mkMapView (mkSelector "removeAnnotations:") retVoid [argPtr (castPtr raw_annotations :: Ptr ())]

-- | @- viewForAnnotation:@
viewForAnnotation :: IsMKMapView mkMapView => mkMapView -> RawId -> IO (Id MKAnnotationView)
viewForAnnotation mkMapView  annotation =
  sendMsg mkMapView (mkSelector "viewForAnnotation:") (retPtr retVoid) [argPtr (castPtr (unRawId annotation) :: Ptr ())] >>= retainedObject . castPtr

-- | @- dequeueReusableAnnotationViewWithIdentifier:@
dequeueReusableAnnotationViewWithIdentifier :: (IsMKMapView mkMapView, IsNSString identifier) => mkMapView -> identifier -> IO (Id MKAnnotationView)
dequeueReusableAnnotationViewWithIdentifier mkMapView  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg mkMapView (mkSelector "dequeueReusableAnnotationViewWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- dequeueReusableAnnotationViewWithIdentifier:forAnnotation:@
dequeueReusableAnnotationViewWithIdentifier_forAnnotation :: (IsMKMapView mkMapView, IsNSString identifier) => mkMapView -> identifier -> RawId -> IO (Id MKAnnotationView)
dequeueReusableAnnotationViewWithIdentifier_forAnnotation mkMapView  identifier annotation =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg mkMapView (mkSelector "dequeueReusableAnnotationViewWithIdentifier:forAnnotation:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr (unRawId annotation) :: Ptr ())] >>= retainedObject . castPtr

-- | @- registerClass:forAnnotationViewWithReuseIdentifier:@
registerClass_forAnnotationViewWithReuseIdentifier :: (IsMKMapView mkMapView, IsNSString identifier) => mkMapView -> Class -> identifier -> IO ()
registerClass_forAnnotationViewWithReuseIdentifier mkMapView  viewClass identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg mkMapView (mkSelector "registerClass:forAnnotationViewWithReuseIdentifier:") retVoid [argPtr (unClass viewClass), argPtr (castPtr raw_identifier :: Ptr ())]

-- | @- selectAnnotation:animated:@
selectAnnotation_animated :: IsMKMapView mkMapView => mkMapView -> RawId -> Bool -> IO ()
selectAnnotation_animated mkMapView  annotation animated =
  sendMsg mkMapView (mkSelector "selectAnnotation:animated:") retVoid [argPtr (castPtr (unRawId annotation) :: Ptr ()), argCULong (if animated then 1 else 0)]

-- | @- deselectAnnotation:animated:@
deselectAnnotation_animated :: IsMKMapView mkMapView => mkMapView -> RawId -> Bool -> IO ()
deselectAnnotation_animated mkMapView  annotation animated =
  sendMsg mkMapView (mkSelector "deselectAnnotation:animated:") retVoid [argPtr (castPtr (unRawId annotation) :: Ptr ()), argCULong (if animated then 1 else 0)]

-- | @- showAnnotations:animated:@
showAnnotations_animated :: (IsMKMapView mkMapView, IsNSArray annotations) => mkMapView -> annotations -> Bool -> IO ()
showAnnotations_animated mkMapView  annotations animated =
withObjCPtr annotations $ \raw_annotations ->
    sendMsg mkMapView (mkSelector "showAnnotations:animated:") retVoid [argPtr (castPtr raw_annotations :: Ptr ()), argCULong (if animated then 1 else 0)]

-- | @- addOverlay:level:@
addOverlay_level :: IsMKMapView mkMapView => mkMapView -> RawId -> MKOverlayLevel -> IO ()
addOverlay_level mkMapView  overlay level =
  sendMsg mkMapView (mkSelector "addOverlay:level:") retVoid [argPtr (castPtr (unRawId overlay) :: Ptr ()), argCLong (coerce level)]

-- | @- addOverlays:level:@
addOverlays_level :: (IsMKMapView mkMapView, IsNSArray overlays) => mkMapView -> overlays -> MKOverlayLevel -> IO ()
addOverlays_level mkMapView  overlays level =
withObjCPtr overlays $ \raw_overlays ->
    sendMsg mkMapView (mkSelector "addOverlays:level:") retVoid [argPtr (castPtr raw_overlays :: Ptr ()), argCLong (coerce level)]

-- | @- removeOverlay:@
removeOverlay :: IsMKMapView mkMapView => mkMapView -> RawId -> IO ()
removeOverlay mkMapView  overlay =
  sendMsg mkMapView (mkSelector "removeOverlay:") retVoid [argPtr (castPtr (unRawId overlay) :: Ptr ())]

-- | @- removeOverlays:@
removeOverlays :: (IsMKMapView mkMapView, IsNSArray overlays) => mkMapView -> overlays -> IO ()
removeOverlays mkMapView  overlays =
withObjCPtr overlays $ \raw_overlays ->
    sendMsg mkMapView (mkSelector "removeOverlays:") retVoid [argPtr (castPtr raw_overlays :: Ptr ())]

-- | @- insertOverlay:atIndex:level:@
insertOverlay_atIndex_level :: IsMKMapView mkMapView => mkMapView -> RawId -> CULong -> MKOverlayLevel -> IO ()
insertOverlay_atIndex_level mkMapView  overlay index level =
  sendMsg mkMapView (mkSelector "insertOverlay:atIndex:level:") retVoid [argPtr (castPtr (unRawId overlay) :: Ptr ()), argCULong (fromIntegral index), argCLong (coerce level)]

-- | @- insertOverlay:aboveOverlay:@
insertOverlay_aboveOverlay :: IsMKMapView mkMapView => mkMapView -> RawId -> RawId -> IO ()
insertOverlay_aboveOverlay mkMapView  overlay sibling =
  sendMsg mkMapView (mkSelector "insertOverlay:aboveOverlay:") retVoid [argPtr (castPtr (unRawId overlay) :: Ptr ()), argPtr (castPtr (unRawId sibling) :: Ptr ())]

-- | @- insertOverlay:belowOverlay:@
insertOverlay_belowOverlay :: IsMKMapView mkMapView => mkMapView -> RawId -> RawId -> IO ()
insertOverlay_belowOverlay mkMapView  overlay sibling =
  sendMsg mkMapView (mkSelector "insertOverlay:belowOverlay:") retVoid [argPtr (castPtr (unRawId overlay) :: Ptr ()), argPtr (castPtr (unRawId sibling) :: Ptr ())]

-- | @- exchangeOverlay:withOverlay:@
exchangeOverlay_withOverlay :: IsMKMapView mkMapView => mkMapView -> RawId -> RawId -> IO ()
exchangeOverlay_withOverlay mkMapView  overlay1 overlay2 =
  sendMsg mkMapView (mkSelector "exchangeOverlay:withOverlay:") retVoid [argPtr (castPtr (unRawId overlay1) :: Ptr ()), argPtr (castPtr (unRawId overlay2) :: Ptr ())]

-- | @- overlaysInLevel:@
overlaysInLevel :: IsMKMapView mkMapView => mkMapView -> MKOverlayLevel -> IO (Id NSArray)
overlaysInLevel mkMapView  level =
  sendMsg mkMapView (mkSelector "overlaysInLevel:") (retPtr retVoid) [argCLong (coerce level)] >>= retainedObject . castPtr

-- | @- rendererForOverlay:@
rendererForOverlay :: IsMKMapView mkMapView => mkMapView -> RawId -> IO (Id MKOverlayRenderer)
rendererForOverlay mkMapView  overlay =
  sendMsg mkMapView (mkSelector "rendererForOverlay:") (retPtr retVoid) [argPtr (castPtr (unRawId overlay) :: Ptr ())] >>= retainedObject . castPtr

-- | @- addOverlay:@
addOverlay :: IsMKMapView mkMapView => mkMapView -> RawId -> IO ()
addOverlay mkMapView  overlay =
  sendMsg mkMapView (mkSelector "addOverlay:") retVoid [argPtr (castPtr (unRawId overlay) :: Ptr ())]

-- | @- addOverlays:@
addOverlays :: (IsMKMapView mkMapView, IsNSArray overlays) => mkMapView -> overlays -> IO ()
addOverlays mkMapView  overlays =
withObjCPtr overlays $ \raw_overlays ->
    sendMsg mkMapView (mkSelector "addOverlays:") retVoid [argPtr (castPtr raw_overlays :: Ptr ())]

-- | @- insertOverlay:atIndex:@
insertOverlay_atIndex :: IsMKMapView mkMapView => mkMapView -> RawId -> CULong -> IO ()
insertOverlay_atIndex mkMapView  overlay index =
  sendMsg mkMapView (mkSelector "insertOverlay:atIndex:") retVoid [argPtr (castPtr (unRawId overlay) :: Ptr ()), argCULong (fromIntegral index)]

-- | @- exchangeOverlayAtIndex:withOverlayAtIndex:@
exchangeOverlayAtIndex_withOverlayAtIndex :: IsMKMapView mkMapView => mkMapView -> CULong -> CULong -> IO ()
exchangeOverlayAtIndex_withOverlayAtIndex mkMapView  index1 index2 =
  sendMsg mkMapView (mkSelector "exchangeOverlayAtIndex:withOverlayAtIndex:") retVoid [argCULong (fromIntegral index1), argCULong (fromIntegral index2)]

-- | @- mapType@
mapType :: IsMKMapView mkMapView => mkMapView -> IO MKMapType
mapType mkMapView  =
  fmap (coerce :: CULong -> MKMapType) $ sendMsg mkMapView (mkSelector "mapType") retCULong []

-- | @- setMapType:@
setMapType :: IsMKMapView mkMapView => mkMapView -> MKMapType -> IO ()
setMapType mkMapView  value =
  sendMsg mkMapView (mkSelector "setMapType:") retVoid [argCULong (coerce value)]

-- | @- centerCoordinate@
centerCoordinate :: IsMKMapView mkMapView => mkMapView -> IO CLLocationCoordinate2D
centerCoordinate mkMapView  =
  sendMsgStret mkMapView (mkSelector "centerCoordinate") retCLLocationCoordinate2D []

-- | @- setCenterCoordinate:@
setCenterCoordinate :: IsMKMapView mkMapView => mkMapView -> CLLocationCoordinate2D -> IO ()
setCenterCoordinate mkMapView  value =
  sendMsg mkMapView (mkSelector "setCenterCoordinate:") retVoid [argCLLocationCoordinate2D value]

-- | @- zoomEnabled@
zoomEnabled :: IsMKMapView mkMapView => mkMapView -> IO Bool
zoomEnabled mkMapView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapView (mkSelector "zoomEnabled") retCULong []

-- | @- setZoomEnabled:@
setZoomEnabled :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setZoomEnabled mkMapView  value =
  sendMsg mkMapView (mkSelector "setZoomEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- scrollEnabled@
scrollEnabled :: IsMKMapView mkMapView => mkMapView -> IO Bool
scrollEnabled mkMapView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapView (mkSelector "scrollEnabled") retCULong []

-- | @- setScrollEnabled:@
setScrollEnabled :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setScrollEnabled mkMapView  value =
  sendMsg mkMapView (mkSelector "setScrollEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- rotateEnabled@
rotateEnabled :: IsMKMapView mkMapView => mkMapView -> IO Bool
rotateEnabled mkMapView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapView (mkSelector "rotateEnabled") retCULong []

-- | @- setRotateEnabled:@
setRotateEnabled :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setRotateEnabled mkMapView  value =
  sendMsg mkMapView (mkSelector "setRotateEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- pitchEnabled@
pitchEnabled :: IsMKMapView mkMapView => mkMapView -> IO Bool
pitchEnabled mkMapView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapView (mkSelector "pitchEnabled") retCULong []

-- | @- setPitchEnabled:@
setPitchEnabled :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setPitchEnabled mkMapView  value =
  sendMsg mkMapView (mkSelector "setPitchEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsUserTrackingButton@
showsUserTrackingButton :: IsMKMapView mkMapView => mkMapView -> IO Bool
showsUserTrackingButton mkMapView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapView (mkSelector "showsUserTrackingButton") retCULong []

-- | @- setShowsUserTrackingButton:@
setShowsUserTrackingButton :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setShowsUserTrackingButton mkMapView  value =
  sendMsg mkMapView (mkSelector "setShowsUserTrackingButton:") retVoid [argCULong (if value then 1 else 0)]

-- | @- pitchButtonVisibility@
pitchButtonVisibility :: IsMKMapView mkMapView => mkMapView -> IO MKFeatureVisibility
pitchButtonVisibility mkMapView  =
  fmap (coerce :: CLong -> MKFeatureVisibility) $ sendMsg mkMapView (mkSelector "pitchButtonVisibility") retCLong []

-- | @- setPitchButtonVisibility:@
setPitchButtonVisibility :: IsMKMapView mkMapView => mkMapView -> MKFeatureVisibility -> IO ()
setPitchButtonVisibility mkMapView  value =
  sendMsg mkMapView (mkSelector "setPitchButtonVisibility:") retVoid [argCLong (coerce value)]

-- | @- showsPitchControl@
showsPitchControl :: IsMKMapView mkMapView => mkMapView -> IO Bool
showsPitchControl mkMapView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapView (mkSelector "showsPitchControl") retCULong []

-- | @- setShowsPitchControl:@
setShowsPitchControl :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setShowsPitchControl mkMapView  value =
  sendMsg mkMapView (mkSelector "setShowsPitchControl:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsZoomControls@
showsZoomControls :: IsMKMapView mkMapView => mkMapView -> IO Bool
showsZoomControls mkMapView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapView (mkSelector "showsZoomControls") retCULong []

-- | @- setShowsZoomControls:@
setShowsZoomControls :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setShowsZoomControls mkMapView  value =
  sendMsg mkMapView (mkSelector "setShowsZoomControls:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsCompass@
showsCompass :: IsMKMapView mkMapView => mkMapView -> IO Bool
showsCompass mkMapView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapView (mkSelector "showsCompass") retCULong []

-- | @- setShowsCompass:@
setShowsCompass :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setShowsCompass mkMapView  value =
  sendMsg mkMapView (mkSelector "setShowsCompass:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsScale@
showsScale :: IsMKMapView mkMapView => mkMapView -> IO Bool
showsScale mkMapView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapView (mkSelector "showsScale") retCULong []

-- | @- setShowsScale:@
setShowsScale :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setShowsScale mkMapView  value =
  sendMsg mkMapView (mkSelector "setShowsScale:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsPointsOfInterest@
showsPointsOfInterest :: IsMKMapView mkMapView => mkMapView -> IO Bool
showsPointsOfInterest mkMapView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapView (mkSelector "showsPointsOfInterest") retCULong []

-- | @- setShowsPointsOfInterest:@
setShowsPointsOfInterest :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setShowsPointsOfInterest mkMapView  value =
  sendMsg mkMapView (mkSelector "setShowsPointsOfInterest:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsBuildings@
showsBuildings :: IsMKMapView mkMapView => mkMapView -> IO Bool
showsBuildings mkMapView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapView (mkSelector "showsBuildings") retCULong []

-- | @- setShowsBuildings:@
setShowsBuildings :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setShowsBuildings mkMapView  value =
  sendMsg mkMapView (mkSelector "setShowsBuildings:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsTraffic@
showsTraffic :: IsMKMapView mkMapView => mkMapView -> IO Bool
showsTraffic mkMapView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapView (mkSelector "showsTraffic") retCULong []

-- | @- setShowsTraffic:@
setShowsTraffic :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setShowsTraffic mkMapView  value =
  sendMsg mkMapView (mkSelector "setShowsTraffic:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsUserLocation@
showsUserLocation :: IsMKMapView mkMapView => mkMapView -> IO Bool
showsUserLocation mkMapView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapView (mkSelector "showsUserLocation") retCULong []

-- | @- setShowsUserLocation:@
setShowsUserLocation :: IsMKMapView mkMapView => mkMapView -> Bool -> IO ()
setShowsUserLocation mkMapView  value =
  sendMsg mkMapView (mkSelector "setShowsUserLocation:") retVoid [argCULong (if value then 1 else 0)]

-- | @- userLocation@
userLocation :: IsMKMapView mkMapView => mkMapView -> IO (Id MKUserLocation)
userLocation mkMapView  =
  sendMsg mkMapView (mkSelector "userLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- userTrackingMode@
userTrackingMode :: IsMKMapView mkMapView => mkMapView -> IO MKUserTrackingMode
userTrackingMode mkMapView  =
  fmap (coerce :: CLong -> MKUserTrackingMode) $ sendMsg mkMapView (mkSelector "userTrackingMode") retCLong []

-- | @- setUserTrackingMode:@
setUserTrackingMode :: IsMKMapView mkMapView => mkMapView -> MKUserTrackingMode -> IO ()
setUserTrackingMode mkMapView  value =
  sendMsg mkMapView (mkSelector "setUserTrackingMode:") retVoid [argCLong (coerce value)]

-- | @- userLocationVisible@
userLocationVisible :: IsMKMapView mkMapView => mkMapView -> IO Bool
userLocationVisible mkMapView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapView (mkSelector "userLocationVisible") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setCenterCoordinate:animated:@
setCenterCoordinate_animatedSelector :: Selector
setCenterCoordinate_animatedSelector = mkSelector "setCenterCoordinate:animated:"

-- | @Selector@ for @setCamera:animated:@
setCamera_animatedSelector :: Selector
setCamera_animatedSelector = mkSelector "setCamera:animated:"

-- | @Selector@ for @setCameraZoomRange:animated:@
setCameraZoomRange_animatedSelector :: Selector
setCameraZoomRange_animatedSelector = mkSelector "setCameraZoomRange:animated:"

-- | @Selector@ for @setCameraBoundary:animated:@
setCameraBoundary_animatedSelector :: Selector
setCameraBoundary_animatedSelector = mkSelector "setCameraBoundary:animated:"

-- | @Selector@ for @setUserTrackingMode:animated:@
setUserTrackingMode_animatedSelector :: Selector
setUserTrackingMode_animatedSelector = mkSelector "setUserTrackingMode:animated:"

-- | @Selector@ for @addAnnotation:@
addAnnotationSelector :: Selector
addAnnotationSelector = mkSelector "addAnnotation:"

-- | @Selector@ for @addAnnotations:@
addAnnotationsSelector :: Selector
addAnnotationsSelector = mkSelector "addAnnotations:"

-- | @Selector@ for @removeAnnotation:@
removeAnnotationSelector :: Selector
removeAnnotationSelector = mkSelector "removeAnnotation:"

-- | @Selector@ for @removeAnnotations:@
removeAnnotationsSelector :: Selector
removeAnnotationsSelector = mkSelector "removeAnnotations:"

-- | @Selector@ for @viewForAnnotation:@
viewForAnnotationSelector :: Selector
viewForAnnotationSelector = mkSelector "viewForAnnotation:"

-- | @Selector@ for @dequeueReusableAnnotationViewWithIdentifier:@
dequeueReusableAnnotationViewWithIdentifierSelector :: Selector
dequeueReusableAnnotationViewWithIdentifierSelector = mkSelector "dequeueReusableAnnotationViewWithIdentifier:"

-- | @Selector@ for @dequeueReusableAnnotationViewWithIdentifier:forAnnotation:@
dequeueReusableAnnotationViewWithIdentifier_forAnnotationSelector :: Selector
dequeueReusableAnnotationViewWithIdentifier_forAnnotationSelector = mkSelector "dequeueReusableAnnotationViewWithIdentifier:forAnnotation:"

-- | @Selector@ for @registerClass:forAnnotationViewWithReuseIdentifier:@
registerClass_forAnnotationViewWithReuseIdentifierSelector :: Selector
registerClass_forAnnotationViewWithReuseIdentifierSelector = mkSelector "registerClass:forAnnotationViewWithReuseIdentifier:"

-- | @Selector@ for @selectAnnotation:animated:@
selectAnnotation_animatedSelector :: Selector
selectAnnotation_animatedSelector = mkSelector "selectAnnotation:animated:"

-- | @Selector@ for @deselectAnnotation:animated:@
deselectAnnotation_animatedSelector :: Selector
deselectAnnotation_animatedSelector = mkSelector "deselectAnnotation:animated:"

-- | @Selector@ for @showAnnotations:animated:@
showAnnotations_animatedSelector :: Selector
showAnnotations_animatedSelector = mkSelector "showAnnotations:animated:"

-- | @Selector@ for @addOverlay:level:@
addOverlay_levelSelector :: Selector
addOverlay_levelSelector = mkSelector "addOverlay:level:"

-- | @Selector@ for @addOverlays:level:@
addOverlays_levelSelector :: Selector
addOverlays_levelSelector = mkSelector "addOverlays:level:"

-- | @Selector@ for @removeOverlay:@
removeOverlaySelector :: Selector
removeOverlaySelector = mkSelector "removeOverlay:"

-- | @Selector@ for @removeOverlays:@
removeOverlaysSelector :: Selector
removeOverlaysSelector = mkSelector "removeOverlays:"

-- | @Selector@ for @insertOverlay:atIndex:level:@
insertOverlay_atIndex_levelSelector :: Selector
insertOverlay_atIndex_levelSelector = mkSelector "insertOverlay:atIndex:level:"

-- | @Selector@ for @insertOverlay:aboveOverlay:@
insertOverlay_aboveOverlaySelector :: Selector
insertOverlay_aboveOverlaySelector = mkSelector "insertOverlay:aboveOverlay:"

-- | @Selector@ for @insertOverlay:belowOverlay:@
insertOverlay_belowOverlaySelector :: Selector
insertOverlay_belowOverlaySelector = mkSelector "insertOverlay:belowOverlay:"

-- | @Selector@ for @exchangeOverlay:withOverlay:@
exchangeOverlay_withOverlaySelector :: Selector
exchangeOverlay_withOverlaySelector = mkSelector "exchangeOverlay:withOverlay:"

-- | @Selector@ for @overlaysInLevel:@
overlaysInLevelSelector :: Selector
overlaysInLevelSelector = mkSelector "overlaysInLevel:"

-- | @Selector@ for @rendererForOverlay:@
rendererForOverlaySelector :: Selector
rendererForOverlaySelector = mkSelector "rendererForOverlay:"

-- | @Selector@ for @addOverlay:@
addOverlaySelector :: Selector
addOverlaySelector = mkSelector "addOverlay:"

-- | @Selector@ for @addOverlays:@
addOverlaysSelector :: Selector
addOverlaysSelector = mkSelector "addOverlays:"

-- | @Selector@ for @insertOverlay:atIndex:@
insertOverlay_atIndexSelector :: Selector
insertOverlay_atIndexSelector = mkSelector "insertOverlay:atIndex:"

-- | @Selector@ for @exchangeOverlayAtIndex:withOverlayAtIndex:@
exchangeOverlayAtIndex_withOverlayAtIndexSelector :: Selector
exchangeOverlayAtIndex_withOverlayAtIndexSelector = mkSelector "exchangeOverlayAtIndex:withOverlayAtIndex:"

-- | @Selector@ for @mapType@
mapTypeSelector :: Selector
mapTypeSelector = mkSelector "mapType"

-- | @Selector@ for @setMapType:@
setMapTypeSelector :: Selector
setMapTypeSelector = mkSelector "setMapType:"

-- | @Selector@ for @centerCoordinate@
centerCoordinateSelector :: Selector
centerCoordinateSelector = mkSelector "centerCoordinate"

-- | @Selector@ for @setCenterCoordinate:@
setCenterCoordinateSelector :: Selector
setCenterCoordinateSelector = mkSelector "setCenterCoordinate:"

-- | @Selector@ for @zoomEnabled@
zoomEnabledSelector :: Selector
zoomEnabledSelector = mkSelector "zoomEnabled"

-- | @Selector@ for @setZoomEnabled:@
setZoomEnabledSelector :: Selector
setZoomEnabledSelector = mkSelector "setZoomEnabled:"

-- | @Selector@ for @scrollEnabled@
scrollEnabledSelector :: Selector
scrollEnabledSelector = mkSelector "scrollEnabled"

-- | @Selector@ for @setScrollEnabled:@
setScrollEnabledSelector :: Selector
setScrollEnabledSelector = mkSelector "setScrollEnabled:"

-- | @Selector@ for @rotateEnabled@
rotateEnabledSelector :: Selector
rotateEnabledSelector = mkSelector "rotateEnabled"

-- | @Selector@ for @setRotateEnabled:@
setRotateEnabledSelector :: Selector
setRotateEnabledSelector = mkSelector "setRotateEnabled:"

-- | @Selector@ for @pitchEnabled@
pitchEnabledSelector :: Selector
pitchEnabledSelector = mkSelector "pitchEnabled"

-- | @Selector@ for @setPitchEnabled:@
setPitchEnabledSelector :: Selector
setPitchEnabledSelector = mkSelector "setPitchEnabled:"

-- | @Selector@ for @showsUserTrackingButton@
showsUserTrackingButtonSelector :: Selector
showsUserTrackingButtonSelector = mkSelector "showsUserTrackingButton"

-- | @Selector@ for @setShowsUserTrackingButton:@
setShowsUserTrackingButtonSelector :: Selector
setShowsUserTrackingButtonSelector = mkSelector "setShowsUserTrackingButton:"

-- | @Selector@ for @pitchButtonVisibility@
pitchButtonVisibilitySelector :: Selector
pitchButtonVisibilitySelector = mkSelector "pitchButtonVisibility"

-- | @Selector@ for @setPitchButtonVisibility:@
setPitchButtonVisibilitySelector :: Selector
setPitchButtonVisibilitySelector = mkSelector "setPitchButtonVisibility:"

-- | @Selector@ for @showsPitchControl@
showsPitchControlSelector :: Selector
showsPitchControlSelector = mkSelector "showsPitchControl"

-- | @Selector@ for @setShowsPitchControl:@
setShowsPitchControlSelector :: Selector
setShowsPitchControlSelector = mkSelector "setShowsPitchControl:"

-- | @Selector@ for @showsZoomControls@
showsZoomControlsSelector :: Selector
showsZoomControlsSelector = mkSelector "showsZoomControls"

-- | @Selector@ for @setShowsZoomControls:@
setShowsZoomControlsSelector :: Selector
setShowsZoomControlsSelector = mkSelector "setShowsZoomControls:"

-- | @Selector@ for @showsCompass@
showsCompassSelector :: Selector
showsCompassSelector = mkSelector "showsCompass"

-- | @Selector@ for @setShowsCompass:@
setShowsCompassSelector :: Selector
setShowsCompassSelector = mkSelector "setShowsCompass:"

-- | @Selector@ for @showsScale@
showsScaleSelector :: Selector
showsScaleSelector = mkSelector "showsScale"

-- | @Selector@ for @setShowsScale:@
setShowsScaleSelector :: Selector
setShowsScaleSelector = mkSelector "setShowsScale:"

-- | @Selector@ for @showsPointsOfInterest@
showsPointsOfInterestSelector :: Selector
showsPointsOfInterestSelector = mkSelector "showsPointsOfInterest"

-- | @Selector@ for @setShowsPointsOfInterest:@
setShowsPointsOfInterestSelector :: Selector
setShowsPointsOfInterestSelector = mkSelector "setShowsPointsOfInterest:"

-- | @Selector@ for @showsBuildings@
showsBuildingsSelector :: Selector
showsBuildingsSelector = mkSelector "showsBuildings"

-- | @Selector@ for @setShowsBuildings:@
setShowsBuildingsSelector :: Selector
setShowsBuildingsSelector = mkSelector "setShowsBuildings:"

-- | @Selector@ for @showsTraffic@
showsTrafficSelector :: Selector
showsTrafficSelector = mkSelector "showsTraffic"

-- | @Selector@ for @setShowsTraffic:@
setShowsTrafficSelector :: Selector
setShowsTrafficSelector = mkSelector "setShowsTraffic:"

-- | @Selector@ for @showsUserLocation@
showsUserLocationSelector :: Selector
showsUserLocationSelector = mkSelector "showsUserLocation"

-- | @Selector@ for @setShowsUserLocation:@
setShowsUserLocationSelector :: Selector
setShowsUserLocationSelector = mkSelector "setShowsUserLocation:"

-- | @Selector@ for @userLocation@
userLocationSelector :: Selector
userLocationSelector = mkSelector "userLocation"

-- | @Selector@ for @userTrackingMode@
userTrackingModeSelector :: Selector
userTrackingModeSelector = mkSelector "userTrackingMode"

-- | @Selector@ for @setUserTrackingMode:@
setUserTrackingModeSelector :: Selector
setUserTrackingModeSelector = mkSelector "setUserTrackingMode:"

-- | @Selector@ for @userLocationVisible@
userLocationVisibleSelector :: Selector
userLocationVisibleSelector = mkSelector "userLocationVisible"

