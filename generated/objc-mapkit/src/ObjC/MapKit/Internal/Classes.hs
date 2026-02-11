{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.MapKit.Internal.Classes (
    module ObjC.MapKit.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Contacts.Internal.Classes,
    module ObjC.CoreLocation.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Contacts.Internal.Classes
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- MKAddress ----------

-- | Phantom type for @MKAddress@.
data MKAddress

instance IsObjCObject (Id MKAddress) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKAddress"

class IsNSObject a => IsMKAddress a where
  toMKAddress :: a -> Id MKAddress

instance IsMKAddress (Id MKAddress) where
  toMKAddress = unsafeCastId

instance IsNSObject (Id MKAddress) where
  toNSObject = unsafeCastId

-- ---------- MKAddressFilter ----------

-- | Phantom type for @MKAddressFilter@.
data MKAddressFilter

instance IsObjCObject (Id MKAddressFilter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKAddressFilter"

class IsNSObject a => IsMKAddressFilter a where
  toMKAddressFilter :: a -> Id MKAddressFilter

instance IsMKAddressFilter (Id MKAddressFilter) where
  toMKAddressFilter = unsafeCastId

instance IsNSObject (Id MKAddressFilter) where
  toNSObject = unsafeCastId

-- ---------- MKAddressRepresentations ----------

-- | Phantom type for @MKAddressRepresentations@.
data MKAddressRepresentations

instance IsObjCObject (Id MKAddressRepresentations) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKAddressRepresentations"

class IsNSObject a => IsMKAddressRepresentations a where
  toMKAddressRepresentations :: a -> Id MKAddressRepresentations

instance IsMKAddressRepresentations (Id MKAddressRepresentations) where
  toMKAddressRepresentations = unsafeCastId

instance IsNSObject (Id MKAddressRepresentations) where
  toNSObject = unsafeCastId

-- ---------- MKClusterAnnotation ----------

-- | Phantom type for @MKClusterAnnotation@.
data MKClusterAnnotation

instance IsObjCObject (Id MKClusterAnnotation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKClusterAnnotation"

class IsNSObject a => IsMKClusterAnnotation a where
  toMKClusterAnnotation :: a -> Id MKClusterAnnotation

instance IsMKClusterAnnotation (Id MKClusterAnnotation) where
  toMKClusterAnnotation = unsafeCastId

instance IsNSObject (Id MKClusterAnnotation) where
  toNSObject = unsafeCastId

-- ---------- MKDirections ----------

-- | Phantom type for @MKDirections@.
data MKDirections

instance IsObjCObject (Id MKDirections) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKDirections"

class IsNSObject a => IsMKDirections a where
  toMKDirections :: a -> Id MKDirections

instance IsMKDirections (Id MKDirections) where
  toMKDirections = unsafeCastId

instance IsNSObject (Id MKDirections) where
  toNSObject = unsafeCastId

-- ---------- MKDirectionsRequest ----------

-- | Phantom type for @MKDirectionsRequest@.
data MKDirectionsRequest

instance IsObjCObject (Id MKDirectionsRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKDirectionsRequest"

class IsNSObject a => IsMKDirectionsRequest a where
  toMKDirectionsRequest :: a -> Id MKDirectionsRequest

instance IsMKDirectionsRequest (Id MKDirectionsRequest) where
  toMKDirectionsRequest = unsafeCastId

instance IsNSObject (Id MKDirectionsRequest) where
  toNSObject = unsafeCastId

-- ---------- MKDirectionsResponse ----------

-- | Phantom type for @MKDirectionsResponse@.
data MKDirectionsResponse

instance IsObjCObject (Id MKDirectionsResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKDirectionsResponse"

class IsNSObject a => IsMKDirectionsResponse a where
  toMKDirectionsResponse :: a -> Id MKDirectionsResponse

instance IsMKDirectionsResponse (Id MKDirectionsResponse) where
  toMKDirectionsResponse = unsafeCastId

instance IsNSObject (Id MKDirectionsResponse) where
  toNSObject = unsafeCastId

-- ---------- MKETAResponse ----------

-- | Phantom type for @MKETAResponse@.
data MKETAResponse

instance IsObjCObject (Id MKETAResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKETAResponse"

class IsNSObject a => IsMKETAResponse a where
  toMKETAResponse :: a -> Id MKETAResponse

instance IsMKETAResponse (Id MKETAResponse) where
  toMKETAResponse = unsafeCastId

instance IsNSObject (Id MKETAResponse) where
  toNSObject = unsafeCastId

-- ---------- MKGeoJSONDecoder ----------

-- | Phantom type for @MKGeoJSONDecoder@.
data MKGeoJSONDecoder

instance IsObjCObject (Id MKGeoJSONDecoder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKGeoJSONDecoder"

class IsNSObject a => IsMKGeoJSONDecoder a where
  toMKGeoJSONDecoder :: a -> Id MKGeoJSONDecoder

instance IsMKGeoJSONDecoder (Id MKGeoJSONDecoder) where
  toMKGeoJSONDecoder = unsafeCastId

instance IsNSObject (Id MKGeoJSONDecoder) where
  toNSObject = unsafeCastId

-- ---------- MKGeoJSONFeature ----------

-- | Phantom type for @MKGeoJSONFeature@.
data MKGeoJSONFeature

instance IsObjCObject (Id MKGeoJSONFeature) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKGeoJSONFeature"

class IsNSObject a => IsMKGeoJSONFeature a where
  toMKGeoJSONFeature :: a -> Id MKGeoJSONFeature

instance IsMKGeoJSONFeature (Id MKGeoJSONFeature) where
  toMKGeoJSONFeature = unsafeCastId

instance IsNSObject (Id MKGeoJSONFeature) where
  toNSObject = unsafeCastId

-- ---------- MKGeocodingRequest ----------

-- | Phantom type for @MKGeocodingRequest@.
data MKGeocodingRequest

instance IsObjCObject (Id MKGeocodingRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKGeocodingRequest"

class IsNSObject a => IsMKGeocodingRequest a where
  toMKGeocodingRequest :: a -> Id MKGeocodingRequest

instance IsMKGeocodingRequest (Id MKGeocodingRequest) where
  toMKGeocodingRequest = unsafeCastId

instance IsNSObject (Id MKGeocodingRequest) where
  toNSObject = unsafeCastId

-- ---------- MKLocalPointsOfInterestRequest ----------

-- | Phantom type for @MKLocalPointsOfInterestRequest@.
data MKLocalPointsOfInterestRequest

instance IsObjCObject (Id MKLocalPointsOfInterestRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKLocalPointsOfInterestRequest"

class IsNSObject a => IsMKLocalPointsOfInterestRequest a where
  toMKLocalPointsOfInterestRequest :: a -> Id MKLocalPointsOfInterestRequest

instance IsMKLocalPointsOfInterestRequest (Id MKLocalPointsOfInterestRequest) where
  toMKLocalPointsOfInterestRequest = unsafeCastId

instance IsNSObject (Id MKLocalPointsOfInterestRequest) where
  toNSObject = unsafeCastId

-- ---------- MKLocalSearch ----------

-- | Phantom type for @MKLocalSearch@.
data MKLocalSearch

instance IsObjCObject (Id MKLocalSearch) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKLocalSearch"

class IsNSObject a => IsMKLocalSearch a where
  toMKLocalSearch :: a -> Id MKLocalSearch

instance IsMKLocalSearch (Id MKLocalSearch) where
  toMKLocalSearch = unsafeCastId

instance IsNSObject (Id MKLocalSearch) where
  toNSObject = unsafeCastId

-- ---------- MKLocalSearchCompleter ----------

-- | Phantom type for @MKLocalSearchCompleter@.
data MKLocalSearchCompleter

instance IsObjCObject (Id MKLocalSearchCompleter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKLocalSearchCompleter"

class IsNSObject a => IsMKLocalSearchCompleter a where
  toMKLocalSearchCompleter :: a -> Id MKLocalSearchCompleter

instance IsMKLocalSearchCompleter (Id MKLocalSearchCompleter) where
  toMKLocalSearchCompleter = unsafeCastId

instance IsNSObject (Id MKLocalSearchCompleter) where
  toNSObject = unsafeCastId

-- ---------- MKLocalSearchCompletion ----------

-- | Phantom type for @MKLocalSearchCompletion@.
data MKLocalSearchCompletion

instance IsObjCObject (Id MKLocalSearchCompletion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKLocalSearchCompletion"

class IsNSObject a => IsMKLocalSearchCompletion a where
  toMKLocalSearchCompletion :: a -> Id MKLocalSearchCompletion

instance IsMKLocalSearchCompletion (Id MKLocalSearchCompletion) where
  toMKLocalSearchCompletion = unsafeCastId

instance IsNSObject (Id MKLocalSearchCompletion) where
  toNSObject = unsafeCastId

-- ---------- MKLocalSearchRequest ----------

-- | Phantom type for @MKLocalSearchRequest@.
data MKLocalSearchRequest

instance IsObjCObject (Id MKLocalSearchRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKLocalSearchRequest"

class IsNSObject a => IsMKLocalSearchRequest a where
  toMKLocalSearchRequest :: a -> Id MKLocalSearchRequest

instance IsMKLocalSearchRequest (Id MKLocalSearchRequest) where
  toMKLocalSearchRequest = unsafeCastId

instance IsNSObject (Id MKLocalSearchRequest) where
  toNSObject = unsafeCastId

-- ---------- MKLocalSearchResponse ----------

-- | Phantom type for @MKLocalSearchResponse@.
data MKLocalSearchResponse

instance IsObjCObject (Id MKLocalSearchResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKLocalSearchResponse"

class IsNSObject a => IsMKLocalSearchResponse a where
  toMKLocalSearchResponse :: a -> Id MKLocalSearchResponse

instance IsMKLocalSearchResponse (Id MKLocalSearchResponse) where
  toMKLocalSearchResponse = unsafeCastId

instance IsNSObject (Id MKLocalSearchResponse) where
  toNSObject = unsafeCastId

-- ---------- MKLookAroundScene ----------

-- | Phantom type for @MKLookAroundScene@.
data MKLookAroundScene

instance IsObjCObject (Id MKLookAroundScene) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKLookAroundScene"

class IsNSObject a => IsMKLookAroundScene a where
  toMKLookAroundScene :: a -> Id MKLookAroundScene

instance IsMKLookAroundScene (Id MKLookAroundScene) where
  toMKLookAroundScene = unsafeCastId

instance IsNSObject (Id MKLookAroundScene) where
  toNSObject = unsafeCastId

-- ---------- MKLookAroundSceneRequest ----------

-- | Phantom type for @MKLookAroundSceneRequest@.
data MKLookAroundSceneRequest

instance IsObjCObject (Id MKLookAroundSceneRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKLookAroundSceneRequest"

class IsNSObject a => IsMKLookAroundSceneRequest a where
  toMKLookAroundSceneRequest :: a -> Id MKLookAroundSceneRequest

instance IsMKLookAroundSceneRequest (Id MKLookAroundSceneRequest) where
  toMKLookAroundSceneRequest = unsafeCastId

instance IsNSObject (Id MKLookAroundSceneRequest) where
  toNSObject = unsafeCastId

-- ---------- MKLookAroundSnapshot ----------

-- | Phantom type for @MKLookAroundSnapshot@.
data MKLookAroundSnapshot

instance IsObjCObject (Id MKLookAroundSnapshot) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKLookAroundSnapshot"

class IsNSObject a => IsMKLookAroundSnapshot a where
  toMKLookAroundSnapshot :: a -> Id MKLookAroundSnapshot

instance IsMKLookAroundSnapshot (Id MKLookAroundSnapshot) where
  toMKLookAroundSnapshot = unsafeCastId

instance IsNSObject (Id MKLookAroundSnapshot) where
  toNSObject = unsafeCastId

-- ---------- MKLookAroundSnapshotOptions ----------

-- | Phantom type for @MKLookAroundSnapshotOptions@.
data MKLookAroundSnapshotOptions

instance IsObjCObject (Id MKLookAroundSnapshotOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKLookAroundSnapshotOptions"

class IsNSObject a => IsMKLookAroundSnapshotOptions a where
  toMKLookAroundSnapshotOptions :: a -> Id MKLookAroundSnapshotOptions

instance IsMKLookAroundSnapshotOptions (Id MKLookAroundSnapshotOptions) where
  toMKLookAroundSnapshotOptions = unsafeCastId

instance IsNSObject (Id MKLookAroundSnapshotOptions) where
  toNSObject = unsafeCastId

-- ---------- MKLookAroundSnapshotter ----------

-- | Phantom type for @MKLookAroundSnapshotter@.
data MKLookAroundSnapshotter

instance IsObjCObject (Id MKLookAroundSnapshotter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKLookAroundSnapshotter"

class IsNSObject a => IsMKLookAroundSnapshotter a where
  toMKLookAroundSnapshotter :: a -> Id MKLookAroundSnapshotter

instance IsMKLookAroundSnapshotter (Id MKLookAroundSnapshotter) where
  toMKLookAroundSnapshotter = unsafeCastId

instance IsNSObject (Id MKLookAroundSnapshotter) where
  toNSObject = unsafeCastId

-- ---------- MKMapCamera ----------

-- | Phantom type for @MKMapCamera@.
data MKMapCamera

instance IsObjCObject (Id MKMapCamera) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKMapCamera"

class IsNSObject a => IsMKMapCamera a where
  toMKMapCamera :: a -> Id MKMapCamera

instance IsMKMapCamera (Id MKMapCamera) where
  toMKMapCamera = unsafeCastId

instance IsNSObject (Id MKMapCamera) where
  toNSObject = unsafeCastId

-- ---------- MKMapCameraBoundary ----------

-- | Phantom type for @MKMapCameraBoundary@.
data MKMapCameraBoundary

instance IsObjCObject (Id MKMapCameraBoundary) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKMapCameraBoundary"

class IsNSObject a => IsMKMapCameraBoundary a where
  toMKMapCameraBoundary :: a -> Id MKMapCameraBoundary

instance IsMKMapCameraBoundary (Id MKMapCameraBoundary) where
  toMKMapCameraBoundary = unsafeCastId

instance IsNSObject (Id MKMapCameraBoundary) where
  toNSObject = unsafeCastId

-- ---------- MKMapCameraZoomRange ----------

-- | Phantom type for @MKMapCameraZoomRange@.
data MKMapCameraZoomRange

instance IsObjCObject (Id MKMapCameraZoomRange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKMapCameraZoomRange"

class IsNSObject a => IsMKMapCameraZoomRange a where
  toMKMapCameraZoomRange :: a -> Id MKMapCameraZoomRange

instance IsMKMapCameraZoomRange (Id MKMapCameraZoomRange) where
  toMKMapCameraZoomRange = unsafeCastId

instance IsNSObject (Id MKMapCameraZoomRange) where
  toNSObject = unsafeCastId

-- ---------- MKMapConfiguration ----------

-- | Phantom type for @MKMapConfiguration@.
data MKMapConfiguration

instance IsObjCObject (Id MKMapConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKMapConfiguration"

class IsNSObject a => IsMKMapConfiguration a where
  toMKMapConfiguration :: a -> Id MKMapConfiguration

instance IsMKMapConfiguration (Id MKMapConfiguration) where
  toMKMapConfiguration = unsafeCastId

instance IsNSObject (Id MKMapConfiguration) where
  toNSObject = unsafeCastId

-- ---------- MKMapItem ----------

-- | Phantom type for @MKMapItem@.
data MKMapItem

instance IsObjCObject (Id MKMapItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKMapItem"

class IsNSObject a => IsMKMapItem a where
  toMKMapItem :: a -> Id MKMapItem

instance IsMKMapItem (Id MKMapItem) where
  toMKMapItem = unsafeCastId

instance IsNSObject (Id MKMapItem) where
  toNSObject = unsafeCastId

-- ---------- MKMapItemAnnotation ----------

-- | Phantom type for @MKMapItemAnnotation@.
data MKMapItemAnnotation

instance IsObjCObject (Id MKMapItemAnnotation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKMapItemAnnotation"

class IsNSObject a => IsMKMapItemAnnotation a where
  toMKMapItemAnnotation :: a -> Id MKMapItemAnnotation

instance IsMKMapItemAnnotation (Id MKMapItemAnnotation) where
  toMKMapItemAnnotation = unsafeCastId

instance IsNSObject (Id MKMapItemAnnotation) where
  toNSObject = unsafeCastId

-- ---------- MKMapItemDetailSelectionAccessoryPresentationStyle ----------

-- | Phantom type for @MKMapItemDetailSelectionAccessoryPresentationStyle@.
data MKMapItemDetailSelectionAccessoryPresentationStyle

instance IsObjCObject (Id MKMapItemDetailSelectionAccessoryPresentationStyle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKMapItemDetailSelectionAccessoryPresentationStyle"

class IsNSObject a => IsMKMapItemDetailSelectionAccessoryPresentationStyle a where
  toMKMapItemDetailSelectionAccessoryPresentationStyle :: a -> Id MKMapItemDetailSelectionAccessoryPresentationStyle

instance IsMKMapItemDetailSelectionAccessoryPresentationStyle (Id MKMapItemDetailSelectionAccessoryPresentationStyle) where
  toMKMapItemDetailSelectionAccessoryPresentationStyle = unsafeCastId

instance IsNSObject (Id MKMapItemDetailSelectionAccessoryPresentationStyle) where
  toNSObject = unsafeCastId

-- ---------- MKMapItemIdentifier ----------

-- | Phantom type for @MKMapItemIdentifier@.
data MKMapItemIdentifier

instance IsObjCObject (Id MKMapItemIdentifier) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKMapItemIdentifier"

class IsNSObject a => IsMKMapItemIdentifier a where
  toMKMapItemIdentifier :: a -> Id MKMapItemIdentifier

instance IsMKMapItemIdentifier (Id MKMapItemIdentifier) where
  toMKMapItemIdentifier = unsafeCastId

instance IsNSObject (Id MKMapItemIdentifier) where
  toNSObject = unsafeCastId

-- ---------- MKMapItemRequest ----------

-- | Phantom type for @MKMapItemRequest@.
data MKMapItemRequest

instance IsObjCObject (Id MKMapItemRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKMapItemRequest"

class IsNSObject a => IsMKMapItemRequest a where
  toMKMapItemRequest :: a -> Id MKMapItemRequest

instance IsMKMapItemRequest (Id MKMapItemRequest) where
  toMKMapItemRequest = unsafeCastId

instance IsNSObject (Id MKMapItemRequest) where
  toNSObject = unsafeCastId

-- ---------- MKMapSnapshot ----------

-- | Phantom type for @MKMapSnapshot@.
data MKMapSnapshot

instance IsObjCObject (Id MKMapSnapshot) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKMapSnapshot"

class IsNSObject a => IsMKMapSnapshot a where
  toMKMapSnapshot :: a -> Id MKMapSnapshot

instance IsMKMapSnapshot (Id MKMapSnapshot) where
  toMKMapSnapshot = unsafeCastId

instance IsNSObject (Id MKMapSnapshot) where
  toNSObject = unsafeCastId

-- ---------- MKMapSnapshotOptions ----------

-- | Phantom type for @MKMapSnapshotOptions@.
data MKMapSnapshotOptions

instance IsObjCObject (Id MKMapSnapshotOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKMapSnapshotOptions"

class IsNSObject a => IsMKMapSnapshotOptions a where
  toMKMapSnapshotOptions :: a -> Id MKMapSnapshotOptions

instance IsMKMapSnapshotOptions (Id MKMapSnapshotOptions) where
  toMKMapSnapshotOptions = unsafeCastId

instance IsNSObject (Id MKMapSnapshotOptions) where
  toNSObject = unsafeCastId

-- ---------- MKMapSnapshotter ----------

-- | Phantom type for @MKMapSnapshotter@.
data MKMapSnapshotter

instance IsObjCObject (Id MKMapSnapshotter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKMapSnapshotter"

class IsNSObject a => IsMKMapSnapshotter a where
  toMKMapSnapshotter :: a -> Id MKMapSnapshotter

instance IsMKMapSnapshotter (Id MKMapSnapshotter) where
  toMKMapSnapshotter = unsafeCastId

instance IsNSObject (Id MKMapSnapshotter) where
  toNSObject = unsafeCastId

-- ---------- MKOverlayRenderer ----------

-- | Phantom type for @MKOverlayRenderer@.
data MKOverlayRenderer

instance IsObjCObject (Id MKOverlayRenderer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKOverlayRenderer"

class IsNSObject a => IsMKOverlayRenderer a where
  toMKOverlayRenderer :: a -> Id MKOverlayRenderer

instance IsMKOverlayRenderer (Id MKOverlayRenderer) where
  toMKOverlayRenderer = unsafeCastId

instance IsNSObject (Id MKOverlayRenderer) where
  toNSObject = unsafeCastId

-- ---------- MKPointOfInterestFilter ----------

-- | Phantom type for @MKPointOfInterestFilter@.
data MKPointOfInterestFilter

instance IsObjCObject (Id MKPointOfInterestFilter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKPointOfInterestFilter"

class IsNSObject a => IsMKPointOfInterestFilter a where
  toMKPointOfInterestFilter :: a -> Id MKPointOfInterestFilter

instance IsMKPointOfInterestFilter (Id MKPointOfInterestFilter) where
  toMKPointOfInterestFilter = unsafeCastId

instance IsNSObject (Id MKPointOfInterestFilter) where
  toNSObject = unsafeCastId

-- ---------- MKReverseGeocodingRequest ----------

-- | Phantom type for @MKReverseGeocodingRequest@.
data MKReverseGeocodingRequest

instance IsObjCObject (Id MKReverseGeocodingRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKReverseGeocodingRequest"

class IsNSObject a => IsMKReverseGeocodingRequest a where
  toMKReverseGeocodingRequest :: a -> Id MKReverseGeocodingRequest

instance IsMKReverseGeocodingRequest (Id MKReverseGeocodingRequest) where
  toMKReverseGeocodingRequest = unsafeCastId

instance IsNSObject (Id MKReverseGeocodingRequest) where
  toNSObject = unsafeCastId

-- ---------- MKRoute ----------

-- | Phantom type for @MKRoute@.
data MKRoute

instance IsObjCObject (Id MKRoute) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKRoute"

class IsNSObject a => IsMKRoute a where
  toMKRoute :: a -> Id MKRoute

instance IsMKRoute (Id MKRoute) where
  toMKRoute = unsafeCastId

instance IsNSObject (Id MKRoute) where
  toNSObject = unsafeCastId

-- ---------- MKRouteStep ----------

-- | Phantom type for @MKRouteStep@.
data MKRouteStep

instance IsObjCObject (Id MKRouteStep) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKRouteStep"

class IsNSObject a => IsMKRouteStep a where
  toMKRouteStep :: a -> Id MKRouteStep

instance IsMKRouteStep (Id MKRouteStep) where
  toMKRouteStep = unsafeCastId

instance IsNSObject (Id MKRouteStep) where
  toNSObject = unsafeCastId

-- ---------- MKSelectionAccessory ----------

-- | Phantom type for @MKSelectionAccessory@.
data MKSelectionAccessory

instance IsObjCObject (Id MKSelectionAccessory) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKSelectionAccessory"

class IsNSObject a => IsMKSelectionAccessory a where
  toMKSelectionAccessory :: a -> Id MKSelectionAccessory

instance IsMKSelectionAccessory (Id MKSelectionAccessory) where
  toMKSelectionAccessory = unsafeCastId

instance IsNSObject (Id MKSelectionAccessory) where
  toNSObject = unsafeCastId

-- ---------- MKShape ----------

-- | Phantom type for @MKShape@.
data MKShape

instance IsObjCObject (Id MKShape) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKShape"

class IsNSObject a => IsMKShape a where
  toMKShape :: a -> Id MKShape

instance IsMKShape (Id MKShape) where
  toMKShape = unsafeCastId

instance IsNSObject (Id MKShape) where
  toNSObject = unsafeCastId

-- ---------- MKTileOverlay ----------

-- | Phantom type for @MKTileOverlay@.
data MKTileOverlay

instance IsObjCObject (Id MKTileOverlay) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKTileOverlay"

class IsNSObject a => IsMKTileOverlay a where
  toMKTileOverlay :: a -> Id MKTileOverlay

instance IsMKTileOverlay (Id MKTileOverlay) where
  toMKTileOverlay = unsafeCastId

instance IsNSObject (Id MKTileOverlay) where
  toNSObject = unsafeCastId

-- ---------- MKUserLocation ----------

-- | Phantom type for @MKUserLocation@.
data MKUserLocation

instance IsObjCObject (Id MKUserLocation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKUserLocation"

class IsNSObject a => IsMKUserLocation a where
  toMKUserLocation :: a -> Id MKUserLocation

instance IsMKUserLocation (Id MKUserLocation) where
  toMKUserLocation = unsafeCastId

instance IsNSObject (Id MKUserLocation) where
  toNSObject = unsafeCastId

-- ---------- MKPlacemark ----------

-- | Phantom type for @MKPlacemark@.
data MKPlacemark

instance IsObjCObject (Id MKPlacemark) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKPlacemark"

class IsCLPlacemark a => IsMKPlacemark a where
  toMKPlacemark :: a -> Id MKPlacemark

instance IsMKPlacemark (Id MKPlacemark) where
  toMKPlacemark = unsafeCastId

instance IsCLPlacemark (Id MKPlacemark) where
  toCLPlacemark = unsafeCastId

instance IsNSObject (Id MKPlacemark) where
  toNSObject = unsafeCastId

-- ---------- MKHybridMapConfiguration ----------

-- | Phantom type for @MKHybridMapConfiguration@.
data MKHybridMapConfiguration

instance IsObjCObject (Id MKHybridMapConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKHybridMapConfiguration"

class IsMKMapConfiguration a => IsMKHybridMapConfiguration a where
  toMKHybridMapConfiguration :: a -> Id MKHybridMapConfiguration

instance IsMKHybridMapConfiguration (Id MKHybridMapConfiguration) where
  toMKHybridMapConfiguration = unsafeCastId

instance IsMKMapConfiguration (Id MKHybridMapConfiguration) where
  toMKMapConfiguration = unsafeCastId

instance IsNSObject (Id MKHybridMapConfiguration) where
  toNSObject = unsafeCastId

-- ---------- MKImageryMapConfiguration ----------

-- | Phantom type for @MKImageryMapConfiguration@.
data MKImageryMapConfiguration

instance IsObjCObject (Id MKImageryMapConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKImageryMapConfiguration"

class IsMKMapConfiguration a => IsMKImageryMapConfiguration a where
  toMKImageryMapConfiguration :: a -> Id MKImageryMapConfiguration

instance IsMKImageryMapConfiguration (Id MKImageryMapConfiguration) where
  toMKImageryMapConfiguration = unsafeCastId

instance IsMKMapConfiguration (Id MKImageryMapConfiguration) where
  toMKMapConfiguration = unsafeCastId

instance IsNSObject (Id MKImageryMapConfiguration) where
  toNSObject = unsafeCastId

-- ---------- MKStandardMapConfiguration ----------

-- | Phantom type for @MKStandardMapConfiguration@.
data MKStandardMapConfiguration

instance IsObjCObject (Id MKStandardMapConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKStandardMapConfiguration"

class IsMKMapConfiguration a => IsMKStandardMapConfiguration a where
  toMKStandardMapConfiguration :: a -> Id MKStandardMapConfiguration

instance IsMKStandardMapConfiguration (Id MKStandardMapConfiguration) where
  toMKStandardMapConfiguration = unsafeCastId

instance IsMKMapConfiguration (Id MKStandardMapConfiguration) where
  toMKMapConfiguration = unsafeCastId

instance IsNSObject (Id MKStandardMapConfiguration) where
  toNSObject = unsafeCastId

-- ---------- MKOverlayPathRenderer ----------

-- | Phantom type for @MKOverlayPathRenderer@.
data MKOverlayPathRenderer

instance IsObjCObject (Id MKOverlayPathRenderer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKOverlayPathRenderer"

class IsMKOverlayRenderer a => IsMKOverlayPathRenderer a where
  toMKOverlayPathRenderer :: a -> Id MKOverlayPathRenderer

instance IsMKOverlayPathRenderer (Id MKOverlayPathRenderer) where
  toMKOverlayPathRenderer = unsafeCastId

instance IsMKOverlayRenderer (Id MKOverlayPathRenderer) where
  toMKOverlayRenderer = unsafeCastId

instance IsNSObject (Id MKOverlayPathRenderer) where
  toNSObject = unsafeCastId

-- ---------- MKTileOverlayRenderer ----------

-- | Phantom type for @MKTileOverlayRenderer@.
data MKTileOverlayRenderer

instance IsObjCObject (Id MKTileOverlayRenderer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKTileOverlayRenderer"

class IsMKOverlayRenderer a => IsMKTileOverlayRenderer a where
  toMKTileOverlayRenderer :: a -> Id MKTileOverlayRenderer

instance IsMKTileOverlayRenderer (Id MKTileOverlayRenderer) where
  toMKTileOverlayRenderer = unsafeCastId

instance IsMKOverlayRenderer (Id MKTileOverlayRenderer) where
  toMKOverlayRenderer = unsafeCastId

instance IsNSObject (Id MKTileOverlayRenderer) where
  toNSObject = unsafeCastId

-- ---------- MKCircle ----------

-- | Phantom type for @MKCircle@.
data MKCircle

instance IsObjCObject (Id MKCircle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKCircle"

class IsMKShape a => IsMKCircle a where
  toMKCircle :: a -> Id MKCircle

instance IsMKCircle (Id MKCircle) where
  toMKCircle = unsafeCastId

instance IsMKShape (Id MKCircle) where
  toMKShape = unsafeCastId

instance IsNSObject (Id MKCircle) where
  toNSObject = unsafeCastId

-- ---------- MKMultiPoint ----------

-- | Phantom type for @MKMultiPoint@.
data MKMultiPoint

instance IsObjCObject (Id MKMultiPoint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKMultiPoint"

class IsMKShape a => IsMKMultiPoint a where
  toMKMultiPoint :: a -> Id MKMultiPoint

instance IsMKMultiPoint (Id MKMultiPoint) where
  toMKMultiPoint = unsafeCastId

instance IsMKShape (Id MKMultiPoint) where
  toMKShape = unsafeCastId

instance IsNSObject (Id MKMultiPoint) where
  toNSObject = unsafeCastId

-- ---------- MKMultiPolygon ----------

-- | Phantom type for @MKMultiPolygon@.
data MKMultiPolygon

instance IsObjCObject (Id MKMultiPolygon) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKMultiPolygon"

class IsMKShape a => IsMKMultiPolygon a where
  toMKMultiPolygon :: a -> Id MKMultiPolygon

instance IsMKMultiPolygon (Id MKMultiPolygon) where
  toMKMultiPolygon = unsafeCastId

instance IsMKShape (Id MKMultiPolygon) where
  toMKShape = unsafeCastId

instance IsNSObject (Id MKMultiPolygon) where
  toNSObject = unsafeCastId

-- ---------- MKMultiPolyline ----------

-- | Phantom type for @MKMultiPolyline@.
data MKMultiPolyline

instance IsObjCObject (Id MKMultiPolyline) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKMultiPolyline"

class IsMKShape a => IsMKMultiPolyline a where
  toMKMultiPolyline :: a -> Id MKMultiPolyline

instance IsMKMultiPolyline (Id MKMultiPolyline) where
  toMKMultiPolyline = unsafeCastId

instance IsMKShape (Id MKMultiPolyline) where
  toMKShape = unsafeCastId

instance IsNSObject (Id MKMultiPolyline) where
  toNSObject = unsafeCastId

-- ---------- MKPointAnnotation ----------

-- | Phantom type for @MKPointAnnotation@.
data MKPointAnnotation

instance IsObjCObject (Id MKPointAnnotation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKPointAnnotation"

class IsMKShape a => IsMKPointAnnotation a where
  toMKPointAnnotation :: a -> Id MKPointAnnotation

instance IsMKPointAnnotation (Id MKPointAnnotation) where
  toMKPointAnnotation = unsafeCastId

instance IsMKShape (Id MKPointAnnotation) where
  toMKShape = unsafeCastId

instance IsNSObject (Id MKPointAnnotation) where
  toNSObject = unsafeCastId

-- ---------- MKDistanceFormatter ----------

-- | Phantom type for @MKDistanceFormatter@.
data MKDistanceFormatter

instance IsObjCObject (Id MKDistanceFormatter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKDistanceFormatter"

class IsNSFormatter a => IsMKDistanceFormatter a where
  toMKDistanceFormatter :: a -> Id MKDistanceFormatter

instance IsMKDistanceFormatter (Id MKDistanceFormatter) where
  toMKDistanceFormatter = unsafeCastId

instance IsNSFormatter (Id MKDistanceFormatter) where
  toNSFormatter = unsafeCastId

instance IsNSObject (Id MKDistanceFormatter) where
  toNSObject = unsafeCastId

-- ---------- MKCircleRenderer ----------

-- | Phantom type for @MKCircleRenderer@.
data MKCircleRenderer

instance IsObjCObject (Id MKCircleRenderer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKCircleRenderer"

class IsMKOverlayPathRenderer a => IsMKCircleRenderer a where
  toMKCircleRenderer :: a -> Id MKCircleRenderer

instance IsMKCircleRenderer (Id MKCircleRenderer) where
  toMKCircleRenderer = unsafeCastId

instance IsMKOverlayPathRenderer (Id MKCircleRenderer) where
  toMKOverlayPathRenderer = unsafeCastId

instance IsMKOverlayRenderer (Id MKCircleRenderer) where
  toMKOverlayRenderer = unsafeCastId

instance IsNSObject (Id MKCircleRenderer) where
  toNSObject = unsafeCastId

-- ---------- MKMultiPolygonRenderer ----------

-- | Phantom type for @MKMultiPolygonRenderer@.
data MKMultiPolygonRenderer

instance IsObjCObject (Id MKMultiPolygonRenderer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKMultiPolygonRenderer"

class IsMKOverlayPathRenderer a => IsMKMultiPolygonRenderer a where
  toMKMultiPolygonRenderer :: a -> Id MKMultiPolygonRenderer

instance IsMKMultiPolygonRenderer (Id MKMultiPolygonRenderer) where
  toMKMultiPolygonRenderer = unsafeCastId

instance IsMKOverlayPathRenderer (Id MKMultiPolygonRenderer) where
  toMKOverlayPathRenderer = unsafeCastId

instance IsMKOverlayRenderer (Id MKMultiPolygonRenderer) where
  toMKOverlayRenderer = unsafeCastId

instance IsNSObject (Id MKMultiPolygonRenderer) where
  toNSObject = unsafeCastId

-- ---------- MKMultiPolylineRenderer ----------

-- | Phantom type for @MKMultiPolylineRenderer@.
data MKMultiPolylineRenderer

instance IsObjCObject (Id MKMultiPolylineRenderer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKMultiPolylineRenderer"

class IsMKOverlayPathRenderer a => IsMKMultiPolylineRenderer a where
  toMKMultiPolylineRenderer :: a -> Id MKMultiPolylineRenderer

instance IsMKMultiPolylineRenderer (Id MKMultiPolylineRenderer) where
  toMKMultiPolylineRenderer = unsafeCastId

instance IsMKOverlayPathRenderer (Id MKMultiPolylineRenderer) where
  toMKOverlayPathRenderer = unsafeCastId

instance IsMKOverlayRenderer (Id MKMultiPolylineRenderer) where
  toMKOverlayRenderer = unsafeCastId

instance IsNSObject (Id MKMultiPolylineRenderer) where
  toNSObject = unsafeCastId

-- ---------- MKPolygonRenderer ----------

-- | Phantom type for @MKPolygonRenderer@.
data MKPolygonRenderer

instance IsObjCObject (Id MKPolygonRenderer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKPolygonRenderer"

class IsMKOverlayPathRenderer a => IsMKPolygonRenderer a where
  toMKPolygonRenderer :: a -> Id MKPolygonRenderer

instance IsMKPolygonRenderer (Id MKPolygonRenderer) where
  toMKPolygonRenderer = unsafeCastId

instance IsMKOverlayPathRenderer (Id MKPolygonRenderer) where
  toMKOverlayPathRenderer = unsafeCastId

instance IsMKOverlayRenderer (Id MKPolygonRenderer) where
  toMKOverlayRenderer = unsafeCastId

instance IsNSObject (Id MKPolygonRenderer) where
  toNSObject = unsafeCastId

-- ---------- MKPolylineRenderer ----------

-- | Phantom type for @MKPolylineRenderer@.
data MKPolylineRenderer

instance IsObjCObject (Id MKPolylineRenderer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKPolylineRenderer"

class IsMKOverlayPathRenderer a => IsMKPolylineRenderer a where
  toMKPolylineRenderer :: a -> Id MKPolylineRenderer

instance IsMKPolylineRenderer (Id MKPolylineRenderer) where
  toMKPolylineRenderer = unsafeCastId

instance IsMKOverlayPathRenderer (Id MKPolylineRenderer) where
  toMKOverlayPathRenderer = unsafeCastId

instance IsMKOverlayRenderer (Id MKPolylineRenderer) where
  toMKOverlayRenderer = unsafeCastId

instance IsNSObject (Id MKPolylineRenderer) where
  toNSObject = unsafeCastId

-- ---------- MKPolygon ----------

-- | Phantom type for @MKPolygon@.
data MKPolygon

instance IsObjCObject (Id MKPolygon) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKPolygon"

class IsMKMultiPoint a => IsMKPolygon a where
  toMKPolygon :: a -> Id MKPolygon

instance IsMKPolygon (Id MKPolygon) where
  toMKPolygon = unsafeCastId

instance IsMKMultiPoint (Id MKPolygon) where
  toMKMultiPoint = unsafeCastId

instance IsMKShape (Id MKPolygon) where
  toMKShape = unsafeCastId

instance IsNSObject (Id MKPolygon) where
  toNSObject = unsafeCastId

-- ---------- MKPolyline ----------

-- | Phantom type for @MKPolyline@.
data MKPolyline

instance IsObjCObject (Id MKPolyline) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKPolyline"

class IsMKMultiPoint a => IsMKPolyline a where
  toMKPolyline :: a -> Id MKPolyline

instance IsMKPolyline (Id MKPolyline) where
  toMKPolyline = unsafeCastId

instance IsMKMultiPoint (Id MKPolyline) where
  toMKMultiPoint = unsafeCastId

instance IsMKShape (Id MKPolyline) where
  toMKShape = unsafeCastId

instance IsNSObject (Id MKPolyline) where
  toNSObject = unsafeCastId

-- ---------- MKAnnotationView ----------

-- | Phantom type for @MKAnnotationView@.
data MKAnnotationView

instance IsObjCObject (Id MKAnnotationView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKAnnotationView"

class IsNSView a => IsMKAnnotationView a where
  toMKAnnotationView :: a -> Id MKAnnotationView

instance IsMKAnnotationView (Id MKAnnotationView) where
  toMKAnnotationView = unsafeCastId

instance IsNSObject (Id MKAnnotationView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id MKAnnotationView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id MKAnnotationView) where
  toNSView = unsafeCastId

-- ---------- MKCompassButton ----------

-- | Phantom type for @MKCompassButton@.
data MKCompassButton

instance IsObjCObject (Id MKCompassButton) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKCompassButton"

class IsNSView a => IsMKCompassButton a where
  toMKCompassButton :: a -> Id MKCompassButton

instance IsMKCompassButton (Id MKCompassButton) where
  toMKCompassButton = unsafeCastId

instance IsNSObject (Id MKCompassButton) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id MKCompassButton) where
  toNSResponder = unsafeCastId

instance IsNSView (Id MKCompassButton) where
  toNSView = unsafeCastId

-- ---------- MKMapView ----------

-- | Phantom type for @MKMapView@.
data MKMapView

instance IsObjCObject (Id MKMapView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKMapView"

class IsNSView a => IsMKMapView a where
  toMKMapView :: a -> Id MKMapView

instance IsMKMapView (Id MKMapView) where
  toMKMapView = unsafeCastId

instance IsNSObject (Id MKMapView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id MKMapView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id MKMapView) where
  toNSView = unsafeCastId

-- ---------- MKPitchControl ----------

-- | Phantom type for @MKPitchControl@.
data MKPitchControl

instance IsObjCObject (Id MKPitchControl) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKPitchControl"

class IsNSView a => IsMKPitchControl a where
  toMKPitchControl :: a -> Id MKPitchControl

instance IsMKPitchControl (Id MKPitchControl) where
  toMKPitchControl = unsafeCastId

instance IsNSObject (Id MKPitchControl) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id MKPitchControl) where
  toNSResponder = unsafeCastId

instance IsNSView (Id MKPitchControl) where
  toNSView = unsafeCastId

-- ---------- MKZoomControl ----------

-- | Phantom type for @MKZoomControl@.
data MKZoomControl

instance IsObjCObject (Id MKZoomControl) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKZoomControl"

class IsNSView a => IsMKZoomControl a where
  toMKZoomControl :: a -> Id MKZoomControl

instance IsMKZoomControl (Id MKZoomControl) where
  toMKZoomControl = unsafeCastId

instance IsNSObject (Id MKZoomControl) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id MKZoomControl) where
  toNSResponder = unsafeCastId

instance IsNSView (Id MKZoomControl) where
  toNSView = unsafeCastId

-- ---------- MKLookAroundViewController ----------

-- | Phantom type for @MKLookAroundViewController@.
data MKLookAroundViewController

instance IsObjCObject (Id MKLookAroundViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKLookAroundViewController"

class IsNSViewController a => IsMKLookAroundViewController a where
  toMKLookAroundViewController :: a -> Id MKLookAroundViewController

instance IsMKLookAroundViewController (Id MKLookAroundViewController) where
  toMKLookAroundViewController = unsafeCastId

instance IsNSObject (Id MKLookAroundViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id MKLookAroundViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id MKLookAroundViewController) where
  toNSViewController = unsafeCastId

-- ---------- MKMapItemDetailViewController ----------

-- | Phantom type for @MKMapItemDetailViewController@.
data MKMapItemDetailViewController

instance IsObjCObject (Id MKMapItemDetailViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKMapItemDetailViewController"

class IsNSViewController a => IsMKMapItemDetailViewController a where
  toMKMapItemDetailViewController :: a -> Id MKMapItemDetailViewController

instance IsMKMapItemDetailViewController (Id MKMapItemDetailViewController) where
  toMKMapItemDetailViewController = unsafeCastId

instance IsNSObject (Id MKMapItemDetailViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id MKMapItemDetailViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id MKMapItemDetailViewController) where
  toNSViewController = unsafeCastId

-- ---------- MKGradientPolylineRenderer ----------

-- | Phantom type for @MKGradientPolylineRenderer@.
data MKGradientPolylineRenderer

instance IsObjCObject (Id MKGradientPolylineRenderer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKGradientPolylineRenderer"

class IsMKPolylineRenderer a => IsMKGradientPolylineRenderer a where
  toMKGradientPolylineRenderer :: a -> Id MKGradientPolylineRenderer

instance IsMKGradientPolylineRenderer (Id MKGradientPolylineRenderer) where
  toMKGradientPolylineRenderer = unsafeCastId

instance IsMKOverlayPathRenderer (Id MKGradientPolylineRenderer) where
  toMKOverlayPathRenderer = unsafeCastId

instance IsMKOverlayRenderer (Id MKGradientPolylineRenderer) where
  toMKOverlayRenderer = unsafeCastId

instance IsMKPolylineRenderer (Id MKGradientPolylineRenderer) where
  toMKPolylineRenderer = unsafeCastId

instance IsNSObject (Id MKGradientPolylineRenderer) where
  toNSObject = unsafeCastId

-- ---------- MKGeodesicPolyline ----------

-- | Phantom type for @MKGeodesicPolyline@.
data MKGeodesicPolyline

instance IsObjCObject (Id MKGeodesicPolyline) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKGeodesicPolyline"

class IsMKPolyline a => IsMKGeodesicPolyline a where
  toMKGeodesicPolyline :: a -> Id MKGeodesicPolyline

instance IsMKGeodesicPolyline (Id MKGeodesicPolyline) where
  toMKGeodesicPolyline = unsafeCastId

instance IsMKMultiPoint (Id MKGeodesicPolyline) where
  toMKMultiPoint = unsafeCastId

instance IsMKPolyline (Id MKGeodesicPolyline) where
  toMKPolyline = unsafeCastId

instance IsMKShape (Id MKGeodesicPolyline) where
  toMKShape = unsafeCastId

instance IsNSObject (Id MKGeodesicPolyline) where
  toNSObject = unsafeCastId

-- ---------- MKMarkerAnnotationView ----------

-- | Phantom type for @MKMarkerAnnotationView@.
data MKMarkerAnnotationView

instance IsObjCObject (Id MKMarkerAnnotationView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKMarkerAnnotationView"

class IsMKAnnotationView a => IsMKMarkerAnnotationView a where
  toMKMarkerAnnotationView :: a -> Id MKMarkerAnnotationView

instance IsMKMarkerAnnotationView (Id MKMarkerAnnotationView) where
  toMKMarkerAnnotationView = unsafeCastId

instance IsMKAnnotationView (Id MKMarkerAnnotationView) where
  toMKAnnotationView = unsafeCastId

instance IsNSObject (Id MKMarkerAnnotationView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id MKMarkerAnnotationView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id MKMarkerAnnotationView) where
  toNSView = unsafeCastId

-- ---------- MKPinAnnotationView ----------

-- | Phantom type for @MKPinAnnotationView@.
data MKPinAnnotationView

instance IsObjCObject (Id MKPinAnnotationView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKPinAnnotationView"

class IsMKAnnotationView a => IsMKPinAnnotationView a where
  toMKPinAnnotationView :: a -> Id MKPinAnnotationView

instance IsMKPinAnnotationView (Id MKPinAnnotationView) where
  toMKPinAnnotationView = unsafeCastId

instance IsMKAnnotationView (Id MKPinAnnotationView) where
  toMKAnnotationView = unsafeCastId

instance IsNSObject (Id MKPinAnnotationView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id MKPinAnnotationView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id MKPinAnnotationView) where
  toNSView = unsafeCastId

-- ---------- MKUserLocationView ----------

-- | Phantom type for @MKUserLocationView@.
data MKUserLocationView

instance IsObjCObject (Id MKUserLocationView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MKUserLocationView"

class IsMKAnnotationView a => IsMKUserLocationView a where
  toMKUserLocationView :: a -> Id MKUserLocationView

instance IsMKUserLocationView (Id MKUserLocationView) where
  toMKUserLocationView = unsafeCastId

instance IsMKAnnotationView (Id MKUserLocationView) where
  toMKAnnotationView = unsafeCastId

instance IsNSObject (Id MKUserLocationView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id MKUserLocationView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id MKUserLocationView) where
  toNSView = unsafeCastId
