{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKAnnotationView@.
module ObjC.MapKit.MKAnnotationView
  ( MKAnnotationView
  , IsMKAnnotationView(..)
  , initWithAnnotation_reuseIdentifier
  , initWithCoder
  , prepareForReuse
  , prepareForDisplay
  , setSelected_animated
  , setDragState_animated
  , reuseIdentifier
  , annotation
  , setAnnotation
  , image
  , setImage
  , enabled
  , setEnabled
  , highlighted
  , setHighlighted
  , selected
  , setSelected
  , canShowCallout
  , setCanShowCallout
  , leftCalloutAccessoryView
  , setLeftCalloutAccessoryView
  , rightCalloutAccessoryView
  , setRightCalloutAccessoryView
  , detailCalloutAccessoryView
  , setDetailCalloutAccessoryView
  , draggable
  , setDraggable
  , dragState
  , setDragState
  , clusteringIdentifier
  , setClusteringIdentifier
  , clusterAnnotationView
  , displayPriority
  , setDisplayPriority
  , zPriority
  , setZPriority
  , selectedZPriority
  , setSelectedZPriority
  , collisionMode
  , setCollisionMode
  , annotationSelector
  , canShowCalloutSelector
  , clusterAnnotationViewSelector
  , clusteringIdentifierSelector
  , collisionModeSelector
  , detailCalloutAccessoryViewSelector
  , displayPrioritySelector
  , dragStateSelector
  , draggableSelector
  , enabledSelector
  , highlightedSelector
  , imageSelector
  , initWithAnnotation_reuseIdentifierSelector
  , initWithCoderSelector
  , leftCalloutAccessoryViewSelector
  , prepareForDisplaySelector
  , prepareForReuseSelector
  , reuseIdentifierSelector
  , rightCalloutAccessoryViewSelector
  , selectedSelector
  , selectedZPrioritySelector
  , setAnnotationSelector
  , setCanShowCalloutSelector
  , setClusteringIdentifierSelector
  , setCollisionModeSelector
  , setDetailCalloutAccessoryViewSelector
  , setDisplayPrioritySelector
  , setDragStateSelector
  , setDragState_animatedSelector
  , setDraggableSelector
  , setEnabledSelector
  , setHighlightedSelector
  , setImageSelector
  , setLeftCalloutAccessoryViewSelector
  , setRightCalloutAccessoryViewSelector
  , setSelectedSelector
  , setSelectedZPrioritySelector
  , setSelected_animatedSelector
  , setZPrioritySelector
  , zPrioritySelector

  -- * Enum types
  , MKAnnotationViewCollisionMode(MKAnnotationViewCollisionMode)
  , pattern MKAnnotationViewCollisionModeRectangle
  , pattern MKAnnotationViewCollisionModeCircle
  , pattern MKAnnotationViewCollisionModeNone
  , MKAnnotationViewDragState(MKAnnotationViewDragState)
  , pattern MKAnnotationViewDragStateNone
  , pattern MKAnnotationViewDragStateStarting
  , pattern MKAnnotationViewDragStateDragging
  , pattern MKAnnotationViewDragStateCanceling
  , pattern MKAnnotationViewDragStateEnding

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

-- | @- initWithAnnotation:reuseIdentifier:@
initWithAnnotation_reuseIdentifier :: (IsMKAnnotationView mkAnnotationView, IsNSString reuseIdentifier) => mkAnnotationView -> RawId -> reuseIdentifier -> IO (Id MKAnnotationView)
initWithAnnotation_reuseIdentifier mkAnnotationView annotation reuseIdentifier =
  sendOwnedMessage mkAnnotationView initWithAnnotation_reuseIdentifierSelector annotation (toNSString reuseIdentifier)

-- | @- initWithCoder:@
initWithCoder :: (IsMKAnnotationView mkAnnotationView, IsNSCoder aDecoder) => mkAnnotationView -> aDecoder -> IO (Id MKAnnotationView)
initWithCoder mkAnnotationView aDecoder =
  sendOwnedMessage mkAnnotationView initWithCoderSelector (toNSCoder aDecoder)

-- | @- prepareForReuse@
prepareForReuse :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO ()
prepareForReuse mkAnnotationView =
  sendMessage mkAnnotationView prepareForReuseSelector

-- | @- prepareForDisplay@
prepareForDisplay :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO ()
prepareForDisplay mkAnnotationView =
  sendMessage mkAnnotationView prepareForDisplaySelector

-- | @- setSelected:animated:@
setSelected_animated :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> Bool -> Bool -> IO ()
setSelected_animated mkAnnotationView selected animated =
  sendMessage mkAnnotationView setSelected_animatedSelector selected animated

-- | @- setDragState:animated:@
setDragState_animated :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> MKAnnotationViewDragState -> Bool -> IO ()
setDragState_animated mkAnnotationView newDragState animated =
  sendMessage mkAnnotationView setDragState_animatedSelector newDragState animated

-- | @- reuseIdentifier@
reuseIdentifier :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO (Id NSString)
reuseIdentifier mkAnnotationView =
  sendMessage mkAnnotationView reuseIdentifierSelector

-- | @- annotation@
annotation :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO RawId
annotation mkAnnotationView =
  sendMessage mkAnnotationView annotationSelector

-- | @- setAnnotation:@
setAnnotation :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> RawId -> IO ()
setAnnotation mkAnnotationView value =
  sendMessage mkAnnotationView setAnnotationSelector value

-- | @- image@
image :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO (Id NSImage)
image mkAnnotationView =
  sendMessage mkAnnotationView imageSelector

-- | @- setImage:@
setImage :: (IsMKAnnotationView mkAnnotationView, IsNSImage value) => mkAnnotationView -> value -> IO ()
setImage mkAnnotationView value =
  sendMessage mkAnnotationView setImageSelector (toNSImage value)

-- | @- enabled@
enabled :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO Bool
enabled mkAnnotationView =
  sendMessage mkAnnotationView enabledSelector

-- | @- setEnabled:@
setEnabled :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> Bool -> IO ()
setEnabled mkAnnotationView value =
  sendMessage mkAnnotationView setEnabledSelector value

-- | @- highlighted@
highlighted :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO Bool
highlighted mkAnnotationView =
  sendMessage mkAnnotationView highlightedSelector

-- | @- setHighlighted:@
setHighlighted :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> Bool -> IO ()
setHighlighted mkAnnotationView value =
  sendMessage mkAnnotationView setHighlightedSelector value

-- | @- selected@
selected :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO Bool
selected mkAnnotationView =
  sendMessage mkAnnotationView selectedSelector

-- | @- setSelected:@
setSelected :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> Bool -> IO ()
setSelected mkAnnotationView value =
  sendMessage mkAnnotationView setSelectedSelector value

-- | @- canShowCallout@
canShowCallout :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO Bool
canShowCallout mkAnnotationView =
  sendMessage mkAnnotationView canShowCalloutSelector

-- | @- setCanShowCallout:@
setCanShowCallout :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> Bool -> IO ()
setCanShowCallout mkAnnotationView value =
  sendMessage mkAnnotationView setCanShowCalloutSelector value

-- | @- leftCalloutAccessoryView@
leftCalloutAccessoryView :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO (Id NSView)
leftCalloutAccessoryView mkAnnotationView =
  sendMessage mkAnnotationView leftCalloutAccessoryViewSelector

-- | @- setLeftCalloutAccessoryView:@
setLeftCalloutAccessoryView :: (IsMKAnnotationView mkAnnotationView, IsNSView value) => mkAnnotationView -> value -> IO ()
setLeftCalloutAccessoryView mkAnnotationView value =
  sendMessage mkAnnotationView setLeftCalloutAccessoryViewSelector (toNSView value)

-- | @- rightCalloutAccessoryView@
rightCalloutAccessoryView :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO (Id NSView)
rightCalloutAccessoryView mkAnnotationView =
  sendMessage mkAnnotationView rightCalloutAccessoryViewSelector

-- | @- setRightCalloutAccessoryView:@
setRightCalloutAccessoryView :: (IsMKAnnotationView mkAnnotationView, IsNSView value) => mkAnnotationView -> value -> IO ()
setRightCalloutAccessoryView mkAnnotationView value =
  sendMessage mkAnnotationView setRightCalloutAccessoryViewSelector (toNSView value)

-- | @- detailCalloutAccessoryView@
detailCalloutAccessoryView :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO RawId
detailCalloutAccessoryView mkAnnotationView =
  sendMessage mkAnnotationView detailCalloutAccessoryViewSelector

-- | @- setDetailCalloutAccessoryView:@
setDetailCalloutAccessoryView :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> RawId -> IO ()
setDetailCalloutAccessoryView mkAnnotationView value =
  sendMessage mkAnnotationView setDetailCalloutAccessoryViewSelector value

-- | @- draggable@
draggable :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO Bool
draggable mkAnnotationView =
  sendMessage mkAnnotationView draggableSelector

-- | @- setDraggable:@
setDraggable :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> Bool -> IO ()
setDraggable mkAnnotationView value =
  sendMessage mkAnnotationView setDraggableSelector value

-- | @- dragState@
dragState :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO MKAnnotationViewDragState
dragState mkAnnotationView =
  sendMessage mkAnnotationView dragStateSelector

-- | @- setDragState:@
setDragState :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> MKAnnotationViewDragState -> IO ()
setDragState mkAnnotationView value =
  sendMessage mkAnnotationView setDragStateSelector value

-- | @- clusteringIdentifier@
clusteringIdentifier :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO (Id NSString)
clusteringIdentifier mkAnnotationView =
  sendMessage mkAnnotationView clusteringIdentifierSelector

-- | @- setClusteringIdentifier:@
setClusteringIdentifier :: (IsMKAnnotationView mkAnnotationView, IsNSString value) => mkAnnotationView -> value -> IO ()
setClusteringIdentifier mkAnnotationView value =
  sendMessage mkAnnotationView setClusteringIdentifierSelector (toNSString value)

-- | @- clusterAnnotationView@
clusterAnnotationView :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO (Id MKAnnotationView)
clusterAnnotationView mkAnnotationView =
  sendMessage mkAnnotationView clusterAnnotationViewSelector

-- | @- displayPriority@
displayPriority :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO CFloat
displayPriority mkAnnotationView =
  sendMessage mkAnnotationView displayPrioritySelector

-- | @- setDisplayPriority:@
setDisplayPriority :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> CFloat -> IO ()
setDisplayPriority mkAnnotationView value =
  sendMessage mkAnnotationView setDisplayPrioritySelector value

-- | @- zPriority@
zPriority :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO CFloat
zPriority mkAnnotationView =
  sendMessage mkAnnotationView zPrioritySelector

-- | @- setZPriority:@
setZPriority :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> CFloat -> IO ()
setZPriority mkAnnotationView value =
  sendMessage mkAnnotationView setZPrioritySelector value

-- | @- selectedZPriority@
selectedZPriority :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO CFloat
selectedZPriority mkAnnotationView =
  sendMessage mkAnnotationView selectedZPrioritySelector

-- | @- setSelectedZPriority:@
setSelectedZPriority :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> CFloat -> IO ()
setSelectedZPriority mkAnnotationView value =
  sendMessage mkAnnotationView setSelectedZPrioritySelector value

-- | @- collisionMode@
collisionMode :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO MKAnnotationViewCollisionMode
collisionMode mkAnnotationView =
  sendMessage mkAnnotationView collisionModeSelector

-- | @- setCollisionMode:@
setCollisionMode :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> MKAnnotationViewCollisionMode -> IO ()
setCollisionMode mkAnnotationView value =
  sendMessage mkAnnotationView setCollisionModeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAnnotation:reuseIdentifier:@
initWithAnnotation_reuseIdentifierSelector :: Selector '[RawId, Id NSString] (Id MKAnnotationView)
initWithAnnotation_reuseIdentifierSelector = mkSelector "initWithAnnotation:reuseIdentifier:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id MKAnnotationView)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @prepareForReuse@
prepareForReuseSelector :: Selector '[] ()
prepareForReuseSelector = mkSelector "prepareForReuse"

-- | @Selector@ for @prepareForDisplay@
prepareForDisplaySelector :: Selector '[] ()
prepareForDisplaySelector = mkSelector "prepareForDisplay"

-- | @Selector@ for @setSelected:animated:@
setSelected_animatedSelector :: Selector '[Bool, Bool] ()
setSelected_animatedSelector = mkSelector "setSelected:animated:"

-- | @Selector@ for @setDragState:animated:@
setDragState_animatedSelector :: Selector '[MKAnnotationViewDragState, Bool] ()
setDragState_animatedSelector = mkSelector "setDragState:animated:"

-- | @Selector@ for @reuseIdentifier@
reuseIdentifierSelector :: Selector '[] (Id NSString)
reuseIdentifierSelector = mkSelector "reuseIdentifier"

-- | @Selector@ for @annotation@
annotationSelector :: Selector '[] RawId
annotationSelector = mkSelector "annotation"

-- | @Selector@ for @setAnnotation:@
setAnnotationSelector :: Selector '[RawId] ()
setAnnotationSelector = mkSelector "setAnnotation:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id NSImage] ()
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @highlighted@
highlightedSelector :: Selector '[] Bool
highlightedSelector = mkSelector "highlighted"

-- | @Selector@ for @setHighlighted:@
setHighlightedSelector :: Selector '[Bool] ()
setHighlightedSelector = mkSelector "setHighlighted:"

-- | @Selector@ for @selected@
selectedSelector :: Selector '[] Bool
selectedSelector = mkSelector "selected"

-- | @Selector@ for @setSelected:@
setSelectedSelector :: Selector '[Bool] ()
setSelectedSelector = mkSelector "setSelected:"

-- | @Selector@ for @canShowCallout@
canShowCalloutSelector :: Selector '[] Bool
canShowCalloutSelector = mkSelector "canShowCallout"

-- | @Selector@ for @setCanShowCallout:@
setCanShowCalloutSelector :: Selector '[Bool] ()
setCanShowCalloutSelector = mkSelector "setCanShowCallout:"

-- | @Selector@ for @leftCalloutAccessoryView@
leftCalloutAccessoryViewSelector :: Selector '[] (Id NSView)
leftCalloutAccessoryViewSelector = mkSelector "leftCalloutAccessoryView"

-- | @Selector@ for @setLeftCalloutAccessoryView:@
setLeftCalloutAccessoryViewSelector :: Selector '[Id NSView] ()
setLeftCalloutAccessoryViewSelector = mkSelector "setLeftCalloutAccessoryView:"

-- | @Selector@ for @rightCalloutAccessoryView@
rightCalloutAccessoryViewSelector :: Selector '[] (Id NSView)
rightCalloutAccessoryViewSelector = mkSelector "rightCalloutAccessoryView"

-- | @Selector@ for @setRightCalloutAccessoryView:@
setRightCalloutAccessoryViewSelector :: Selector '[Id NSView] ()
setRightCalloutAccessoryViewSelector = mkSelector "setRightCalloutAccessoryView:"

-- | @Selector@ for @detailCalloutAccessoryView@
detailCalloutAccessoryViewSelector :: Selector '[] RawId
detailCalloutAccessoryViewSelector = mkSelector "detailCalloutAccessoryView"

-- | @Selector@ for @setDetailCalloutAccessoryView:@
setDetailCalloutAccessoryViewSelector :: Selector '[RawId] ()
setDetailCalloutAccessoryViewSelector = mkSelector "setDetailCalloutAccessoryView:"

-- | @Selector@ for @draggable@
draggableSelector :: Selector '[] Bool
draggableSelector = mkSelector "draggable"

-- | @Selector@ for @setDraggable:@
setDraggableSelector :: Selector '[Bool] ()
setDraggableSelector = mkSelector "setDraggable:"

-- | @Selector@ for @dragState@
dragStateSelector :: Selector '[] MKAnnotationViewDragState
dragStateSelector = mkSelector "dragState"

-- | @Selector@ for @setDragState:@
setDragStateSelector :: Selector '[MKAnnotationViewDragState] ()
setDragStateSelector = mkSelector "setDragState:"

-- | @Selector@ for @clusteringIdentifier@
clusteringIdentifierSelector :: Selector '[] (Id NSString)
clusteringIdentifierSelector = mkSelector "clusteringIdentifier"

-- | @Selector@ for @setClusteringIdentifier:@
setClusteringIdentifierSelector :: Selector '[Id NSString] ()
setClusteringIdentifierSelector = mkSelector "setClusteringIdentifier:"

-- | @Selector@ for @clusterAnnotationView@
clusterAnnotationViewSelector :: Selector '[] (Id MKAnnotationView)
clusterAnnotationViewSelector = mkSelector "clusterAnnotationView"

-- | @Selector@ for @displayPriority@
displayPrioritySelector :: Selector '[] CFloat
displayPrioritySelector = mkSelector "displayPriority"

-- | @Selector@ for @setDisplayPriority:@
setDisplayPrioritySelector :: Selector '[CFloat] ()
setDisplayPrioritySelector = mkSelector "setDisplayPriority:"

-- | @Selector@ for @zPriority@
zPrioritySelector :: Selector '[] CFloat
zPrioritySelector = mkSelector "zPriority"

-- | @Selector@ for @setZPriority:@
setZPrioritySelector :: Selector '[CFloat] ()
setZPrioritySelector = mkSelector "setZPriority:"

-- | @Selector@ for @selectedZPriority@
selectedZPrioritySelector :: Selector '[] CFloat
selectedZPrioritySelector = mkSelector "selectedZPriority"

-- | @Selector@ for @setSelectedZPriority:@
setSelectedZPrioritySelector :: Selector '[CFloat] ()
setSelectedZPrioritySelector = mkSelector "setSelectedZPriority:"

-- | @Selector@ for @collisionMode@
collisionModeSelector :: Selector '[] MKAnnotationViewCollisionMode
collisionModeSelector = mkSelector "collisionMode"

-- | @Selector@ for @setCollisionMode:@
setCollisionModeSelector :: Selector '[MKAnnotationViewCollisionMode] ()
setCollisionModeSelector = mkSelector "setCollisionMode:"

