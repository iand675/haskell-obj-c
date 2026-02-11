{-# LANGUAGE PatternSynonyms #-}
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
  , initWithAnnotation_reuseIdentifierSelector
  , initWithCoderSelector
  , prepareForReuseSelector
  , prepareForDisplaySelector
  , setSelected_animatedSelector
  , setDragState_animatedSelector
  , reuseIdentifierSelector
  , annotationSelector
  , setAnnotationSelector
  , imageSelector
  , setImageSelector
  , enabledSelector
  , setEnabledSelector
  , highlightedSelector
  , setHighlightedSelector
  , selectedSelector
  , setSelectedSelector
  , canShowCalloutSelector
  , setCanShowCalloutSelector
  , leftCalloutAccessoryViewSelector
  , setLeftCalloutAccessoryViewSelector
  , rightCalloutAccessoryViewSelector
  , setRightCalloutAccessoryViewSelector
  , detailCalloutAccessoryViewSelector
  , setDetailCalloutAccessoryViewSelector
  , draggableSelector
  , setDraggableSelector
  , dragStateSelector
  , setDragStateSelector
  , clusteringIdentifierSelector
  , setClusteringIdentifierSelector
  , clusterAnnotationViewSelector
  , displayPrioritySelector
  , setDisplayPrioritySelector
  , zPrioritySelector
  , setZPrioritySelector
  , selectedZPrioritySelector
  , setSelectedZPrioritySelector
  , collisionModeSelector
  , setCollisionModeSelector

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
import ObjC.MapKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithAnnotation:reuseIdentifier:@
initWithAnnotation_reuseIdentifier :: (IsMKAnnotationView mkAnnotationView, IsNSString reuseIdentifier) => mkAnnotationView -> RawId -> reuseIdentifier -> IO (Id MKAnnotationView)
initWithAnnotation_reuseIdentifier mkAnnotationView  annotation reuseIdentifier =
  withObjCPtr reuseIdentifier $ \raw_reuseIdentifier ->
      sendMsg mkAnnotationView (mkSelector "initWithAnnotation:reuseIdentifier:") (retPtr retVoid) [argPtr (castPtr (unRawId annotation) :: Ptr ()), argPtr (castPtr raw_reuseIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsMKAnnotationView mkAnnotationView, IsNSCoder aDecoder) => mkAnnotationView -> aDecoder -> IO (Id MKAnnotationView)
initWithCoder mkAnnotationView  aDecoder =
  withObjCPtr aDecoder $ \raw_aDecoder ->
      sendMsg mkAnnotationView (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- prepareForReuse@
prepareForReuse :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO ()
prepareForReuse mkAnnotationView  =
    sendMsg mkAnnotationView (mkSelector "prepareForReuse") retVoid []

-- | @- prepareForDisplay@
prepareForDisplay :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO ()
prepareForDisplay mkAnnotationView  =
    sendMsg mkAnnotationView (mkSelector "prepareForDisplay") retVoid []

-- | @- setSelected:animated:@
setSelected_animated :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> Bool -> Bool -> IO ()
setSelected_animated mkAnnotationView  selected animated =
    sendMsg mkAnnotationView (mkSelector "setSelected:animated:") retVoid [argCULong (if selected then 1 else 0), argCULong (if animated then 1 else 0)]

-- | @- setDragState:animated:@
setDragState_animated :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> MKAnnotationViewDragState -> Bool -> IO ()
setDragState_animated mkAnnotationView  newDragState animated =
    sendMsg mkAnnotationView (mkSelector "setDragState:animated:") retVoid [argCULong (coerce newDragState), argCULong (if animated then 1 else 0)]

-- | @- reuseIdentifier@
reuseIdentifier :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO (Id NSString)
reuseIdentifier mkAnnotationView  =
    sendMsg mkAnnotationView (mkSelector "reuseIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- annotation@
annotation :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO RawId
annotation mkAnnotationView  =
    fmap (RawId . castPtr) $ sendMsg mkAnnotationView (mkSelector "annotation") (retPtr retVoid) []

-- | @- setAnnotation:@
setAnnotation :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> RawId -> IO ()
setAnnotation mkAnnotationView  value =
    sendMsg mkAnnotationView (mkSelector "setAnnotation:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- image@
image :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO (Id NSImage)
image mkAnnotationView  =
    sendMsg mkAnnotationView (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImage:@
setImage :: (IsMKAnnotationView mkAnnotationView, IsNSImage value) => mkAnnotationView -> value -> IO ()
setImage mkAnnotationView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mkAnnotationView (mkSelector "setImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- enabled@
enabled :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO Bool
enabled mkAnnotationView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkAnnotationView (mkSelector "enabled") retCULong []

-- | @- setEnabled:@
setEnabled :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> Bool -> IO ()
setEnabled mkAnnotationView  value =
    sendMsg mkAnnotationView (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- highlighted@
highlighted :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO Bool
highlighted mkAnnotationView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkAnnotationView (mkSelector "highlighted") retCULong []

-- | @- setHighlighted:@
setHighlighted :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> Bool -> IO ()
setHighlighted mkAnnotationView  value =
    sendMsg mkAnnotationView (mkSelector "setHighlighted:") retVoid [argCULong (if value then 1 else 0)]

-- | @- selected@
selected :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO Bool
selected mkAnnotationView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkAnnotationView (mkSelector "selected") retCULong []

-- | @- setSelected:@
setSelected :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> Bool -> IO ()
setSelected mkAnnotationView  value =
    sendMsg mkAnnotationView (mkSelector "setSelected:") retVoid [argCULong (if value then 1 else 0)]

-- | @- canShowCallout@
canShowCallout :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO Bool
canShowCallout mkAnnotationView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkAnnotationView (mkSelector "canShowCallout") retCULong []

-- | @- setCanShowCallout:@
setCanShowCallout :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> Bool -> IO ()
setCanShowCallout mkAnnotationView  value =
    sendMsg mkAnnotationView (mkSelector "setCanShowCallout:") retVoid [argCULong (if value then 1 else 0)]

-- | @- leftCalloutAccessoryView@
leftCalloutAccessoryView :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO (Id NSView)
leftCalloutAccessoryView mkAnnotationView  =
    sendMsg mkAnnotationView (mkSelector "leftCalloutAccessoryView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLeftCalloutAccessoryView:@
setLeftCalloutAccessoryView :: (IsMKAnnotationView mkAnnotationView, IsNSView value) => mkAnnotationView -> value -> IO ()
setLeftCalloutAccessoryView mkAnnotationView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mkAnnotationView (mkSelector "setLeftCalloutAccessoryView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rightCalloutAccessoryView@
rightCalloutAccessoryView :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO (Id NSView)
rightCalloutAccessoryView mkAnnotationView  =
    sendMsg mkAnnotationView (mkSelector "rightCalloutAccessoryView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRightCalloutAccessoryView:@
setRightCalloutAccessoryView :: (IsMKAnnotationView mkAnnotationView, IsNSView value) => mkAnnotationView -> value -> IO ()
setRightCalloutAccessoryView mkAnnotationView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mkAnnotationView (mkSelector "setRightCalloutAccessoryView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- detailCalloutAccessoryView@
detailCalloutAccessoryView :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO RawId
detailCalloutAccessoryView mkAnnotationView  =
    fmap (RawId . castPtr) $ sendMsg mkAnnotationView (mkSelector "detailCalloutAccessoryView") (retPtr retVoid) []

-- | @- setDetailCalloutAccessoryView:@
setDetailCalloutAccessoryView :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> RawId -> IO ()
setDetailCalloutAccessoryView mkAnnotationView  value =
    sendMsg mkAnnotationView (mkSelector "setDetailCalloutAccessoryView:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- draggable@
draggable :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO Bool
draggable mkAnnotationView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkAnnotationView (mkSelector "draggable") retCULong []

-- | @- setDraggable:@
setDraggable :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> Bool -> IO ()
setDraggable mkAnnotationView  value =
    sendMsg mkAnnotationView (mkSelector "setDraggable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- dragState@
dragState :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO MKAnnotationViewDragState
dragState mkAnnotationView  =
    fmap (coerce :: CULong -> MKAnnotationViewDragState) $ sendMsg mkAnnotationView (mkSelector "dragState") retCULong []

-- | @- setDragState:@
setDragState :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> MKAnnotationViewDragState -> IO ()
setDragState mkAnnotationView  value =
    sendMsg mkAnnotationView (mkSelector "setDragState:") retVoid [argCULong (coerce value)]

-- | @- clusteringIdentifier@
clusteringIdentifier :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO (Id NSString)
clusteringIdentifier mkAnnotationView  =
    sendMsg mkAnnotationView (mkSelector "clusteringIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setClusteringIdentifier:@
setClusteringIdentifier :: (IsMKAnnotationView mkAnnotationView, IsNSString value) => mkAnnotationView -> value -> IO ()
setClusteringIdentifier mkAnnotationView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mkAnnotationView (mkSelector "setClusteringIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- clusterAnnotationView@
clusterAnnotationView :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO (Id MKAnnotationView)
clusterAnnotationView mkAnnotationView  =
    sendMsg mkAnnotationView (mkSelector "clusterAnnotationView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- displayPriority@
displayPriority :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO CFloat
displayPriority mkAnnotationView  =
    sendMsg mkAnnotationView (mkSelector "displayPriority") retCFloat []

-- | @- setDisplayPriority:@
setDisplayPriority :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> CFloat -> IO ()
setDisplayPriority mkAnnotationView  value =
    sendMsg mkAnnotationView (mkSelector "setDisplayPriority:") retVoid [argCFloat value]

-- | @- zPriority@
zPriority :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO CFloat
zPriority mkAnnotationView  =
    sendMsg mkAnnotationView (mkSelector "zPriority") retCFloat []

-- | @- setZPriority:@
setZPriority :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> CFloat -> IO ()
setZPriority mkAnnotationView  value =
    sendMsg mkAnnotationView (mkSelector "setZPriority:") retVoid [argCFloat value]

-- | @- selectedZPriority@
selectedZPriority :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO CFloat
selectedZPriority mkAnnotationView  =
    sendMsg mkAnnotationView (mkSelector "selectedZPriority") retCFloat []

-- | @- setSelectedZPriority:@
setSelectedZPriority :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> CFloat -> IO ()
setSelectedZPriority mkAnnotationView  value =
    sendMsg mkAnnotationView (mkSelector "setSelectedZPriority:") retVoid [argCFloat value]

-- | @- collisionMode@
collisionMode :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> IO MKAnnotationViewCollisionMode
collisionMode mkAnnotationView  =
    fmap (coerce :: CLong -> MKAnnotationViewCollisionMode) $ sendMsg mkAnnotationView (mkSelector "collisionMode") retCLong []

-- | @- setCollisionMode:@
setCollisionMode :: IsMKAnnotationView mkAnnotationView => mkAnnotationView -> MKAnnotationViewCollisionMode -> IO ()
setCollisionMode mkAnnotationView  value =
    sendMsg mkAnnotationView (mkSelector "setCollisionMode:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAnnotation:reuseIdentifier:@
initWithAnnotation_reuseIdentifierSelector :: Selector
initWithAnnotation_reuseIdentifierSelector = mkSelector "initWithAnnotation:reuseIdentifier:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @prepareForReuse@
prepareForReuseSelector :: Selector
prepareForReuseSelector = mkSelector "prepareForReuse"

-- | @Selector@ for @prepareForDisplay@
prepareForDisplaySelector :: Selector
prepareForDisplaySelector = mkSelector "prepareForDisplay"

-- | @Selector@ for @setSelected:animated:@
setSelected_animatedSelector :: Selector
setSelected_animatedSelector = mkSelector "setSelected:animated:"

-- | @Selector@ for @setDragState:animated:@
setDragState_animatedSelector :: Selector
setDragState_animatedSelector = mkSelector "setDragState:animated:"

-- | @Selector@ for @reuseIdentifier@
reuseIdentifierSelector :: Selector
reuseIdentifierSelector = mkSelector "reuseIdentifier"

-- | @Selector@ for @annotation@
annotationSelector :: Selector
annotationSelector = mkSelector "annotation"

-- | @Selector@ for @setAnnotation:@
setAnnotationSelector :: Selector
setAnnotationSelector = mkSelector "setAnnotation:"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @highlighted@
highlightedSelector :: Selector
highlightedSelector = mkSelector "highlighted"

-- | @Selector@ for @setHighlighted:@
setHighlightedSelector :: Selector
setHighlightedSelector = mkSelector "setHighlighted:"

-- | @Selector@ for @selected@
selectedSelector :: Selector
selectedSelector = mkSelector "selected"

-- | @Selector@ for @setSelected:@
setSelectedSelector :: Selector
setSelectedSelector = mkSelector "setSelected:"

-- | @Selector@ for @canShowCallout@
canShowCalloutSelector :: Selector
canShowCalloutSelector = mkSelector "canShowCallout"

-- | @Selector@ for @setCanShowCallout:@
setCanShowCalloutSelector :: Selector
setCanShowCalloutSelector = mkSelector "setCanShowCallout:"

-- | @Selector@ for @leftCalloutAccessoryView@
leftCalloutAccessoryViewSelector :: Selector
leftCalloutAccessoryViewSelector = mkSelector "leftCalloutAccessoryView"

-- | @Selector@ for @setLeftCalloutAccessoryView:@
setLeftCalloutAccessoryViewSelector :: Selector
setLeftCalloutAccessoryViewSelector = mkSelector "setLeftCalloutAccessoryView:"

-- | @Selector@ for @rightCalloutAccessoryView@
rightCalloutAccessoryViewSelector :: Selector
rightCalloutAccessoryViewSelector = mkSelector "rightCalloutAccessoryView"

-- | @Selector@ for @setRightCalloutAccessoryView:@
setRightCalloutAccessoryViewSelector :: Selector
setRightCalloutAccessoryViewSelector = mkSelector "setRightCalloutAccessoryView:"

-- | @Selector@ for @detailCalloutAccessoryView@
detailCalloutAccessoryViewSelector :: Selector
detailCalloutAccessoryViewSelector = mkSelector "detailCalloutAccessoryView"

-- | @Selector@ for @setDetailCalloutAccessoryView:@
setDetailCalloutAccessoryViewSelector :: Selector
setDetailCalloutAccessoryViewSelector = mkSelector "setDetailCalloutAccessoryView:"

-- | @Selector@ for @draggable@
draggableSelector :: Selector
draggableSelector = mkSelector "draggable"

-- | @Selector@ for @setDraggable:@
setDraggableSelector :: Selector
setDraggableSelector = mkSelector "setDraggable:"

-- | @Selector@ for @dragState@
dragStateSelector :: Selector
dragStateSelector = mkSelector "dragState"

-- | @Selector@ for @setDragState:@
setDragStateSelector :: Selector
setDragStateSelector = mkSelector "setDragState:"

-- | @Selector@ for @clusteringIdentifier@
clusteringIdentifierSelector :: Selector
clusteringIdentifierSelector = mkSelector "clusteringIdentifier"

-- | @Selector@ for @setClusteringIdentifier:@
setClusteringIdentifierSelector :: Selector
setClusteringIdentifierSelector = mkSelector "setClusteringIdentifier:"

-- | @Selector@ for @clusterAnnotationView@
clusterAnnotationViewSelector :: Selector
clusterAnnotationViewSelector = mkSelector "clusterAnnotationView"

-- | @Selector@ for @displayPriority@
displayPrioritySelector :: Selector
displayPrioritySelector = mkSelector "displayPriority"

-- | @Selector@ for @setDisplayPriority:@
setDisplayPrioritySelector :: Selector
setDisplayPrioritySelector = mkSelector "setDisplayPriority:"

-- | @Selector@ for @zPriority@
zPrioritySelector :: Selector
zPrioritySelector = mkSelector "zPriority"

-- | @Selector@ for @setZPriority:@
setZPrioritySelector :: Selector
setZPrioritySelector = mkSelector "setZPriority:"

-- | @Selector@ for @selectedZPriority@
selectedZPrioritySelector :: Selector
selectedZPrioritySelector = mkSelector "selectedZPriority"

-- | @Selector@ for @setSelectedZPriority:@
setSelectedZPrioritySelector :: Selector
setSelectedZPrioritySelector = mkSelector "setSelectedZPriority:"

-- | @Selector@ for @collisionMode@
collisionModeSelector :: Selector
collisionModeSelector = mkSelector "collisionMode"

-- | @Selector@ for @setCollisionMode:@
setCollisionModeSelector :: Selector
setCollisionModeSelector = mkSelector "setCollisionMode:"

