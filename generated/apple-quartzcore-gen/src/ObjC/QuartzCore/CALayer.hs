{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The base layer class. *
--
-- Generated bindings for @CALayer@.
module ObjC.QuartzCore.CALayer
  ( CALayer
  , IsCALayer(..)
  , layer
  , init_
  , initWithLayer
  , presentationLayer
  , modelLayer
  , defaultValueForKey
  , needsDisplayForKey
  , shouldArchiveValueForKey
  , contentsAreFlipped
  , removeFromSuperlayer
  , addSublayer
  , insertSublayer_atIndex
  , insertSublayer_below
  , insertSublayer_above
  , replaceSublayer_with
  , convertTime_fromLayer
  , convertTime_toLayer
  , display
  , setNeedsDisplay
  , needsDisplay
  , displayIfNeeded
  , drawInContext
  , renderInContext
  , cornerCurveExpansionFactor
  , setNeedsLayout
  , needsLayout
  , layoutIfNeeded
  , layoutSublayers
  , defaultActionForKey
  , actionForKey
  , addAnimation_forKey
  , removeAllAnimations
  , removeAnimationForKey
  , animationKeys
  , animationForKey
  , layerWithRemoteClientId
  , addConstraint
  , zPosition
  , setZPosition
  , anchorPointZ
  , setAnchorPointZ
  , transform
  , setTransform
  , hidden
  , setHidden
  , doubleSided
  , setDoubleSided
  , geometryFlipped
  , setGeometryFlipped
  , superlayer
  , sublayers
  , setSublayers
  , sublayerTransform
  , setSublayerTransform
  , mask
  , setMask
  , masksToBounds
  , setMasksToBounds
  , contents
  , setContents
  , contentsGravity
  , setContentsGravity
  , contentsScale
  , setContentsScale
  , contentsFormat
  , setContentsFormat
  , wantsExtendedDynamicRangeContent
  , setWantsExtendedDynamicRangeContent
  , toneMapMode
  , setToneMapMode
  , preferredDynamicRange
  , setPreferredDynamicRange
  , contentsHeadroom
  , setContentsHeadroom
  , wantsDynamicContentScaling
  , setWantsDynamicContentScaling
  , minificationFilter
  , setMinificationFilter
  , magnificationFilter
  , setMagnificationFilter
  , minificationFilterBias
  , setMinificationFilterBias
  , opaque
  , setOpaque
  , needsDisplayOnBoundsChange
  , setNeedsDisplayOnBoundsChange
  , drawsAsynchronously
  , setDrawsAsynchronously
  , edgeAntialiasingMask
  , setEdgeAntialiasingMask
  , allowsEdgeAntialiasing
  , setAllowsEdgeAntialiasing
  , backgroundColor
  , setBackgroundColor
  , cornerRadius
  , setCornerRadius
  , maskedCorners
  , setMaskedCorners
  , cornerCurve
  , setCornerCurve
  , borderWidth
  , setBorderWidth
  , borderColor
  , setBorderColor
  , opacity
  , setOpacity
  , allowsGroupOpacity
  , setAllowsGroupOpacity
  , compositingFilter
  , setCompositingFilter
  , filters
  , setFilters
  , backgroundFilters
  , setBackgroundFilters
  , shouldRasterize
  , setShouldRasterize
  , rasterizationScale
  , setRasterizationScale
  , shadowColor
  , setShadowColor
  , shadowOpacity
  , setShadowOpacity
  , shadowRadius
  , setShadowRadius
  , shadowPath
  , setShadowPath
  , autoresizingMask
  , setAutoresizingMask
  , layoutManager
  , setLayoutManager
  , actions
  , setActions
  , name
  , setName
  , delegate
  , setDelegate
  , style
  , setStyle
  , constraints
  , setConstraints
  , layerSelector
  , initSelector
  , initWithLayerSelector
  , presentationLayerSelector
  , modelLayerSelector
  , defaultValueForKeySelector
  , needsDisplayForKeySelector
  , shouldArchiveValueForKeySelector
  , contentsAreFlippedSelector
  , removeFromSuperlayerSelector
  , addSublayerSelector
  , insertSublayer_atIndexSelector
  , insertSublayer_belowSelector
  , insertSublayer_aboveSelector
  , replaceSublayer_withSelector
  , convertTime_fromLayerSelector
  , convertTime_toLayerSelector
  , displaySelector
  , setNeedsDisplaySelector
  , needsDisplaySelector
  , displayIfNeededSelector
  , drawInContextSelector
  , renderInContextSelector
  , cornerCurveExpansionFactorSelector
  , setNeedsLayoutSelector
  , needsLayoutSelector
  , layoutIfNeededSelector
  , layoutSublayersSelector
  , defaultActionForKeySelector
  , actionForKeySelector
  , addAnimation_forKeySelector
  , removeAllAnimationsSelector
  , removeAnimationForKeySelector
  , animationKeysSelector
  , animationForKeySelector
  , layerWithRemoteClientIdSelector
  , addConstraintSelector
  , zPositionSelector
  , setZPositionSelector
  , anchorPointZSelector
  , setAnchorPointZSelector
  , transformSelector
  , setTransformSelector
  , hiddenSelector
  , setHiddenSelector
  , doubleSidedSelector
  , setDoubleSidedSelector
  , geometryFlippedSelector
  , setGeometryFlippedSelector
  , superlayerSelector
  , sublayersSelector
  , setSublayersSelector
  , sublayerTransformSelector
  , setSublayerTransformSelector
  , maskSelector
  , setMaskSelector
  , masksToBoundsSelector
  , setMasksToBoundsSelector
  , contentsSelector
  , setContentsSelector
  , contentsGravitySelector
  , setContentsGravitySelector
  , contentsScaleSelector
  , setContentsScaleSelector
  , contentsFormatSelector
  , setContentsFormatSelector
  , wantsExtendedDynamicRangeContentSelector
  , setWantsExtendedDynamicRangeContentSelector
  , toneMapModeSelector
  , setToneMapModeSelector
  , preferredDynamicRangeSelector
  , setPreferredDynamicRangeSelector
  , contentsHeadroomSelector
  , setContentsHeadroomSelector
  , wantsDynamicContentScalingSelector
  , setWantsDynamicContentScalingSelector
  , minificationFilterSelector
  , setMinificationFilterSelector
  , magnificationFilterSelector
  , setMagnificationFilterSelector
  , minificationFilterBiasSelector
  , setMinificationFilterBiasSelector
  , opaqueSelector
  , setOpaqueSelector
  , needsDisplayOnBoundsChangeSelector
  , setNeedsDisplayOnBoundsChangeSelector
  , drawsAsynchronouslySelector
  , setDrawsAsynchronouslySelector
  , edgeAntialiasingMaskSelector
  , setEdgeAntialiasingMaskSelector
  , allowsEdgeAntialiasingSelector
  , setAllowsEdgeAntialiasingSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , cornerRadiusSelector
  , setCornerRadiusSelector
  , maskedCornersSelector
  , setMaskedCornersSelector
  , cornerCurveSelector
  , setCornerCurveSelector
  , borderWidthSelector
  , setBorderWidthSelector
  , borderColorSelector
  , setBorderColorSelector
  , opacitySelector
  , setOpacitySelector
  , allowsGroupOpacitySelector
  , setAllowsGroupOpacitySelector
  , compositingFilterSelector
  , setCompositingFilterSelector
  , filtersSelector
  , setFiltersSelector
  , backgroundFiltersSelector
  , setBackgroundFiltersSelector
  , shouldRasterizeSelector
  , setShouldRasterizeSelector
  , rasterizationScaleSelector
  , setRasterizationScaleSelector
  , shadowColorSelector
  , setShadowColorSelector
  , shadowOpacitySelector
  , setShadowOpacitySelector
  , shadowRadiusSelector
  , setShadowRadiusSelector
  , shadowPathSelector
  , setShadowPathSelector
  , autoresizingMaskSelector
  , setAutoresizingMaskSelector
  , layoutManagerSelector
  , setLayoutManagerSelector
  , actionsSelector
  , setActionsSelector
  , nameSelector
  , setNameSelector
  , delegateSelector
  , setDelegateSelector
  , styleSelector
  , setStyleSelector
  , constraintsSelector
  , setConstraintsSelector

  -- * Enum types
  , CAAutoresizingMask(CAAutoresizingMask)
  , pattern KCALayerNotSizable
  , pattern KCALayerMinXMargin
  , pattern KCALayerWidthSizable
  , pattern KCALayerMaxXMargin
  , pattern KCALayerMinYMargin
  , pattern KCALayerHeightSizable
  , pattern KCALayerMaxYMargin
  , CACornerMask(CACornerMask)
  , pattern KCALayerMinXMinYCorner
  , pattern KCALayerMaxXMinYCorner
  , pattern KCALayerMinXMaxYCorner
  , pattern KCALayerMaxXMaxYCorner
  , CAEdgeAntialiasingMask(CAEdgeAntialiasingMask)
  , pattern KCALayerLeftEdge
  , pattern KCALayerRightEdge
  , pattern KCALayerBottomEdge
  , pattern KCALayerTopEdge

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

import ObjC.QuartzCore.Internal.Classes
import ObjC.QuartzCore.Internal.Structs
import ObjC.QuartzCore.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Layer creation and initialization. *
--
-- ObjC selector: @+ layer@
layer :: IO (Id CALayer)
layer  =
  do
    cls' <- getRequiredClass "CALayer"
    sendClassMsg cls' (mkSelector "layer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsCALayer caLayer => caLayer -> IO (Id CALayer)
init_ caLayer  =
    sendMsg caLayer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithLayer:@
initWithLayer :: IsCALayer caLayer => caLayer -> RawId -> IO (Id CALayer)
initWithLayer caLayer  layer =
    sendMsg caLayer (mkSelector "initWithLayer:") (retPtr retVoid) [argPtr (castPtr (unRawId layer) :: Ptr ())] >>= ownedObject . castPtr

-- | @- presentationLayer@
presentationLayer :: IsCALayer caLayer => caLayer -> IO (Id CALayer)
presentationLayer caLayer  =
    sendMsg caLayer (mkSelector "presentationLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- modelLayer@
modelLayer :: IsCALayer caLayer => caLayer -> IO (Id CALayer)
modelLayer caLayer  =
    sendMsg caLayer (mkSelector "modelLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Property methods. *
--
-- ObjC selector: @+ defaultValueForKey:@
defaultValueForKey :: IsNSString key => key -> IO RawId
defaultValueForKey key =
  do
    cls' <- getRequiredClass "CALayer"
    withObjCPtr key $ \raw_key ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "defaultValueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @+ needsDisplayForKey:@
needsDisplayForKey :: IsNSString key => key -> IO Bool
needsDisplayForKey key =
  do
    cls' <- getRequiredClass "CALayer"
    withObjCPtr key $ \raw_key ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "needsDisplayForKey:") retCULong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- shouldArchiveValueForKey:@
shouldArchiveValueForKey :: (IsCALayer caLayer, IsNSString key) => caLayer -> key -> IO Bool
shouldArchiveValueForKey caLayer  key =
  withObjCPtr key $ \raw_key ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg caLayer (mkSelector "shouldArchiveValueForKey:") retCULong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- contentsAreFlipped@
contentsAreFlipped :: IsCALayer caLayer => caLayer -> IO Bool
contentsAreFlipped caLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caLayer (mkSelector "contentsAreFlipped") retCULong []

-- | @- removeFromSuperlayer@
removeFromSuperlayer :: IsCALayer caLayer => caLayer -> IO ()
removeFromSuperlayer caLayer  =
    sendMsg caLayer (mkSelector "removeFromSuperlayer") retVoid []

-- | @- addSublayer:@
addSublayer :: (IsCALayer caLayer, IsCALayer layer) => caLayer -> layer -> IO ()
addSublayer caLayer  layer =
  withObjCPtr layer $ \raw_layer ->
      sendMsg caLayer (mkSelector "addSublayer:") retVoid [argPtr (castPtr raw_layer :: Ptr ())]

-- | @- insertSublayer:atIndex:@
insertSublayer_atIndex :: (IsCALayer caLayer, IsCALayer layer) => caLayer -> layer -> CUInt -> IO ()
insertSublayer_atIndex caLayer  layer idx =
  withObjCPtr layer $ \raw_layer ->
      sendMsg caLayer (mkSelector "insertSublayer:atIndex:") retVoid [argPtr (castPtr raw_layer :: Ptr ()), argCUInt idx]

-- | @- insertSublayer:below:@
insertSublayer_below :: (IsCALayer caLayer, IsCALayer layer, IsCALayer sibling) => caLayer -> layer -> sibling -> IO ()
insertSublayer_below caLayer  layer sibling =
  withObjCPtr layer $ \raw_layer ->
    withObjCPtr sibling $ \raw_sibling ->
        sendMsg caLayer (mkSelector "insertSublayer:below:") retVoid [argPtr (castPtr raw_layer :: Ptr ()), argPtr (castPtr raw_sibling :: Ptr ())]

-- | @- insertSublayer:above:@
insertSublayer_above :: (IsCALayer caLayer, IsCALayer layer, IsCALayer sibling) => caLayer -> layer -> sibling -> IO ()
insertSublayer_above caLayer  layer sibling =
  withObjCPtr layer $ \raw_layer ->
    withObjCPtr sibling $ \raw_sibling ->
        sendMsg caLayer (mkSelector "insertSublayer:above:") retVoid [argPtr (castPtr raw_layer :: Ptr ()), argPtr (castPtr raw_sibling :: Ptr ())]

-- | @- replaceSublayer:with:@
replaceSublayer_with :: (IsCALayer caLayer, IsCALayer oldLayer, IsCALayer newLayer) => caLayer -> oldLayer -> newLayer -> IO ()
replaceSublayer_with caLayer  oldLayer newLayer =
  withObjCPtr oldLayer $ \raw_oldLayer ->
    withObjCPtr newLayer $ \raw_newLayer ->
        sendMsg caLayer (mkSelector "replaceSublayer:with:") retVoid [argPtr (castPtr raw_oldLayer :: Ptr ()), argPtr (castPtr raw_newLayer :: Ptr ())]

-- | @- convertTime:fromLayer:@
convertTime_fromLayer :: (IsCALayer caLayer, IsCALayer l) => caLayer -> CDouble -> l -> IO CDouble
convertTime_fromLayer caLayer  t l =
  withObjCPtr l $ \raw_l ->
      sendMsg caLayer (mkSelector "convertTime:fromLayer:") retCDouble [argCDouble t, argPtr (castPtr raw_l :: Ptr ())]

-- | @- convertTime:toLayer:@
convertTime_toLayer :: (IsCALayer caLayer, IsCALayer l) => caLayer -> CDouble -> l -> IO CDouble
convertTime_toLayer caLayer  t l =
  withObjCPtr l $ \raw_l ->
      sendMsg caLayer (mkSelector "convertTime:toLayer:") retCDouble [argCDouble t, argPtr (castPtr raw_l :: Ptr ())]

-- | @- display@
display :: IsCALayer caLayer => caLayer -> IO ()
display caLayer  =
    sendMsg caLayer (mkSelector "display") retVoid []

-- | @- setNeedsDisplay@
setNeedsDisplay :: IsCALayer caLayer => caLayer -> IO ()
setNeedsDisplay caLayer  =
    sendMsg caLayer (mkSelector "setNeedsDisplay") retVoid []

-- | @- needsDisplay@
needsDisplay :: IsCALayer caLayer => caLayer -> IO Bool
needsDisplay caLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caLayer (mkSelector "needsDisplay") retCULong []

-- | @- displayIfNeeded@
displayIfNeeded :: IsCALayer caLayer => caLayer -> IO ()
displayIfNeeded caLayer  =
    sendMsg caLayer (mkSelector "displayIfNeeded") retVoid []

-- | @- drawInContext:@
drawInContext :: IsCALayer caLayer => caLayer -> Ptr () -> IO ()
drawInContext caLayer  ctx =
    sendMsg caLayer (mkSelector "drawInContext:") retVoid [argPtr ctx]

-- | Rendering properties and methods. *
--
-- ObjC selector: @- renderInContext:@
renderInContext :: IsCALayer caLayer => caLayer -> Ptr () -> IO ()
renderInContext caLayer  ctx =
    sendMsg caLayer (mkSelector "renderInContext:") retVoid [argPtr ctx]

-- | @+ cornerCurveExpansionFactor:@
cornerCurveExpansionFactor :: IsNSString curve => curve -> IO CDouble
cornerCurveExpansionFactor curve =
  do
    cls' <- getRequiredClass "CALayer"
    withObjCPtr curve $ \raw_curve ->
      sendClassMsg cls' (mkSelector "cornerCurveExpansionFactor:") retCDouble [argPtr (castPtr raw_curve :: Ptr ())]

-- | @- setNeedsLayout@
setNeedsLayout :: IsCALayer caLayer => caLayer -> IO ()
setNeedsLayout caLayer  =
    sendMsg caLayer (mkSelector "setNeedsLayout") retVoid []

-- | @- needsLayout@
needsLayout :: IsCALayer caLayer => caLayer -> IO Bool
needsLayout caLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caLayer (mkSelector "needsLayout") retCULong []

-- | @- layoutIfNeeded@
layoutIfNeeded :: IsCALayer caLayer => caLayer -> IO ()
layoutIfNeeded caLayer  =
    sendMsg caLayer (mkSelector "layoutIfNeeded") retVoid []

-- | @- layoutSublayers@
layoutSublayers :: IsCALayer caLayer => caLayer -> IO ()
layoutSublayers caLayer  =
    sendMsg caLayer (mkSelector "layoutSublayers") retVoid []

-- | Action methods. *
--
-- ObjC selector: @+ defaultActionForKey:@
defaultActionForKey :: IsNSString event => event -> IO RawId
defaultActionForKey event =
  do
    cls' <- getRequiredClass "CALayer"
    withObjCPtr event $ \raw_event ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "defaultActionForKey:") (retPtr retVoid) [argPtr (castPtr raw_event :: Ptr ())]

-- | @- actionForKey:@
actionForKey :: (IsCALayer caLayer, IsNSString event) => caLayer -> event -> IO RawId
actionForKey caLayer  event =
  withObjCPtr event $ \raw_event ->
      fmap (RawId . castPtr) $ sendMsg caLayer (mkSelector "actionForKey:") (retPtr retVoid) [argPtr (castPtr raw_event :: Ptr ())]

-- | Animation methods. *
--
-- ObjC selector: @- addAnimation:forKey:@
addAnimation_forKey :: (IsCALayer caLayer, IsCAAnimation anim, IsNSString key) => caLayer -> anim -> key -> IO ()
addAnimation_forKey caLayer  anim key =
  withObjCPtr anim $ \raw_anim ->
    withObjCPtr key $ \raw_key ->
        sendMsg caLayer (mkSelector "addAnimation:forKey:") retVoid [argPtr (castPtr raw_anim :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- removeAllAnimations@
removeAllAnimations :: IsCALayer caLayer => caLayer -> IO ()
removeAllAnimations caLayer  =
    sendMsg caLayer (mkSelector "removeAllAnimations") retVoid []

-- | @- removeAnimationForKey:@
removeAnimationForKey :: (IsCALayer caLayer, IsNSString key) => caLayer -> key -> IO ()
removeAnimationForKey caLayer  key =
  withObjCPtr key $ \raw_key ->
      sendMsg caLayer (mkSelector "removeAnimationForKey:") retVoid [argPtr (castPtr raw_key :: Ptr ())]

-- | @- animationKeys@
animationKeys :: IsCALayer caLayer => caLayer -> IO (Id NSArray)
animationKeys caLayer  =
    sendMsg caLayer (mkSelector "animationKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- animationForKey:@
animationForKey :: (IsCALayer caLayer, IsNSString key) => caLayer -> key -> IO (Id CAAnimation)
animationForKey caLayer  key =
  withObjCPtr key $ \raw_key ->
      sendMsg caLayer (mkSelector "animationForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @+ layerWithRemoteClientId:@
layerWithRemoteClientId :: CUInt -> IO (Id CALayer)
layerWithRemoteClientId client_id =
  do
    cls' <- getRequiredClass "CALayer"
    sendClassMsg cls' (mkSelector "layerWithRemoteClientId:") (retPtr retVoid) [argCUInt client_id] >>= retainedObject . castPtr

-- | @- addConstraint:@
addConstraint :: (IsCALayer caLayer, IsCAConstraint c) => caLayer -> c -> IO ()
addConstraint caLayer  c =
  withObjCPtr c $ \raw_c ->
      sendMsg caLayer (mkSelector "addConstraint:") retVoid [argPtr (castPtr raw_c :: Ptr ())]

-- | @- zPosition@
zPosition :: IsCALayer caLayer => caLayer -> IO CDouble
zPosition caLayer  =
    sendMsg caLayer (mkSelector "zPosition") retCDouble []

-- | @- setZPosition:@
setZPosition :: IsCALayer caLayer => caLayer -> CDouble -> IO ()
setZPosition caLayer  value =
    sendMsg caLayer (mkSelector "setZPosition:") retVoid [argCDouble value]

-- | @- anchorPointZ@
anchorPointZ :: IsCALayer caLayer => caLayer -> IO CDouble
anchorPointZ caLayer  =
    sendMsg caLayer (mkSelector "anchorPointZ") retCDouble []

-- | @- setAnchorPointZ:@
setAnchorPointZ :: IsCALayer caLayer => caLayer -> CDouble -> IO ()
setAnchorPointZ caLayer  value =
    sendMsg caLayer (mkSelector "setAnchorPointZ:") retVoid [argCDouble value]

-- | @- transform@
transform :: IsCALayer caLayer => caLayer -> IO CATransform3D
transform caLayer  =
    sendMsgStret caLayer (mkSelector "transform") retCATransform3D []

-- | @- setTransform:@
setTransform :: IsCALayer caLayer => caLayer -> CATransform3D -> IO ()
setTransform caLayer  value =
    sendMsg caLayer (mkSelector "setTransform:") retVoid [argCATransform3D value]

-- | @- hidden@
hidden :: IsCALayer caLayer => caLayer -> IO Bool
hidden caLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caLayer (mkSelector "hidden") retCULong []

-- | @- setHidden:@
setHidden :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setHidden caLayer  value =
    sendMsg caLayer (mkSelector "setHidden:") retVoid [argCULong (if value then 1 else 0)]

-- | @- doubleSided@
doubleSided :: IsCALayer caLayer => caLayer -> IO Bool
doubleSided caLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caLayer (mkSelector "doubleSided") retCULong []

-- | @- setDoubleSided:@
setDoubleSided :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setDoubleSided caLayer  value =
    sendMsg caLayer (mkSelector "setDoubleSided:") retVoid [argCULong (if value then 1 else 0)]

-- | @- geometryFlipped@
geometryFlipped :: IsCALayer caLayer => caLayer -> IO Bool
geometryFlipped caLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caLayer (mkSelector "geometryFlipped") retCULong []

-- | @- setGeometryFlipped:@
setGeometryFlipped :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setGeometryFlipped caLayer  value =
    sendMsg caLayer (mkSelector "setGeometryFlipped:") retVoid [argCULong (if value then 1 else 0)]

-- | @- superlayer@
superlayer :: IsCALayer caLayer => caLayer -> IO (Id CALayer)
superlayer caLayer  =
    sendMsg caLayer (mkSelector "superlayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sublayers@
sublayers :: IsCALayer caLayer => caLayer -> IO (Id NSArray)
sublayers caLayer  =
    sendMsg caLayer (mkSelector "sublayers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSublayers:@
setSublayers :: (IsCALayer caLayer, IsNSArray value) => caLayer -> value -> IO ()
setSublayers caLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg caLayer (mkSelector "setSublayers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sublayerTransform@
sublayerTransform :: IsCALayer caLayer => caLayer -> IO CATransform3D
sublayerTransform caLayer  =
    sendMsgStret caLayer (mkSelector "sublayerTransform") retCATransform3D []

-- | @- setSublayerTransform:@
setSublayerTransform :: IsCALayer caLayer => caLayer -> CATransform3D -> IO ()
setSublayerTransform caLayer  value =
    sendMsg caLayer (mkSelector "setSublayerTransform:") retVoid [argCATransform3D value]

-- | @- mask@
mask :: IsCALayer caLayer => caLayer -> IO (Id CALayer)
mask caLayer  =
    sendMsg caLayer (mkSelector "mask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMask:@
setMask :: (IsCALayer caLayer, IsCALayer value) => caLayer -> value -> IO ()
setMask caLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg caLayer (mkSelector "setMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- masksToBounds@
masksToBounds :: IsCALayer caLayer => caLayer -> IO Bool
masksToBounds caLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caLayer (mkSelector "masksToBounds") retCULong []

-- | @- setMasksToBounds:@
setMasksToBounds :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setMasksToBounds caLayer  value =
    sendMsg caLayer (mkSelector "setMasksToBounds:") retVoid [argCULong (if value then 1 else 0)]

-- | Layer content properties and methods. *
--
-- ObjC selector: @- contents@
contents :: IsCALayer caLayer => caLayer -> IO RawId
contents caLayer  =
    fmap (RawId . castPtr) $ sendMsg caLayer (mkSelector "contents") (retPtr retVoid) []

-- | Layer content properties and methods. *
--
-- ObjC selector: @- setContents:@
setContents :: IsCALayer caLayer => caLayer -> RawId -> IO ()
setContents caLayer  value =
    sendMsg caLayer (mkSelector "setContents:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- contentsGravity@
contentsGravity :: IsCALayer caLayer => caLayer -> IO (Id NSString)
contentsGravity caLayer  =
    sendMsg caLayer (mkSelector "contentsGravity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentsGravity:@
setContentsGravity :: (IsCALayer caLayer, IsNSString value) => caLayer -> value -> IO ()
setContentsGravity caLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg caLayer (mkSelector "setContentsGravity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contentsScale@
contentsScale :: IsCALayer caLayer => caLayer -> IO CDouble
contentsScale caLayer  =
    sendMsg caLayer (mkSelector "contentsScale") retCDouble []

-- | @- setContentsScale:@
setContentsScale :: IsCALayer caLayer => caLayer -> CDouble -> IO ()
setContentsScale caLayer  value =
    sendMsg caLayer (mkSelector "setContentsScale:") retVoid [argCDouble value]

-- | @- contentsFormat@
contentsFormat :: IsCALayer caLayer => caLayer -> IO (Id NSString)
contentsFormat caLayer  =
    sendMsg caLayer (mkSelector "contentsFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentsFormat:@
setContentsFormat :: (IsCALayer caLayer, IsNSString value) => caLayer -> value -> IO ()
setContentsFormat caLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg caLayer (mkSelector "setContentsFormat:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- wantsExtendedDynamicRangeContent@
wantsExtendedDynamicRangeContent :: IsCALayer caLayer => caLayer -> IO Bool
wantsExtendedDynamicRangeContent caLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caLayer (mkSelector "wantsExtendedDynamicRangeContent") retCULong []

-- | @- setWantsExtendedDynamicRangeContent:@
setWantsExtendedDynamicRangeContent :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setWantsExtendedDynamicRangeContent caLayer  value =
    sendMsg caLayer (mkSelector "setWantsExtendedDynamicRangeContent:") retVoid [argCULong (if value then 1 else 0)]

-- | @- toneMapMode@
toneMapMode :: IsCALayer caLayer => caLayer -> IO (Id NSString)
toneMapMode caLayer  =
    sendMsg caLayer (mkSelector "toneMapMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setToneMapMode:@
setToneMapMode :: (IsCALayer caLayer, IsNSString value) => caLayer -> value -> IO ()
setToneMapMode caLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg caLayer (mkSelector "setToneMapMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- preferredDynamicRange@
preferredDynamicRange :: IsCALayer caLayer => caLayer -> IO (Id NSString)
preferredDynamicRange caLayer  =
    sendMsg caLayer (mkSelector "preferredDynamicRange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreferredDynamicRange:@
setPreferredDynamicRange :: (IsCALayer caLayer, IsNSString value) => caLayer -> value -> IO ()
setPreferredDynamicRange caLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg caLayer (mkSelector "setPreferredDynamicRange:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contentsHeadroom@
contentsHeadroom :: IsCALayer caLayer => caLayer -> IO CDouble
contentsHeadroom caLayer  =
    sendMsg caLayer (mkSelector "contentsHeadroom") retCDouble []

-- | @- setContentsHeadroom:@
setContentsHeadroom :: IsCALayer caLayer => caLayer -> CDouble -> IO ()
setContentsHeadroom caLayer  value =
    sendMsg caLayer (mkSelector "setContentsHeadroom:") retVoid [argCDouble value]

-- | @- wantsDynamicContentScaling@
wantsDynamicContentScaling :: IsCALayer caLayer => caLayer -> IO Bool
wantsDynamicContentScaling caLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caLayer (mkSelector "wantsDynamicContentScaling") retCULong []

-- | @- setWantsDynamicContentScaling:@
setWantsDynamicContentScaling :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setWantsDynamicContentScaling caLayer  value =
    sendMsg caLayer (mkSelector "setWantsDynamicContentScaling:") retVoid [argCULong (if value then 1 else 0)]

-- | @- minificationFilter@
minificationFilter :: IsCALayer caLayer => caLayer -> IO (Id NSString)
minificationFilter caLayer  =
    sendMsg caLayer (mkSelector "minificationFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinificationFilter:@
setMinificationFilter :: (IsCALayer caLayer, IsNSString value) => caLayer -> value -> IO ()
setMinificationFilter caLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg caLayer (mkSelector "setMinificationFilter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- magnificationFilter@
magnificationFilter :: IsCALayer caLayer => caLayer -> IO (Id NSString)
magnificationFilter caLayer  =
    sendMsg caLayer (mkSelector "magnificationFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMagnificationFilter:@
setMagnificationFilter :: (IsCALayer caLayer, IsNSString value) => caLayer -> value -> IO ()
setMagnificationFilter caLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg caLayer (mkSelector "setMagnificationFilter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minificationFilterBias@
minificationFilterBias :: IsCALayer caLayer => caLayer -> IO CFloat
minificationFilterBias caLayer  =
    sendMsg caLayer (mkSelector "minificationFilterBias") retCFloat []

-- | @- setMinificationFilterBias:@
setMinificationFilterBias :: IsCALayer caLayer => caLayer -> CFloat -> IO ()
setMinificationFilterBias caLayer  value =
    sendMsg caLayer (mkSelector "setMinificationFilterBias:") retVoid [argCFloat value]

-- | @- opaque@
opaque :: IsCALayer caLayer => caLayer -> IO Bool
opaque caLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caLayer (mkSelector "opaque") retCULong []

-- | @- setOpaque:@
setOpaque :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setOpaque caLayer  value =
    sendMsg caLayer (mkSelector "setOpaque:") retVoid [argCULong (if value then 1 else 0)]

-- | @- needsDisplayOnBoundsChange@
needsDisplayOnBoundsChange :: IsCALayer caLayer => caLayer -> IO Bool
needsDisplayOnBoundsChange caLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caLayer (mkSelector "needsDisplayOnBoundsChange") retCULong []

-- | @- setNeedsDisplayOnBoundsChange:@
setNeedsDisplayOnBoundsChange :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setNeedsDisplayOnBoundsChange caLayer  value =
    sendMsg caLayer (mkSelector "setNeedsDisplayOnBoundsChange:") retVoid [argCULong (if value then 1 else 0)]

-- | @- drawsAsynchronously@
drawsAsynchronously :: IsCALayer caLayer => caLayer -> IO Bool
drawsAsynchronously caLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caLayer (mkSelector "drawsAsynchronously") retCULong []

-- | @- setDrawsAsynchronously:@
setDrawsAsynchronously :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setDrawsAsynchronously caLayer  value =
    sendMsg caLayer (mkSelector "setDrawsAsynchronously:") retVoid [argCULong (if value then 1 else 0)]

-- | @- edgeAntialiasingMask@
edgeAntialiasingMask :: IsCALayer caLayer => caLayer -> IO CAEdgeAntialiasingMask
edgeAntialiasingMask caLayer  =
    fmap (coerce :: CUInt -> CAEdgeAntialiasingMask) $ sendMsg caLayer (mkSelector "edgeAntialiasingMask") retCUInt []

-- | @- setEdgeAntialiasingMask:@
setEdgeAntialiasingMask :: IsCALayer caLayer => caLayer -> CAEdgeAntialiasingMask -> IO ()
setEdgeAntialiasingMask caLayer  value =
    sendMsg caLayer (mkSelector "setEdgeAntialiasingMask:") retVoid [argCUInt (coerce value)]

-- | @- allowsEdgeAntialiasing@
allowsEdgeAntialiasing :: IsCALayer caLayer => caLayer -> IO Bool
allowsEdgeAntialiasing caLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caLayer (mkSelector "allowsEdgeAntialiasing") retCULong []

-- | @- setAllowsEdgeAntialiasing:@
setAllowsEdgeAntialiasing :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setAllowsEdgeAntialiasing caLayer  value =
    sendMsg caLayer (mkSelector "setAllowsEdgeAntialiasing:") retVoid [argCULong (if value then 1 else 0)]

-- | @- backgroundColor@
backgroundColor :: IsCALayer caLayer => caLayer -> IO (Ptr ())
backgroundColor caLayer  =
    fmap castPtr $ sendMsg caLayer (mkSelector "backgroundColor") (retPtr retVoid) []

-- | @- setBackgroundColor:@
setBackgroundColor :: IsCALayer caLayer => caLayer -> Ptr () -> IO ()
setBackgroundColor caLayer  value =
    sendMsg caLayer (mkSelector "setBackgroundColor:") retVoid [argPtr value]

-- | @- cornerRadius@
cornerRadius :: IsCALayer caLayer => caLayer -> IO CDouble
cornerRadius caLayer  =
    sendMsg caLayer (mkSelector "cornerRadius") retCDouble []

-- | @- setCornerRadius:@
setCornerRadius :: IsCALayer caLayer => caLayer -> CDouble -> IO ()
setCornerRadius caLayer  value =
    sendMsg caLayer (mkSelector "setCornerRadius:") retVoid [argCDouble value]

-- | @- maskedCorners@
maskedCorners :: IsCALayer caLayer => caLayer -> IO CACornerMask
maskedCorners caLayer  =
    fmap (coerce :: CULong -> CACornerMask) $ sendMsg caLayer (mkSelector "maskedCorners") retCULong []

-- | @- setMaskedCorners:@
setMaskedCorners :: IsCALayer caLayer => caLayer -> CACornerMask -> IO ()
setMaskedCorners caLayer  value =
    sendMsg caLayer (mkSelector "setMaskedCorners:") retVoid [argCULong (coerce value)]

-- | @- cornerCurve@
cornerCurve :: IsCALayer caLayer => caLayer -> IO (Id NSString)
cornerCurve caLayer  =
    sendMsg caLayer (mkSelector "cornerCurve") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCornerCurve:@
setCornerCurve :: (IsCALayer caLayer, IsNSString value) => caLayer -> value -> IO ()
setCornerCurve caLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg caLayer (mkSelector "setCornerCurve:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- borderWidth@
borderWidth :: IsCALayer caLayer => caLayer -> IO CDouble
borderWidth caLayer  =
    sendMsg caLayer (mkSelector "borderWidth") retCDouble []

-- | @- setBorderWidth:@
setBorderWidth :: IsCALayer caLayer => caLayer -> CDouble -> IO ()
setBorderWidth caLayer  value =
    sendMsg caLayer (mkSelector "setBorderWidth:") retVoid [argCDouble value]

-- | @- borderColor@
borderColor :: IsCALayer caLayer => caLayer -> IO (Ptr ())
borderColor caLayer  =
    fmap castPtr $ sendMsg caLayer (mkSelector "borderColor") (retPtr retVoid) []

-- | @- setBorderColor:@
setBorderColor :: IsCALayer caLayer => caLayer -> Ptr () -> IO ()
setBorderColor caLayer  value =
    sendMsg caLayer (mkSelector "setBorderColor:") retVoid [argPtr value]

-- | @- opacity@
opacity :: IsCALayer caLayer => caLayer -> IO CFloat
opacity caLayer  =
    sendMsg caLayer (mkSelector "opacity") retCFloat []

-- | @- setOpacity:@
setOpacity :: IsCALayer caLayer => caLayer -> CFloat -> IO ()
setOpacity caLayer  value =
    sendMsg caLayer (mkSelector "setOpacity:") retVoid [argCFloat value]

-- | @- allowsGroupOpacity@
allowsGroupOpacity :: IsCALayer caLayer => caLayer -> IO Bool
allowsGroupOpacity caLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caLayer (mkSelector "allowsGroupOpacity") retCULong []

-- | @- setAllowsGroupOpacity:@
setAllowsGroupOpacity :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setAllowsGroupOpacity caLayer  value =
    sendMsg caLayer (mkSelector "setAllowsGroupOpacity:") retVoid [argCULong (if value then 1 else 0)]

-- | @- compositingFilter@
compositingFilter :: IsCALayer caLayer => caLayer -> IO RawId
compositingFilter caLayer  =
    fmap (RawId . castPtr) $ sendMsg caLayer (mkSelector "compositingFilter") (retPtr retVoid) []

-- | @- setCompositingFilter:@
setCompositingFilter :: IsCALayer caLayer => caLayer -> RawId -> IO ()
setCompositingFilter caLayer  value =
    sendMsg caLayer (mkSelector "setCompositingFilter:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- filters@
filters :: IsCALayer caLayer => caLayer -> IO (Id NSArray)
filters caLayer  =
    sendMsg caLayer (mkSelector "filters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFilters:@
setFilters :: (IsCALayer caLayer, IsNSArray value) => caLayer -> value -> IO ()
setFilters caLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg caLayer (mkSelector "setFilters:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- backgroundFilters@
backgroundFilters :: IsCALayer caLayer => caLayer -> IO (Id NSArray)
backgroundFilters caLayer  =
    sendMsg caLayer (mkSelector "backgroundFilters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundFilters:@
setBackgroundFilters :: (IsCALayer caLayer, IsNSArray value) => caLayer -> value -> IO ()
setBackgroundFilters caLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg caLayer (mkSelector "setBackgroundFilters:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shouldRasterize@
shouldRasterize :: IsCALayer caLayer => caLayer -> IO Bool
shouldRasterize caLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caLayer (mkSelector "shouldRasterize") retCULong []

-- | @- setShouldRasterize:@
setShouldRasterize :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setShouldRasterize caLayer  value =
    sendMsg caLayer (mkSelector "setShouldRasterize:") retVoid [argCULong (if value then 1 else 0)]

-- | @- rasterizationScale@
rasterizationScale :: IsCALayer caLayer => caLayer -> IO CDouble
rasterizationScale caLayer  =
    sendMsg caLayer (mkSelector "rasterizationScale") retCDouble []

-- | @- setRasterizationScale:@
setRasterizationScale :: IsCALayer caLayer => caLayer -> CDouble -> IO ()
setRasterizationScale caLayer  value =
    sendMsg caLayer (mkSelector "setRasterizationScale:") retVoid [argCDouble value]

-- | Shadow properties. *
--
-- ObjC selector: @- shadowColor@
shadowColor :: IsCALayer caLayer => caLayer -> IO (Ptr ())
shadowColor caLayer  =
    fmap castPtr $ sendMsg caLayer (mkSelector "shadowColor") (retPtr retVoid) []

-- | Shadow properties. *
--
-- ObjC selector: @- setShadowColor:@
setShadowColor :: IsCALayer caLayer => caLayer -> Ptr () -> IO ()
setShadowColor caLayer  value =
    sendMsg caLayer (mkSelector "setShadowColor:") retVoid [argPtr value]

-- | @- shadowOpacity@
shadowOpacity :: IsCALayer caLayer => caLayer -> IO CFloat
shadowOpacity caLayer  =
    sendMsg caLayer (mkSelector "shadowOpacity") retCFloat []

-- | @- setShadowOpacity:@
setShadowOpacity :: IsCALayer caLayer => caLayer -> CFloat -> IO ()
setShadowOpacity caLayer  value =
    sendMsg caLayer (mkSelector "setShadowOpacity:") retVoid [argCFloat value]

-- | @- shadowRadius@
shadowRadius :: IsCALayer caLayer => caLayer -> IO CDouble
shadowRadius caLayer  =
    sendMsg caLayer (mkSelector "shadowRadius") retCDouble []

-- | @- setShadowRadius:@
setShadowRadius :: IsCALayer caLayer => caLayer -> CDouble -> IO ()
setShadowRadius caLayer  value =
    sendMsg caLayer (mkSelector "setShadowRadius:") retVoid [argCDouble value]

-- | @- shadowPath@
shadowPath :: IsCALayer caLayer => caLayer -> IO RawId
shadowPath caLayer  =
    fmap (RawId . castPtr) $ sendMsg caLayer (mkSelector "shadowPath") (retPtr retVoid) []

-- | @- setShadowPath:@
setShadowPath :: IsCALayer caLayer => caLayer -> RawId -> IO ()
setShadowPath caLayer  value =
    sendMsg caLayer (mkSelector "setShadowPath:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Layout methods. *
--
-- ObjC selector: @- autoresizingMask@
autoresizingMask :: IsCALayer caLayer => caLayer -> IO CAAutoresizingMask
autoresizingMask caLayer  =
    fmap (coerce :: CUInt -> CAAutoresizingMask) $ sendMsg caLayer (mkSelector "autoresizingMask") retCUInt []

-- | Layout methods. *
--
-- ObjC selector: @- setAutoresizingMask:@
setAutoresizingMask :: IsCALayer caLayer => caLayer -> CAAutoresizingMask -> IO ()
setAutoresizingMask caLayer  value =
    sendMsg caLayer (mkSelector "setAutoresizingMask:") retVoid [argCUInt (coerce value)]

-- | @- layoutManager@
layoutManager :: IsCALayer caLayer => caLayer -> IO RawId
layoutManager caLayer  =
    fmap (RawId . castPtr) $ sendMsg caLayer (mkSelector "layoutManager") (retPtr retVoid) []

-- | @- setLayoutManager:@
setLayoutManager :: IsCALayer caLayer => caLayer -> RawId -> IO ()
setLayoutManager caLayer  value =
    sendMsg caLayer (mkSelector "setLayoutManager:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- actions@
actions :: IsCALayer caLayer => caLayer -> IO (Id NSDictionary)
actions caLayer  =
    sendMsg caLayer (mkSelector "actions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setActions:@
setActions :: (IsCALayer caLayer, IsNSDictionary value) => caLayer -> value -> IO ()
setActions caLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg caLayer (mkSelector "setActions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Miscellaneous properties. *
--
-- ObjC selector: @- name@
name :: IsCALayer caLayer => caLayer -> IO (Id NSString)
name caLayer  =
    sendMsg caLayer (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Miscellaneous properties. *
--
-- ObjC selector: @- setName:@
setName :: (IsCALayer caLayer, IsNSString value) => caLayer -> value -> IO ()
setName caLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg caLayer (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- delegate@
delegate :: IsCALayer caLayer => caLayer -> IO RawId
delegate caLayer  =
    fmap (RawId . castPtr) $ sendMsg caLayer (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsCALayer caLayer => caLayer -> RawId -> IO ()
setDelegate caLayer  value =
    sendMsg caLayer (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- style@
style :: IsCALayer caLayer => caLayer -> IO (Id NSDictionary)
style caLayer  =
    sendMsg caLayer (mkSelector "style") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStyle:@
setStyle :: (IsCALayer caLayer, IsNSDictionary value) => caLayer -> value -> IO ()
setStyle caLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg caLayer (mkSelector "setStyle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- constraints@
constraints :: IsCALayer caLayer => caLayer -> IO (Id NSArray)
constraints caLayer  =
    sendMsg caLayer (mkSelector "constraints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setConstraints:@
setConstraints :: (IsCALayer caLayer, IsNSArray value) => caLayer -> value -> IO ()
setConstraints caLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg caLayer (mkSelector "setConstraints:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layer@
layerSelector :: Selector
layerSelector = mkSelector "layer"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithLayer:@
initWithLayerSelector :: Selector
initWithLayerSelector = mkSelector "initWithLayer:"

-- | @Selector@ for @presentationLayer@
presentationLayerSelector :: Selector
presentationLayerSelector = mkSelector "presentationLayer"

-- | @Selector@ for @modelLayer@
modelLayerSelector :: Selector
modelLayerSelector = mkSelector "modelLayer"

-- | @Selector@ for @defaultValueForKey:@
defaultValueForKeySelector :: Selector
defaultValueForKeySelector = mkSelector "defaultValueForKey:"

-- | @Selector@ for @needsDisplayForKey:@
needsDisplayForKeySelector :: Selector
needsDisplayForKeySelector = mkSelector "needsDisplayForKey:"

-- | @Selector@ for @shouldArchiveValueForKey:@
shouldArchiveValueForKeySelector :: Selector
shouldArchiveValueForKeySelector = mkSelector "shouldArchiveValueForKey:"

-- | @Selector@ for @contentsAreFlipped@
contentsAreFlippedSelector :: Selector
contentsAreFlippedSelector = mkSelector "contentsAreFlipped"

-- | @Selector@ for @removeFromSuperlayer@
removeFromSuperlayerSelector :: Selector
removeFromSuperlayerSelector = mkSelector "removeFromSuperlayer"

-- | @Selector@ for @addSublayer:@
addSublayerSelector :: Selector
addSublayerSelector = mkSelector "addSublayer:"

-- | @Selector@ for @insertSublayer:atIndex:@
insertSublayer_atIndexSelector :: Selector
insertSublayer_atIndexSelector = mkSelector "insertSublayer:atIndex:"

-- | @Selector@ for @insertSublayer:below:@
insertSublayer_belowSelector :: Selector
insertSublayer_belowSelector = mkSelector "insertSublayer:below:"

-- | @Selector@ for @insertSublayer:above:@
insertSublayer_aboveSelector :: Selector
insertSublayer_aboveSelector = mkSelector "insertSublayer:above:"

-- | @Selector@ for @replaceSublayer:with:@
replaceSublayer_withSelector :: Selector
replaceSublayer_withSelector = mkSelector "replaceSublayer:with:"

-- | @Selector@ for @convertTime:fromLayer:@
convertTime_fromLayerSelector :: Selector
convertTime_fromLayerSelector = mkSelector "convertTime:fromLayer:"

-- | @Selector@ for @convertTime:toLayer:@
convertTime_toLayerSelector :: Selector
convertTime_toLayerSelector = mkSelector "convertTime:toLayer:"

-- | @Selector@ for @display@
displaySelector :: Selector
displaySelector = mkSelector "display"

-- | @Selector@ for @setNeedsDisplay@
setNeedsDisplaySelector :: Selector
setNeedsDisplaySelector = mkSelector "setNeedsDisplay"

-- | @Selector@ for @needsDisplay@
needsDisplaySelector :: Selector
needsDisplaySelector = mkSelector "needsDisplay"

-- | @Selector@ for @displayIfNeeded@
displayIfNeededSelector :: Selector
displayIfNeededSelector = mkSelector "displayIfNeeded"

-- | @Selector@ for @drawInContext:@
drawInContextSelector :: Selector
drawInContextSelector = mkSelector "drawInContext:"

-- | @Selector@ for @renderInContext:@
renderInContextSelector :: Selector
renderInContextSelector = mkSelector "renderInContext:"

-- | @Selector@ for @cornerCurveExpansionFactor:@
cornerCurveExpansionFactorSelector :: Selector
cornerCurveExpansionFactorSelector = mkSelector "cornerCurveExpansionFactor:"

-- | @Selector@ for @setNeedsLayout@
setNeedsLayoutSelector :: Selector
setNeedsLayoutSelector = mkSelector "setNeedsLayout"

-- | @Selector@ for @needsLayout@
needsLayoutSelector :: Selector
needsLayoutSelector = mkSelector "needsLayout"

-- | @Selector@ for @layoutIfNeeded@
layoutIfNeededSelector :: Selector
layoutIfNeededSelector = mkSelector "layoutIfNeeded"

-- | @Selector@ for @layoutSublayers@
layoutSublayersSelector :: Selector
layoutSublayersSelector = mkSelector "layoutSublayers"

-- | @Selector@ for @defaultActionForKey:@
defaultActionForKeySelector :: Selector
defaultActionForKeySelector = mkSelector "defaultActionForKey:"

-- | @Selector@ for @actionForKey:@
actionForKeySelector :: Selector
actionForKeySelector = mkSelector "actionForKey:"

-- | @Selector@ for @addAnimation:forKey:@
addAnimation_forKeySelector :: Selector
addAnimation_forKeySelector = mkSelector "addAnimation:forKey:"

-- | @Selector@ for @removeAllAnimations@
removeAllAnimationsSelector :: Selector
removeAllAnimationsSelector = mkSelector "removeAllAnimations"

-- | @Selector@ for @removeAnimationForKey:@
removeAnimationForKeySelector :: Selector
removeAnimationForKeySelector = mkSelector "removeAnimationForKey:"

-- | @Selector@ for @animationKeys@
animationKeysSelector :: Selector
animationKeysSelector = mkSelector "animationKeys"

-- | @Selector@ for @animationForKey:@
animationForKeySelector :: Selector
animationForKeySelector = mkSelector "animationForKey:"

-- | @Selector@ for @layerWithRemoteClientId:@
layerWithRemoteClientIdSelector :: Selector
layerWithRemoteClientIdSelector = mkSelector "layerWithRemoteClientId:"

-- | @Selector@ for @addConstraint:@
addConstraintSelector :: Selector
addConstraintSelector = mkSelector "addConstraint:"

-- | @Selector@ for @zPosition@
zPositionSelector :: Selector
zPositionSelector = mkSelector "zPosition"

-- | @Selector@ for @setZPosition:@
setZPositionSelector :: Selector
setZPositionSelector = mkSelector "setZPosition:"

-- | @Selector@ for @anchorPointZ@
anchorPointZSelector :: Selector
anchorPointZSelector = mkSelector "anchorPointZ"

-- | @Selector@ for @setAnchorPointZ:@
setAnchorPointZSelector :: Selector
setAnchorPointZSelector = mkSelector "setAnchorPointZ:"

-- | @Selector@ for @transform@
transformSelector :: Selector
transformSelector = mkSelector "transform"

-- | @Selector@ for @setTransform:@
setTransformSelector :: Selector
setTransformSelector = mkSelector "setTransform:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @doubleSided@
doubleSidedSelector :: Selector
doubleSidedSelector = mkSelector "doubleSided"

-- | @Selector@ for @setDoubleSided:@
setDoubleSidedSelector :: Selector
setDoubleSidedSelector = mkSelector "setDoubleSided:"

-- | @Selector@ for @geometryFlipped@
geometryFlippedSelector :: Selector
geometryFlippedSelector = mkSelector "geometryFlipped"

-- | @Selector@ for @setGeometryFlipped:@
setGeometryFlippedSelector :: Selector
setGeometryFlippedSelector = mkSelector "setGeometryFlipped:"

-- | @Selector@ for @superlayer@
superlayerSelector :: Selector
superlayerSelector = mkSelector "superlayer"

-- | @Selector@ for @sublayers@
sublayersSelector :: Selector
sublayersSelector = mkSelector "sublayers"

-- | @Selector@ for @setSublayers:@
setSublayersSelector :: Selector
setSublayersSelector = mkSelector "setSublayers:"

-- | @Selector@ for @sublayerTransform@
sublayerTransformSelector :: Selector
sublayerTransformSelector = mkSelector "sublayerTransform"

-- | @Selector@ for @setSublayerTransform:@
setSublayerTransformSelector :: Selector
setSublayerTransformSelector = mkSelector "setSublayerTransform:"

-- | @Selector@ for @mask@
maskSelector :: Selector
maskSelector = mkSelector "mask"

-- | @Selector@ for @setMask:@
setMaskSelector :: Selector
setMaskSelector = mkSelector "setMask:"

-- | @Selector@ for @masksToBounds@
masksToBoundsSelector :: Selector
masksToBoundsSelector = mkSelector "masksToBounds"

-- | @Selector@ for @setMasksToBounds:@
setMasksToBoundsSelector :: Selector
setMasksToBoundsSelector = mkSelector "setMasksToBounds:"

-- | @Selector@ for @contents@
contentsSelector :: Selector
contentsSelector = mkSelector "contents"

-- | @Selector@ for @setContents:@
setContentsSelector :: Selector
setContentsSelector = mkSelector "setContents:"

-- | @Selector@ for @contentsGravity@
contentsGravitySelector :: Selector
contentsGravitySelector = mkSelector "contentsGravity"

-- | @Selector@ for @setContentsGravity:@
setContentsGravitySelector :: Selector
setContentsGravitySelector = mkSelector "setContentsGravity:"

-- | @Selector@ for @contentsScale@
contentsScaleSelector :: Selector
contentsScaleSelector = mkSelector "contentsScale"

-- | @Selector@ for @setContentsScale:@
setContentsScaleSelector :: Selector
setContentsScaleSelector = mkSelector "setContentsScale:"

-- | @Selector@ for @contentsFormat@
contentsFormatSelector :: Selector
contentsFormatSelector = mkSelector "contentsFormat"

-- | @Selector@ for @setContentsFormat:@
setContentsFormatSelector :: Selector
setContentsFormatSelector = mkSelector "setContentsFormat:"

-- | @Selector@ for @wantsExtendedDynamicRangeContent@
wantsExtendedDynamicRangeContentSelector :: Selector
wantsExtendedDynamicRangeContentSelector = mkSelector "wantsExtendedDynamicRangeContent"

-- | @Selector@ for @setWantsExtendedDynamicRangeContent:@
setWantsExtendedDynamicRangeContentSelector :: Selector
setWantsExtendedDynamicRangeContentSelector = mkSelector "setWantsExtendedDynamicRangeContent:"

-- | @Selector@ for @toneMapMode@
toneMapModeSelector :: Selector
toneMapModeSelector = mkSelector "toneMapMode"

-- | @Selector@ for @setToneMapMode:@
setToneMapModeSelector :: Selector
setToneMapModeSelector = mkSelector "setToneMapMode:"

-- | @Selector@ for @preferredDynamicRange@
preferredDynamicRangeSelector :: Selector
preferredDynamicRangeSelector = mkSelector "preferredDynamicRange"

-- | @Selector@ for @setPreferredDynamicRange:@
setPreferredDynamicRangeSelector :: Selector
setPreferredDynamicRangeSelector = mkSelector "setPreferredDynamicRange:"

-- | @Selector@ for @contentsHeadroom@
contentsHeadroomSelector :: Selector
contentsHeadroomSelector = mkSelector "contentsHeadroom"

-- | @Selector@ for @setContentsHeadroom:@
setContentsHeadroomSelector :: Selector
setContentsHeadroomSelector = mkSelector "setContentsHeadroom:"

-- | @Selector@ for @wantsDynamicContentScaling@
wantsDynamicContentScalingSelector :: Selector
wantsDynamicContentScalingSelector = mkSelector "wantsDynamicContentScaling"

-- | @Selector@ for @setWantsDynamicContentScaling:@
setWantsDynamicContentScalingSelector :: Selector
setWantsDynamicContentScalingSelector = mkSelector "setWantsDynamicContentScaling:"

-- | @Selector@ for @minificationFilter@
minificationFilterSelector :: Selector
minificationFilterSelector = mkSelector "minificationFilter"

-- | @Selector@ for @setMinificationFilter:@
setMinificationFilterSelector :: Selector
setMinificationFilterSelector = mkSelector "setMinificationFilter:"

-- | @Selector@ for @magnificationFilter@
magnificationFilterSelector :: Selector
magnificationFilterSelector = mkSelector "magnificationFilter"

-- | @Selector@ for @setMagnificationFilter:@
setMagnificationFilterSelector :: Selector
setMagnificationFilterSelector = mkSelector "setMagnificationFilter:"

-- | @Selector@ for @minificationFilterBias@
minificationFilterBiasSelector :: Selector
minificationFilterBiasSelector = mkSelector "minificationFilterBias"

-- | @Selector@ for @setMinificationFilterBias:@
setMinificationFilterBiasSelector :: Selector
setMinificationFilterBiasSelector = mkSelector "setMinificationFilterBias:"

-- | @Selector@ for @opaque@
opaqueSelector :: Selector
opaqueSelector = mkSelector "opaque"

-- | @Selector@ for @setOpaque:@
setOpaqueSelector :: Selector
setOpaqueSelector = mkSelector "setOpaque:"

-- | @Selector@ for @needsDisplayOnBoundsChange@
needsDisplayOnBoundsChangeSelector :: Selector
needsDisplayOnBoundsChangeSelector = mkSelector "needsDisplayOnBoundsChange"

-- | @Selector@ for @setNeedsDisplayOnBoundsChange:@
setNeedsDisplayOnBoundsChangeSelector :: Selector
setNeedsDisplayOnBoundsChangeSelector = mkSelector "setNeedsDisplayOnBoundsChange:"

-- | @Selector@ for @drawsAsynchronously@
drawsAsynchronouslySelector :: Selector
drawsAsynchronouslySelector = mkSelector "drawsAsynchronously"

-- | @Selector@ for @setDrawsAsynchronously:@
setDrawsAsynchronouslySelector :: Selector
setDrawsAsynchronouslySelector = mkSelector "setDrawsAsynchronously:"

-- | @Selector@ for @edgeAntialiasingMask@
edgeAntialiasingMaskSelector :: Selector
edgeAntialiasingMaskSelector = mkSelector "edgeAntialiasingMask"

-- | @Selector@ for @setEdgeAntialiasingMask:@
setEdgeAntialiasingMaskSelector :: Selector
setEdgeAntialiasingMaskSelector = mkSelector "setEdgeAntialiasingMask:"

-- | @Selector@ for @allowsEdgeAntialiasing@
allowsEdgeAntialiasingSelector :: Selector
allowsEdgeAntialiasingSelector = mkSelector "allowsEdgeAntialiasing"

-- | @Selector@ for @setAllowsEdgeAntialiasing:@
setAllowsEdgeAntialiasingSelector :: Selector
setAllowsEdgeAntialiasingSelector = mkSelector "setAllowsEdgeAntialiasing:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @cornerRadius@
cornerRadiusSelector :: Selector
cornerRadiusSelector = mkSelector "cornerRadius"

-- | @Selector@ for @setCornerRadius:@
setCornerRadiusSelector :: Selector
setCornerRadiusSelector = mkSelector "setCornerRadius:"

-- | @Selector@ for @maskedCorners@
maskedCornersSelector :: Selector
maskedCornersSelector = mkSelector "maskedCorners"

-- | @Selector@ for @setMaskedCorners:@
setMaskedCornersSelector :: Selector
setMaskedCornersSelector = mkSelector "setMaskedCorners:"

-- | @Selector@ for @cornerCurve@
cornerCurveSelector :: Selector
cornerCurveSelector = mkSelector "cornerCurve"

-- | @Selector@ for @setCornerCurve:@
setCornerCurveSelector :: Selector
setCornerCurveSelector = mkSelector "setCornerCurve:"

-- | @Selector@ for @borderWidth@
borderWidthSelector :: Selector
borderWidthSelector = mkSelector "borderWidth"

-- | @Selector@ for @setBorderWidth:@
setBorderWidthSelector :: Selector
setBorderWidthSelector = mkSelector "setBorderWidth:"

-- | @Selector@ for @borderColor@
borderColorSelector :: Selector
borderColorSelector = mkSelector "borderColor"

-- | @Selector@ for @setBorderColor:@
setBorderColorSelector :: Selector
setBorderColorSelector = mkSelector "setBorderColor:"

-- | @Selector@ for @opacity@
opacitySelector :: Selector
opacitySelector = mkSelector "opacity"

-- | @Selector@ for @setOpacity:@
setOpacitySelector :: Selector
setOpacitySelector = mkSelector "setOpacity:"

-- | @Selector@ for @allowsGroupOpacity@
allowsGroupOpacitySelector :: Selector
allowsGroupOpacitySelector = mkSelector "allowsGroupOpacity"

-- | @Selector@ for @setAllowsGroupOpacity:@
setAllowsGroupOpacitySelector :: Selector
setAllowsGroupOpacitySelector = mkSelector "setAllowsGroupOpacity:"

-- | @Selector@ for @compositingFilter@
compositingFilterSelector :: Selector
compositingFilterSelector = mkSelector "compositingFilter"

-- | @Selector@ for @setCompositingFilter:@
setCompositingFilterSelector :: Selector
setCompositingFilterSelector = mkSelector "setCompositingFilter:"

-- | @Selector@ for @filters@
filtersSelector :: Selector
filtersSelector = mkSelector "filters"

-- | @Selector@ for @setFilters:@
setFiltersSelector :: Selector
setFiltersSelector = mkSelector "setFilters:"

-- | @Selector@ for @backgroundFilters@
backgroundFiltersSelector :: Selector
backgroundFiltersSelector = mkSelector "backgroundFilters"

-- | @Selector@ for @setBackgroundFilters:@
setBackgroundFiltersSelector :: Selector
setBackgroundFiltersSelector = mkSelector "setBackgroundFilters:"

-- | @Selector@ for @shouldRasterize@
shouldRasterizeSelector :: Selector
shouldRasterizeSelector = mkSelector "shouldRasterize"

-- | @Selector@ for @setShouldRasterize:@
setShouldRasterizeSelector :: Selector
setShouldRasterizeSelector = mkSelector "setShouldRasterize:"

-- | @Selector@ for @rasterizationScale@
rasterizationScaleSelector :: Selector
rasterizationScaleSelector = mkSelector "rasterizationScale"

-- | @Selector@ for @setRasterizationScale:@
setRasterizationScaleSelector :: Selector
setRasterizationScaleSelector = mkSelector "setRasterizationScale:"

-- | @Selector@ for @shadowColor@
shadowColorSelector :: Selector
shadowColorSelector = mkSelector "shadowColor"

-- | @Selector@ for @setShadowColor:@
setShadowColorSelector :: Selector
setShadowColorSelector = mkSelector "setShadowColor:"

-- | @Selector@ for @shadowOpacity@
shadowOpacitySelector :: Selector
shadowOpacitySelector = mkSelector "shadowOpacity"

-- | @Selector@ for @setShadowOpacity:@
setShadowOpacitySelector :: Selector
setShadowOpacitySelector = mkSelector "setShadowOpacity:"

-- | @Selector@ for @shadowRadius@
shadowRadiusSelector :: Selector
shadowRadiusSelector = mkSelector "shadowRadius"

-- | @Selector@ for @setShadowRadius:@
setShadowRadiusSelector :: Selector
setShadowRadiusSelector = mkSelector "setShadowRadius:"

-- | @Selector@ for @shadowPath@
shadowPathSelector :: Selector
shadowPathSelector = mkSelector "shadowPath"

-- | @Selector@ for @setShadowPath:@
setShadowPathSelector :: Selector
setShadowPathSelector = mkSelector "setShadowPath:"

-- | @Selector@ for @autoresizingMask@
autoresizingMaskSelector :: Selector
autoresizingMaskSelector = mkSelector "autoresizingMask"

-- | @Selector@ for @setAutoresizingMask:@
setAutoresizingMaskSelector :: Selector
setAutoresizingMaskSelector = mkSelector "setAutoresizingMask:"

-- | @Selector@ for @layoutManager@
layoutManagerSelector :: Selector
layoutManagerSelector = mkSelector "layoutManager"

-- | @Selector@ for @setLayoutManager:@
setLayoutManagerSelector :: Selector
setLayoutManagerSelector = mkSelector "setLayoutManager:"

-- | @Selector@ for @actions@
actionsSelector :: Selector
actionsSelector = mkSelector "actions"

-- | @Selector@ for @setActions:@
setActionsSelector :: Selector
setActionsSelector = mkSelector "setActions:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @style@
styleSelector :: Selector
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector
setStyleSelector = mkSelector "setStyle:"

-- | @Selector@ for @constraints@
constraintsSelector :: Selector
constraintsSelector = mkSelector "constraints"

-- | @Selector@ for @setConstraints:@
setConstraintsSelector :: Selector
setConstraintsSelector = mkSelector "setConstraints:"

