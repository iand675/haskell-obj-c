{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , actionForKeySelector
  , actionsSelector
  , addAnimation_forKeySelector
  , addConstraintSelector
  , addSublayerSelector
  , allowsEdgeAntialiasingSelector
  , allowsGroupOpacitySelector
  , anchorPointZSelector
  , animationForKeySelector
  , animationKeysSelector
  , autoresizingMaskSelector
  , backgroundColorSelector
  , backgroundFiltersSelector
  , borderColorSelector
  , borderWidthSelector
  , compositingFilterSelector
  , constraintsSelector
  , contentsAreFlippedSelector
  , contentsFormatSelector
  , contentsGravitySelector
  , contentsHeadroomSelector
  , contentsScaleSelector
  , contentsSelector
  , convertTime_fromLayerSelector
  , convertTime_toLayerSelector
  , cornerCurveExpansionFactorSelector
  , cornerCurveSelector
  , cornerRadiusSelector
  , defaultActionForKeySelector
  , defaultValueForKeySelector
  , delegateSelector
  , displayIfNeededSelector
  , displaySelector
  , doubleSidedSelector
  , drawInContextSelector
  , drawsAsynchronouslySelector
  , edgeAntialiasingMaskSelector
  , filtersSelector
  , geometryFlippedSelector
  , hiddenSelector
  , initSelector
  , initWithLayerSelector
  , insertSublayer_aboveSelector
  , insertSublayer_atIndexSelector
  , insertSublayer_belowSelector
  , layerSelector
  , layerWithRemoteClientIdSelector
  , layoutIfNeededSelector
  , layoutManagerSelector
  , layoutSublayersSelector
  , magnificationFilterSelector
  , maskSelector
  , maskedCornersSelector
  , masksToBoundsSelector
  , minificationFilterBiasSelector
  , minificationFilterSelector
  , modelLayerSelector
  , nameSelector
  , needsDisplayForKeySelector
  , needsDisplayOnBoundsChangeSelector
  , needsDisplaySelector
  , needsLayoutSelector
  , opacitySelector
  , opaqueSelector
  , preferredDynamicRangeSelector
  , presentationLayerSelector
  , rasterizationScaleSelector
  , removeAllAnimationsSelector
  , removeAnimationForKeySelector
  , removeFromSuperlayerSelector
  , renderInContextSelector
  , replaceSublayer_withSelector
  , setActionsSelector
  , setAllowsEdgeAntialiasingSelector
  , setAllowsGroupOpacitySelector
  , setAnchorPointZSelector
  , setAutoresizingMaskSelector
  , setBackgroundColorSelector
  , setBackgroundFiltersSelector
  , setBorderColorSelector
  , setBorderWidthSelector
  , setCompositingFilterSelector
  , setConstraintsSelector
  , setContentsFormatSelector
  , setContentsGravitySelector
  , setContentsHeadroomSelector
  , setContentsScaleSelector
  , setContentsSelector
  , setCornerCurveSelector
  , setCornerRadiusSelector
  , setDelegateSelector
  , setDoubleSidedSelector
  , setDrawsAsynchronouslySelector
  , setEdgeAntialiasingMaskSelector
  , setFiltersSelector
  , setGeometryFlippedSelector
  , setHiddenSelector
  , setLayoutManagerSelector
  , setMagnificationFilterSelector
  , setMaskSelector
  , setMaskedCornersSelector
  , setMasksToBoundsSelector
  , setMinificationFilterBiasSelector
  , setMinificationFilterSelector
  , setNameSelector
  , setNeedsDisplayOnBoundsChangeSelector
  , setNeedsDisplaySelector
  , setNeedsLayoutSelector
  , setOpacitySelector
  , setOpaqueSelector
  , setPreferredDynamicRangeSelector
  , setRasterizationScaleSelector
  , setShadowColorSelector
  , setShadowOpacitySelector
  , setShadowPathSelector
  , setShadowRadiusSelector
  , setShouldRasterizeSelector
  , setStyleSelector
  , setSublayerTransformSelector
  , setSublayersSelector
  , setToneMapModeSelector
  , setTransformSelector
  , setWantsDynamicContentScalingSelector
  , setWantsExtendedDynamicRangeContentSelector
  , setZPositionSelector
  , shadowColorSelector
  , shadowOpacitySelector
  , shadowPathSelector
  , shadowRadiusSelector
  , shouldArchiveValueForKeySelector
  , shouldRasterizeSelector
  , styleSelector
  , sublayerTransformSelector
  , sublayersSelector
  , superlayerSelector
  , toneMapModeSelector
  , transformSelector
  , wantsDynamicContentScalingSelector
  , wantsExtendedDynamicRangeContentSelector
  , zPositionSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' layerSelector

-- | @- init@
init_ :: IsCALayer caLayer => caLayer -> IO (Id CALayer)
init_ caLayer =
  sendOwnedMessage caLayer initSelector

-- | @- initWithLayer:@
initWithLayer :: IsCALayer caLayer => caLayer -> RawId -> IO (Id CALayer)
initWithLayer caLayer layer =
  sendOwnedMessage caLayer initWithLayerSelector layer

-- | @- presentationLayer@
presentationLayer :: IsCALayer caLayer => caLayer -> IO (Id CALayer)
presentationLayer caLayer =
  sendMessage caLayer presentationLayerSelector

-- | @- modelLayer@
modelLayer :: IsCALayer caLayer => caLayer -> IO (Id CALayer)
modelLayer caLayer =
  sendMessage caLayer modelLayerSelector

-- | Property methods. *
--
-- ObjC selector: @+ defaultValueForKey:@
defaultValueForKey :: IsNSString key => key -> IO RawId
defaultValueForKey key =
  do
    cls' <- getRequiredClass "CALayer"
    sendClassMessage cls' defaultValueForKeySelector (toNSString key)

-- | @+ needsDisplayForKey:@
needsDisplayForKey :: IsNSString key => key -> IO Bool
needsDisplayForKey key =
  do
    cls' <- getRequiredClass "CALayer"
    sendClassMessage cls' needsDisplayForKeySelector (toNSString key)

-- | @- shouldArchiveValueForKey:@
shouldArchiveValueForKey :: (IsCALayer caLayer, IsNSString key) => caLayer -> key -> IO Bool
shouldArchiveValueForKey caLayer key =
  sendMessage caLayer shouldArchiveValueForKeySelector (toNSString key)

-- | @- contentsAreFlipped@
contentsAreFlipped :: IsCALayer caLayer => caLayer -> IO Bool
contentsAreFlipped caLayer =
  sendMessage caLayer contentsAreFlippedSelector

-- | @- removeFromSuperlayer@
removeFromSuperlayer :: IsCALayer caLayer => caLayer -> IO ()
removeFromSuperlayer caLayer =
  sendMessage caLayer removeFromSuperlayerSelector

-- | @- addSublayer:@
addSublayer :: (IsCALayer caLayer, IsCALayer layer) => caLayer -> layer -> IO ()
addSublayer caLayer layer =
  sendMessage caLayer addSublayerSelector (toCALayer layer)

-- | @- insertSublayer:atIndex:@
insertSublayer_atIndex :: (IsCALayer caLayer, IsCALayer layer) => caLayer -> layer -> CUInt -> IO ()
insertSublayer_atIndex caLayer layer idx =
  sendMessage caLayer insertSublayer_atIndexSelector (toCALayer layer) idx

-- | @- insertSublayer:below:@
insertSublayer_below :: (IsCALayer caLayer, IsCALayer layer, IsCALayer sibling) => caLayer -> layer -> sibling -> IO ()
insertSublayer_below caLayer layer sibling =
  sendMessage caLayer insertSublayer_belowSelector (toCALayer layer) (toCALayer sibling)

-- | @- insertSublayer:above:@
insertSublayer_above :: (IsCALayer caLayer, IsCALayer layer, IsCALayer sibling) => caLayer -> layer -> sibling -> IO ()
insertSublayer_above caLayer layer sibling =
  sendMessage caLayer insertSublayer_aboveSelector (toCALayer layer) (toCALayer sibling)

-- | @- replaceSublayer:with:@
replaceSublayer_with :: (IsCALayer caLayer, IsCALayer oldLayer, IsCALayer newLayer) => caLayer -> oldLayer -> newLayer -> IO ()
replaceSublayer_with caLayer oldLayer newLayer =
  sendMessage caLayer replaceSublayer_withSelector (toCALayer oldLayer) (toCALayer newLayer)

-- | @- convertTime:fromLayer:@
convertTime_fromLayer :: (IsCALayer caLayer, IsCALayer l) => caLayer -> CDouble -> l -> IO CDouble
convertTime_fromLayer caLayer t l =
  sendMessage caLayer convertTime_fromLayerSelector t (toCALayer l)

-- | @- convertTime:toLayer:@
convertTime_toLayer :: (IsCALayer caLayer, IsCALayer l) => caLayer -> CDouble -> l -> IO CDouble
convertTime_toLayer caLayer t l =
  sendMessage caLayer convertTime_toLayerSelector t (toCALayer l)

-- | @- display@
display :: IsCALayer caLayer => caLayer -> IO ()
display caLayer =
  sendMessage caLayer displaySelector

-- | @- setNeedsDisplay@
setNeedsDisplay :: IsCALayer caLayer => caLayer -> IO ()
setNeedsDisplay caLayer =
  sendMessage caLayer setNeedsDisplaySelector

-- | @- needsDisplay@
needsDisplay :: IsCALayer caLayer => caLayer -> IO Bool
needsDisplay caLayer =
  sendMessage caLayer needsDisplaySelector

-- | @- displayIfNeeded@
displayIfNeeded :: IsCALayer caLayer => caLayer -> IO ()
displayIfNeeded caLayer =
  sendMessage caLayer displayIfNeededSelector

-- | @- drawInContext:@
drawInContext :: IsCALayer caLayer => caLayer -> Ptr () -> IO ()
drawInContext caLayer ctx =
  sendMessage caLayer drawInContextSelector ctx

-- | Rendering properties and methods. *
--
-- ObjC selector: @- renderInContext:@
renderInContext :: IsCALayer caLayer => caLayer -> Ptr () -> IO ()
renderInContext caLayer ctx =
  sendMessage caLayer renderInContextSelector ctx

-- | @+ cornerCurveExpansionFactor:@
cornerCurveExpansionFactor :: IsNSString curve => curve -> IO CDouble
cornerCurveExpansionFactor curve =
  do
    cls' <- getRequiredClass "CALayer"
    sendClassMessage cls' cornerCurveExpansionFactorSelector (toNSString curve)

-- | @- setNeedsLayout@
setNeedsLayout :: IsCALayer caLayer => caLayer -> IO ()
setNeedsLayout caLayer =
  sendMessage caLayer setNeedsLayoutSelector

-- | @- needsLayout@
needsLayout :: IsCALayer caLayer => caLayer -> IO Bool
needsLayout caLayer =
  sendMessage caLayer needsLayoutSelector

-- | @- layoutIfNeeded@
layoutIfNeeded :: IsCALayer caLayer => caLayer -> IO ()
layoutIfNeeded caLayer =
  sendMessage caLayer layoutIfNeededSelector

-- | @- layoutSublayers@
layoutSublayers :: IsCALayer caLayer => caLayer -> IO ()
layoutSublayers caLayer =
  sendMessage caLayer layoutSublayersSelector

-- | Action methods. *
--
-- ObjC selector: @+ defaultActionForKey:@
defaultActionForKey :: IsNSString event => event -> IO RawId
defaultActionForKey event =
  do
    cls' <- getRequiredClass "CALayer"
    sendClassMessage cls' defaultActionForKeySelector (toNSString event)

-- | @- actionForKey:@
actionForKey :: (IsCALayer caLayer, IsNSString event) => caLayer -> event -> IO RawId
actionForKey caLayer event =
  sendMessage caLayer actionForKeySelector (toNSString event)

-- | Animation methods. *
--
-- ObjC selector: @- addAnimation:forKey:@
addAnimation_forKey :: (IsCALayer caLayer, IsCAAnimation anim, IsNSString key) => caLayer -> anim -> key -> IO ()
addAnimation_forKey caLayer anim key =
  sendMessage caLayer addAnimation_forKeySelector (toCAAnimation anim) (toNSString key)

-- | @- removeAllAnimations@
removeAllAnimations :: IsCALayer caLayer => caLayer -> IO ()
removeAllAnimations caLayer =
  sendMessage caLayer removeAllAnimationsSelector

-- | @- removeAnimationForKey:@
removeAnimationForKey :: (IsCALayer caLayer, IsNSString key) => caLayer -> key -> IO ()
removeAnimationForKey caLayer key =
  sendMessage caLayer removeAnimationForKeySelector (toNSString key)

-- | @- animationKeys@
animationKeys :: IsCALayer caLayer => caLayer -> IO (Id NSArray)
animationKeys caLayer =
  sendMessage caLayer animationKeysSelector

-- | @- animationForKey:@
animationForKey :: (IsCALayer caLayer, IsNSString key) => caLayer -> key -> IO (Id CAAnimation)
animationForKey caLayer key =
  sendMessage caLayer animationForKeySelector (toNSString key)

-- | @+ layerWithRemoteClientId:@
layerWithRemoteClientId :: CUInt -> IO (Id CALayer)
layerWithRemoteClientId client_id =
  do
    cls' <- getRequiredClass "CALayer"
    sendClassMessage cls' layerWithRemoteClientIdSelector client_id

-- | @- addConstraint:@
addConstraint :: (IsCALayer caLayer, IsCAConstraint c) => caLayer -> c -> IO ()
addConstraint caLayer c =
  sendMessage caLayer addConstraintSelector (toCAConstraint c)

-- | @- zPosition@
zPosition :: IsCALayer caLayer => caLayer -> IO CDouble
zPosition caLayer =
  sendMessage caLayer zPositionSelector

-- | @- setZPosition:@
setZPosition :: IsCALayer caLayer => caLayer -> CDouble -> IO ()
setZPosition caLayer value =
  sendMessage caLayer setZPositionSelector value

-- | @- anchorPointZ@
anchorPointZ :: IsCALayer caLayer => caLayer -> IO CDouble
anchorPointZ caLayer =
  sendMessage caLayer anchorPointZSelector

-- | @- setAnchorPointZ:@
setAnchorPointZ :: IsCALayer caLayer => caLayer -> CDouble -> IO ()
setAnchorPointZ caLayer value =
  sendMessage caLayer setAnchorPointZSelector value

-- | @- transform@
transform :: IsCALayer caLayer => caLayer -> IO CATransform3D
transform caLayer =
  sendMessage caLayer transformSelector

-- | @- setTransform:@
setTransform :: IsCALayer caLayer => caLayer -> CATransform3D -> IO ()
setTransform caLayer value =
  sendMessage caLayer setTransformSelector value

-- | @- hidden@
hidden :: IsCALayer caLayer => caLayer -> IO Bool
hidden caLayer =
  sendMessage caLayer hiddenSelector

-- | @- setHidden:@
setHidden :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setHidden caLayer value =
  sendMessage caLayer setHiddenSelector value

-- | @- doubleSided@
doubleSided :: IsCALayer caLayer => caLayer -> IO Bool
doubleSided caLayer =
  sendMessage caLayer doubleSidedSelector

-- | @- setDoubleSided:@
setDoubleSided :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setDoubleSided caLayer value =
  sendMessage caLayer setDoubleSidedSelector value

-- | @- geometryFlipped@
geometryFlipped :: IsCALayer caLayer => caLayer -> IO Bool
geometryFlipped caLayer =
  sendMessage caLayer geometryFlippedSelector

-- | @- setGeometryFlipped:@
setGeometryFlipped :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setGeometryFlipped caLayer value =
  sendMessage caLayer setGeometryFlippedSelector value

-- | @- superlayer@
superlayer :: IsCALayer caLayer => caLayer -> IO (Id CALayer)
superlayer caLayer =
  sendMessage caLayer superlayerSelector

-- | @- sublayers@
sublayers :: IsCALayer caLayer => caLayer -> IO (Id NSArray)
sublayers caLayer =
  sendMessage caLayer sublayersSelector

-- | @- setSublayers:@
setSublayers :: (IsCALayer caLayer, IsNSArray value) => caLayer -> value -> IO ()
setSublayers caLayer value =
  sendMessage caLayer setSublayersSelector (toNSArray value)

-- | @- sublayerTransform@
sublayerTransform :: IsCALayer caLayer => caLayer -> IO CATransform3D
sublayerTransform caLayer =
  sendMessage caLayer sublayerTransformSelector

-- | @- setSublayerTransform:@
setSublayerTransform :: IsCALayer caLayer => caLayer -> CATransform3D -> IO ()
setSublayerTransform caLayer value =
  sendMessage caLayer setSublayerTransformSelector value

-- | @- mask@
mask :: IsCALayer caLayer => caLayer -> IO (Id CALayer)
mask caLayer =
  sendMessage caLayer maskSelector

-- | @- setMask:@
setMask :: (IsCALayer caLayer, IsCALayer value) => caLayer -> value -> IO ()
setMask caLayer value =
  sendMessage caLayer setMaskSelector (toCALayer value)

-- | @- masksToBounds@
masksToBounds :: IsCALayer caLayer => caLayer -> IO Bool
masksToBounds caLayer =
  sendMessage caLayer masksToBoundsSelector

-- | @- setMasksToBounds:@
setMasksToBounds :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setMasksToBounds caLayer value =
  sendMessage caLayer setMasksToBoundsSelector value

-- | Layer content properties and methods. *
--
-- ObjC selector: @- contents@
contents :: IsCALayer caLayer => caLayer -> IO RawId
contents caLayer =
  sendMessage caLayer contentsSelector

-- | Layer content properties and methods. *
--
-- ObjC selector: @- setContents:@
setContents :: IsCALayer caLayer => caLayer -> RawId -> IO ()
setContents caLayer value =
  sendMessage caLayer setContentsSelector value

-- | @- contentsGravity@
contentsGravity :: IsCALayer caLayer => caLayer -> IO (Id NSString)
contentsGravity caLayer =
  sendMessage caLayer contentsGravitySelector

-- | @- setContentsGravity:@
setContentsGravity :: (IsCALayer caLayer, IsNSString value) => caLayer -> value -> IO ()
setContentsGravity caLayer value =
  sendMessage caLayer setContentsGravitySelector (toNSString value)

-- | @- contentsScale@
contentsScale :: IsCALayer caLayer => caLayer -> IO CDouble
contentsScale caLayer =
  sendMessage caLayer contentsScaleSelector

-- | @- setContentsScale:@
setContentsScale :: IsCALayer caLayer => caLayer -> CDouble -> IO ()
setContentsScale caLayer value =
  sendMessage caLayer setContentsScaleSelector value

-- | @- contentsFormat@
contentsFormat :: IsCALayer caLayer => caLayer -> IO (Id NSString)
contentsFormat caLayer =
  sendMessage caLayer contentsFormatSelector

-- | @- setContentsFormat:@
setContentsFormat :: (IsCALayer caLayer, IsNSString value) => caLayer -> value -> IO ()
setContentsFormat caLayer value =
  sendMessage caLayer setContentsFormatSelector (toNSString value)

-- | @- wantsExtendedDynamicRangeContent@
wantsExtendedDynamicRangeContent :: IsCALayer caLayer => caLayer -> IO Bool
wantsExtendedDynamicRangeContent caLayer =
  sendMessage caLayer wantsExtendedDynamicRangeContentSelector

-- | @- setWantsExtendedDynamicRangeContent:@
setWantsExtendedDynamicRangeContent :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setWantsExtendedDynamicRangeContent caLayer value =
  sendMessage caLayer setWantsExtendedDynamicRangeContentSelector value

-- | @- toneMapMode@
toneMapMode :: IsCALayer caLayer => caLayer -> IO (Id NSString)
toneMapMode caLayer =
  sendMessage caLayer toneMapModeSelector

-- | @- setToneMapMode:@
setToneMapMode :: (IsCALayer caLayer, IsNSString value) => caLayer -> value -> IO ()
setToneMapMode caLayer value =
  sendMessage caLayer setToneMapModeSelector (toNSString value)

-- | @- preferredDynamicRange@
preferredDynamicRange :: IsCALayer caLayer => caLayer -> IO (Id NSString)
preferredDynamicRange caLayer =
  sendMessage caLayer preferredDynamicRangeSelector

-- | @- setPreferredDynamicRange:@
setPreferredDynamicRange :: (IsCALayer caLayer, IsNSString value) => caLayer -> value -> IO ()
setPreferredDynamicRange caLayer value =
  sendMessage caLayer setPreferredDynamicRangeSelector (toNSString value)

-- | @- contentsHeadroom@
contentsHeadroom :: IsCALayer caLayer => caLayer -> IO CDouble
contentsHeadroom caLayer =
  sendMessage caLayer contentsHeadroomSelector

-- | @- setContentsHeadroom:@
setContentsHeadroom :: IsCALayer caLayer => caLayer -> CDouble -> IO ()
setContentsHeadroom caLayer value =
  sendMessage caLayer setContentsHeadroomSelector value

-- | @- wantsDynamicContentScaling@
wantsDynamicContentScaling :: IsCALayer caLayer => caLayer -> IO Bool
wantsDynamicContentScaling caLayer =
  sendMessage caLayer wantsDynamicContentScalingSelector

-- | @- setWantsDynamicContentScaling:@
setWantsDynamicContentScaling :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setWantsDynamicContentScaling caLayer value =
  sendMessage caLayer setWantsDynamicContentScalingSelector value

-- | @- minificationFilter@
minificationFilter :: IsCALayer caLayer => caLayer -> IO (Id NSString)
minificationFilter caLayer =
  sendMessage caLayer minificationFilterSelector

-- | @- setMinificationFilter:@
setMinificationFilter :: (IsCALayer caLayer, IsNSString value) => caLayer -> value -> IO ()
setMinificationFilter caLayer value =
  sendMessage caLayer setMinificationFilterSelector (toNSString value)

-- | @- magnificationFilter@
magnificationFilter :: IsCALayer caLayer => caLayer -> IO (Id NSString)
magnificationFilter caLayer =
  sendMessage caLayer magnificationFilterSelector

-- | @- setMagnificationFilter:@
setMagnificationFilter :: (IsCALayer caLayer, IsNSString value) => caLayer -> value -> IO ()
setMagnificationFilter caLayer value =
  sendMessage caLayer setMagnificationFilterSelector (toNSString value)

-- | @- minificationFilterBias@
minificationFilterBias :: IsCALayer caLayer => caLayer -> IO CFloat
minificationFilterBias caLayer =
  sendMessage caLayer minificationFilterBiasSelector

-- | @- setMinificationFilterBias:@
setMinificationFilterBias :: IsCALayer caLayer => caLayer -> CFloat -> IO ()
setMinificationFilterBias caLayer value =
  sendMessage caLayer setMinificationFilterBiasSelector value

-- | @- opaque@
opaque :: IsCALayer caLayer => caLayer -> IO Bool
opaque caLayer =
  sendMessage caLayer opaqueSelector

-- | @- setOpaque:@
setOpaque :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setOpaque caLayer value =
  sendMessage caLayer setOpaqueSelector value

-- | @- needsDisplayOnBoundsChange@
needsDisplayOnBoundsChange :: IsCALayer caLayer => caLayer -> IO Bool
needsDisplayOnBoundsChange caLayer =
  sendMessage caLayer needsDisplayOnBoundsChangeSelector

-- | @- setNeedsDisplayOnBoundsChange:@
setNeedsDisplayOnBoundsChange :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setNeedsDisplayOnBoundsChange caLayer value =
  sendMessage caLayer setNeedsDisplayOnBoundsChangeSelector value

-- | @- drawsAsynchronously@
drawsAsynchronously :: IsCALayer caLayer => caLayer -> IO Bool
drawsAsynchronously caLayer =
  sendMessage caLayer drawsAsynchronouslySelector

-- | @- setDrawsAsynchronously:@
setDrawsAsynchronously :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setDrawsAsynchronously caLayer value =
  sendMessage caLayer setDrawsAsynchronouslySelector value

-- | @- edgeAntialiasingMask@
edgeAntialiasingMask :: IsCALayer caLayer => caLayer -> IO CAEdgeAntialiasingMask
edgeAntialiasingMask caLayer =
  sendMessage caLayer edgeAntialiasingMaskSelector

-- | @- setEdgeAntialiasingMask:@
setEdgeAntialiasingMask :: IsCALayer caLayer => caLayer -> CAEdgeAntialiasingMask -> IO ()
setEdgeAntialiasingMask caLayer value =
  sendMessage caLayer setEdgeAntialiasingMaskSelector value

-- | @- allowsEdgeAntialiasing@
allowsEdgeAntialiasing :: IsCALayer caLayer => caLayer -> IO Bool
allowsEdgeAntialiasing caLayer =
  sendMessage caLayer allowsEdgeAntialiasingSelector

-- | @- setAllowsEdgeAntialiasing:@
setAllowsEdgeAntialiasing :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setAllowsEdgeAntialiasing caLayer value =
  sendMessage caLayer setAllowsEdgeAntialiasingSelector value

-- | @- backgroundColor@
backgroundColor :: IsCALayer caLayer => caLayer -> IO (Ptr ())
backgroundColor caLayer =
  sendMessage caLayer backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: IsCALayer caLayer => caLayer -> Ptr () -> IO ()
setBackgroundColor caLayer value =
  sendMessage caLayer setBackgroundColorSelector value

-- | @- cornerRadius@
cornerRadius :: IsCALayer caLayer => caLayer -> IO CDouble
cornerRadius caLayer =
  sendMessage caLayer cornerRadiusSelector

-- | @- setCornerRadius:@
setCornerRadius :: IsCALayer caLayer => caLayer -> CDouble -> IO ()
setCornerRadius caLayer value =
  sendMessage caLayer setCornerRadiusSelector value

-- | @- maskedCorners@
maskedCorners :: IsCALayer caLayer => caLayer -> IO CACornerMask
maskedCorners caLayer =
  sendMessage caLayer maskedCornersSelector

-- | @- setMaskedCorners:@
setMaskedCorners :: IsCALayer caLayer => caLayer -> CACornerMask -> IO ()
setMaskedCorners caLayer value =
  sendMessage caLayer setMaskedCornersSelector value

-- | @- cornerCurve@
cornerCurve :: IsCALayer caLayer => caLayer -> IO (Id NSString)
cornerCurve caLayer =
  sendMessage caLayer cornerCurveSelector

-- | @- setCornerCurve:@
setCornerCurve :: (IsCALayer caLayer, IsNSString value) => caLayer -> value -> IO ()
setCornerCurve caLayer value =
  sendMessage caLayer setCornerCurveSelector (toNSString value)

-- | @- borderWidth@
borderWidth :: IsCALayer caLayer => caLayer -> IO CDouble
borderWidth caLayer =
  sendMessage caLayer borderWidthSelector

-- | @- setBorderWidth:@
setBorderWidth :: IsCALayer caLayer => caLayer -> CDouble -> IO ()
setBorderWidth caLayer value =
  sendMessage caLayer setBorderWidthSelector value

-- | @- borderColor@
borderColor :: IsCALayer caLayer => caLayer -> IO (Ptr ())
borderColor caLayer =
  sendMessage caLayer borderColorSelector

-- | @- setBorderColor:@
setBorderColor :: IsCALayer caLayer => caLayer -> Ptr () -> IO ()
setBorderColor caLayer value =
  sendMessage caLayer setBorderColorSelector value

-- | @- opacity@
opacity :: IsCALayer caLayer => caLayer -> IO CFloat
opacity caLayer =
  sendMessage caLayer opacitySelector

-- | @- setOpacity:@
setOpacity :: IsCALayer caLayer => caLayer -> CFloat -> IO ()
setOpacity caLayer value =
  sendMessage caLayer setOpacitySelector value

-- | @- allowsGroupOpacity@
allowsGroupOpacity :: IsCALayer caLayer => caLayer -> IO Bool
allowsGroupOpacity caLayer =
  sendMessage caLayer allowsGroupOpacitySelector

-- | @- setAllowsGroupOpacity:@
setAllowsGroupOpacity :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setAllowsGroupOpacity caLayer value =
  sendMessage caLayer setAllowsGroupOpacitySelector value

-- | @- compositingFilter@
compositingFilter :: IsCALayer caLayer => caLayer -> IO RawId
compositingFilter caLayer =
  sendMessage caLayer compositingFilterSelector

-- | @- setCompositingFilter:@
setCompositingFilter :: IsCALayer caLayer => caLayer -> RawId -> IO ()
setCompositingFilter caLayer value =
  sendMessage caLayer setCompositingFilterSelector value

-- | @- filters@
filters :: IsCALayer caLayer => caLayer -> IO (Id NSArray)
filters caLayer =
  sendMessage caLayer filtersSelector

-- | @- setFilters:@
setFilters :: (IsCALayer caLayer, IsNSArray value) => caLayer -> value -> IO ()
setFilters caLayer value =
  sendMessage caLayer setFiltersSelector (toNSArray value)

-- | @- backgroundFilters@
backgroundFilters :: IsCALayer caLayer => caLayer -> IO (Id NSArray)
backgroundFilters caLayer =
  sendMessage caLayer backgroundFiltersSelector

-- | @- setBackgroundFilters:@
setBackgroundFilters :: (IsCALayer caLayer, IsNSArray value) => caLayer -> value -> IO ()
setBackgroundFilters caLayer value =
  sendMessage caLayer setBackgroundFiltersSelector (toNSArray value)

-- | @- shouldRasterize@
shouldRasterize :: IsCALayer caLayer => caLayer -> IO Bool
shouldRasterize caLayer =
  sendMessage caLayer shouldRasterizeSelector

-- | @- setShouldRasterize:@
setShouldRasterize :: IsCALayer caLayer => caLayer -> Bool -> IO ()
setShouldRasterize caLayer value =
  sendMessage caLayer setShouldRasterizeSelector value

-- | @- rasterizationScale@
rasterizationScale :: IsCALayer caLayer => caLayer -> IO CDouble
rasterizationScale caLayer =
  sendMessage caLayer rasterizationScaleSelector

-- | @- setRasterizationScale:@
setRasterizationScale :: IsCALayer caLayer => caLayer -> CDouble -> IO ()
setRasterizationScale caLayer value =
  sendMessage caLayer setRasterizationScaleSelector value

-- | Shadow properties. *
--
-- ObjC selector: @- shadowColor@
shadowColor :: IsCALayer caLayer => caLayer -> IO (Ptr ())
shadowColor caLayer =
  sendMessage caLayer shadowColorSelector

-- | Shadow properties. *
--
-- ObjC selector: @- setShadowColor:@
setShadowColor :: IsCALayer caLayer => caLayer -> Ptr () -> IO ()
setShadowColor caLayer value =
  sendMessage caLayer setShadowColorSelector value

-- | @- shadowOpacity@
shadowOpacity :: IsCALayer caLayer => caLayer -> IO CFloat
shadowOpacity caLayer =
  sendMessage caLayer shadowOpacitySelector

-- | @- setShadowOpacity:@
setShadowOpacity :: IsCALayer caLayer => caLayer -> CFloat -> IO ()
setShadowOpacity caLayer value =
  sendMessage caLayer setShadowOpacitySelector value

-- | @- shadowRadius@
shadowRadius :: IsCALayer caLayer => caLayer -> IO CDouble
shadowRadius caLayer =
  sendMessage caLayer shadowRadiusSelector

-- | @- setShadowRadius:@
setShadowRadius :: IsCALayer caLayer => caLayer -> CDouble -> IO ()
setShadowRadius caLayer value =
  sendMessage caLayer setShadowRadiusSelector value

-- | @- shadowPath@
shadowPath :: IsCALayer caLayer => caLayer -> IO RawId
shadowPath caLayer =
  sendMessage caLayer shadowPathSelector

-- | @- setShadowPath:@
setShadowPath :: IsCALayer caLayer => caLayer -> RawId -> IO ()
setShadowPath caLayer value =
  sendMessage caLayer setShadowPathSelector value

-- | Layout methods. *
--
-- ObjC selector: @- autoresizingMask@
autoresizingMask :: IsCALayer caLayer => caLayer -> IO CAAutoresizingMask
autoresizingMask caLayer =
  sendMessage caLayer autoresizingMaskSelector

-- | Layout methods. *
--
-- ObjC selector: @- setAutoresizingMask:@
setAutoresizingMask :: IsCALayer caLayer => caLayer -> CAAutoresizingMask -> IO ()
setAutoresizingMask caLayer value =
  sendMessage caLayer setAutoresizingMaskSelector value

-- | @- layoutManager@
layoutManager :: IsCALayer caLayer => caLayer -> IO RawId
layoutManager caLayer =
  sendMessage caLayer layoutManagerSelector

-- | @- setLayoutManager:@
setLayoutManager :: IsCALayer caLayer => caLayer -> RawId -> IO ()
setLayoutManager caLayer value =
  sendMessage caLayer setLayoutManagerSelector value

-- | @- actions@
actions :: IsCALayer caLayer => caLayer -> IO (Id NSDictionary)
actions caLayer =
  sendMessage caLayer actionsSelector

-- | @- setActions:@
setActions :: (IsCALayer caLayer, IsNSDictionary value) => caLayer -> value -> IO ()
setActions caLayer value =
  sendMessage caLayer setActionsSelector (toNSDictionary value)

-- | Miscellaneous properties. *
--
-- ObjC selector: @- name@
name :: IsCALayer caLayer => caLayer -> IO (Id NSString)
name caLayer =
  sendMessage caLayer nameSelector

-- | Miscellaneous properties. *
--
-- ObjC selector: @- setName:@
setName :: (IsCALayer caLayer, IsNSString value) => caLayer -> value -> IO ()
setName caLayer value =
  sendMessage caLayer setNameSelector (toNSString value)

-- | @- delegate@
delegate :: IsCALayer caLayer => caLayer -> IO RawId
delegate caLayer =
  sendMessage caLayer delegateSelector

-- | @- setDelegate:@
setDelegate :: IsCALayer caLayer => caLayer -> RawId -> IO ()
setDelegate caLayer value =
  sendMessage caLayer setDelegateSelector value

-- | @- style@
style :: IsCALayer caLayer => caLayer -> IO (Id NSDictionary)
style caLayer =
  sendMessage caLayer styleSelector

-- | @- setStyle:@
setStyle :: (IsCALayer caLayer, IsNSDictionary value) => caLayer -> value -> IO ()
setStyle caLayer value =
  sendMessage caLayer setStyleSelector (toNSDictionary value)

-- | @- constraints@
constraints :: IsCALayer caLayer => caLayer -> IO (Id NSArray)
constraints caLayer =
  sendMessage caLayer constraintsSelector

-- | @- setConstraints:@
setConstraints :: (IsCALayer caLayer, IsNSArray value) => caLayer -> value -> IO ()
setConstraints caLayer value =
  sendMessage caLayer setConstraintsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layer@
layerSelector :: Selector '[] (Id CALayer)
layerSelector = mkSelector "layer"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CALayer)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithLayer:@
initWithLayerSelector :: Selector '[RawId] (Id CALayer)
initWithLayerSelector = mkSelector "initWithLayer:"

-- | @Selector@ for @presentationLayer@
presentationLayerSelector :: Selector '[] (Id CALayer)
presentationLayerSelector = mkSelector "presentationLayer"

-- | @Selector@ for @modelLayer@
modelLayerSelector :: Selector '[] (Id CALayer)
modelLayerSelector = mkSelector "modelLayer"

-- | @Selector@ for @defaultValueForKey:@
defaultValueForKeySelector :: Selector '[Id NSString] RawId
defaultValueForKeySelector = mkSelector "defaultValueForKey:"

-- | @Selector@ for @needsDisplayForKey:@
needsDisplayForKeySelector :: Selector '[Id NSString] Bool
needsDisplayForKeySelector = mkSelector "needsDisplayForKey:"

-- | @Selector@ for @shouldArchiveValueForKey:@
shouldArchiveValueForKeySelector :: Selector '[Id NSString] Bool
shouldArchiveValueForKeySelector = mkSelector "shouldArchiveValueForKey:"

-- | @Selector@ for @contentsAreFlipped@
contentsAreFlippedSelector :: Selector '[] Bool
contentsAreFlippedSelector = mkSelector "contentsAreFlipped"

-- | @Selector@ for @removeFromSuperlayer@
removeFromSuperlayerSelector :: Selector '[] ()
removeFromSuperlayerSelector = mkSelector "removeFromSuperlayer"

-- | @Selector@ for @addSublayer:@
addSublayerSelector :: Selector '[Id CALayer] ()
addSublayerSelector = mkSelector "addSublayer:"

-- | @Selector@ for @insertSublayer:atIndex:@
insertSublayer_atIndexSelector :: Selector '[Id CALayer, CUInt] ()
insertSublayer_atIndexSelector = mkSelector "insertSublayer:atIndex:"

-- | @Selector@ for @insertSublayer:below:@
insertSublayer_belowSelector :: Selector '[Id CALayer, Id CALayer] ()
insertSublayer_belowSelector = mkSelector "insertSublayer:below:"

-- | @Selector@ for @insertSublayer:above:@
insertSublayer_aboveSelector :: Selector '[Id CALayer, Id CALayer] ()
insertSublayer_aboveSelector = mkSelector "insertSublayer:above:"

-- | @Selector@ for @replaceSublayer:with:@
replaceSublayer_withSelector :: Selector '[Id CALayer, Id CALayer] ()
replaceSublayer_withSelector = mkSelector "replaceSublayer:with:"

-- | @Selector@ for @convertTime:fromLayer:@
convertTime_fromLayerSelector :: Selector '[CDouble, Id CALayer] CDouble
convertTime_fromLayerSelector = mkSelector "convertTime:fromLayer:"

-- | @Selector@ for @convertTime:toLayer:@
convertTime_toLayerSelector :: Selector '[CDouble, Id CALayer] CDouble
convertTime_toLayerSelector = mkSelector "convertTime:toLayer:"

-- | @Selector@ for @display@
displaySelector :: Selector '[] ()
displaySelector = mkSelector "display"

-- | @Selector@ for @setNeedsDisplay@
setNeedsDisplaySelector :: Selector '[] ()
setNeedsDisplaySelector = mkSelector "setNeedsDisplay"

-- | @Selector@ for @needsDisplay@
needsDisplaySelector :: Selector '[] Bool
needsDisplaySelector = mkSelector "needsDisplay"

-- | @Selector@ for @displayIfNeeded@
displayIfNeededSelector :: Selector '[] ()
displayIfNeededSelector = mkSelector "displayIfNeeded"

-- | @Selector@ for @drawInContext:@
drawInContextSelector :: Selector '[Ptr ()] ()
drawInContextSelector = mkSelector "drawInContext:"

-- | @Selector@ for @renderInContext:@
renderInContextSelector :: Selector '[Ptr ()] ()
renderInContextSelector = mkSelector "renderInContext:"

-- | @Selector@ for @cornerCurveExpansionFactor:@
cornerCurveExpansionFactorSelector :: Selector '[Id NSString] CDouble
cornerCurveExpansionFactorSelector = mkSelector "cornerCurveExpansionFactor:"

-- | @Selector@ for @setNeedsLayout@
setNeedsLayoutSelector :: Selector '[] ()
setNeedsLayoutSelector = mkSelector "setNeedsLayout"

-- | @Selector@ for @needsLayout@
needsLayoutSelector :: Selector '[] Bool
needsLayoutSelector = mkSelector "needsLayout"

-- | @Selector@ for @layoutIfNeeded@
layoutIfNeededSelector :: Selector '[] ()
layoutIfNeededSelector = mkSelector "layoutIfNeeded"

-- | @Selector@ for @layoutSublayers@
layoutSublayersSelector :: Selector '[] ()
layoutSublayersSelector = mkSelector "layoutSublayers"

-- | @Selector@ for @defaultActionForKey:@
defaultActionForKeySelector :: Selector '[Id NSString] RawId
defaultActionForKeySelector = mkSelector "defaultActionForKey:"

-- | @Selector@ for @actionForKey:@
actionForKeySelector :: Selector '[Id NSString] RawId
actionForKeySelector = mkSelector "actionForKey:"

-- | @Selector@ for @addAnimation:forKey:@
addAnimation_forKeySelector :: Selector '[Id CAAnimation, Id NSString] ()
addAnimation_forKeySelector = mkSelector "addAnimation:forKey:"

-- | @Selector@ for @removeAllAnimations@
removeAllAnimationsSelector :: Selector '[] ()
removeAllAnimationsSelector = mkSelector "removeAllAnimations"

-- | @Selector@ for @removeAnimationForKey:@
removeAnimationForKeySelector :: Selector '[Id NSString] ()
removeAnimationForKeySelector = mkSelector "removeAnimationForKey:"

-- | @Selector@ for @animationKeys@
animationKeysSelector :: Selector '[] (Id NSArray)
animationKeysSelector = mkSelector "animationKeys"

-- | @Selector@ for @animationForKey:@
animationForKeySelector :: Selector '[Id NSString] (Id CAAnimation)
animationForKeySelector = mkSelector "animationForKey:"

-- | @Selector@ for @layerWithRemoteClientId:@
layerWithRemoteClientIdSelector :: Selector '[CUInt] (Id CALayer)
layerWithRemoteClientIdSelector = mkSelector "layerWithRemoteClientId:"

-- | @Selector@ for @addConstraint:@
addConstraintSelector :: Selector '[Id CAConstraint] ()
addConstraintSelector = mkSelector "addConstraint:"

-- | @Selector@ for @zPosition@
zPositionSelector :: Selector '[] CDouble
zPositionSelector = mkSelector "zPosition"

-- | @Selector@ for @setZPosition:@
setZPositionSelector :: Selector '[CDouble] ()
setZPositionSelector = mkSelector "setZPosition:"

-- | @Selector@ for @anchorPointZ@
anchorPointZSelector :: Selector '[] CDouble
anchorPointZSelector = mkSelector "anchorPointZ"

-- | @Selector@ for @setAnchorPointZ:@
setAnchorPointZSelector :: Selector '[CDouble] ()
setAnchorPointZSelector = mkSelector "setAnchorPointZ:"

-- | @Selector@ for @transform@
transformSelector :: Selector '[] CATransform3D
transformSelector = mkSelector "transform"

-- | @Selector@ for @setTransform:@
setTransformSelector :: Selector '[CATransform3D] ()
setTransformSelector = mkSelector "setTransform:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector '[] Bool
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector '[Bool] ()
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @doubleSided@
doubleSidedSelector :: Selector '[] Bool
doubleSidedSelector = mkSelector "doubleSided"

-- | @Selector@ for @setDoubleSided:@
setDoubleSidedSelector :: Selector '[Bool] ()
setDoubleSidedSelector = mkSelector "setDoubleSided:"

-- | @Selector@ for @geometryFlipped@
geometryFlippedSelector :: Selector '[] Bool
geometryFlippedSelector = mkSelector "geometryFlipped"

-- | @Selector@ for @setGeometryFlipped:@
setGeometryFlippedSelector :: Selector '[Bool] ()
setGeometryFlippedSelector = mkSelector "setGeometryFlipped:"

-- | @Selector@ for @superlayer@
superlayerSelector :: Selector '[] (Id CALayer)
superlayerSelector = mkSelector "superlayer"

-- | @Selector@ for @sublayers@
sublayersSelector :: Selector '[] (Id NSArray)
sublayersSelector = mkSelector "sublayers"

-- | @Selector@ for @setSublayers:@
setSublayersSelector :: Selector '[Id NSArray] ()
setSublayersSelector = mkSelector "setSublayers:"

-- | @Selector@ for @sublayerTransform@
sublayerTransformSelector :: Selector '[] CATransform3D
sublayerTransformSelector = mkSelector "sublayerTransform"

-- | @Selector@ for @setSublayerTransform:@
setSublayerTransformSelector :: Selector '[CATransform3D] ()
setSublayerTransformSelector = mkSelector "setSublayerTransform:"

-- | @Selector@ for @mask@
maskSelector :: Selector '[] (Id CALayer)
maskSelector = mkSelector "mask"

-- | @Selector@ for @setMask:@
setMaskSelector :: Selector '[Id CALayer] ()
setMaskSelector = mkSelector "setMask:"

-- | @Selector@ for @masksToBounds@
masksToBoundsSelector :: Selector '[] Bool
masksToBoundsSelector = mkSelector "masksToBounds"

-- | @Selector@ for @setMasksToBounds:@
setMasksToBoundsSelector :: Selector '[Bool] ()
setMasksToBoundsSelector = mkSelector "setMasksToBounds:"

-- | @Selector@ for @contents@
contentsSelector :: Selector '[] RawId
contentsSelector = mkSelector "contents"

-- | @Selector@ for @setContents:@
setContentsSelector :: Selector '[RawId] ()
setContentsSelector = mkSelector "setContents:"

-- | @Selector@ for @contentsGravity@
contentsGravitySelector :: Selector '[] (Id NSString)
contentsGravitySelector = mkSelector "contentsGravity"

-- | @Selector@ for @setContentsGravity:@
setContentsGravitySelector :: Selector '[Id NSString] ()
setContentsGravitySelector = mkSelector "setContentsGravity:"

-- | @Selector@ for @contentsScale@
contentsScaleSelector :: Selector '[] CDouble
contentsScaleSelector = mkSelector "contentsScale"

-- | @Selector@ for @setContentsScale:@
setContentsScaleSelector :: Selector '[CDouble] ()
setContentsScaleSelector = mkSelector "setContentsScale:"

-- | @Selector@ for @contentsFormat@
contentsFormatSelector :: Selector '[] (Id NSString)
contentsFormatSelector = mkSelector "contentsFormat"

-- | @Selector@ for @setContentsFormat:@
setContentsFormatSelector :: Selector '[Id NSString] ()
setContentsFormatSelector = mkSelector "setContentsFormat:"

-- | @Selector@ for @wantsExtendedDynamicRangeContent@
wantsExtendedDynamicRangeContentSelector :: Selector '[] Bool
wantsExtendedDynamicRangeContentSelector = mkSelector "wantsExtendedDynamicRangeContent"

-- | @Selector@ for @setWantsExtendedDynamicRangeContent:@
setWantsExtendedDynamicRangeContentSelector :: Selector '[Bool] ()
setWantsExtendedDynamicRangeContentSelector = mkSelector "setWantsExtendedDynamicRangeContent:"

-- | @Selector@ for @toneMapMode@
toneMapModeSelector :: Selector '[] (Id NSString)
toneMapModeSelector = mkSelector "toneMapMode"

-- | @Selector@ for @setToneMapMode:@
setToneMapModeSelector :: Selector '[Id NSString] ()
setToneMapModeSelector = mkSelector "setToneMapMode:"

-- | @Selector@ for @preferredDynamicRange@
preferredDynamicRangeSelector :: Selector '[] (Id NSString)
preferredDynamicRangeSelector = mkSelector "preferredDynamicRange"

-- | @Selector@ for @setPreferredDynamicRange:@
setPreferredDynamicRangeSelector :: Selector '[Id NSString] ()
setPreferredDynamicRangeSelector = mkSelector "setPreferredDynamicRange:"

-- | @Selector@ for @contentsHeadroom@
contentsHeadroomSelector :: Selector '[] CDouble
contentsHeadroomSelector = mkSelector "contentsHeadroom"

-- | @Selector@ for @setContentsHeadroom:@
setContentsHeadroomSelector :: Selector '[CDouble] ()
setContentsHeadroomSelector = mkSelector "setContentsHeadroom:"

-- | @Selector@ for @wantsDynamicContentScaling@
wantsDynamicContentScalingSelector :: Selector '[] Bool
wantsDynamicContentScalingSelector = mkSelector "wantsDynamicContentScaling"

-- | @Selector@ for @setWantsDynamicContentScaling:@
setWantsDynamicContentScalingSelector :: Selector '[Bool] ()
setWantsDynamicContentScalingSelector = mkSelector "setWantsDynamicContentScaling:"

-- | @Selector@ for @minificationFilter@
minificationFilterSelector :: Selector '[] (Id NSString)
minificationFilterSelector = mkSelector "minificationFilter"

-- | @Selector@ for @setMinificationFilter:@
setMinificationFilterSelector :: Selector '[Id NSString] ()
setMinificationFilterSelector = mkSelector "setMinificationFilter:"

-- | @Selector@ for @magnificationFilter@
magnificationFilterSelector :: Selector '[] (Id NSString)
magnificationFilterSelector = mkSelector "magnificationFilter"

-- | @Selector@ for @setMagnificationFilter:@
setMagnificationFilterSelector :: Selector '[Id NSString] ()
setMagnificationFilterSelector = mkSelector "setMagnificationFilter:"

-- | @Selector@ for @minificationFilterBias@
minificationFilterBiasSelector :: Selector '[] CFloat
minificationFilterBiasSelector = mkSelector "minificationFilterBias"

-- | @Selector@ for @setMinificationFilterBias:@
setMinificationFilterBiasSelector :: Selector '[CFloat] ()
setMinificationFilterBiasSelector = mkSelector "setMinificationFilterBias:"

-- | @Selector@ for @opaque@
opaqueSelector :: Selector '[] Bool
opaqueSelector = mkSelector "opaque"

-- | @Selector@ for @setOpaque:@
setOpaqueSelector :: Selector '[Bool] ()
setOpaqueSelector = mkSelector "setOpaque:"

-- | @Selector@ for @needsDisplayOnBoundsChange@
needsDisplayOnBoundsChangeSelector :: Selector '[] Bool
needsDisplayOnBoundsChangeSelector = mkSelector "needsDisplayOnBoundsChange"

-- | @Selector@ for @setNeedsDisplayOnBoundsChange:@
setNeedsDisplayOnBoundsChangeSelector :: Selector '[Bool] ()
setNeedsDisplayOnBoundsChangeSelector = mkSelector "setNeedsDisplayOnBoundsChange:"

-- | @Selector@ for @drawsAsynchronously@
drawsAsynchronouslySelector :: Selector '[] Bool
drawsAsynchronouslySelector = mkSelector "drawsAsynchronously"

-- | @Selector@ for @setDrawsAsynchronously:@
setDrawsAsynchronouslySelector :: Selector '[Bool] ()
setDrawsAsynchronouslySelector = mkSelector "setDrawsAsynchronously:"

-- | @Selector@ for @edgeAntialiasingMask@
edgeAntialiasingMaskSelector :: Selector '[] CAEdgeAntialiasingMask
edgeAntialiasingMaskSelector = mkSelector "edgeAntialiasingMask"

-- | @Selector@ for @setEdgeAntialiasingMask:@
setEdgeAntialiasingMaskSelector :: Selector '[CAEdgeAntialiasingMask] ()
setEdgeAntialiasingMaskSelector = mkSelector "setEdgeAntialiasingMask:"

-- | @Selector@ for @allowsEdgeAntialiasing@
allowsEdgeAntialiasingSelector :: Selector '[] Bool
allowsEdgeAntialiasingSelector = mkSelector "allowsEdgeAntialiasing"

-- | @Selector@ for @setAllowsEdgeAntialiasing:@
setAllowsEdgeAntialiasingSelector :: Selector '[Bool] ()
setAllowsEdgeAntialiasingSelector = mkSelector "setAllowsEdgeAntialiasing:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Ptr ())
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Ptr ()] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @cornerRadius@
cornerRadiusSelector :: Selector '[] CDouble
cornerRadiusSelector = mkSelector "cornerRadius"

-- | @Selector@ for @setCornerRadius:@
setCornerRadiusSelector :: Selector '[CDouble] ()
setCornerRadiusSelector = mkSelector "setCornerRadius:"

-- | @Selector@ for @maskedCorners@
maskedCornersSelector :: Selector '[] CACornerMask
maskedCornersSelector = mkSelector "maskedCorners"

-- | @Selector@ for @setMaskedCorners:@
setMaskedCornersSelector :: Selector '[CACornerMask] ()
setMaskedCornersSelector = mkSelector "setMaskedCorners:"

-- | @Selector@ for @cornerCurve@
cornerCurveSelector :: Selector '[] (Id NSString)
cornerCurveSelector = mkSelector "cornerCurve"

-- | @Selector@ for @setCornerCurve:@
setCornerCurveSelector :: Selector '[Id NSString] ()
setCornerCurveSelector = mkSelector "setCornerCurve:"

-- | @Selector@ for @borderWidth@
borderWidthSelector :: Selector '[] CDouble
borderWidthSelector = mkSelector "borderWidth"

-- | @Selector@ for @setBorderWidth:@
setBorderWidthSelector :: Selector '[CDouble] ()
setBorderWidthSelector = mkSelector "setBorderWidth:"

-- | @Selector@ for @borderColor@
borderColorSelector :: Selector '[] (Ptr ())
borderColorSelector = mkSelector "borderColor"

-- | @Selector@ for @setBorderColor:@
setBorderColorSelector :: Selector '[Ptr ()] ()
setBorderColorSelector = mkSelector "setBorderColor:"

-- | @Selector@ for @opacity@
opacitySelector :: Selector '[] CFloat
opacitySelector = mkSelector "opacity"

-- | @Selector@ for @setOpacity:@
setOpacitySelector :: Selector '[CFloat] ()
setOpacitySelector = mkSelector "setOpacity:"

-- | @Selector@ for @allowsGroupOpacity@
allowsGroupOpacitySelector :: Selector '[] Bool
allowsGroupOpacitySelector = mkSelector "allowsGroupOpacity"

-- | @Selector@ for @setAllowsGroupOpacity:@
setAllowsGroupOpacitySelector :: Selector '[Bool] ()
setAllowsGroupOpacitySelector = mkSelector "setAllowsGroupOpacity:"

-- | @Selector@ for @compositingFilter@
compositingFilterSelector :: Selector '[] RawId
compositingFilterSelector = mkSelector "compositingFilter"

-- | @Selector@ for @setCompositingFilter:@
setCompositingFilterSelector :: Selector '[RawId] ()
setCompositingFilterSelector = mkSelector "setCompositingFilter:"

-- | @Selector@ for @filters@
filtersSelector :: Selector '[] (Id NSArray)
filtersSelector = mkSelector "filters"

-- | @Selector@ for @setFilters:@
setFiltersSelector :: Selector '[Id NSArray] ()
setFiltersSelector = mkSelector "setFilters:"

-- | @Selector@ for @backgroundFilters@
backgroundFiltersSelector :: Selector '[] (Id NSArray)
backgroundFiltersSelector = mkSelector "backgroundFilters"

-- | @Selector@ for @setBackgroundFilters:@
setBackgroundFiltersSelector :: Selector '[Id NSArray] ()
setBackgroundFiltersSelector = mkSelector "setBackgroundFilters:"

-- | @Selector@ for @shouldRasterize@
shouldRasterizeSelector :: Selector '[] Bool
shouldRasterizeSelector = mkSelector "shouldRasterize"

-- | @Selector@ for @setShouldRasterize:@
setShouldRasterizeSelector :: Selector '[Bool] ()
setShouldRasterizeSelector = mkSelector "setShouldRasterize:"

-- | @Selector@ for @rasterizationScale@
rasterizationScaleSelector :: Selector '[] CDouble
rasterizationScaleSelector = mkSelector "rasterizationScale"

-- | @Selector@ for @setRasterizationScale:@
setRasterizationScaleSelector :: Selector '[CDouble] ()
setRasterizationScaleSelector = mkSelector "setRasterizationScale:"

-- | @Selector@ for @shadowColor@
shadowColorSelector :: Selector '[] (Ptr ())
shadowColorSelector = mkSelector "shadowColor"

-- | @Selector@ for @setShadowColor:@
setShadowColorSelector :: Selector '[Ptr ()] ()
setShadowColorSelector = mkSelector "setShadowColor:"

-- | @Selector@ for @shadowOpacity@
shadowOpacitySelector :: Selector '[] CFloat
shadowOpacitySelector = mkSelector "shadowOpacity"

-- | @Selector@ for @setShadowOpacity:@
setShadowOpacitySelector :: Selector '[CFloat] ()
setShadowOpacitySelector = mkSelector "setShadowOpacity:"

-- | @Selector@ for @shadowRadius@
shadowRadiusSelector :: Selector '[] CDouble
shadowRadiusSelector = mkSelector "shadowRadius"

-- | @Selector@ for @setShadowRadius:@
setShadowRadiusSelector :: Selector '[CDouble] ()
setShadowRadiusSelector = mkSelector "setShadowRadius:"

-- | @Selector@ for @shadowPath@
shadowPathSelector :: Selector '[] RawId
shadowPathSelector = mkSelector "shadowPath"

-- | @Selector@ for @setShadowPath:@
setShadowPathSelector :: Selector '[RawId] ()
setShadowPathSelector = mkSelector "setShadowPath:"

-- | @Selector@ for @autoresizingMask@
autoresizingMaskSelector :: Selector '[] CAAutoresizingMask
autoresizingMaskSelector = mkSelector "autoresizingMask"

-- | @Selector@ for @setAutoresizingMask:@
setAutoresizingMaskSelector :: Selector '[CAAutoresizingMask] ()
setAutoresizingMaskSelector = mkSelector "setAutoresizingMask:"

-- | @Selector@ for @layoutManager@
layoutManagerSelector :: Selector '[] RawId
layoutManagerSelector = mkSelector "layoutManager"

-- | @Selector@ for @setLayoutManager:@
setLayoutManagerSelector :: Selector '[RawId] ()
setLayoutManagerSelector = mkSelector "setLayoutManager:"

-- | @Selector@ for @actions@
actionsSelector :: Selector '[] (Id NSDictionary)
actionsSelector = mkSelector "actions"

-- | @Selector@ for @setActions:@
setActionsSelector :: Selector '[Id NSDictionary] ()
setActionsSelector = mkSelector "setActions:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @style@
styleSelector :: Selector '[] (Id NSDictionary)
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector '[Id NSDictionary] ()
setStyleSelector = mkSelector "setStyle:"

-- | @Selector@ for @constraints@
constraintsSelector :: Selector '[] (Id NSArray)
constraintsSelector = mkSelector "constraints"

-- | @Selector@ for @setConstraints:@
setConstraintsSelector :: Selector '[Id NSArray] ()
setConstraintsSelector = mkSelector "setConstraints:"

