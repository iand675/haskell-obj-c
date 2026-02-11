{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.QuartzCore.Internal.Classes (
    module ObjC.QuartzCore.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.SceneKit.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes
import ObjC.SceneKit.Internal.Classes

-- ---------- CAAnimation ----------

-- | The base animation class. *
-- 
-- Phantom type for @CAAnimation@.
data CAAnimation

instance IsObjCObject (Id CAAnimation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CAAnimation"

class IsNSObject a => IsCAAnimation a where
  toCAAnimation :: a -> Id CAAnimation

instance IsCAAnimation (Id CAAnimation) where
  toCAAnimation = unsafeCastId

instance IsNSObject (Id CAAnimation) where
  toNSObject = unsafeCastId

-- ---------- CAConstraint ----------

-- | The class representing a single layout constraint. *
-- 
-- Phantom type for @CAConstraint@.
data CAConstraint

instance IsObjCObject (Id CAConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CAConstraint"

class IsNSObject a => IsCAConstraint a where
  toCAConstraint :: a -> Id CAConstraint

instance IsCAConstraint (Id CAConstraint) where
  toCAConstraint = unsafeCastId

instance IsNSObject (Id CAConstraint) where
  toNSObject = unsafeCastId

-- ---------- CAConstraintLayoutManager ----------

-- | The constraint-based layout manager. *
-- 
-- Phantom type for @CAConstraintLayoutManager@.
data CAConstraintLayoutManager

instance IsObjCObject (Id CAConstraintLayoutManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CAConstraintLayoutManager"

class IsNSObject a => IsCAConstraintLayoutManager a where
  toCAConstraintLayoutManager :: a -> Id CAConstraintLayoutManager

instance IsCAConstraintLayoutManager (Id CAConstraintLayoutManager) where
  toCAConstraintLayoutManager = unsafeCastId

instance IsNSObject (Id CAConstraintLayoutManager) where
  toNSObject = unsafeCastId

-- ---------- CADisplayLink ----------

-- | Class representing a timer bound to the display vsync. *
-- 
-- Phantom type for @CADisplayLink@.
data CADisplayLink

instance IsObjCObject (Id CADisplayLink) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CADisplayLink"

class IsNSObject a => IsCADisplayLink a where
  toCADisplayLink :: a -> Id CADisplayLink

instance IsCADisplayLink (Id CADisplayLink) where
  toCADisplayLink = unsafeCastId

instance IsNSObject (Id CADisplayLink) where
  toNSObject = unsafeCastId

-- ---------- CAEDRMetadata ----------

-- | Phantom type for @CAEDRMetadata@.
data CAEDRMetadata

instance IsObjCObject (Id CAEDRMetadata) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CAEDRMetadata"

class IsNSObject a => IsCAEDRMetadata a where
  toCAEDRMetadata :: a -> Id CAEDRMetadata

instance IsCAEDRMetadata (Id CAEDRMetadata) where
  toCAEDRMetadata = unsafeCastId

instance IsNSObject (Id CAEDRMetadata) where
  toNSObject = unsafeCastId

-- ---------- CAEmitterCell ----------

-- | Phantom type for @CAEmitterCell@.
data CAEmitterCell

instance IsObjCObject (Id CAEmitterCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CAEmitterCell"

class IsNSObject a => IsCAEmitterCell a where
  toCAEmitterCell :: a -> Id CAEmitterCell

instance IsCAEmitterCell (Id CAEmitterCell) where
  toCAEmitterCell = unsafeCastId

instance IsNSObject (Id CAEmitterCell) where
  toNSObject = unsafeCastId

-- ---------- CALayer ----------

-- | The base layer class. *
-- 
-- Phantom type for @CALayer@.
data CALayer

instance IsObjCObject (Id CALayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CALayer"

class IsNSObject a => IsCALayer a where
  toCALayer :: a -> Id CALayer

instance IsCALayer (Id CALayer) where
  toCALayer = unsafeCastId

instance IsNSObject (Id CALayer) where
  toNSObject = unsafeCastId

-- ---------- CAMediaTimingFunction ----------

-- | Phantom type for @CAMediaTimingFunction@.
data CAMediaTimingFunction

instance IsObjCObject (Id CAMediaTimingFunction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CAMediaTimingFunction"

class IsNSObject a => IsCAMediaTimingFunction a where
  toCAMediaTimingFunction :: a -> Id CAMediaTimingFunction

instance IsCAMediaTimingFunction (Id CAMediaTimingFunction) where
  toCAMediaTimingFunction = unsafeCastId

instance IsNSObject (Id CAMediaTimingFunction) where
  toNSObject = unsafeCastId

-- ---------- CAMetalDisplayLink ----------

-- | Phantom type for @CAMetalDisplayLink@.
data CAMetalDisplayLink

instance IsObjCObject (Id CAMetalDisplayLink) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CAMetalDisplayLink"

class IsNSObject a => IsCAMetalDisplayLink a where
  toCAMetalDisplayLink :: a -> Id CAMetalDisplayLink

instance IsCAMetalDisplayLink (Id CAMetalDisplayLink) where
  toCAMetalDisplayLink = unsafeCastId

instance IsNSObject (Id CAMetalDisplayLink) where
  toNSObject = unsafeCastId

-- ---------- CAMetalDisplayLinkUpdate ----------

-- | Phantom type for @CAMetalDisplayLinkUpdate@.
data CAMetalDisplayLinkUpdate

instance IsObjCObject (Id CAMetalDisplayLinkUpdate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CAMetalDisplayLinkUpdate"

class IsNSObject a => IsCAMetalDisplayLinkUpdate a where
  toCAMetalDisplayLinkUpdate :: a -> Id CAMetalDisplayLinkUpdate

instance IsCAMetalDisplayLinkUpdate (Id CAMetalDisplayLinkUpdate) where
  toCAMetalDisplayLinkUpdate = unsafeCastId

instance IsNSObject (Id CAMetalDisplayLinkUpdate) where
  toNSObject = unsafeCastId

-- ---------- CARemoteLayerClient ----------

-- | Phantom type for @CARemoteLayerClient@.
data CARemoteLayerClient

instance IsObjCObject (Id CARemoteLayerClient) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CARemoteLayerClient"

class IsNSObject a => IsCARemoteLayerClient a where
  toCARemoteLayerClient :: a -> Id CARemoteLayerClient

instance IsCARemoteLayerClient (Id CARemoteLayerClient) where
  toCARemoteLayerClient = unsafeCastId

instance IsNSObject (Id CARemoteLayerClient) where
  toNSObject = unsafeCastId

-- ---------- CARemoteLayerServer ----------

-- | Phantom type for @CARemoteLayerServer@.
data CARemoteLayerServer

instance IsObjCObject (Id CARemoteLayerServer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CARemoteLayerServer"

class IsNSObject a => IsCARemoteLayerServer a where
  toCARemoteLayerServer :: a -> Id CARemoteLayerServer

instance IsCARemoteLayerServer (Id CARemoteLayerServer) where
  toCARemoteLayerServer = unsafeCastId

instance IsNSObject (Id CARemoteLayerServer) where
  toNSObject = unsafeCastId

-- ---------- CARenderer ----------

-- | Phantom type for @CARenderer@.
data CARenderer

instance IsObjCObject (Id CARenderer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CARenderer"

class IsNSObject a => IsCARenderer a where
  toCARenderer :: a -> Id CARenderer

instance IsCARenderer (Id CARenderer) where
  toCARenderer = unsafeCastId

instance IsNSObject (Id CARenderer) where
  toNSObject = unsafeCastId

-- ---------- CATransaction ----------

-- | Phantom type for @CATransaction@.
data CATransaction

instance IsObjCObject (Id CATransaction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CATransaction"

class IsNSObject a => IsCATransaction a where
  toCATransaction :: a -> Id CATransaction

instance IsCATransaction (Id CATransaction) where
  toCATransaction = unsafeCastId

instance IsNSObject (Id CATransaction) where
  toNSObject = unsafeCastId

-- ---------- CAValueFunction ----------

-- | Phantom type for @CAValueFunction@.
data CAValueFunction

instance IsObjCObject (Id CAValueFunction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CAValueFunction"

class IsNSObject a => IsCAValueFunction a where
  toCAValueFunction :: a -> Id CAValueFunction

instance IsCAValueFunction (Id CAValueFunction) where
  toCAValueFunction = unsafeCastId

instance IsNSObject (Id CAValueFunction) where
  toNSObject = unsafeCastId

-- ---------- CAAnimationGroup ----------

-- | Animation subclass for grouped animations. *
-- 
-- Phantom type for @CAAnimationGroup@.
data CAAnimationGroup

instance IsObjCObject (Id CAAnimationGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CAAnimationGroup"

class IsCAAnimation a => IsCAAnimationGroup a where
  toCAAnimationGroup :: a -> Id CAAnimationGroup

instance IsCAAnimationGroup (Id CAAnimationGroup) where
  toCAAnimationGroup = unsafeCastId

instance IsCAAnimation (Id CAAnimationGroup) where
  toCAAnimation = unsafeCastId

instance IsNSObject (Id CAAnimationGroup) where
  toNSObject = unsafeCastId

-- ---------- CAPropertyAnimation ----------

-- | Subclass for property-based animations. *
-- 
-- Phantom type for @CAPropertyAnimation@.
data CAPropertyAnimation

instance IsObjCObject (Id CAPropertyAnimation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CAPropertyAnimation"

class IsCAAnimation a => IsCAPropertyAnimation a where
  toCAPropertyAnimation :: a -> Id CAPropertyAnimation

instance IsCAPropertyAnimation (Id CAPropertyAnimation) where
  toCAPropertyAnimation = unsafeCastId

instance IsCAAnimation (Id CAPropertyAnimation) where
  toCAAnimation = unsafeCastId

instance IsNSObject (Id CAPropertyAnimation) where
  toNSObject = unsafeCastId

-- ---------- CATransition ----------

-- | Transition animation subclass. *
-- 
-- Phantom type for @CATransition@.
data CATransition

instance IsObjCObject (Id CATransition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CATransition"

class IsCAAnimation a => IsCATransition a where
  toCATransition :: a -> Id CATransition

instance IsCATransition (Id CATransition) where
  toCATransition = unsafeCastId

instance IsCAAnimation (Id CATransition) where
  toCAAnimation = unsafeCastId

instance IsNSObject (Id CATransition) where
  toNSObject = unsafeCastId

-- ---------- CAEmitterLayer ----------

-- | Phantom type for @CAEmitterLayer@.
data CAEmitterLayer

instance IsObjCObject (Id CAEmitterLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CAEmitterLayer"

class IsCALayer a => IsCAEmitterLayer a where
  toCAEmitterLayer :: a -> Id CAEmitterLayer

instance IsCAEmitterLayer (Id CAEmitterLayer) where
  toCAEmitterLayer = unsafeCastId

instance IsCALayer (Id CAEmitterLayer) where
  toCALayer = unsafeCastId

instance IsNSObject (Id CAEmitterLayer) where
  toNSObject = unsafeCastId

-- ---------- CAGradientLayer ----------

-- | Phantom type for @CAGradientLayer@.
data CAGradientLayer

instance IsObjCObject (Id CAGradientLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CAGradientLayer"

class IsCALayer a => IsCAGradientLayer a where
  toCAGradientLayer :: a -> Id CAGradientLayer

instance IsCAGradientLayer (Id CAGradientLayer) where
  toCAGradientLayer = unsafeCastId

instance IsCALayer (Id CAGradientLayer) where
  toCALayer = unsafeCastId

instance IsNSObject (Id CAGradientLayer) where
  toNSObject = unsafeCastId

-- ---------- CAMetalLayer ----------

-- | Phantom type for @CAMetalLayer@.
data CAMetalLayer

instance IsObjCObject (Id CAMetalLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CAMetalLayer"

class IsCALayer a => IsCAMetalLayer a where
  toCAMetalLayer :: a -> Id CAMetalLayer

instance IsCAMetalLayer (Id CAMetalLayer) where
  toCAMetalLayer = unsafeCastId

instance IsCALayer (Id CAMetalLayer) where
  toCALayer = unsafeCastId

instance IsNSObject (Id CAMetalLayer) where
  toNSObject = unsafeCastId

-- ---------- CAOpenGLLayer ----------

-- | Phantom type for @CAOpenGLLayer@.
data CAOpenGLLayer

instance IsObjCObject (Id CAOpenGLLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CAOpenGLLayer"

class IsCALayer a => IsCAOpenGLLayer a where
  toCAOpenGLLayer :: a -> Id CAOpenGLLayer

instance IsCAOpenGLLayer (Id CAOpenGLLayer) where
  toCAOpenGLLayer = unsafeCastId

instance IsCALayer (Id CAOpenGLLayer) where
  toCALayer = unsafeCastId

instance IsNSObject (Id CAOpenGLLayer) where
  toNSObject = unsafeCastId

-- ---------- CAReplicatorLayer ----------

-- | Phantom type for @CAReplicatorLayer@.
data CAReplicatorLayer

instance IsObjCObject (Id CAReplicatorLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CAReplicatorLayer"

class IsCALayer a => IsCAReplicatorLayer a where
  toCAReplicatorLayer :: a -> Id CAReplicatorLayer

instance IsCAReplicatorLayer (Id CAReplicatorLayer) where
  toCAReplicatorLayer = unsafeCastId

instance IsCALayer (Id CAReplicatorLayer) where
  toCALayer = unsafeCastId

instance IsNSObject (Id CAReplicatorLayer) where
  toNSObject = unsafeCastId

-- ---------- CAScrollLayer ----------

-- | Phantom type for @CAScrollLayer@.
data CAScrollLayer

instance IsObjCObject (Id CAScrollLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CAScrollLayer"

class IsCALayer a => IsCAScrollLayer a where
  toCAScrollLayer :: a -> Id CAScrollLayer

instance IsCAScrollLayer (Id CAScrollLayer) where
  toCAScrollLayer = unsafeCastId

instance IsCALayer (Id CAScrollLayer) where
  toCALayer = unsafeCastId

instance IsNSObject (Id CAScrollLayer) where
  toNSObject = unsafeCastId

-- ---------- CAShapeLayer ----------

-- | Phantom type for @CAShapeLayer@.
data CAShapeLayer

instance IsObjCObject (Id CAShapeLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CAShapeLayer"

class IsCALayer a => IsCAShapeLayer a where
  toCAShapeLayer :: a -> Id CAShapeLayer

instance IsCAShapeLayer (Id CAShapeLayer) where
  toCAShapeLayer = unsafeCastId

instance IsCALayer (Id CAShapeLayer) where
  toCALayer = unsafeCastId

instance IsNSObject (Id CAShapeLayer) where
  toNSObject = unsafeCastId

-- ---------- CATextLayer ----------

-- | Phantom type for @CATextLayer@.
data CATextLayer

instance IsObjCObject (Id CATextLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CATextLayer"

class IsCALayer a => IsCATextLayer a where
  toCATextLayer :: a -> Id CATextLayer

instance IsCATextLayer (Id CATextLayer) where
  toCATextLayer = unsafeCastId

instance IsCALayer (Id CATextLayer) where
  toCALayer = unsafeCastId

instance IsNSObject (Id CATextLayer) where
  toNSObject = unsafeCastId

-- ---------- CATiledLayer ----------

-- | Phantom type for @CATiledLayer@.
data CATiledLayer

instance IsObjCObject (Id CATiledLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CATiledLayer"

class IsCALayer a => IsCATiledLayer a where
  toCATiledLayer :: a -> Id CATiledLayer

instance IsCATiledLayer (Id CATiledLayer) where
  toCATiledLayer = unsafeCastId

instance IsCALayer (Id CATiledLayer) where
  toCALayer = unsafeCastId

instance IsNSObject (Id CATiledLayer) where
  toNSObject = unsafeCastId

-- ---------- CATransformLayer ----------

-- | Phantom type for @CATransformLayer@.
data CATransformLayer

instance IsObjCObject (Id CATransformLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CATransformLayer"

class IsCALayer a => IsCATransformLayer a where
  toCATransformLayer :: a -> Id CATransformLayer

instance IsCATransformLayer (Id CATransformLayer) where
  toCATransformLayer = unsafeCastId

instance IsCALayer (Id CATransformLayer) where
  toCALayer = unsafeCastId

instance IsNSObject (Id CATransformLayer) where
  toNSObject = unsafeCastId

-- ---------- CABasicAnimation ----------

-- | Subclass for basic (single-keyframe) animations. *
-- 
-- Phantom type for @CABasicAnimation@.
data CABasicAnimation

instance IsObjCObject (Id CABasicAnimation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CABasicAnimation"

class IsCAPropertyAnimation a => IsCABasicAnimation a where
  toCABasicAnimation :: a -> Id CABasicAnimation

instance IsCABasicAnimation (Id CABasicAnimation) where
  toCABasicAnimation = unsafeCastId

instance IsCAAnimation (Id CABasicAnimation) where
  toCAAnimation = unsafeCastId

instance IsCAPropertyAnimation (Id CABasicAnimation) where
  toCAPropertyAnimation = unsafeCastId

instance IsNSObject (Id CABasicAnimation) where
  toNSObject = unsafeCastId

-- ---------- CAKeyframeAnimation ----------

-- | General keyframe animation class. *
-- 
-- Phantom type for @CAKeyframeAnimation@.
data CAKeyframeAnimation

instance IsObjCObject (Id CAKeyframeAnimation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CAKeyframeAnimation"

class IsCAPropertyAnimation a => IsCAKeyframeAnimation a where
  toCAKeyframeAnimation :: a -> Id CAKeyframeAnimation

instance IsCAKeyframeAnimation (Id CAKeyframeAnimation) where
  toCAKeyframeAnimation = unsafeCastId

instance IsCAAnimation (Id CAKeyframeAnimation) where
  toCAAnimation = unsafeCastId

instance IsCAPropertyAnimation (Id CAKeyframeAnimation) where
  toCAPropertyAnimation = unsafeCastId

instance IsNSObject (Id CAKeyframeAnimation) where
  toNSObject = unsafeCastId

-- ---------- CASpringAnimation ----------

-- | Subclass for mass-spring animations.
-- 
-- Phantom type for @CASpringAnimation@.
data CASpringAnimation

instance IsObjCObject (Id CASpringAnimation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CASpringAnimation"

class IsCABasicAnimation a => IsCASpringAnimation a where
  toCASpringAnimation :: a -> Id CASpringAnimation

instance IsCASpringAnimation (Id CASpringAnimation) where
  toCASpringAnimation = unsafeCastId

instance IsCAAnimation (Id CASpringAnimation) where
  toCAAnimation = unsafeCastId

instance IsCABasicAnimation (Id CASpringAnimation) where
  toCABasicAnimation = unsafeCastId

instance IsCAPropertyAnimation (Id CASpringAnimation) where
  toCAPropertyAnimation = unsafeCastId

instance IsNSObject (Id CASpringAnimation) where
  toNSObject = unsafeCastId
