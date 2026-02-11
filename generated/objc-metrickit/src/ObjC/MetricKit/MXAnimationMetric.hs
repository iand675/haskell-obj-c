{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXAnimationMetric
--
-- An MXMetric subclass that encapsulates app animation metrics.
--
-- Generated bindings for @MXAnimationMetric@.
module ObjC.MetricKit.MXAnimationMetric
  ( MXAnimationMetric
  , IsMXAnimationMetric(..)
  , scrollHitchTimeRatio
  , hitchTimeRatio
  , scrollHitchTimeRatioSelector
  , hitchTimeRatioSelector


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

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | scrollHitchTimeRatio
--
-- Ratio of time the application spent hitching while scrolling.
--
-- Scroll hitches are user perceptible animation issues that occur during scrolling.
--
-- This metric only applies to UIScrollViews.
--
-- Dimensionless.
--
-- ObjC selector: @- scrollHitchTimeRatio@
scrollHitchTimeRatio :: IsMXAnimationMetric mxAnimationMetric => mxAnimationMetric -> IO (Id NSMeasurement)
scrollHitchTimeRatio mxAnimationMetric  =
  sendMsg mxAnimationMetric (mkSelector "scrollHitchTimeRatio") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | hitchTimeRatio
--
-- Ratio of time the application spent hitching during tracked animations.
--
-- Hitches are user perceptible frame delays that can occur during animations and scrolling.
--
-- This metric incorporates adjustments that optimize for user perception, and typically will be the most accurate representation of what hitches users experience during app usage.
--
-- This metric is normalized against total animation duration.
--
-- Many animations are tracked by default. You can track additional animations using the -[NSProcessInfo beginActivityWithOptions:reason:] method with the NSActivityAnimationTrackingEnabled option.
--
-- Dimensionless.
--
-- ObjC selector: @- hitchTimeRatio@
hitchTimeRatio :: IsMXAnimationMetric mxAnimationMetric => mxAnimationMetric -> IO (Id NSMeasurement)
hitchTimeRatio mxAnimationMetric  =
  sendMsg mxAnimationMetric (mkSelector "hitchTimeRatio") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scrollHitchTimeRatio@
scrollHitchTimeRatioSelector :: Selector
scrollHitchTimeRatioSelector = mkSelector "scrollHitchTimeRatio"

-- | @Selector@ for @hitchTimeRatio@
hitchTimeRatioSelector :: Selector
hitchTimeRatioSelector = mkSelector "hitchTimeRatio"

