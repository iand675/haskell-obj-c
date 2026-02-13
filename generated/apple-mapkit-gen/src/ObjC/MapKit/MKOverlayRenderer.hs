{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKOverlayRenderer@.
module ObjC.MapKit.MKOverlayRenderer
  ( MKOverlayRenderer
  , IsMKOverlayRenderer(..)
  , initWithOverlay
  , setNeedsDisplay
  , overlay
  , alpha
  , setAlpha
  , contentScaleFactor
  , blendMode
  , setBlendMode
  , alphaSelector
  , blendModeSelector
  , contentScaleFactorSelector
  , initWithOverlaySelector
  , overlaySelector
  , setAlphaSelector
  , setBlendModeSelector
  , setNeedsDisplaySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithOverlay:@
initWithOverlay :: IsMKOverlayRenderer mkOverlayRenderer => mkOverlayRenderer -> RawId -> IO (Id MKOverlayRenderer)
initWithOverlay mkOverlayRenderer overlay =
  sendOwnedMessage mkOverlayRenderer initWithOverlaySelector overlay

-- | @- setNeedsDisplay@
setNeedsDisplay :: IsMKOverlayRenderer mkOverlayRenderer => mkOverlayRenderer -> IO ()
setNeedsDisplay mkOverlayRenderer =
  sendMessage mkOverlayRenderer setNeedsDisplaySelector

-- | @- overlay@
overlay :: IsMKOverlayRenderer mkOverlayRenderer => mkOverlayRenderer -> IO RawId
overlay mkOverlayRenderer =
  sendMessage mkOverlayRenderer overlaySelector

-- | @- alpha@
alpha :: IsMKOverlayRenderer mkOverlayRenderer => mkOverlayRenderer -> IO CDouble
alpha mkOverlayRenderer =
  sendMessage mkOverlayRenderer alphaSelector

-- | @- setAlpha:@
setAlpha :: IsMKOverlayRenderer mkOverlayRenderer => mkOverlayRenderer -> CDouble -> IO ()
setAlpha mkOverlayRenderer value =
  sendMessage mkOverlayRenderer setAlphaSelector value

-- | @- contentScaleFactor@
contentScaleFactor :: IsMKOverlayRenderer mkOverlayRenderer => mkOverlayRenderer -> IO CDouble
contentScaleFactor mkOverlayRenderer =
  sendMessage mkOverlayRenderer contentScaleFactorSelector

-- | @- blendMode@
blendMode :: IsMKOverlayRenderer mkOverlayRenderer => mkOverlayRenderer -> IO CInt
blendMode mkOverlayRenderer =
  sendMessage mkOverlayRenderer blendModeSelector

-- | @- setBlendMode:@
setBlendMode :: IsMKOverlayRenderer mkOverlayRenderer => mkOverlayRenderer -> CInt -> IO ()
setBlendMode mkOverlayRenderer value =
  sendMessage mkOverlayRenderer setBlendModeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithOverlay:@
initWithOverlaySelector :: Selector '[RawId] (Id MKOverlayRenderer)
initWithOverlaySelector = mkSelector "initWithOverlay:"

-- | @Selector@ for @setNeedsDisplay@
setNeedsDisplaySelector :: Selector '[] ()
setNeedsDisplaySelector = mkSelector "setNeedsDisplay"

-- | @Selector@ for @overlay@
overlaySelector :: Selector '[] RawId
overlaySelector = mkSelector "overlay"

-- | @Selector@ for @alpha@
alphaSelector :: Selector '[] CDouble
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector '[CDouble] ()
setAlphaSelector = mkSelector "setAlpha:"

-- | @Selector@ for @contentScaleFactor@
contentScaleFactorSelector :: Selector '[] CDouble
contentScaleFactorSelector = mkSelector "contentScaleFactor"

-- | @Selector@ for @blendMode@
blendModeSelector :: Selector '[] CInt
blendModeSelector = mkSelector "blendMode"

-- | @Selector@ for @setBlendMode:@
setBlendModeSelector :: Selector '[CInt] ()
setBlendModeSelector = mkSelector "setBlendMode:"

