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
  , initWithOverlaySelector
  , setNeedsDisplaySelector
  , overlaySelector
  , alphaSelector
  , setAlphaSelector
  , contentScaleFactorSelector
  , blendModeSelector
  , setBlendModeSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithOverlay:@
initWithOverlay :: IsMKOverlayRenderer mkOverlayRenderer => mkOverlayRenderer -> RawId -> IO (Id MKOverlayRenderer)
initWithOverlay mkOverlayRenderer  overlay =
    sendMsg mkOverlayRenderer (mkSelector "initWithOverlay:") (retPtr retVoid) [argPtr (castPtr (unRawId overlay) :: Ptr ())] >>= ownedObject . castPtr

-- | @- setNeedsDisplay@
setNeedsDisplay :: IsMKOverlayRenderer mkOverlayRenderer => mkOverlayRenderer -> IO ()
setNeedsDisplay mkOverlayRenderer  =
    sendMsg mkOverlayRenderer (mkSelector "setNeedsDisplay") retVoid []

-- | @- overlay@
overlay :: IsMKOverlayRenderer mkOverlayRenderer => mkOverlayRenderer -> IO RawId
overlay mkOverlayRenderer  =
    fmap (RawId . castPtr) $ sendMsg mkOverlayRenderer (mkSelector "overlay") (retPtr retVoid) []

-- | @- alpha@
alpha :: IsMKOverlayRenderer mkOverlayRenderer => mkOverlayRenderer -> IO CDouble
alpha mkOverlayRenderer  =
    sendMsg mkOverlayRenderer (mkSelector "alpha") retCDouble []

-- | @- setAlpha:@
setAlpha :: IsMKOverlayRenderer mkOverlayRenderer => mkOverlayRenderer -> CDouble -> IO ()
setAlpha mkOverlayRenderer  value =
    sendMsg mkOverlayRenderer (mkSelector "setAlpha:") retVoid [argCDouble value]

-- | @- contentScaleFactor@
contentScaleFactor :: IsMKOverlayRenderer mkOverlayRenderer => mkOverlayRenderer -> IO CDouble
contentScaleFactor mkOverlayRenderer  =
    sendMsg mkOverlayRenderer (mkSelector "contentScaleFactor") retCDouble []

-- | @- blendMode@
blendMode :: IsMKOverlayRenderer mkOverlayRenderer => mkOverlayRenderer -> IO CInt
blendMode mkOverlayRenderer  =
    sendMsg mkOverlayRenderer (mkSelector "blendMode") retCInt []

-- | @- setBlendMode:@
setBlendMode :: IsMKOverlayRenderer mkOverlayRenderer => mkOverlayRenderer -> CInt -> IO ()
setBlendMode mkOverlayRenderer  value =
    sendMsg mkOverlayRenderer (mkSelector "setBlendMode:") retVoid [argCInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithOverlay:@
initWithOverlaySelector :: Selector
initWithOverlaySelector = mkSelector "initWithOverlay:"

-- | @Selector@ for @setNeedsDisplay@
setNeedsDisplaySelector :: Selector
setNeedsDisplaySelector = mkSelector "setNeedsDisplay"

-- | @Selector@ for @overlay@
overlaySelector :: Selector
overlaySelector = mkSelector "overlay"

-- | @Selector@ for @alpha@
alphaSelector :: Selector
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector
setAlphaSelector = mkSelector "setAlpha:"

-- | @Selector@ for @contentScaleFactor@
contentScaleFactorSelector :: Selector
contentScaleFactorSelector = mkSelector "contentScaleFactor"

-- | @Selector@ for @blendMode@
blendModeSelector :: Selector
blendModeSelector = mkSelector "blendMode"

-- | @Selector@ for @setBlendMode:@
setBlendModeSelector :: Selector
setBlendModeSelector = mkSelector "setBlendMode:"

