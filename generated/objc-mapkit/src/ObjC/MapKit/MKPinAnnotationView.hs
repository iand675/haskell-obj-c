{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKPinAnnotationView@.
module ObjC.MapKit.MKPinAnnotationView
  ( MKPinAnnotationView
  , IsMKPinAnnotationView(..)
  , redPinColor
  , greenPinColor
  , purplePinColor
  , animatesDrop
  , setAnimatesDrop
  , pinColor
  , setPinColor
  , redPinColorSelector
  , greenPinColorSelector
  , purplePinColorSelector
  , animatesDropSelector
  , setAnimatesDropSelector
  , pinColorSelector
  , setPinColorSelector

  -- * Enum types
  , MKPinAnnotationColor(MKPinAnnotationColor)
  , pattern MKPinAnnotationColorRed
  , pattern MKPinAnnotationColorGreen
  , pattern MKPinAnnotationColorPurple

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

import ObjC.MapKit.Internal.Classes
import ObjC.MapKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ redPinColor@
redPinColor :: IO (Id NSColor)
redPinColor  =
  do
    cls' <- getRequiredClass "MKPinAnnotationView"
    sendClassMsg cls' (mkSelector "redPinColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ greenPinColor@
greenPinColor :: IO (Id NSColor)
greenPinColor  =
  do
    cls' <- getRequiredClass "MKPinAnnotationView"
    sendClassMsg cls' (mkSelector "greenPinColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ purplePinColor@
purplePinColor :: IO (Id NSColor)
purplePinColor  =
  do
    cls' <- getRequiredClass "MKPinAnnotationView"
    sendClassMsg cls' (mkSelector "purplePinColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- animatesDrop@
animatesDrop :: IsMKPinAnnotationView mkPinAnnotationView => mkPinAnnotationView -> IO Bool
animatesDrop mkPinAnnotationView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkPinAnnotationView (mkSelector "animatesDrop") retCULong []

-- | @- setAnimatesDrop:@
setAnimatesDrop :: IsMKPinAnnotationView mkPinAnnotationView => mkPinAnnotationView -> Bool -> IO ()
setAnimatesDrop mkPinAnnotationView  value =
  sendMsg mkPinAnnotationView (mkSelector "setAnimatesDrop:") retVoid [argCULong (if value then 1 else 0)]

-- | @- pinColor@
pinColor :: IsMKPinAnnotationView mkPinAnnotationView => mkPinAnnotationView -> IO MKPinAnnotationColor
pinColor mkPinAnnotationView  =
  fmap (coerce :: CULong -> MKPinAnnotationColor) $ sendMsg mkPinAnnotationView (mkSelector "pinColor") retCULong []

-- | @- setPinColor:@
setPinColor :: IsMKPinAnnotationView mkPinAnnotationView => mkPinAnnotationView -> MKPinAnnotationColor -> IO ()
setPinColor mkPinAnnotationView  value =
  sendMsg mkPinAnnotationView (mkSelector "setPinColor:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @redPinColor@
redPinColorSelector :: Selector
redPinColorSelector = mkSelector "redPinColor"

-- | @Selector@ for @greenPinColor@
greenPinColorSelector :: Selector
greenPinColorSelector = mkSelector "greenPinColor"

-- | @Selector@ for @purplePinColor@
purplePinColorSelector :: Selector
purplePinColorSelector = mkSelector "purplePinColor"

-- | @Selector@ for @animatesDrop@
animatesDropSelector :: Selector
animatesDropSelector = mkSelector "animatesDrop"

-- | @Selector@ for @setAnimatesDrop:@
setAnimatesDropSelector :: Selector
setAnimatesDropSelector = mkSelector "setAnimatesDrop:"

-- | @Selector@ for @pinColor@
pinColorSelector :: Selector
pinColorSelector = mkSelector "pinColor"

-- | @Selector@ for @setPinColor:@
setPinColorSelector :: Selector
setPinColorSelector = mkSelector "setPinColor:"

