{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , pinTintColor
  , setPinTintColor
  , animatesDrop
  , setAnimatesDrop
  , pinColor
  , setPinColor
  , animatesDropSelector
  , greenPinColorSelector
  , pinColorSelector
  , pinTintColorSelector
  , purplePinColorSelector
  , redPinColorSelector
  , setAnimatesDropSelector
  , setPinColorSelector
  , setPinTintColorSelector

  -- * Enum types
  , MKPinAnnotationColor(MKPinAnnotationColor)
  , pattern MKPinAnnotationColorRed
  , pattern MKPinAnnotationColorGreen
  , pattern MKPinAnnotationColorPurple

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

-- | @+ redPinColor@
redPinColor :: IO (Id NSColor)
redPinColor  =
  do
    cls' <- getRequiredClass "MKPinAnnotationView"
    sendClassMessage cls' redPinColorSelector

-- | @+ greenPinColor@
greenPinColor :: IO (Id NSColor)
greenPinColor  =
  do
    cls' <- getRequiredClass "MKPinAnnotationView"
    sendClassMessage cls' greenPinColorSelector

-- | @+ purplePinColor@
purplePinColor :: IO (Id NSColor)
purplePinColor  =
  do
    cls' <- getRequiredClass "MKPinAnnotationView"
    sendClassMessage cls' purplePinColorSelector

-- | @- pinTintColor@
pinTintColor :: IsMKPinAnnotationView mkPinAnnotationView => mkPinAnnotationView -> IO RawId
pinTintColor mkPinAnnotationView =
  sendMessage mkPinAnnotationView pinTintColorSelector

-- | @- setPinTintColor:@
setPinTintColor :: IsMKPinAnnotationView mkPinAnnotationView => mkPinAnnotationView -> RawId -> IO ()
setPinTintColor mkPinAnnotationView value =
  sendMessage mkPinAnnotationView setPinTintColorSelector value

-- | @- animatesDrop@
animatesDrop :: IsMKPinAnnotationView mkPinAnnotationView => mkPinAnnotationView -> IO Bool
animatesDrop mkPinAnnotationView =
  sendMessage mkPinAnnotationView animatesDropSelector

-- | @- setAnimatesDrop:@
setAnimatesDrop :: IsMKPinAnnotationView mkPinAnnotationView => mkPinAnnotationView -> Bool -> IO ()
setAnimatesDrop mkPinAnnotationView value =
  sendMessage mkPinAnnotationView setAnimatesDropSelector value

-- | @- pinColor@
pinColor :: IsMKPinAnnotationView mkPinAnnotationView => mkPinAnnotationView -> IO MKPinAnnotationColor
pinColor mkPinAnnotationView =
  sendMessage mkPinAnnotationView pinColorSelector

-- | @- setPinColor:@
setPinColor :: IsMKPinAnnotationView mkPinAnnotationView => mkPinAnnotationView -> MKPinAnnotationColor -> IO ()
setPinColor mkPinAnnotationView value =
  sendMessage mkPinAnnotationView setPinColorSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @redPinColor@
redPinColorSelector :: Selector '[] (Id NSColor)
redPinColorSelector = mkSelector "redPinColor"

-- | @Selector@ for @greenPinColor@
greenPinColorSelector :: Selector '[] (Id NSColor)
greenPinColorSelector = mkSelector "greenPinColor"

-- | @Selector@ for @purplePinColor@
purplePinColorSelector :: Selector '[] (Id NSColor)
purplePinColorSelector = mkSelector "purplePinColor"

-- | @Selector@ for @pinTintColor@
pinTintColorSelector :: Selector '[] RawId
pinTintColorSelector = mkSelector "pinTintColor"

-- | @Selector@ for @setPinTintColor:@
setPinTintColorSelector :: Selector '[RawId] ()
setPinTintColorSelector = mkSelector "setPinTintColor:"

-- | @Selector@ for @animatesDrop@
animatesDropSelector :: Selector '[] Bool
animatesDropSelector = mkSelector "animatesDrop"

-- | @Selector@ for @setAnimatesDrop:@
setAnimatesDropSelector :: Selector '[Bool] ()
setAnimatesDropSelector = mkSelector "setAnimatesDrop:"

-- | @Selector@ for @pinColor@
pinColorSelector :: Selector '[] MKPinAnnotationColor
pinColorSelector = mkSelector "pinColor"

-- | @Selector@ for @setPinColor:@
setPinColorSelector :: Selector '[MKPinAnnotationColor] ()
setPinColorSelector = mkSelector "setPinColor:"

