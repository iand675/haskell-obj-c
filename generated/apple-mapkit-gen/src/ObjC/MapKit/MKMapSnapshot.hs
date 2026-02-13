{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMapSnapshot@.
module ObjC.MapKit.MKMapSnapshot
  ( MKMapSnapshot
  , IsMKMapSnapshot(..)
  , image
  , appearance
  , appearanceSelector
  , imageSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- image@
image :: IsMKMapSnapshot mkMapSnapshot => mkMapSnapshot -> IO (Id NSImage)
image mkMapSnapshot =
  sendMessage mkMapSnapshot imageSelector

-- | @- appearance@
appearance :: IsMKMapSnapshot mkMapSnapshot => mkMapSnapshot -> IO (Id NSAppearance)
appearance mkMapSnapshot =
  sendMessage mkMapSnapshot appearanceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @appearance@
appearanceSelector :: Selector '[] (Id NSAppearance)
appearanceSelector = mkSelector "appearance"

