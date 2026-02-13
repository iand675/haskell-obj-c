{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKImageryMapConfiguration@.
module ObjC.MapKit.MKImageryMapConfiguration
  ( MKImageryMapConfiguration
  , IsMKImageryMapConfiguration(..)
  , init_
  , initWithElevationStyle
  , initSelector
  , initWithElevationStyleSelector

  -- * Enum types
  , MKMapElevationStyle(MKMapElevationStyle)
  , pattern MKMapElevationStyleFlat
  , pattern MKMapElevationStyleRealistic

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.MapKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMKImageryMapConfiguration mkImageryMapConfiguration => mkImageryMapConfiguration -> IO (Id MKImageryMapConfiguration)
init_ mkImageryMapConfiguration =
  sendOwnedMessage mkImageryMapConfiguration initSelector

-- | @- initWithElevationStyle:@
initWithElevationStyle :: IsMKImageryMapConfiguration mkImageryMapConfiguration => mkImageryMapConfiguration -> MKMapElevationStyle -> IO (Id MKImageryMapConfiguration)
initWithElevationStyle mkImageryMapConfiguration elevationStyle =
  sendOwnedMessage mkImageryMapConfiguration initWithElevationStyleSelector elevationStyle

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MKImageryMapConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithElevationStyle:@
initWithElevationStyleSelector :: Selector '[MKMapElevationStyle] (Id MKImageryMapConfiguration)
initWithElevationStyleSelector = mkSelector "initWithElevationStyle:"

