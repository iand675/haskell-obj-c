{-# LANGUAGE PatternSynonyms #-}
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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMKImageryMapConfiguration mkImageryMapConfiguration => mkImageryMapConfiguration -> IO (Id MKImageryMapConfiguration)
init_ mkImageryMapConfiguration  =
  sendMsg mkImageryMapConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithElevationStyle:@
initWithElevationStyle :: IsMKImageryMapConfiguration mkImageryMapConfiguration => mkImageryMapConfiguration -> MKMapElevationStyle -> IO (Id MKImageryMapConfiguration)
initWithElevationStyle mkImageryMapConfiguration  elevationStyle =
  sendMsg mkImageryMapConfiguration (mkSelector "initWithElevationStyle:") (retPtr retVoid) [argCLong (coerce elevationStyle)] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithElevationStyle:@
initWithElevationStyleSelector :: Selector
initWithElevationStyleSelector = mkSelector "initWithElevationStyle:"

