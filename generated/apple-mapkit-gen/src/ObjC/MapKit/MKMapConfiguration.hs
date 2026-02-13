{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMapConfiguration@.
module ObjC.MapKit.MKMapConfiguration
  ( MKMapConfiguration
  , IsMKMapConfiguration(..)
  , init_
  , new
  , elevationStyle
  , setElevationStyle
  , elevationStyleSelector
  , initSelector
  , newSelector
  , setElevationStyleSelector

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
init_ :: IsMKMapConfiguration mkMapConfiguration => mkMapConfiguration -> IO (Id MKMapConfiguration)
init_ mkMapConfiguration =
  sendOwnedMessage mkMapConfiguration initSelector

-- | @+ new@
new :: IO (Id MKMapConfiguration)
new  =
  do
    cls' <- getRequiredClass "MKMapConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- elevationStyle@
elevationStyle :: IsMKMapConfiguration mkMapConfiguration => mkMapConfiguration -> IO MKMapElevationStyle
elevationStyle mkMapConfiguration =
  sendMessage mkMapConfiguration elevationStyleSelector

-- | @- setElevationStyle:@
setElevationStyle :: IsMKMapConfiguration mkMapConfiguration => mkMapConfiguration -> MKMapElevationStyle -> IO ()
setElevationStyle mkMapConfiguration value =
  sendMessage mkMapConfiguration setElevationStyleSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MKMapConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MKMapConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @elevationStyle@
elevationStyleSelector :: Selector '[] MKMapElevationStyle
elevationStyleSelector = mkSelector "elevationStyle"

-- | @Selector@ for @setElevationStyle:@
setElevationStyleSelector :: Selector '[MKMapElevationStyle] ()
setElevationStyleSelector = mkSelector "setElevationStyle:"

