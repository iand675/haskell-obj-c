{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , elevationStyleSelector
  , setElevationStyleSelector

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
init_ :: IsMKMapConfiguration mkMapConfiguration => mkMapConfiguration -> IO (Id MKMapConfiguration)
init_ mkMapConfiguration  =
  sendMsg mkMapConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MKMapConfiguration)
new  =
  do
    cls' <- getRequiredClass "MKMapConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- elevationStyle@
elevationStyle :: IsMKMapConfiguration mkMapConfiguration => mkMapConfiguration -> IO MKMapElevationStyle
elevationStyle mkMapConfiguration  =
  fmap (coerce :: CLong -> MKMapElevationStyle) $ sendMsg mkMapConfiguration (mkSelector "elevationStyle") retCLong []

-- | @- setElevationStyle:@
setElevationStyle :: IsMKMapConfiguration mkMapConfiguration => mkMapConfiguration -> MKMapElevationStyle -> IO ()
setElevationStyle mkMapConfiguration  value =
  sendMsg mkMapConfiguration (mkSelector "setElevationStyle:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @elevationStyle@
elevationStyleSelector :: Selector
elevationStyleSelector = mkSelector "elevationStyle"

-- | @Selector@ for @setElevationStyle:@
setElevationStyleSelector :: Selector
setElevationStyleSelector = mkSelector "setElevationStyle:"

