{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLCircularGeographicCondition@.
module ObjC.CoreLocation.CLCircularGeographicCondition
  ( CLCircularGeographicCondition
  , IsCLCircularGeographicCondition(..)
  , initWithCenter_radius
  , center
  , radius
  , initWithCenter_radiusSelector
  , centerSelector
  , radiusSelector


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

import ObjC.CoreLocation.Internal.Classes
import ObjC.CoreLocation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- initWithCenter:radius:@
initWithCenter_radius :: IsCLCircularGeographicCondition clCircularGeographicCondition => clCircularGeographicCondition -> CLLocationCoordinate2D -> CDouble -> IO (Id CLCircularGeographicCondition)
initWithCenter_radius clCircularGeographicCondition  center radius =
  sendMsg clCircularGeographicCondition (mkSelector "initWithCenter:radius:") (retPtr retVoid) [argCLLocationCoordinate2D center, argCDouble (fromIntegral radius)] >>= ownedObject . castPtr

-- | @- center@
center :: IsCLCircularGeographicCondition clCircularGeographicCondition => clCircularGeographicCondition -> IO CLLocationCoordinate2D
center clCircularGeographicCondition  =
  sendMsgStret clCircularGeographicCondition (mkSelector "center") retCLLocationCoordinate2D []

-- | @- radius@
radius :: IsCLCircularGeographicCondition clCircularGeographicCondition => clCircularGeographicCondition -> IO CDouble
radius clCircularGeographicCondition  =
  sendMsg clCircularGeographicCondition (mkSelector "radius") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCenter:radius:@
initWithCenter_radiusSelector :: Selector
initWithCenter_radiusSelector = mkSelector "initWithCenter:radius:"

-- | @Selector@ for @center@
centerSelector :: Selector
centerSelector = mkSelector "center"

-- | @Selector@ for @radius@
radiusSelector :: Selector
radiusSelector = mkSelector "radius"

