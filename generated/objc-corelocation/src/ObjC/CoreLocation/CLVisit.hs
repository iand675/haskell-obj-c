{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLVisit@.
module ObjC.CoreLocation.CLVisit
  ( CLVisit
  , IsCLVisit(..)
  , arrivalDate
  , departureDate
  , coordinate
  , horizontalAccuracy
  , arrivalDateSelector
  , departureDateSelector
  , coordinateSelector
  , horizontalAccuracySelector


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

-- | @- arrivalDate@
arrivalDate :: IsCLVisit clVisit => clVisit -> IO (Id NSDate)
arrivalDate clVisit  =
  sendMsg clVisit (mkSelector "arrivalDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- departureDate@
departureDate :: IsCLVisit clVisit => clVisit -> IO (Id NSDate)
departureDate clVisit  =
  sendMsg clVisit (mkSelector "departureDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- coordinate@
coordinate :: IsCLVisit clVisit => clVisit -> IO CLLocationCoordinate2D
coordinate clVisit  =
  sendMsgStret clVisit (mkSelector "coordinate") retCLLocationCoordinate2D []

-- | @- horizontalAccuracy@
horizontalAccuracy :: IsCLVisit clVisit => clVisit -> IO CDouble
horizontalAccuracy clVisit  =
  sendMsg clVisit (mkSelector "horizontalAccuracy") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @arrivalDate@
arrivalDateSelector :: Selector
arrivalDateSelector = mkSelector "arrivalDate"

-- | @Selector@ for @departureDate@
departureDateSelector :: Selector
departureDateSelector = mkSelector "departureDate"

-- | @Selector@ for @coordinate@
coordinateSelector :: Selector
coordinateSelector = mkSelector "coordinate"

-- | @Selector@ for @horizontalAccuracy@
horizontalAccuracySelector :: Selector
horizontalAccuracySelector = mkSelector "horizontalAccuracy"

