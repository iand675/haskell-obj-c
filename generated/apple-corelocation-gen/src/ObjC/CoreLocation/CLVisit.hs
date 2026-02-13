{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLVisit@.
module ObjC.CoreLocation.CLVisit
  ( CLVisit
  , IsCLVisit(..)
  , arrivalDate
  , departureDate
  , horizontalAccuracy
  , arrivalDateSelector
  , departureDateSelector
  , horizontalAccuracySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- arrivalDate@
arrivalDate :: IsCLVisit clVisit => clVisit -> IO (Id NSDate)
arrivalDate clVisit =
  sendMessage clVisit arrivalDateSelector

-- | @- departureDate@
departureDate :: IsCLVisit clVisit => clVisit -> IO (Id NSDate)
departureDate clVisit =
  sendMessage clVisit departureDateSelector

-- | @- horizontalAccuracy@
horizontalAccuracy :: IsCLVisit clVisit => clVisit -> IO CDouble
horizontalAccuracy clVisit =
  sendMessage clVisit horizontalAccuracySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @arrivalDate@
arrivalDateSelector :: Selector '[] (Id NSDate)
arrivalDateSelector = mkSelector "arrivalDate"

-- | @Selector@ for @departureDate@
departureDateSelector :: Selector '[] (Id NSDate)
departureDateSelector = mkSelector "departureDate"

-- | @Selector@ for @horizontalAccuracy@
horizontalAccuracySelector :: Selector '[] CDouble
horizontalAccuracySelector = mkSelector "horizontalAccuracy"

