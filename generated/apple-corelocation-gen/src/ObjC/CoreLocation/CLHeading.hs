{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLHeading@.
module ObjC.CoreLocation.CLHeading
  ( CLHeading
  , IsCLHeading(..)
  , magneticHeading
  , trueHeading
  , headingAccuracy
  , x
  , y
  , z
  , timestamp
  , headingAccuracySelector
  , magneticHeadingSelector
  , timestampSelector
  , trueHeadingSelector
  , xSelector
  , ySelector
  , zSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- magneticHeading@
magneticHeading :: IsCLHeading clHeading => clHeading -> IO CDouble
magneticHeading clHeading =
  sendMessage clHeading magneticHeadingSelector

-- | @- trueHeading@
trueHeading :: IsCLHeading clHeading => clHeading -> IO CDouble
trueHeading clHeading =
  sendMessage clHeading trueHeadingSelector

-- | @- headingAccuracy@
headingAccuracy :: IsCLHeading clHeading => clHeading -> IO CDouble
headingAccuracy clHeading =
  sendMessage clHeading headingAccuracySelector

-- | @- x@
x :: IsCLHeading clHeading => clHeading -> IO CDouble
x clHeading =
  sendMessage clHeading xSelector

-- | @- y@
y :: IsCLHeading clHeading => clHeading -> IO CDouble
y clHeading =
  sendMessage clHeading ySelector

-- | @- z@
z :: IsCLHeading clHeading => clHeading -> IO CDouble
z clHeading =
  sendMessage clHeading zSelector

-- | @- timestamp@
timestamp :: IsCLHeading clHeading => clHeading -> IO (Id NSDate)
timestamp clHeading =
  sendMessage clHeading timestampSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @magneticHeading@
magneticHeadingSelector :: Selector '[] CDouble
magneticHeadingSelector = mkSelector "magneticHeading"

-- | @Selector@ for @trueHeading@
trueHeadingSelector :: Selector '[] CDouble
trueHeadingSelector = mkSelector "trueHeading"

-- | @Selector@ for @headingAccuracy@
headingAccuracySelector :: Selector '[] CDouble
headingAccuracySelector = mkSelector "headingAccuracy"

-- | @Selector@ for @x@
xSelector :: Selector '[] CDouble
xSelector = mkSelector "x"

-- | @Selector@ for @y@
ySelector :: Selector '[] CDouble
ySelector = mkSelector "y"

-- | @Selector@ for @z@
zSelector :: Selector '[] CDouble
zSelector = mkSelector "z"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector '[] (Id NSDate)
timestampSelector = mkSelector "timestamp"

