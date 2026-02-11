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
  , magneticHeadingSelector
  , trueHeadingSelector
  , headingAccuracySelector
  , xSelector
  , ySelector
  , zSelector
  , timestampSelector


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

import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- magneticHeading@
magneticHeading :: IsCLHeading clHeading => clHeading -> IO CDouble
magneticHeading clHeading  =
  sendMsg clHeading (mkSelector "magneticHeading") retCDouble []

-- | @- trueHeading@
trueHeading :: IsCLHeading clHeading => clHeading -> IO CDouble
trueHeading clHeading  =
  sendMsg clHeading (mkSelector "trueHeading") retCDouble []

-- | @- headingAccuracy@
headingAccuracy :: IsCLHeading clHeading => clHeading -> IO CDouble
headingAccuracy clHeading  =
  sendMsg clHeading (mkSelector "headingAccuracy") retCDouble []

-- | @- x@
x :: IsCLHeading clHeading => clHeading -> IO CDouble
x clHeading  =
  sendMsg clHeading (mkSelector "x") retCDouble []

-- | @- y@
y :: IsCLHeading clHeading => clHeading -> IO CDouble
y clHeading  =
  sendMsg clHeading (mkSelector "y") retCDouble []

-- | @- z@
z :: IsCLHeading clHeading => clHeading -> IO CDouble
z clHeading  =
  sendMsg clHeading (mkSelector "z") retCDouble []

-- | @- timestamp@
timestamp :: IsCLHeading clHeading => clHeading -> IO (Id NSDate)
timestamp clHeading  =
  sendMsg clHeading (mkSelector "timestamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @magneticHeading@
magneticHeadingSelector :: Selector
magneticHeadingSelector = mkSelector "magneticHeading"

-- | @Selector@ for @trueHeading@
trueHeadingSelector :: Selector
trueHeadingSelector = mkSelector "trueHeading"

-- | @Selector@ for @headingAccuracy@
headingAccuracySelector :: Selector
headingAccuracySelector = mkSelector "headingAccuracy"

-- | @Selector@ for @x@
xSelector :: Selector
xSelector = mkSelector "x"

-- | @Selector@ for @y@
ySelector :: Selector
ySelector = mkSelector "y"

-- | @Selector@ for @z@
zSelector :: Selector
zSelector = mkSelector "z"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector
timestampSelector = mkSelector "timestamp"

