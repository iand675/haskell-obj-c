{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitAngle@.
module ObjC.Foundation.NSUnitAngle
  ( NSUnitAngle
  , IsNSUnitAngle(..)
  , degrees
  , arcMinutes
  , arcSeconds
  , radians
  , gradians
  , revolutions
  , arcMinutesSelector
  , arcSecondsSelector
  , degreesSelector
  , gradiansSelector
  , radiansSelector
  , revolutionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ degrees@
degrees :: IO (Id NSUnitAngle)
degrees  =
  do
    cls' <- getRequiredClass "NSUnitAngle"
    sendClassMessage cls' degreesSelector

-- | @+ arcMinutes@
arcMinutes :: IO (Id NSUnitAngle)
arcMinutes  =
  do
    cls' <- getRequiredClass "NSUnitAngle"
    sendClassMessage cls' arcMinutesSelector

-- | @+ arcSeconds@
arcSeconds :: IO (Id NSUnitAngle)
arcSeconds  =
  do
    cls' <- getRequiredClass "NSUnitAngle"
    sendClassMessage cls' arcSecondsSelector

-- | @+ radians@
radians :: IO (Id NSUnitAngle)
radians  =
  do
    cls' <- getRequiredClass "NSUnitAngle"
    sendClassMessage cls' radiansSelector

-- | @+ gradians@
gradians :: IO (Id NSUnitAngle)
gradians  =
  do
    cls' <- getRequiredClass "NSUnitAngle"
    sendClassMessage cls' gradiansSelector

-- | @+ revolutions@
revolutions :: IO (Id NSUnitAngle)
revolutions  =
  do
    cls' <- getRequiredClass "NSUnitAngle"
    sendClassMessage cls' revolutionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @degrees@
degreesSelector :: Selector '[] (Id NSUnitAngle)
degreesSelector = mkSelector "degrees"

-- | @Selector@ for @arcMinutes@
arcMinutesSelector :: Selector '[] (Id NSUnitAngle)
arcMinutesSelector = mkSelector "arcMinutes"

-- | @Selector@ for @arcSeconds@
arcSecondsSelector :: Selector '[] (Id NSUnitAngle)
arcSecondsSelector = mkSelector "arcSeconds"

-- | @Selector@ for @radians@
radiansSelector :: Selector '[] (Id NSUnitAngle)
radiansSelector = mkSelector "radians"

-- | @Selector@ for @gradians@
gradiansSelector :: Selector '[] (Id NSUnitAngle)
gradiansSelector = mkSelector "gradians"

-- | @Selector@ for @revolutions@
revolutionsSelector :: Selector '[] (Id NSUnitAngle)
revolutionsSelector = mkSelector "revolutions"

