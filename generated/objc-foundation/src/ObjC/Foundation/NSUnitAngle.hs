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
  , degreesSelector
  , arcMinutesSelector
  , arcSecondsSelector
  , radiansSelector
  , gradiansSelector
  , revolutionsSelector


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

import ObjC.Foundation.Internal.Classes

-- | @+ degrees@
degrees :: IO (Id NSUnitAngle)
degrees  =
  do
    cls' <- getRequiredClass "NSUnitAngle"
    sendClassMsg cls' (mkSelector "degrees") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ arcMinutes@
arcMinutes :: IO (Id NSUnitAngle)
arcMinutes  =
  do
    cls' <- getRequiredClass "NSUnitAngle"
    sendClassMsg cls' (mkSelector "arcMinutes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ arcSeconds@
arcSeconds :: IO (Id NSUnitAngle)
arcSeconds  =
  do
    cls' <- getRequiredClass "NSUnitAngle"
    sendClassMsg cls' (mkSelector "arcSeconds") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ radians@
radians :: IO (Id NSUnitAngle)
radians  =
  do
    cls' <- getRequiredClass "NSUnitAngle"
    sendClassMsg cls' (mkSelector "radians") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ gradians@
gradians :: IO (Id NSUnitAngle)
gradians  =
  do
    cls' <- getRequiredClass "NSUnitAngle"
    sendClassMsg cls' (mkSelector "gradians") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ revolutions@
revolutions :: IO (Id NSUnitAngle)
revolutions  =
  do
    cls' <- getRequiredClass "NSUnitAngle"
    sendClassMsg cls' (mkSelector "revolutions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @degrees@
degreesSelector :: Selector
degreesSelector = mkSelector "degrees"

-- | @Selector@ for @arcMinutes@
arcMinutesSelector :: Selector
arcMinutesSelector = mkSelector "arcMinutes"

-- | @Selector@ for @arcSeconds@
arcSecondsSelector :: Selector
arcSecondsSelector = mkSelector "arcSeconds"

-- | @Selector@ for @radians@
radiansSelector :: Selector
radiansSelector = mkSelector "radians"

-- | @Selector@ for @gradians@
gradiansSelector :: Selector
gradiansSelector = mkSelector "gradians"

-- | @Selector@ for @revolutions@
revolutionsSelector :: Selector
revolutionsSelector = mkSelector "revolutions"

