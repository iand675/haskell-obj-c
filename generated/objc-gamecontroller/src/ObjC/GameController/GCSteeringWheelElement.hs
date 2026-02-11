{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GCSteeringWheelElement@.
module ObjC.GameController.GCSteeringWheelElement
  ( GCSteeringWheelElement
  , IsGCSteeringWheelElement(..)
  , init_
  , maximumDegreesOfRotation
  , initSelector
  , maximumDegreesOfRotationSelector


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

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsGCSteeringWheelElement gcSteeringWheelElement => gcSteeringWheelElement -> IO (Id GCSteeringWheelElement)
init_ gcSteeringWheelElement  =
  sendMsg gcSteeringWheelElement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- maximumDegreesOfRotation@
maximumDegreesOfRotation :: IsGCSteeringWheelElement gcSteeringWheelElement => gcSteeringWheelElement -> IO CFloat
maximumDegreesOfRotation gcSteeringWheelElement  =
  sendMsg gcSteeringWheelElement (mkSelector "maximumDegreesOfRotation") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @maximumDegreesOfRotation@
maximumDegreesOfRotationSelector :: Selector
maximumDegreesOfRotationSelector = mkSelector "maximumDegreesOfRotation"

