{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Directional Gamepad profile.
--
-- All controller profiles provide a base level of information about the controller they belong to. A directional gamepad features a subset of the possible inputs on a micro gamepad. It guarantees:    - The gamepad does not support motion, meaning        - -[GCController motion] is always nil        - -[GCDirectionalGamepad allowsRotation] is always NO
--
-- Additionally, the gamepad may have a digital or analog dpad.        - -[GCDirectionalGamepad dpad].analog may be YES or NO        - If -[GCDirectionalGamepad dpad].analog is NO, then -[GCDirectionalGamepad reportsAbsoluteDpadValues] is always YES
--
-- A profile maps the hardware notion of a controller into a logical controller. One that a developer can design for and depend on, no matter the underlying hardware. If your game supports GCMicroGamepad, but does not need the motion and analog dpad functionality of GCMicroGamepad, be sure to add Directional Gamepad to your project's supported Game Controller capabilities.
--
-- See: GCMicroGamepad
--
-- Note: If you want to use the additional functionality of GCDirectionalGamepad, you should set GCSupportsMultipleMicroGamepads to YES and handle microgamepad connections separately.
--
-- Note: This profile represents the 2021 2nd generation Siri Remote. Make sure you set GCSupportsMultipleMicroGamepads to YES to properly support the remote.
--
-- Generated bindings for @GCDirectionalGamepad@.
module ObjC.GameController.GCDirectionalGamepad
  ( GCDirectionalGamepad
  , IsGCDirectionalGamepad(..)


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

