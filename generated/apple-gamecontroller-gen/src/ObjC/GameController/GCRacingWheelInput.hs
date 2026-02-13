{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GCRacingWheelInput@.
module ObjC.GameController.GCRacingWheelInput
  ( GCRacingWheelInput
  , IsGCRacingWheelInput(..)
  , capture
  , nextInputState
  , captureSelector
  , nextInputStateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Polls the current state vector of the racing wheel input and saves it to a new instance.
--
-- ObjC selector: @- capture@
capture :: IsGCRacingWheelInput gcRacingWheelInput => gcRacingWheelInput -> IO (Id GCRacingWheelInputState)
capture gcRacingWheelInput =
  sendMessage gcRacingWheelInput captureSelector

-- | @- nextInputState@
nextInputState :: IsGCRacingWheelInput gcRacingWheelInput => gcRacingWheelInput -> IO (Id GCRacingWheelInputState)
nextInputState gcRacingWheelInput =
  sendMessage gcRacingWheelInput nextInputStateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @capture@
captureSelector :: Selector '[] (Id GCRacingWheelInputState)
captureSelector = mkSelector "capture"

-- | @Selector@ for @nextInputState@
nextInputStateSelector :: Selector '[] (Id GCRacingWheelInputState)
nextInputStateSelector = mkSelector "nextInputState"

