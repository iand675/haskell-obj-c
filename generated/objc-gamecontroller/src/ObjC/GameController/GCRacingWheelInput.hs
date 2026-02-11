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

-- | Polls the current state vector of the racing wheel input and saves it to a new instance.
--
-- ObjC selector: @- capture@
capture :: IsGCRacingWheelInput gcRacingWheelInput => gcRacingWheelInput -> IO (Id GCRacingWheelInputState)
capture gcRacingWheelInput  =
  sendMsg gcRacingWheelInput (mkSelector "capture") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nextInputState@
nextInputState :: IsGCRacingWheelInput gcRacingWheelInput => gcRacingWheelInput -> IO (Id GCRacingWheelInputState)
nextInputState gcRacingWheelInput  =
  sendMsg gcRacingWheelInput (mkSelector "nextInputState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @capture@
captureSelector :: Selector
captureSelector = mkSelector "capture"

-- | @Selector@ for @nextInputState@
nextInputStateSelector :: Selector
nextInputStateSelector = mkSelector "nextInputState"

