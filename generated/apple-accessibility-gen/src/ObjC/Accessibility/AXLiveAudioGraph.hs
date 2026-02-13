{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXLiveAudioGraph@.
module ObjC.Accessibility.AXLiveAudioGraph
  ( AXLiveAudioGraph
  , IsAXLiveAudioGraph(..)
  , start
  , updateValue
  , stop
  , startSelector
  , stopSelector
  , updateValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Begins a live audio graph session.
--
-- ObjC selector: @+ start@
start :: IO ()
start  =
  do
    cls' <- getRequiredClass "AXLiveAudioGraph"
    sendClassMessage cls' startSelector

-- | Sets the pitch of the audio graph's tone. This should be a normalized value in the range [0.0, 1.0], where 0 represents the minimum displayable y-axis value for your series and 1 represents the maximum displayable y-axis value for your series.
--
-- ObjC selector: @+ updateValue:@
updateValue :: CDouble -> IO ()
updateValue value =
  do
    cls' <- getRequiredClass "AXLiveAudioGraph"
    sendClassMessage cls' updateValueSelector value

-- | Ends the live audio graph session.
--
-- ObjC selector: @+ stop@
stop :: IO ()
stop  =
  do
    cls' <- getRequiredClass "AXLiveAudioGraph"
    sendClassMessage cls' stopSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @start@
startSelector :: Selector '[] ()
startSelector = mkSelector "start"

-- | @Selector@ for @updateValue:@
updateValueSelector :: Selector '[CDouble] ()
updateValueSelector = mkSelector "updateValue:"

-- | @Selector@ for @stop@
stopSelector :: Selector '[] ()
stopSelector = mkSelector "stop"

