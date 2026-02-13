{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRElectrocardiogramSession@.
module ObjC.SensorKit.SRElectrocardiogramSession
  ( SRElectrocardiogramSession
  , IsSRElectrocardiogramSession(..)
  , init_
  , new
  , state
  , sessionGuidance
  , identifier
  , identifierSelector
  , initSelector
  , newSelector
  , sessionGuidanceSelector
  , stateSelector

  -- * Enum types
  , SRElectrocardiogramSessionGuidance(SRElectrocardiogramSessionGuidance)
  , pattern SRElectrocardiogramSessionGuidanceGuided
  , pattern SRElectrocardiogramSessionGuidanceUnguided
  , SRElectrocardiogramSessionState(SRElectrocardiogramSessionState)
  , pattern SRElectrocardiogramSessionStateBegin
  , pattern SRElectrocardiogramSessionStateActive
  , pattern SRElectrocardiogramSessionStateEnd

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSRElectrocardiogramSession srElectrocardiogramSession => srElectrocardiogramSession -> IO (Id SRElectrocardiogramSession)
init_ srElectrocardiogramSession =
  sendOwnedMessage srElectrocardiogramSession initSelector

-- | @+ new@
new :: IO (Id SRElectrocardiogramSession)
new  =
  do
    cls' <- getRequiredClass "SRElectrocardiogramSession"
    sendOwnedClassMessage cls' newSelector

-- | state
--
-- The state of the ECG session when the sample was recorded
--
-- ObjC selector: @- state@
state :: IsSRElectrocardiogramSession srElectrocardiogramSession => srElectrocardiogramSession -> IO SRElectrocardiogramSessionState
state srElectrocardiogramSession =
  sendMessage srElectrocardiogramSession stateSelector

-- | sessionGuidance
--
-- The type of session guidance during the the ECG session
--
-- ObjC selector: @- sessionGuidance@
sessionGuidance :: IsSRElectrocardiogramSession srElectrocardiogramSession => srElectrocardiogramSession -> IO SRElectrocardiogramSessionGuidance
sessionGuidance srElectrocardiogramSession =
  sendMessage srElectrocardiogramSession sessionGuidanceSelector

-- | identifier
--
-- Used to tie samples across multiple @SRFetchResult@ s to the same session
--
-- ObjC selector: @- identifier@
identifier :: IsSRElectrocardiogramSession srElectrocardiogramSession => srElectrocardiogramSession -> IO (Id NSString)
identifier srElectrocardiogramSession =
  sendMessage srElectrocardiogramSession identifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SRElectrocardiogramSession)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SRElectrocardiogramSession)
newSelector = mkSelector "new"

-- | @Selector@ for @state@
stateSelector :: Selector '[] SRElectrocardiogramSessionState
stateSelector = mkSelector "state"

-- | @Selector@ for @sessionGuidance@
sessionGuidanceSelector :: Selector '[] SRElectrocardiogramSessionGuidance
sessionGuidanceSelector = mkSelector "sessionGuidance"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

