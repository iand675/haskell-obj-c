{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This object describes a Crash Detection event and response to it.
--
-- SACrashDetectionEvent
--
-- Generated bindings for @SACrashDetectionEvent@.
module ObjC.SafetyKit.SACrashDetectionEvent
  ( SACrashDetectionEvent
  , IsSACrashDetectionEvent(..)
  , new
  , init_
  , date
  , response
  , dateSelector
  , initSelector
  , newSelector
  , responseSelector

  -- * Enum types
  , SACrashDetectionEventResponse(SACrashDetectionEventResponse)
  , pattern SACrashDetectionEventResponseAttempted
  , pattern SACrashDetectionEventResponseDisabled

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SafetyKit.Internal.Classes
import ObjC.SafetyKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id SACrashDetectionEvent)
new  =
  do
    cls' <- getRequiredClass "SACrashDetectionEvent"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsSACrashDetectionEvent saCrashDetectionEvent => saCrashDetectionEvent -> IO (Id SACrashDetectionEvent)
init_ saCrashDetectionEvent =
  sendOwnedMessage saCrashDetectionEvent initSelector

-- | date
--
-- The time a crash was detected
--
-- ObjC selector: @- date@
date :: IsSACrashDetectionEvent saCrashDetectionEvent => saCrashDetectionEvent -> IO (Id NSDate)
date saCrashDetectionEvent =
  sendMessage saCrashDetectionEvent dateSelector

-- | response
--
-- enum value representing the emergency response to the Crash Detection event
--
-- SACrashDetectionEventResponse
--
-- ObjC selector: @- response@
response :: IsSACrashDetectionEvent saCrashDetectionEvent => saCrashDetectionEvent -> IO SACrashDetectionEventResponse
response saCrashDetectionEvent =
  sendMessage saCrashDetectionEvent responseSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SACrashDetectionEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SACrashDetectionEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

-- | @Selector@ for @response@
responseSelector :: Selector '[] SACrashDetectionEventResponse
responseSelector = mkSelector "response"

