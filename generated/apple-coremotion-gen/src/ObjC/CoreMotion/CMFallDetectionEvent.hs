{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Fall Detection Event
--
-- CMFallDetectionEventResolution
--
-- This object represents a Fall Detection event and how it was resolved
--
-- Generated bindings for @CMFallDetectionEvent@.
module ObjC.CoreMotion.CMFallDetectionEvent
  ( CMFallDetectionEvent
  , IsCMFallDetectionEvent(..)
  , init_
  , date
  , resolution
  , dateSelector
  , initSelector
  , resolutionSelector

  -- * Enum types
  , CMFallDetectionEventUserResolution(CMFallDetectionEventUserResolution)
  , pattern CMFallDetectionEventUserResolutionConfirmed
  , pattern CMFallDetectionEventUserResolutionDismissed
  , pattern CMFallDetectionEventUserResolutionRejected
  , pattern CMFallDetectionEventUserResolutionUnresponsive

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.CoreMotion.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCMFallDetectionEvent cmFallDetectionEvent => cmFallDetectionEvent -> IO (Id CMFallDetectionEvent)
init_ cmFallDetectionEvent =
  sendOwnedMessage cmFallDetectionEvent initSelector

-- | date
--
-- The time a fall was detected
--
-- ObjC selector: @- date@
date :: IsCMFallDetectionEvent cmFallDetectionEvent => cmFallDetectionEvent -> IO (Id NSDate)
date cmFallDetectionEvent =
  sendMessage cmFallDetectionEvent dateSelector

-- | resolution
--
-- enum value representing how the Fall Detection event was resolved
--
-- CMFallDetectionEventResolution
--
-- ObjC selector: @- resolution@
resolution :: IsCMFallDetectionEvent cmFallDetectionEvent => cmFallDetectionEvent -> IO CMFallDetectionEventUserResolution
resolution cmFallDetectionEvent =
  sendMessage cmFallDetectionEvent resolutionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CMFallDetectionEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

-- | @Selector@ for @resolution@
resolutionSelector :: Selector '[] CMFallDetectionEventUserResolution
resolutionSelector = mkSelector "resolution"

