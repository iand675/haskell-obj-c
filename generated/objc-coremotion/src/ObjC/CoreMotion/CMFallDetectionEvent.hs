{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , dateSelector
  , resolutionSelector

  -- * Enum types
  , CMFallDetectionEventUserResolution(CMFallDetectionEventUserResolution)
  , pattern CMFallDetectionEventUserResolutionConfirmed
  , pattern CMFallDetectionEventUserResolutionDismissed
  , pattern CMFallDetectionEventUserResolutionRejected
  , pattern CMFallDetectionEventUserResolutionUnresponsive

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

import ObjC.CoreMotion.Internal.Classes
import ObjC.CoreMotion.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCMFallDetectionEvent cmFallDetectionEvent => cmFallDetectionEvent -> IO (Id CMFallDetectionEvent)
init_ cmFallDetectionEvent  =
  sendMsg cmFallDetectionEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | date
--
-- The time a fall was detected
--
-- ObjC selector: @- date@
date :: IsCMFallDetectionEvent cmFallDetectionEvent => cmFallDetectionEvent -> IO (Id NSDate)
date cmFallDetectionEvent  =
  sendMsg cmFallDetectionEvent (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | resolution
--
-- enum value representing how the Fall Detection event was resolved
--
-- CMFallDetectionEventResolution
--
-- ObjC selector: @- resolution@
resolution :: IsCMFallDetectionEvent cmFallDetectionEvent => cmFallDetectionEvent -> IO CMFallDetectionEventUserResolution
resolution cmFallDetectionEvent  =
  fmap (coerce :: CLong -> CMFallDetectionEventUserResolution) $ sendMsg cmFallDetectionEvent (mkSelector "resolution") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

-- | @Selector@ for @resolution@
resolutionSelector :: Selector
resolutionSelector = mkSelector "resolution"

