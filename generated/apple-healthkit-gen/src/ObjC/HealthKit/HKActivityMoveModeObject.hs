{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKActivityMoveModeObject
--
-- A wrapper object for HKActivityMoveMode enumeration.
--
-- Generated bindings for @HKActivityMoveModeObject@.
module ObjC.HealthKit.HKActivityMoveModeObject
  ( HKActivityMoveModeObject
  , IsHKActivityMoveModeObject(..)
  , activityMoveMode
  , activityMoveModeSelector

  -- * Enum types
  , HKActivityMoveMode(HKActivityMoveMode)
  , pattern HKActivityMoveModeActiveEnergy
  , pattern HKActivityMoveModeAppleMoveTime

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- activityMoveMode@
activityMoveMode :: IsHKActivityMoveModeObject hkActivityMoveModeObject => hkActivityMoveModeObject -> IO HKActivityMoveMode
activityMoveMode hkActivityMoveModeObject =
  sendMessage hkActivityMoveModeObject activityMoveModeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @activityMoveMode@
activityMoveModeSelector :: Selector '[] HKActivityMoveMode
activityMoveModeSelector = mkSelector "activityMoveMode"

