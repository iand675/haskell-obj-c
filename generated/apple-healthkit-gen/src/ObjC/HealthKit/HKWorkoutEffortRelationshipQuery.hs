{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKWorkoutEffortRelationshipQuery
--
-- A concrete subclass of HKQuery that provides an interface to observe associations with a workout sample.
--
-- Generated bindings for @HKWorkoutEffortRelationshipQuery@.
module ObjC.HealthKit.HKWorkoutEffortRelationshipQuery
  ( HKWorkoutEffortRelationshipQuery
  , IsHKWorkoutEffortRelationshipQuery(..)

  -- * Enum types
  , HKWorkoutEffortRelationshipQueryOptions(HKWorkoutEffortRelationshipQueryOptions)
  , pattern HKWorkoutEffortRelationshipQueryOptionsDefault
  , pattern HKWorkoutEffortRelationshipQueryOptionsMostRelevant

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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

