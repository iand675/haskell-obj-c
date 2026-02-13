{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKBiologicalSexObject
--
-- A wrapper object for HKBiologicalSex enumeration.
--
-- Generated bindings for @HKBiologicalSexObject@.
module ObjC.HealthKit.HKBiologicalSexObject
  ( HKBiologicalSexObject
  , IsHKBiologicalSexObject(..)
  , biologicalSex
  , biologicalSexSelector

  -- * Enum types
  , HKBiologicalSex(HKBiologicalSex)
  , pattern HKBiologicalSexNotSet
  , pattern HKBiologicalSexFemale
  , pattern HKBiologicalSexMale
  , pattern HKBiologicalSexOther

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

-- | @- biologicalSex@
biologicalSex :: IsHKBiologicalSexObject hkBiologicalSexObject => hkBiologicalSexObject -> IO HKBiologicalSex
biologicalSex hkBiologicalSexObject =
  sendMessage hkBiologicalSexObject biologicalSexSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @biologicalSex@
biologicalSexSelector :: Selector '[] HKBiologicalSex
biologicalSexSelector = mkSelector "biologicalSex"

