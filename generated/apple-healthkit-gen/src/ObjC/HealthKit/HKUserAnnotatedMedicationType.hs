{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKUserAnnotatedMedicationType
--
-- Represents the set of authorizeable HKUserAnnotatedMedications.
--
-- Generated bindings for @HKUserAnnotatedMedicationType@.
module ObjC.HealthKit.HKUserAnnotatedMedicationType
  ( HKUserAnnotatedMedicationType
  , IsHKUserAnnotatedMedicationType(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

