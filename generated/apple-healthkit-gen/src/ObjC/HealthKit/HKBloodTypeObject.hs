{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKBloodTypeObject
--
-- A wrapper object for HKBloodType enumeration.
--
-- Generated bindings for @HKBloodTypeObject@.
module ObjC.HealthKit.HKBloodTypeObject
  ( HKBloodTypeObject
  , IsHKBloodTypeObject(..)
  , bloodType
  , bloodTypeSelector

  -- * Enum types
  , HKBloodType(HKBloodType)
  , pattern HKBloodTypeNotSet
  , pattern HKBloodTypeAPositive
  , pattern HKBloodTypeANegative
  , pattern HKBloodTypeBPositive
  , pattern HKBloodTypeBNegative
  , pattern HKBloodTypeABPositive
  , pattern HKBloodTypeABNegative
  , pattern HKBloodTypeOPositive
  , pattern HKBloodTypeONegative

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

-- | @- bloodType@
bloodType :: IsHKBloodTypeObject hkBloodTypeObject => hkBloodTypeObject -> IO HKBloodType
bloodType hkBloodTypeObject =
  sendMessage hkBloodTypeObject bloodTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bloodType@
bloodTypeSelector :: Selector '[] HKBloodType
bloodTypeSelector = mkSelector "bloodType"

