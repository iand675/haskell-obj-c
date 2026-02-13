{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKWheelchairUseObject
--
-- A wrapper object for HKWheelchairUse enumeration.
--
-- Generated bindings for @HKWheelchairUseObject@.
module ObjC.HealthKit.HKWheelchairUseObject
  ( HKWheelchairUseObject
  , IsHKWheelchairUseObject(..)
  , wheelchairUse
  , wheelchairUseSelector

  -- * Enum types
  , HKWheelchairUse(HKWheelchairUse)
  , pattern HKWheelchairUseNotSet
  , pattern HKWheelchairUseNo
  , pattern HKWheelchairUseYes

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

-- | @- wheelchairUse@
wheelchairUse :: IsHKWheelchairUseObject hkWheelchairUseObject => hkWheelchairUseObject -> IO HKWheelchairUse
wheelchairUse hkWheelchairUseObject =
  sendMessage hkWheelchairUseObject wheelchairUseSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @wheelchairUse@
wheelchairUseSelector :: Selector '[] HKWheelchairUse
wheelchairUseSelector = mkSelector "wheelchairUse"

