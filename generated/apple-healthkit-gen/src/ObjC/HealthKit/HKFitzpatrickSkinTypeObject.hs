{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKFitzpatrickSkinTypeObject
--
-- A wrapper object for HKFitzpatrickSkinType enumeration.
--
-- Generated bindings for @HKFitzpatrickSkinTypeObject@.
module ObjC.HealthKit.HKFitzpatrickSkinTypeObject
  ( HKFitzpatrickSkinTypeObject
  , IsHKFitzpatrickSkinTypeObject(..)
  , skinType
  , skinTypeSelector

  -- * Enum types
  , HKFitzpatrickSkinType(HKFitzpatrickSkinType)
  , pattern HKFitzpatrickSkinTypeNotSet
  , pattern HKFitzpatrickSkinTypeI
  , pattern HKFitzpatrickSkinTypeII
  , pattern HKFitzpatrickSkinTypeIII
  , pattern HKFitzpatrickSkinTypeIV
  , pattern HKFitzpatrickSkinTypeV
  , pattern HKFitzpatrickSkinTypeVI

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

-- | @- skinType@
skinType :: IsHKFitzpatrickSkinTypeObject hkFitzpatrickSkinTypeObject => hkFitzpatrickSkinTypeObject -> IO HKFitzpatrickSkinType
skinType hkFitzpatrickSkinTypeObject =
  sendMessage hkFitzpatrickSkinTypeObject skinTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @skinType@
skinTypeSelector :: Selector '[] HKFitzpatrickSkinType
skinTypeSelector = mkSelector "skinType"

