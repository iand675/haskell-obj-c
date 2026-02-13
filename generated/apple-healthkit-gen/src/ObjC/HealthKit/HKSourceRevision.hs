{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKSourceRevision
--
-- Represents a specific revision of an HKSource.
--
-- Generated bindings for @HKSourceRevision@.
module ObjC.HealthKit.HKSourceRevision
  ( HKSourceRevision
  , IsHKSourceRevision(..)
  , initWithSource_version
  , init_
  , source
  , version
  , productType
  , initSelector
  , initWithSource_versionSelector
  , productTypeSelector
  , sourceSelector
  , versionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithSource:version:
--
-- Initializes a new HKSourceRevision with the given source and version.
--
-- ObjC selector: @- initWithSource:version:@
initWithSource_version :: (IsHKSourceRevision hkSourceRevision, IsHKSource source, IsNSString version) => hkSourceRevision -> source -> version -> IO (Id HKSourceRevision)
initWithSource_version hkSourceRevision source version =
  sendOwnedMessage hkSourceRevision initWithSource_versionSelector (toHKSource source) (toNSString version)

-- | @- init@
init_ :: IsHKSourceRevision hkSourceRevision => hkSourceRevision -> IO (Id HKSourceRevision)
init_ hkSourceRevision =
  sendOwnedMessage hkSourceRevision initSelector

-- | source
--
-- The HKSource of the receiver.
--
-- ObjC selector: @- source@
source :: IsHKSourceRevision hkSourceRevision => hkSourceRevision -> IO (Id HKSource)
source hkSourceRevision =
  sendMessage hkSourceRevision sourceSelector

-- | version
--
-- The version of the source property.
--
-- This value is taken from the CFBundleVersion of the source. May be nil for older data.
--
-- ObjC selector: @- version@
version :: IsHKSourceRevision hkSourceRevision => hkSourceRevision -> IO (Id NSString)
version hkSourceRevision =
  sendMessage hkSourceRevision versionSelector

-- | productType
--
-- Represents the product type of the device running HealthKit when the object was created.
--
-- This value may be nil for older data, which indicates an unknown product type.
--
-- ObjC selector: @- productType@
productType :: IsHKSourceRevision hkSourceRevision => hkSourceRevision -> IO (Id NSString)
productType hkSourceRevision =
  sendMessage hkSourceRevision productTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSource:version:@
initWithSource_versionSelector :: Selector '[Id HKSource, Id NSString] (Id HKSourceRevision)
initWithSource_versionSelector = mkSelector "initWithSource:version:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKSourceRevision)
initSelector = mkSelector "init"

-- | @Selector@ for @source@
sourceSelector :: Selector '[] (Id HKSource)
sourceSelector = mkSelector "source"

-- | @Selector@ for @version@
versionSelector :: Selector '[] (Id NSString)
versionSelector = mkSelector "version"

-- | @Selector@ for @productType@
productTypeSelector :: Selector '[] (Id NSString)
productTypeSelector = mkSelector "productType"

