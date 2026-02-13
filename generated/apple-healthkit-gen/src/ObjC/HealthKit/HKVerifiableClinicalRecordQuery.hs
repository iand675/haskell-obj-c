{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKVerifiableClinicalRecordQuery@.
module ObjC.HealthKit.HKVerifiableClinicalRecordQuery
  ( HKVerifiableClinicalRecordQuery
  , IsHKVerifiableClinicalRecordQuery(..)
  , init_
  , new
  , recordTypes
  , sourceTypes
  , initSelector
  , newSelector
  , recordTypesSelector
  , sourceTypesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsHKVerifiableClinicalRecordQuery hkVerifiableClinicalRecordQuery => hkVerifiableClinicalRecordQuery -> IO (Id HKVerifiableClinicalRecordQuery)
init_ hkVerifiableClinicalRecordQuery =
  sendOwnedMessage hkVerifiableClinicalRecordQuery initSelector

-- | @+ new@
new :: IO (Id HKVerifiableClinicalRecordQuery)
new  =
  do
    cls' <- getRequiredClass "HKVerifiableClinicalRecordQuery"
    sendOwnedClassMessage cls' newSelector

-- | recordTypes
--
-- The record types that need to be present on desired records.
--
-- ObjC selector: @- recordTypes@
recordTypes :: IsHKVerifiableClinicalRecordQuery hkVerifiableClinicalRecordQuery => hkVerifiableClinicalRecordQuery -> IO (Id NSArray)
recordTypes hkVerifiableClinicalRecordQuery =
  sendMessage hkVerifiableClinicalRecordQuery recordTypesSelector

-- | sourceTypes
--
-- The source type(s) of the records.
--
-- ObjC selector: @- sourceTypes@
sourceTypes :: IsHKVerifiableClinicalRecordQuery hkVerifiableClinicalRecordQuery => hkVerifiableClinicalRecordQuery -> IO (Id NSArray)
sourceTypes hkVerifiableClinicalRecordQuery =
  sendMessage hkVerifiableClinicalRecordQuery sourceTypesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKVerifiableClinicalRecordQuery)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id HKVerifiableClinicalRecordQuery)
newSelector = mkSelector "new"

-- | @Selector@ for @recordTypes@
recordTypesSelector :: Selector '[] (Id NSArray)
recordTypesSelector = mkSelector "recordTypes"

-- | @Selector@ for @sourceTypes@
sourceTypesSelector :: Selector '[] (Id NSArray)
sourceTypesSelector = mkSelector "sourceTypes"

