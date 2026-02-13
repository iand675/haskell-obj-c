{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKUserAnnotatedMedicationQuery@.
module ObjC.HealthKit.HKUserAnnotatedMedicationQuery
  ( HKUserAnnotatedMedicationQuery
  , IsHKUserAnnotatedMedicationQuery(..)
  , initWithPredicate_limit_resultsHandler
  , initWithPredicate_limit_resultsHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithPredicate:limit:resultsHandler:
--
-- Returns a query that will retrieve HKUserAnnotatedMedications matching the given predicate and limit.
--
-- @predicate@ — The predicate which user annotated medications should match.
--
-- @limit@ — The maximum number of  user annotated medications to return.  Pass HKObjectQueryNoLimit for no limit.
--
-- @resultsHandler@ — The block to invoke with results to deliver to the client. The results handler will be called with done = YES when there are no more user annotated medications to enumerate.
--
-- ObjC selector: @- initWithPredicate:limit:resultsHandler:@
initWithPredicate_limit_resultsHandler :: (IsHKUserAnnotatedMedicationQuery hkUserAnnotatedMedicationQuery, IsNSPredicate predicate) => hkUserAnnotatedMedicationQuery -> predicate -> CULong -> Ptr () -> IO (Id HKUserAnnotatedMedicationQuery)
initWithPredicate_limit_resultsHandler hkUserAnnotatedMedicationQuery predicate limit resultsHandler =
  sendOwnedMessage hkUserAnnotatedMedicationQuery initWithPredicate_limit_resultsHandlerSelector (toNSPredicate predicate) limit resultsHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPredicate:limit:resultsHandler:@
initWithPredicate_limit_resultsHandlerSelector :: Selector '[Id NSPredicate, CULong, Ptr ()] (Id HKUserAnnotatedMedicationQuery)
initWithPredicate_limit_resultsHandlerSelector = mkSelector "initWithPredicate:limit:resultsHandler:"

