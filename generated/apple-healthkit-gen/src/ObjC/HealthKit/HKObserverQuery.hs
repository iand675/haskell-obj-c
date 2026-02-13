{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKObserverQuery@.
module ObjC.HealthKit.HKObserverQuery
  ( HKObserverQuery
  , IsHKObserverQuery(..)
  , initWithSampleType_predicate_updateHandler
  , initWithSampleType_predicate_updateHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithSampleType:predicate:updateHandler:
--
-- This method installs a handler that is called when a sample type has a new sample added.
--
-- If you have subscribed to background updates you must call the passed completion block                once you have processed data from this notification. Otherwise the system will continue                to notify you of this data.
--
-- ObjC selector: @- initWithSampleType:predicate:updateHandler:@
initWithSampleType_predicate_updateHandler :: (IsHKObserverQuery hkObserverQuery, IsHKSampleType sampleType, IsNSPredicate predicate) => hkObserverQuery -> sampleType -> predicate -> Ptr () -> IO (Id HKObserverQuery)
initWithSampleType_predicate_updateHandler hkObserverQuery sampleType predicate updateHandler =
  sendOwnedMessage hkObserverQuery initWithSampleType_predicate_updateHandlerSelector (toHKSampleType sampleType) (toNSPredicate predicate) updateHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSampleType:predicate:updateHandler:@
initWithSampleType_predicate_updateHandlerSelector :: Selector '[Id HKSampleType, Id NSPredicate, Ptr ()] (Id HKObserverQuery)
initWithSampleType_predicate_updateHandlerSelector = mkSelector "initWithSampleType:predicate:updateHandler:"

