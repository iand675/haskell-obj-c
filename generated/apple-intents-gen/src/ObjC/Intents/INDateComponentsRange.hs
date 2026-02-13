{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INDateComponentsRange@.
module ObjC.Intents.INDateComponentsRange
  ( INDateComponentsRange
  , IsINDateComponentsRange(..)
  , init_
  , initWithStartDateComponents_endDateComponents
  , initWithStartDateComponents_endDateComponents_recurrenceRule
  , initWithEKRecurrenceRule
  , ekRecurrenceRule
  , startDateComponents
  , endDateComponents
  , recurrenceRule
  , ekRecurrenceRuleSelector
  , endDateComponentsSelector
  , initSelector
  , initWithEKRecurrenceRuleSelector
  , initWithStartDateComponents_endDateComponentsSelector
  , initWithStartDateComponents_endDateComponents_recurrenceRuleSelector
  , recurrenceRuleSelector
  , startDateComponentsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.EventKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINDateComponentsRange inDateComponentsRange => inDateComponentsRange -> IO (Id INDateComponentsRange)
init_ inDateComponentsRange =
  sendOwnedMessage inDateComponentsRange initSelector

-- | @- initWithStartDateComponents:endDateComponents:@
initWithStartDateComponents_endDateComponents :: (IsINDateComponentsRange inDateComponentsRange, IsNSDateComponents startDateComponents, IsNSDateComponents endDateComponents) => inDateComponentsRange -> startDateComponents -> endDateComponents -> IO (Id INDateComponentsRange)
initWithStartDateComponents_endDateComponents inDateComponentsRange startDateComponents endDateComponents =
  sendOwnedMessage inDateComponentsRange initWithStartDateComponents_endDateComponentsSelector (toNSDateComponents startDateComponents) (toNSDateComponents endDateComponents)

-- | @- initWithStartDateComponents:endDateComponents:recurrenceRule:@
initWithStartDateComponents_endDateComponents_recurrenceRule :: (IsINDateComponentsRange inDateComponentsRange, IsNSDateComponents startDateComponents, IsNSDateComponents endDateComponents, IsINRecurrenceRule recurrenceRule) => inDateComponentsRange -> startDateComponents -> endDateComponents -> recurrenceRule -> IO (Id INDateComponentsRange)
initWithStartDateComponents_endDateComponents_recurrenceRule inDateComponentsRange startDateComponents endDateComponents recurrenceRule =
  sendOwnedMessage inDateComponentsRange initWithStartDateComponents_endDateComponents_recurrenceRuleSelector (toNSDateComponents startDateComponents) (toNSDateComponents endDateComponents) (toINRecurrenceRule recurrenceRule)

-- | @- initWithEKRecurrenceRule:@
initWithEKRecurrenceRule :: (IsINDateComponentsRange inDateComponentsRange, IsEKRecurrenceRule recurrenceRule) => inDateComponentsRange -> recurrenceRule -> IO (Id INDateComponentsRange)
initWithEKRecurrenceRule inDateComponentsRange recurrenceRule =
  sendOwnedMessage inDateComponentsRange initWithEKRecurrenceRuleSelector (toEKRecurrenceRule recurrenceRule)

-- | @- EKRecurrenceRule@
ekRecurrenceRule :: IsINDateComponentsRange inDateComponentsRange => inDateComponentsRange -> IO (Id EKRecurrenceRule)
ekRecurrenceRule inDateComponentsRange =
  sendMessage inDateComponentsRange ekRecurrenceRuleSelector

-- | @- startDateComponents@
startDateComponents :: IsINDateComponentsRange inDateComponentsRange => inDateComponentsRange -> IO (Id NSDateComponents)
startDateComponents inDateComponentsRange =
  sendMessage inDateComponentsRange startDateComponentsSelector

-- | @- endDateComponents@
endDateComponents :: IsINDateComponentsRange inDateComponentsRange => inDateComponentsRange -> IO (Id NSDateComponents)
endDateComponents inDateComponentsRange =
  sendMessage inDateComponentsRange endDateComponentsSelector

-- | @- recurrenceRule@
recurrenceRule :: IsINDateComponentsRange inDateComponentsRange => inDateComponentsRange -> IO (Id INRecurrenceRule)
recurrenceRule inDateComponentsRange =
  sendMessage inDateComponentsRange recurrenceRuleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INDateComponentsRange)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithStartDateComponents:endDateComponents:@
initWithStartDateComponents_endDateComponentsSelector :: Selector '[Id NSDateComponents, Id NSDateComponents] (Id INDateComponentsRange)
initWithStartDateComponents_endDateComponentsSelector = mkSelector "initWithStartDateComponents:endDateComponents:"

-- | @Selector@ for @initWithStartDateComponents:endDateComponents:recurrenceRule:@
initWithStartDateComponents_endDateComponents_recurrenceRuleSelector :: Selector '[Id NSDateComponents, Id NSDateComponents, Id INRecurrenceRule] (Id INDateComponentsRange)
initWithStartDateComponents_endDateComponents_recurrenceRuleSelector = mkSelector "initWithStartDateComponents:endDateComponents:recurrenceRule:"

-- | @Selector@ for @initWithEKRecurrenceRule:@
initWithEKRecurrenceRuleSelector :: Selector '[Id EKRecurrenceRule] (Id INDateComponentsRange)
initWithEKRecurrenceRuleSelector = mkSelector "initWithEKRecurrenceRule:"

-- | @Selector@ for @EKRecurrenceRule@
ekRecurrenceRuleSelector :: Selector '[] (Id EKRecurrenceRule)
ekRecurrenceRuleSelector = mkSelector "EKRecurrenceRule"

-- | @Selector@ for @startDateComponents@
startDateComponentsSelector :: Selector '[] (Id NSDateComponents)
startDateComponentsSelector = mkSelector "startDateComponents"

-- | @Selector@ for @endDateComponents@
endDateComponentsSelector :: Selector '[] (Id NSDateComponents)
endDateComponentsSelector = mkSelector "endDateComponents"

-- | @Selector@ for @recurrenceRule@
recurrenceRuleSelector :: Selector '[] (Id INRecurrenceRule)
recurrenceRuleSelector = mkSelector "recurrenceRule"

