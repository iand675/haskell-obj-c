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
  , initSelector
  , initWithStartDateComponents_endDateComponentsSelector
  , initWithStartDateComponents_endDateComponents_recurrenceRuleSelector
  , initWithEKRecurrenceRuleSelector
  , ekRecurrenceRuleSelector
  , startDateComponentsSelector
  , endDateComponentsSelector
  , recurrenceRuleSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.EventKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINDateComponentsRange inDateComponentsRange => inDateComponentsRange -> IO (Id INDateComponentsRange)
init_ inDateComponentsRange  =
  sendMsg inDateComponentsRange (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithStartDateComponents:endDateComponents:@
initWithStartDateComponents_endDateComponents :: (IsINDateComponentsRange inDateComponentsRange, IsNSDateComponents startDateComponents, IsNSDateComponents endDateComponents) => inDateComponentsRange -> startDateComponents -> endDateComponents -> IO (Id INDateComponentsRange)
initWithStartDateComponents_endDateComponents inDateComponentsRange  startDateComponents endDateComponents =
withObjCPtr startDateComponents $ \raw_startDateComponents ->
  withObjCPtr endDateComponents $ \raw_endDateComponents ->
      sendMsg inDateComponentsRange (mkSelector "initWithStartDateComponents:endDateComponents:") (retPtr retVoid) [argPtr (castPtr raw_startDateComponents :: Ptr ()), argPtr (castPtr raw_endDateComponents :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithStartDateComponents:endDateComponents:recurrenceRule:@
initWithStartDateComponents_endDateComponents_recurrenceRule :: (IsINDateComponentsRange inDateComponentsRange, IsNSDateComponents startDateComponents, IsNSDateComponents endDateComponents, IsINRecurrenceRule recurrenceRule) => inDateComponentsRange -> startDateComponents -> endDateComponents -> recurrenceRule -> IO (Id INDateComponentsRange)
initWithStartDateComponents_endDateComponents_recurrenceRule inDateComponentsRange  startDateComponents endDateComponents recurrenceRule =
withObjCPtr startDateComponents $ \raw_startDateComponents ->
  withObjCPtr endDateComponents $ \raw_endDateComponents ->
    withObjCPtr recurrenceRule $ \raw_recurrenceRule ->
        sendMsg inDateComponentsRange (mkSelector "initWithStartDateComponents:endDateComponents:recurrenceRule:") (retPtr retVoid) [argPtr (castPtr raw_startDateComponents :: Ptr ()), argPtr (castPtr raw_endDateComponents :: Ptr ()), argPtr (castPtr raw_recurrenceRule :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithEKRecurrenceRule:@
initWithEKRecurrenceRule :: (IsINDateComponentsRange inDateComponentsRange, IsEKRecurrenceRule recurrenceRule) => inDateComponentsRange -> recurrenceRule -> IO (Id INDateComponentsRange)
initWithEKRecurrenceRule inDateComponentsRange  recurrenceRule =
withObjCPtr recurrenceRule $ \raw_recurrenceRule ->
    sendMsg inDateComponentsRange (mkSelector "initWithEKRecurrenceRule:") (retPtr retVoid) [argPtr (castPtr raw_recurrenceRule :: Ptr ())] >>= ownedObject . castPtr

-- | @- EKRecurrenceRule@
ekRecurrenceRule :: IsINDateComponentsRange inDateComponentsRange => inDateComponentsRange -> IO (Id EKRecurrenceRule)
ekRecurrenceRule inDateComponentsRange  =
  sendMsg inDateComponentsRange (mkSelector "EKRecurrenceRule") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- startDateComponents@
startDateComponents :: IsINDateComponentsRange inDateComponentsRange => inDateComponentsRange -> IO (Id NSDateComponents)
startDateComponents inDateComponentsRange  =
  sendMsg inDateComponentsRange (mkSelector "startDateComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- endDateComponents@
endDateComponents :: IsINDateComponentsRange inDateComponentsRange => inDateComponentsRange -> IO (Id NSDateComponents)
endDateComponents inDateComponentsRange  =
  sendMsg inDateComponentsRange (mkSelector "endDateComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- recurrenceRule@
recurrenceRule :: IsINDateComponentsRange inDateComponentsRange => inDateComponentsRange -> IO (Id INRecurrenceRule)
recurrenceRule inDateComponentsRange  =
  sendMsg inDateComponentsRange (mkSelector "recurrenceRule") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithStartDateComponents:endDateComponents:@
initWithStartDateComponents_endDateComponentsSelector :: Selector
initWithStartDateComponents_endDateComponentsSelector = mkSelector "initWithStartDateComponents:endDateComponents:"

-- | @Selector@ for @initWithStartDateComponents:endDateComponents:recurrenceRule:@
initWithStartDateComponents_endDateComponents_recurrenceRuleSelector :: Selector
initWithStartDateComponents_endDateComponents_recurrenceRuleSelector = mkSelector "initWithStartDateComponents:endDateComponents:recurrenceRule:"

-- | @Selector@ for @initWithEKRecurrenceRule:@
initWithEKRecurrenceRuleSelector :: Selector
initWithEKRecurrenceRuleSelector = mkSelector "initWithEKRecurrenceRule:"

-- | @Selector@ for @EKRecurrenceRule@
ekRecurrenceRuleSelector :: Selector
ekRecurrenceRuleSelector = mkSelector "EKRecurrenceRule"

-- | @Selector@ for @startDateComponents@
startDateComponentsSelector :: Selector
startDateComponentsSelector = mkSelector "startDateComponents"

-- | @Selector@ for @endDateComponents@
endDateComponentsSelector :: Selector
endDateComponentsSelector = mkSelector "endDateComponents"

-- | @Selector@ for @recurrenceRule@
recurrenceRuleSelector :: Selector
recurrenceRuleSelector = mkSelector "recurrenceRule"

