{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | EKRecurrenceEnd
--
-- Class which represents when a recurrence should end.
--
-- EKRecurrenceEnd is an attribute of EKRecurrenceRule that defines how long                the recurrence is scheduled to repeat. The recurrence can be defined either                with an NSUInteger that indicates the total number times it repeats, or with                an NSDate, after which it no longer repeats. An event which is set to never                end should have its EKRecurrenceEnd set to nil.
--
-- If the end of the pattern is defines with an NSDate, the client must pass a                valid NSDate, nil cannot be passed. If the end of the pattern is defined as                terms of a number of occurrences, the occurrenceCount passed to the initializer                must be positive, it cannot be 0. If the client attempts to initialize a                EKRecurrenceEnd with a nil NSDate or OccurrenceCount of 0, an exception is raised.
--
-- A EKRecurrenceEnd initialized with an end date will return 0 for occurrenceCount.                One initialized with a number of occurrences will return nil for its endDate.
--
-- Generated bindings for @EKRecurrenceEnd@.
module ObjC.EventKit.EKRecurrenceEnd
  ( EKRecurrenceEnd
  , IsEKRecurrenceEnd(..)
  , recurrenceEndWithEndDate
  , recurrenceEndWithOccurrenceCount
  , endDate
  , occurrenceCount
  , recurrenceEndWithEndDateSelector
  , recurrenceEndWithOccurrenceCountSelector
  , endDateSelector
  , occurrenceCountSelector


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

import ObjC.EventKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | recurrenceEndWithEndDate:
--
-- Creates an autoreleased recurrence end with a specific end date.
--
-- ObjC selector: @+ recurrenceEndWithEndDate:@
recurrenceEndWithEndDate :: IsNSDate endDate => endDate -> IO (Id EKRecurrenceEnd)
recurrenceEndWithEndDate endDate =
  do
    cls' <- getRequiredClass "EKRecurrenceEnd"
    withObjCPtr endDate $ \raw_endDate ->
      sendClassMsg cls' (mkSelector "recurrenceEndWithEndDate:") (retPtr retVoid) [argPtr (castPtr raw_endDate :: Ptr ())] >>= retainedObject . castPtr

-- | recurrenceEndWithOccurrenceCount:
--
-- Creates an autoreleased recurrence end with a maximum occurrence count.
--
-- ObjC selector: @+ recurrenceEndWithOccurrenceCount:@
recurrenceEndWithOccurrenceCount :: CULong -> IO (Id EKRecurrenceEnd)
recurrenceEndWithOccurrenceCount occurrenceCount =
  do
    cls' <- getRequiredClass "EKRecurrenceEnd"
    sendClassMsg cls' (mkSelector "recurrenceEndWithOccurrenceCount:") (retPtr retVoid) [argCULong (fromIntegral occurrenceCount)] >>= retainedObject . castPtr

-- | endDate
--
-- The end date of this recurrence, or nil if it's count-based.
--
-- ObjC selector: @- endDate@
endDate :: IsEKRecurrenceEnd ekRecurrenceEnd => ekRecurrenceEnd -> IO (Id NSDate)
endDate ekRecurrenceEnd  =
  sendMsg ekRecurrenceEnd (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | occurrenceCount
--
-- The maximum occurrence count, or 0 if it's date-based.
--
-- ObjC selector: @- occurrenceCount@
occurrenceCount :: IsEKRecurrenceEnd ekRecurrenceEnd => ekRecurrenceEnd -> IO CULong
occurrenceCount ekRecurrenceEnd  =
  sendMsg ekRecurrenceEnd (mkSelector "occurrenceCount") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @recurrenceEndWithEndDate:@
recurrenceEndWithEndDateSelector :: Selector
recurrenceEndWithEndDateSelector = mkSelector "recurrenceEndWithEndDate:"

-- | @Selector@ for @recurrenceEndWithOccurrenceCount:@
recurrenceEndWithOccurrenceCountSelector :: Selector
recurrenceEndWithOccurrenceCountSelector = mkSelector "recurrenceEndWithOccurrenceCount:"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @occurrenceCount@
occurrenceCountSelector :: Selector
occurrenceCountSelector = mkSelector "occurrenceCount"

