{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CalRecurrenceEnd@.
module ObjC.CalendarStore.CalRecurrenceEnd
  ( CalRecurrenceEnd
  , IsCalRecurrenceEnd(..)
  , recurrenceEndWithEndDate
  , recurrenceEndWithOccurrenceCount
  , usesEndDate
  , endDate
  , occurrenceCount
  , recurrenceEndWithEndDateSelector
  , recurrenceEndWithOccurrenceCountSelector
  , usesEndDateSelector
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

import ObjC.CalendarStore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ recurrenceEndWithEndDate:@
recurrenceEndWithEndDate :: IsNSDate endDate => endDate -> IO RawId
recurrenceEndWithEndDate endDate =
  do
    cls' <- getRequiredClass "CalRecurrenceEnd"
    withObjCPtr endDate $ \raw_endDate ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "recurrenceEndWithEndDate:") (retPtr retVoid) [argPtr (castPtr raw_endDate :: Ptr ())]

-- | @+ recurrenceEndWithOccurrenceCount:@
recurrenceEndWithOccurrenceCount :: CULong -> IO RawId
recurrenceEndWithOccurrenceCount occurrenceCount =
  do
    cls' <- getRequiredClass "CalRecurrenceEnd"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "recurrenceEndWithOccurrenceCount:") (retPtr retVoid) [argCULong (fromIntegral occurrenceCount)]

-- | @- usesEndDate@
usesEndDate :: IsCalRecurrenceEnd calRecurrenceEnd => calRecurrenceEnd -> IO Bool
usesEndDate calRecurrenceEnd  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg calRecurrenceEnd (mkSelector "usesEndDate") retCULong []

-- | @- endDate@
endDate :: IsCalRecurrenceEnd calRecurrenceEnd => calRecurrenceEnd -> IO (Id NSDate)
endDate calRecurrenceEnd  =
  sendMsg calRecurrenceEnd (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- occurrenceCount@
occurrenceCount :: IsCalRecurrenceEnd calRecurrenceEnd => calRecurrenceEnd -> IO CULong
occurrenceCount calRecurrenceEnd  =
  sendMsg calRecurrenceEnd (mkSelector "occurrenceCount") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @recurrenceEndWithEndDate:@
recurrenceEndWithEndDateSelector :: Selector
recurrenceEndWithEndDateSelector = mkSelector "recurrenceEndWithEndDate:"

-- | @Selector@ for @recurrenceEndWithOccurrenceCount:@
recurrenceEndWithOccurrenceCountSelector :: Selector
recurrenceEndWithOccurrenceCountSelector = mkSelector "recurrenceEndWithOccurrenceCount:"

-- | @Selector@ for @usesEndDate@
usesEndDateSelector :: Selector
usesEndDateSelector = mkSelector "usesEndDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @occurrenceCount@
occurrenceCountSelector :: Selector
occurrenceCountSelector = mkSelector "occurrenceCount"

