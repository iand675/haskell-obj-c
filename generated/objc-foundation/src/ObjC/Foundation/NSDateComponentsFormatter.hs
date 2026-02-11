{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDateComponentsFormatter@.
module ObjC.Foundation.NSDateComponentsFormatter
  ( NSDateComponentsFormatter
  , IsNSDateComponentsFormatter(..)
  , stringForObjectValue
  , stringFromDateComponents
  , stringFromDate_toDate
  , stringFromTimeInterval
  , localizedStringFromDateComponents_unitsStyle
  , getObjectValue_forString_errorDescription
  , unitsStyle
  , setUnitsStyle
  , allowedUnits
  , setAllowedUnits
  , zeroFormattingBehavior
  , setZeroFormattingBehavior
  , calendar
  , setCalendar
  , referenceDate
  , setReferenceDate
  , allowsFractionalUnits
  , setAllowsFractionalUnits
  , maximumUnitCount
  , setMaximumUnitCount
  , collapsesLargestUnit
  , setCollapsesLargestUnit
  , includesApproximationPhrase
  , setIncludesApproximationPhrase
  , includesTimeRemainingPhrase
  , setIncludesTimeRemainingPhrase
  , formattingContext
  , setFormattingContext
  , stringForObjectValueSelector
  , stringFromDateComponentsSelector
  , stringFromDate_toDateSelector
  , stringFromTimeIntervalSelector
  , localizedStringFromDateComponents_unitsStyleSelector
  , getObjectValue_forString_errorDescriptionSelector
  , unitsStyleSelector
  , setUnitsStyleSelector
  , allowedUnitsSelector
  , setAllowedUnitsSelector
  , zeroFormattingBehaviorSelector
  , setZeroFormattingBehaviorSelector
  , calendarSelector
  , setCalendarSelector
  , referenceDateSelector
  , setReferenceDateSelector
  , allowsFractionalUnitsSelector
  , setAllowsFractionalUnitsSelector
  , maximumUnitCountSelector
  , setMaximumUnitCountSelector
  , collapsesLargestUnitSelector
  , setCollapsesLargestUnitSelector
  , includesApproximationPhraseSelector
  , setIncludesApproximationPhraseSelector
  , includesTimeRemainingPhraseSelector
  , setIncludesTimeRemainingPhraseSelector
  , formattingContextSelector
  , setFormattingContextSelector

  -- * Enum types
  , NSCalendarUnit(NSCalendarUnit)
  , pattern NSCalendarUnitEra
  , pattern NSCalendarUnitYear
  , pattern NSCalendarUnitMonth
  , pattern NSCalendarUnitDay
  , pattern NSCalendarUnitHour
  , pattern NSCalendarUnitMinute
  , pattern NSCalendarUnitSecond
  , pattern NSCalendarUnitWeekday
  , pattern NSCalendarUnitWeekdayOrdinal
  , pattern NSCalendarUnitQuarter
  , pattern NSCalendarUnitWeekOfMonth
  , pattern NSCalendarUnitWeekOfYear
  , pattern NSCalendarUnitYearForWeekOfYear
  , pattern NSCalendarUnitNanosecond
  , pattern NSCalendarUnitDayOfYear
  , pattern NSCalendarUnitCalendar
  , pattern NSCalendarUnitTimeZone
  , pattern NSCalendarUnitIsLeapMonth
  , pattern NSCalendarUnitIsRepeatedDay
  , pattern NSEraCalendarUnit
  , pattern NSYearCalendarUnit
  , pattern NSMonthCalendarUnit
  , pattern NSDayCalendarUnit
  , pattern NSHourCalendarUnit
  , pattern NSMinuteCalendarUnit
  , pattern NSSecondCalendarUnit
  , pattern NSWeekCalendarUnit
  , pattern NSWeekdayCalendarUnit
  , pattern NSWeekdayOrdinalCalendarUnit
  , pattern NSQuarterCalendarUnit
  , pattern NSWeekOfMonthCalendarUnit
  , pattern NSWeekOfYearCalendarUnit
  , pattern NSYearForWeekOfYearCalendarUnit
  , pattern NSCalendarCalendarUnit
  , pattern NSTimeZoneCalendarUnit
  , NSDateComponentsFormatterUnitsStyle(NSDateComponentsFormatterUnitsStyle)
  , pattern NSDateComponentsFormatterUnitsStylePositional
  , pattern NSDateComponentsFormatterUnitsStyleAbbreviated
  , pattern NSDateComponentsFormatterUnitsStyleShort
  , pattern NSDateComponentsFormatterUnitsStyleFull
  , pattern NSDateComponentsFormatterUnitsStyleSpellOut
  , pattern NSDateComponentsFormatterUnitsStyleBrief
  , NSDateComponentsFormatterZeroFormattingBehavior(NSDateComponentsFormatterZeroFormattingBehavior)
  , pattern NSDateComponentsFormatterZeroFormattingBehaviorNone
  , pattern NSDateComponentsFormatterZeroFormattingBehaviorDefault
  , pattern NSDateComponentsFormatterZeroFormattingBehaviorDropLeading
  , pattern NSDateComponentsFormatterZeroFormattingBehaviorDropMiddle
  , pattern NSDateComponentsFormatterZeroFormattingBehaviorDropTrailing
  , pattern NSDateComponentsFormatterZeroFormattingBehaviorDropAll
  , pattern NSDateComponentsFormatterZeroFormattingBehaviorPad
  , NSFormattingContext(NSFormattingContext)
  , pattern NSFormattingContextUnknown
  , pattern NSFormattingContextDynamic
  , pattern NSFormattingContextStandalone
  , pattern NSFormattingContextListItem
  , pattern NSFormattingContextBeginningOfSentence
  , pattern NSFormattingContextMiddleOfSentence

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- stringForObjectValue:@
stringForObjectValue :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> RawId -> IO (Id NSString)
stringForObjectValue nsDateComponentsFormatter  obj_ =
  sendMsg nsDateComponentsFormatter (mkSelector "stringForObjectValue:") (retPtr retVoid) [argPtr (castPtr (unRawId obj_) :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringFromDateComponents:@
stringFromDateComponents :: (IsNSDateComponentsFormatter nsDateComponentsFormatter, IsNSDateComponents components) => nsDateComponentsFormatter -> components -> IO (Id NSString)
stringFromDateComponents nsDateComponentsFormatter  components =
withObjCPtr components $ \raw_components ->
    sendMsg nsDateComponentsFormatter (mkSelector "stringFromDateComponents:") (retPtr retVoid) [argPtr (castPtr raw_components :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringFromDate:toDate:@
stringFromDate_toDate :: (IsNSDateComponentsFormatter nsDateComponentsFormatter, IsNSDate startDate, IsNSDate endDate) => nsDateComponentsFormatter -> startDate -> endDate -> IO (Id NSString)
stringFromDate_toDate nsDateComponentsFormatter  startDate endDate =
withObjCPtr startDate $ \raw_startDate ->
  withObjCPtr endDate $ \raw_endDate ->
      sendMsg nsDateComponentsFormatter (mkSelector "stringFromDate:toDate:") (retPtr retVoid) [argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringFromTimeInterval:@
stringFromTimeInterval :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> CDouble -> IO (Id NSString)
stringFromTimeInterval nsDateComponentsFormatter  ti =
  sendMsg nsDateComponentsFormatter (mkSelector "stringFromTimeInterval:") (retPtr retVoid) [argCDouble (fromIntegral ti)] >>= retainedObject . castPtr

-- | @+ localizedStringFromDateComponents:unitsStyle:@
localizedStringFromDateComponents_unitsStyle :: IsNSDateComponents components => components -> NSDateComponentsFormatterUnitsStyle -> IO (Id NSString)
localizedStringFromDateComponents_unitsStyle components unitsStyle =
  do
    cls' <- getRequiredClass "NSDateComponentsFormatter"
    withObjCPtr components $ \raw_components ->
      sendClassMsg cls' (mkSelector "localizedStringFromDateComponents:unitsStyle:") (retPtr retVoid) [argPtr (castPtr raw_components :: Ptr ()), argCLong (coerce unitsStyle)] >>= retainedObject . castPtr

-- | @- getObjectValue:forString:errorDescription:@
getObjectValue_forString_errorDescription :: (IsNSDateComponentsFormatter nsDateComponentsFormatter, IsNSString string, IsNSString error_) => nsDateComponentsFormatter -> Ptr RawId -> string -> error_ -> IO Bool
getObjectValue_forString_errorDescription nsDateComponentsFormatter  obj_ string error_ =
withObjCPtr string $ \raw_string ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDateComponentsFormatter (mkSelector "getObjectValue:forString:errorDescription:") retCULong [argPtr obj_, argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- unitsStyle@
unitsStyle :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO NSDateComponentsFormatterUnitsStyle
unitsStyle nsDateComponentsFormatter  =
  fmap (coerce :: CLong -> NSDateComponentsFormatterUnitsStyle) $ sendMsg nsDateComponentsFormatter (mkSelector "unitsStyle") retCLong []

-- | @- setUnitsStyle:@
setUnitsStyle :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> NSDateComponentsFormatterUnitsStyle -> IO ()
setUnitsStyle nsDateComponentsFormatter  value =
  sendMsg nsDateComponentsFormatter (mkSelector "setUnitsStyle:") retVoid [argCLong (coerce value)]

-- | @- allowedUnits@
allowedUnits :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO NSCalendarUnit
allowedUnits nsDateComponentsFormatter  =
  fmap (coerce :: CULong -> NSCalendarUnit) $ sendMsg nsDateComponentsFormatter (mkSelector "allowedUnits") retCULong []

-- | @- setAllowedUnits:@
setAllowedUnits :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> NSCalendarUnit -> IO ()
setAllowedUnits nsDateComponentsFormatter  value =
  sendMsg nsDateComponentsFormatter (mkSelector "setAllowedUnits:") retVoid [argCULong (coerce value)]

-- | @- zeroFormattingBehavior@
zeroFormattingBehavior :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO NSDateComponentsFormatterZeroFormattingBehavior
zeroFormattingBehavior nsDateComponentsFormatter  =
  fmap (coerce :: CULong -> NSDateComponentsFormatterZeroFormattingBehavior) $ sendMsg nsDateComponentsFormatter (mkSelector "zeroFormattingBehavior") retCULong []

-- | @- setZeroFormattingBehavior:@
setZeroFormattingBehavior :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> NSDateComponentsFormatterZeroFormattingBehavior -> IO ()
setZeroFormattingBehavior nsDateComponentsFormatter  value =
  sendMsg nsDateComponentsFormatter (mkSelector "setZeroFormattingBehavior:") retVoid [argCULong (coerce value)]

-- | @- calendar@
calendar :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO (Id NSCalendar)
calendar nsDateComponentsFormatter  =
  sendMsg nsDateComponentsFormatter (mkSelector "calendar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCalendar:@
setCalendar :: (IsNSDateComponentsFormatter nsDateComponentsFormatter, IsNSCalendar value) => nsDateComponentsFormatter -> value -> IO ()
setCalendar nsDateComponentsFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateComponentsFormatter (mkSelector "setCalendar:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- referenceDate@
referenceDate :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO (Id NSDate)
referenceDate nsDateComponentsFormatter  =
  sendMsg nsDateComponentsFormatter (mkSelector "referenceDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReferenceDate:@
setReferenceDate :: (IsNSDateComponentsFormatter nsDateComponentsFormatter, IsNSDate value) => nsDateComponentsFormatter -> value -> IO ()
setReferenceDate nsDateComponentsFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDateComponentsFormatter (mkSelector "setReferenceDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- allowsFractionalUnits@
allowsFractionalUnits :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO Bool
allowsFractionalUnits nsDateComponentsFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDateComponentsFormatter (mkSelector "allowsFractionalUnits") retCULong []

-- | @- setAllowsFractionalUnits:@
setAllowsFractionalUnits :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> Bool -> IO ()
setAllowsFractionalUnits nsDateComponentsFormatter  value =
  sendMsg nsDateComponentsFormatter (mkSelector "setAllowsFractionalUnits:") retVoid [argCULong (if value then 1 else 0)]

-- | @- maximumUnitCount@
maximumUnitCount :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO CLong
maximumUnitCount nsDateComponentsFormatter  =
  sendMsg nsDateComponentsFormatter (mkSelector "maximumUnitCount") retCLong []

-- | @- setMaximumUnitCount:@
setMaximumUnitCount :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> CLong -> IO ()
setMaximumUnitCount nsDateComponentsFormatter  value =
  sendMsg nsDateComponentsFormatter (mkSelector "setMaximumUnitCount:") retVoid [argCLong (fromIntegral value)]

-- | @- collapsesLargestUnit@
collapsesLargestUnit :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO Bool
collapsesLargestUnit nsDateComponentsFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDateComponentsFormatter (mkSelector "collapsesLargestUnit") retCULong []

-- | @- setCollapsesLargestUnit:@
setCollapsesLargestUnit :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> Bool -> IO ()
setCollapsesLargestUnit nsDateComponentsFormatter  value =
  sendMsg nsDateComponentsFormatter (mkSelector "setCollapsesLargestUnit:") retVoid [argCULong (if value then 1 else 0)]

-- | @- includesApproximationPhrase@
includesApproximationPhrase :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO Bool
includesApproximationPhrase nsDateComponentsFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDateComponentsFormatter (mkSelector "includesApproximationPhrase") retCULong []

-- | @- setIncludesApproximationPhrase:@
setIncludesApproximationPhrase :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> Bool -> IO ()
setIncludesApproximationPhrase nsDateComponentsFormatter  value =
  sendMsg nsDateComponentsFormatter (mkSelector "setIncludesApproximationPhrase:") retVoid [argCULong (if value then 1 else 0)]

-- | @- includesTimeRemainingPhrase@
includesTimeRemainingPhrase :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO Bool
includesTimeRemainingPhrase nsDateComponentsFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDateComponentsFormatter (mkSelector "includesTimeRemainingPhrase") retCULong []

-- | @- setIncludesTimeRemainingPhrase:@
setIncludesTimeRemainingPhrase :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> Bool -> IO ()
setIncludesTimeRemainingPhrase nsDateComponentsFormatter  value =
  sendMsg nsDateComponentsFormatter (mkSelector "setIncludesTimeRemainingPhrase:") retVoid [argCULong (if value then 1 else 0)]

-- | @- formattingContext@
formattingContext :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO NSFormattingContext
formattingContext nsDateComponentsFormatter  =
  fmap (coerce :: CLong -> NSFormattingContext) $ sendMsg nsDateComponentsFormatter (mkSelector "formattingContext") retCLong []

-- | @- setFormattingContext:@
setFormattingContext :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> NSFormattingContext -> IO ()
setFormattingContext nsDateComponentsFormatter  value =
  sendMsg nsDateComponentsFormatter (mkSelector "setFormattingContext:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringForObjectValue:@
stringForObjectValueSelector :: Selector
stringForObjectValueSelector = mkSelector "stringForObjectValue:"

-- | @Selector@ for @stringFromDateComponents:@
stringFromDateComponentsSelector :: Selector
stringFromDateComponentsSelector = mkSelector "stringFromDateComponents:"

-- | @Selector@ for @stringFromDate:toDate:@
stringFromDate_toDateSelector :: Selector
stringFromDate_toDateSelector = mkSelector "stringFromDate:toDate:"

-- | @Selector@ for @stringFromTimeInterval:@
stringFromTimeIntervalSelector :: Selector
stringFromTimeIntervalSelector = mkSelector "stringFromTimeInterval:"

-- | @Selector@ for @localizedStringFromDateComponents:unitsStyle:@
localizedStringFromDateComponents_unitsStyleSelector :: Selector
localizedStringFromDateComponents_unitsStyleSelector = mkSelector "localizedStringFromDateComponents:unitsStyle:"

-- | @Selector@ for @getObjectValue:forString:errorDescription:@
getObjectValue_forString_errorDescriptionSelector :: Selector
getObjectValue_forString_errorDescriptionSelector = mkSelector "getObjectValue:forString:errorDescription:"

-- | @Selector@ for @unitsStyle@
unitsStyleSelector :: Selector
unitsStyleSelector = mkSelector "unitsStyle"

-- | @Selector@ for @setUnitsStyle:@
setUnitsStyleSelector :: Selector
setUnitsStyleSelector = mkSelector "setUnitsStyle:"

-- | @Selector@ for @allowedUnits@
allowedUnitsSelector :: Selector
allowedUnitsSelector = mkSelector "allowedUnits"

-- | @Selector@ for @setAllowedUnits:@
setAllowedUnitsSelector :: Selector
setAllowedUnitsSelector = mkSelector "setAllowedUnits:"

-- | @Selector@ for @zeroFormattingBehavior@
zeroFormattingBehaviorSelector :: Selector
zeroFormattingBehaviorSelector = mkSelector "zeroFormattingBehavior"

-- | @Selector@ for @setZeroFormattingBehavior:@
setZeroFormattingBehaviorSelector :: Selector
setZeroFormattingBehaviorSelector = mkSelector "setZeroFormattingBehavior:"

-- | @Selector@ for @calendar@
calendarSelector :: Selector
calendarSelector = mkSelector "calendar"

-- | @Selector@ for @setCalendar:@
setCalendarSelector :: Selector
setCalendarSelector = mkSelector "setCalendar:"

-- | @Selector@ for @referenceDate@
referenceDateSelector :: Selector
referenceDateSelector = mkSelector "referenceDate"

-- | @Selector@ for @setReferenceDate:@
setReferenceDateSelector :: Selector
setReferenceDateSelector = mkSelector "setReferenceDate:"

-- | @Selector@ for @allowsFractionalUnits@
allowsFractionalUnitsSelector :: Selector
allowsFractionalUnitsSelector = mkSelector "allowsFractionalUnits"

-- | @Selector@ for @setAllowsFractionalUnits:@
setAllowsFractionalUnitsSelector :: Selector
setAllowsFractionalUnitsSelector = mkSelector "setAllowsFractionalUnits:"

-- | @Selector@ for @maximumUnitCount@
maximumUnitCountSelector :: Selector
maximumUnitCountSelector = mkSelector "maximumUnitCount"

-- | @Selector@ for @setMaximumUnitCount:@
setMaximumUnitCountSelector :: Selector
setMaximumUnitCountSelector = mkSelector "setMaximumUnitCount:"

-- | @Selector@ for @collapsesLargestUnit@
collapsesLargestUnitSelector :: Selector
collapsesLargestUnitSelector = mkSelector "collapsesLargestUnit"

-- | @Selector@ for @setCollapsesLargestUnit:@
setCollapsesLargestUnitSelector :: Selector
setCollapsesLargestUnitSelector = mkSelector "setCollapsesLargestUnit:"

-- | @Selector@ for @includesApproximationPhrase@
includesApproximationPhraseSelector :: Selector
includesApproximationPhraseSelector = mkSelector "includesApproximationPhrase"

-- | @Selector@ for @setIncludesApproximationPhrase:@
setIncludesApproximationPhraseSelector :: Selector
setIncludesApproximationPhraseSelector = mkSelector "setIncludesApproximationPhrase:"

-- | @Selector@ for @includesTimeRemainingPhrase@
includesTimeRemainingPhraseSelector :: Selector
includesTimeRemainingPhraseSelector = mkSelector "includesTimeRemainingPhrase"

-- | @Selector@ for @setIncludesTimeRemainingPhrase:@
setIncludesTimeRemainingPhraseSelector :: Selector
setIncludesTimeRemainingPhraseSelector = mkSelector "setIncludesTimeRemainingPhrase:"

-- | @Selector@ for @formattingContext@
formattingContextSelector :: Selector
formattingContextSelector = mkSelector "formattingContext"

-- | @Selector@ for @setFormattingContext:@
setFormattingContextSelector :: Selector
setFormattingContextSelector = mkSelector "setFormattingContext:"

