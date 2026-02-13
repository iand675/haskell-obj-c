{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , allowedUnitsSelector
  , allowsFractionalUnitsSelector
  , calendarSelector
  , collapsesLargestUnitSelector
  , formattingContextSelector
  , getObjectValue_forString_errorDescriptionSelector
  , includesApproximationPhraseSelector
  , includesTimeRemainingPhraseSelector
  , localizedStringFromDateComponents_unitsStyleSelector
  , maximumUnitCountSelector
  , referenceDateSelector
  , setAllowedUnitsSelector
  , setAllowsFractionalUnitsSelector
  , setCalendarSelector
  , setCollapsesLargestUnitSelector
  , setFormattingContextSelector
  , setIncludesApproximationPhraseSelector
  , setIncludesTimeRemainingPhraseSelector
  , setMaximumUnitCountSelector
  , setReferenceDateSelector
  , setUnitsStyleSelector
  , setZeroFormattingBehaviorSelector
  , stringForObjectValueSelector
  , stringFromDateComponentsSelector
  , stringFromDate_toDateSelector
  , stringFromTimeIntervalSelector
  , unitsStyleSelector
  , zeroFormattingBehaviorSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- stringForObjectValue:@
stringForObjectValue :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> RawId -> IO (Id NSString)
stringForObjectValue nsDateComponentsFormatter obj_ =
  sendMessage nsDateComponentsFormatter stringForObjectValueSelector obj_

-- | @- stringFromDateComponents:@
stringFromDateComponents :: (IsNSDateComponentsFormatter nsDateComponentsFormatter, IsNSDateComponents components) => nsDateComponentsFormatter -> components -> IO (Id NSString)
stringFromDateComponents nsDateComponentsFormatter components =
  sendMessage nsDateComponentsFormatter stringFromDateComponentsSelector (toNSDateComponents components)

-- | @- stringFromDate:toDate:@
stringFromDate_toDate :: (IsNSDateComponentsFormatter nsDateComponentsFormatter, IsNSDate startDate, IsNSDate endDate) => nsDateComponentsFormatter -> startDate -> endDate -> IO (Id NSString)
stringFromDate_toDate nsDateComponentsFormatter startDate endDate =
  sendMessage nsDateComponentsFormatter stringFromDate_toDateSelector (toNSDate startDate) (toNSDate endDate)

-- | @- stringFromTimeInterval:@
stringFromTimeInterval :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> CDouble -> IO (Id NSString)
stringFromTimeInterval nsDateComponentsFormatter ti =
  sendMessage nsDateComponentsFormatter stringFromTimeIntervalSelector ti

-- | @+ localizedStringFromDateComponents:unitsStyle:@
localizedStringFromDateComponents_unitsStyle :: IsNSDateComponents components => components -> NSDateComponentsFormatterUnitsStyle -> IO (Id NSString)
localizedStringFromDateComponents_unitsStyle components unitsStyle =
  do
    cls' <- getRequiredClass "NSDateComponentsFormatter"
    sendClassMessage cls' localizedStringFromDateComponents_unitsStyleSelector (toNSDateComponents components) unitsStyle

-- | @- getObjectValue:forString:errorDescription:@
getObjectValue_forString_errorDescription :: (IsNSDateComponentsFormatter nsDateComponentsFormatter, IsNSString string, IsNSString error_) => nsDateComponentsFormatter -> Ptr RawId -> string -> error_ -> IO Bool
getObjectValue_forString_errorDescription nsDateComponentsFormatter obj_ string error_ =
  sendMessage nsDateComponentsFormatter getObjectValue_forString_errorDescriptionSelector obj_ (toNSString string) (toNSString error_)

-- | @- unitsStyle@
unitsStyle :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO NSDateComponentsFormatterUnitsStyle
unitsStyle nsDateComponentsFormatter =
  sendMessage nsDateComponentsFormatter unitsStyleSelector

-- | @- setUnitsStyle:@
setUnitsStyle :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> NSDateComponentsFormatterUnitsStyle -> IO ()
setUnitsStyle nsDateComponentsFormatter value =
  sendMessage nsDateComponentsFormatter setUnitsStyleSelector value

-- | @- allowedUnits@
allowedUnits :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO NSCalendarUnit
allowedUnits nsDateComponentsFormatter =
  sendMessage nsDateComponentsFormatter allowedUnitsSelector

-- | @- setAllowedUnits:@
setAllowedUnits :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> NSCalendarUnit -> IO ()
setAllowedUnits nsDateComponentsFormatter value =
  sendMessage nsDateComponentsFormatter setAllowedUnitsSelector value

-- | @- zeroFormattingBehavior@
zeroFormattingBehavior :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO NSDateComponentsFormatterZeroFormattingBehavior
zeroFormattingBehavior nsDateComponentsFormatter =
  sendMessage nsDateComponentsFormatter zeroFormattingBehaviorSelector

-- | @- setZeroFormattingBehavior:@
setZeroFormattingBehavior :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> NSDateComponentsFormatterZeroFormattingBehavior -> IO ()
setZeroFormattingBehavior nsDateComponentsFormatter value =
  sendMessage nsDateComponentsFormatter setZeroFormattingBehaviorSelector value

-- | @- calendar@
calendar :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO (Id NSCalendar)
calendar nsDateComponentsFormatter =
  sendMessage nsDateComponentsFormatter calendarSelector

-- | @- setCalendar:@
setCalendar :: (IsNSDateComponentsFormatter nsDateComponentsFormatter, IsNSCalendar value) => nsDateComponentsFormatter -> value -> IO ()
setCalendar nsDateComponentsFormatter value =
  sendMessage nsDateComponentsFormatter setCalendarSelector (toNSCalendar value)

-- | @- referenceDate@
referenceDate :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO (Id NSDate)
referenceDate nsDateComponentsFormatter =
  sendMessage nsDateComponentsFormatter referenceDateSelector

-- | @- setReferenceDate:@
setReferenceDate :: (IsNSDateComponentsFormatter nsDateComponentsFormatter, IsNSDate value) => nsDateComponentsFormatter -> value -> IO ()
setReferenceDate nsDateComponentsFormatter value =
  sendMessage nsDateComponentsFormatter setReferenceDateSelector (toNSDate value)

-- | @- allowsFractionalUnits@
allowsFractionalUnits :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO Bool
allowsFractionalUnits nsDateComponentsFormatter =
  sendMessage nsDateComponentsFormatter allowsFractionalUnitsSelector

-- | @- setAllowsFractionalUnits:@
setAllowsFractionalUnits :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> Bool -> IO ()
setAllowsFractionalUnits nsDateComponentsFormatter value =
  sendMessage nsDateComponentsFormatter setAllowsFractionalUnitsSelector value

-- | @- maximumUnitCount@
maximumUnitCount :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO CLong
maximumUnitCount nsDateComponentsFormatter =
  sendMessage nsDateComponentsFormatter maximumUnitCountSelector

-- | @- setMaximumUnitCount:@
setMaximumUnitCount :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> CLong -> IO ()
setMaximumUnitCount nsDateComponentsFormatter value =
  sendMessage nsDateComponentsFormatter setMaximumUnitCountSelector value

-- | @- collapsesLargestUnit@
collapsesLargestUnit :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO Bool
collapsesLargestUnit nsDateComponentsFormatter =
  sendMessage nsDateComponentsFormatter collapsesLargestUnitSelector

-- | @- setCollapsesLargestUnit:@
setCollapsesLargestUnit :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> Bool -> IO ()
setCollapsesLargestUnit nsDateComponentsFormatter value =
  sendMessage nsDateComponentsFormatter setCollapsesLargestUnitSelector value

-- | @- includesApproximationPhrase@
includesApproximationPhrase :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO Bool
includesApproximationPhrase nsDateComponentsFormatter =
  sendMessage nsDateComponentsFormatter includesApproximationPhraseSelector

-- | @- setIncludesApproximationPhrase:@
setIncludesApproximationPhrase :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> Bool -> IO ()
setIncludesApproximationPhrase nsDateComponentsFormatter value =
  sendMessage nsDateComponentsFormatter setIncludesApproximationPhraseSelector value

-- | @- includesTimeRemainingPhrase@
includesTimeRemainingPhrase :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO Bool
includesTimeRemainingPhrase nsDateComponentsFormatter =
  sendMessage nsDateComponentsFormatter includesTimeRemainingPhraseSelector

-- | @- setIncludesTimeRemainingPhrase:@
setIncludesTimeRemainingPhrase :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> Bool -> IO ()
setIncludesTimeRemainingPhrase nsDateComponentsFormatter value =
  sendMessage nsDateComponentsFormatter setIncludesTimeRemainingPhraseSelector value

-- | @- formattingContext@
formattingContext :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> IO NSFormattingContext
formattingContext nsDateComponentsFormatter =
  sendMessage nsDateComponentsFormatter formattingContextSelector

-- | @- setFormattingContext:@
setFormattingContext :: IsNSDateComponentsFormatter nsDateComponentsFormatter => nsDateComponentsFormatter -> NSFormattingContext -> IO ()
setFormattingContext nsDateComponentsFormatter value =
  sendMessage nsDateComponentsFormatter setFormattingContextSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringForObjectValue:@
stringForObjectValueSelector :: Selector '[RawId] (Id NSString)
stringForObjectValueSelector = mkSelector "stringForObjectValue:"

-- | @Selector@ for @stringFromDateComponents:@
stringFromDateComponentsSelector :: Selector '[Id NSDateComponents] (Id NSString)
stringFromDateComponentsSelector = mkSelector "stringFromDateComponents:"

-- | @Selector@ for @stringFromDate:toDate:@
stringFromDate_toDateSelector :: Selector '[Id NSDate, Id NSDate] (Id NSString)
stringFromDate_toDateSelector = mkSelector "stringFromDate:toDate:"

-- | @Selector@ for @stringFromTimeInterval:@
stringFromTimeIntervalSelector :: Selector '[CDouble] (Id NSString)
stringFromTimeIntervalSelector = mkSelector "stringFromTimeInterval:"

-- | @Selector@ for @localizedStringFromDateComponents:unitsStyle:@
localizedStringFromDateComponents_unitsStyleSelector :: Selector '[Id NSDateComponents, NSDateComponentsFormatterUnitsStyle] (Id NSString)
localizedStringFromDateComponents_unitsStyleSelector = mkSelector "localizedStringFromDateComponents:unitsStyle:"

-- | @Selector@ for @getObjectValue:forString:errorDescription:@
getObjectValue_forString_errorDescriptionSelector :: Selector '[Ptr RawId, Id NSString, Id NSString] Bool
getObjectValue_forString_errorDescriptionSelector = mkSelector "getObjectValue:forString:errorDescription:"

-- | @Selector@ for @unitsStyle@
unitsStyleSelector :: Selector '[] NSDateComponentsFormatterUnitsStyle
unitsStyleSelector = mkSelector "unitsStyle"

-- | @Selector@ for @setUnitsStyle:@
setUnitsStyleSelector :: Selector '[NSDateComponentsFormatterUnitsStyle] ()
setUnitsStyleSelector = mkSelector "setUnitsStyle:"

-- | @Selector@ for @allowedUnits@
allowedUnitsSelector :: Selector '[] NSCalendarUnit
allowedUnitsSelector = mkSelector "allowedUnits"

-- | @Selector@ for @setAllowedUnits:@
setAllowedUnitsSelector :: Selector '[NSCalendarUnit] ()
setAllowedUnitsSelector = mkSelector "setAllowedUnits:"

-- | @Selector@ for @zeroFormattingBehavior@
zeroFormattingBehaviorSelector :: Selector '[] NSDateComponentsFormatterZeroFormattingBehavior
zeroFormattingBehaviorSelector = mkSelector "zeroFormattingBehavior"

-- | @Selector@ for @setZeroFormattingBehavior:@
setZeroFormattingBehaviorSelector :: Selector '[NSDateComponentsFormatterZeroFormattingBehavior] ()
setZeroFormattingBehaviorSelector = mkSelector "setZeroFormattingBehavior:"

-- | @Selector@ for @calendar@
calendarSelector :: Selector '[] (Id NSCalendar)
calendarSelector = mkSelector "calendar"

-- | @Selector@ for @setCalendar:@
setCalendarSelector :: Selector '[Id NSCalendar] ()
setCalendarSelector = mkSelector "setCalendar:"

-- | @Selector@ for @referenceDate@
referenceDateSelector :: Selector '[] (Id NSDate)
referenceDateSelector = mkSelector "referenceDate"

-- | @Selector@ for @setReferenceDate:@
setReferenceDateSelector :: Selector '[Id NSDate] ()
setReferenceDateSelector = mkSelector "setReferenceDate:"

-- | @Selector@ for @allowsFractionalUnits@
allowsFractionalUnitsSelector :: Selector '[] Bool
allowsFractionalUnitsSelector = mkSelector "allowsFractionalUnits"

-- | @Selector@ for @setAllowsFractionalUnits:@
setAllowsFractionalUnitsSelector :: Selector '[Bool] ()
setAllowsFractionalUnitsSelector = mkSelector "setAllowsFractionalUnits:"

-- | @Selector@ for @maximumUnitCount@
maximumUnitCountSelector :: Selector '[] CLong
maximumUnitCountSelector = mkSelector "maximumUnitCount"

-- | @Selector@ for @setMaximumUnitCount:@
setMaximumUnitCountSelector :: Selector '[CLong] ()
setMaximumUnitCountSelector = mkSelector "setMaximumUnitCount:"

-- | @Selector@ for @collapsesLargestUnit@
collapsesLargestUnitSelector :: Selector '[] Bool
collapsesLargestUnitSelector = mkSelector "collapsesLargestUnit"

-- | @Selector@ for @setCollapsesLargestUnit:@
setCollapsesLargestUnitSelector :: Selector '[Bool] ()
setCollapsesLargestUnitSelector = mkSelector "setCollapsesLargestUnit:"

-- | @Selector@ for @includesApproximationPhrase@
includesApproximationPhraseSelector :: Selector '[] Bool
includesApproximationPhraseSelector = mkSelector "includesApproximationPhrase"

-- | @Selector@ for @setIncludesApproximationPhrase:@
setIncludesApproximationPhraseSelector :: Selector '[Bool] ()
setIncludesApproximationPhraseSelector = mkSelector "setIncludesApproximationPhrase:"

-- | @Selector@ for @includesTimeRemainingPhrase@
includesTimeRemainingPhraseSelector :: Selector '[] Bool
includesTimeRemainingPhraseSelector = mkSelector "includesTimeRemainingPhrase"

-- | @Selector@ for @setIncludesTimeRemainingPhrase:@
setIncludesTimeRemainingPhraseSelector :: Selector '[Bool] ()
setIncludesTimeRemainingPhraseSelector = mkSelector "setIncludesTimeRemainingPhrase:"

-- | @Selector@ for @formattingContext@
formattingContextSelector :: Selector '[] NSFormattingContext
formattingContextSelector = mkSelector "formattingContext"

-- | @Selector@ for @setFormattingContext:@
setFormattingContextSelector :: Selector '[NSFormattingContext] ()
setFormattingContextSelector = mkSelector "setFormattingContext:"

