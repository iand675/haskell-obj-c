{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKQuery@.
module ObjC.HealthKit.HKQuery
  ( HKQuery
  , IsHKQuery(..)
  , init_
  , predicateForUserAnnotatedMedicationsWithIsArchived
  , predicateForUserAnnotatedMedicationsWithHasSchedule
  , predicateForMedicationDoseEventWithStatus
  , predicateForMedicationDoseEventWithStatuses
  , predicateForMedicationDoseEventWithScheduledDate
  , predicateForMedicationDoseEventWithScheduledDates
  , predicateForMedicationDoseEventWithScheduledStartDate_endDate
  , predicateForMedicationDoseEventWithMedicationConceptIdentifier
  , predicateForMedicationDoseEventWithMedicationConceptIdentifiers
  , predicateForStatesOfMindWithValence_operatorType
  , predicateForStatesOfMindWithKind
  , predicateForStatesOfMindWithLabel
  , predicateForStatesOfMindWithAssociation
  , predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateInterval
  , predicateForElectrocardiogramsWithClassification
  , predicateForElectrocardiogramsWithSymptomsStatus
  , predicateForClinicalRecordsWithFHIRResourceType
  , predicateForClinicalRecordsFromSource_FHIRResourceType_identifier
  , predicateForActivitySummaryWithDateComponents
  , predicateForActivitySummariesBetweenStartDateComponents_endDateComponents
  , predicateForWorkoutActivitiesWithWorkoutActivityType
  , predicateForWorkoutActivitiesWithOperatorType_duration
  , predicateForWorkoutActivitiesWithStartDate_endDate_options
  , predicateForWorkoutActivitiesWithOperatorType_quantityType_sumQuantity
  , predicateForWorkoutActivitiesWithOperatorType_quantityType_minimumQuantity
  , predicateForWorkoutActivitiesWithOperatorType_quantityType_maximumQuantity
  , predicateForWorkoutActivitiesWithOperatorType_quantityType_averageQuantity
  , predicateForWorkoutsWithActivityPredicate
  , predicateForWorkoutsWithWorkoutActivityType
  , predicateForWorkoutsWithOperatorType_duration
  , predicateForWorkoutsWithOperatorType_totalEnergyBurned
  , predicateForWorkoutsWithOperatorType_totalDistance
  , predicateForWorkoutsWithOperatorType_totalSwimmingStrokeCount
  , predicateForWorkoutsWithOperatorType_totalFlightsClimbed
  , predicateForWorkoutsWithOperatorType_quantityType_sumQuantity
  , predicateForWorkoutsWithOperatorType_quantityType_minimumQuantity
  , predicateForWorkoutsWithOperatorType_quantityType_maximumQuantity
  , predicateForWorkoutsWithOperatorType_quantityType_averageQuantity
  , predicateForCategorySamplesWithOperatorType_value
  , predicateForCategorySamplesEqualToValues
  , predicateForQuantitySamplesWithOperatorType_quantity
  , predicateForSamplesWithStartDate_endDate_options
  , predicateForObjectsWithMetadataKey
  , predicateForObjectsWithMetadataKey_allowedValues
  , predicateForObjectsWithMetadataKey_operatorType_value
  , predicateForObjectsFromSource
  , predicateForObjectsFromSources
  , predicateForObjectsFromSourceRevisions
  , predicateForObjectsFromDevices
  , predicateForObjectsWithDeviceProperty_allowedValues
  , predicateForObjectWithUUID
  , predicateForObjectsWithUUIDs
  , predicateForObjectsWithNoCorrelation
  , predicateForObjectsFromWorkout
  , predicateForObjectsAssociatedWithElectrocardiogram
  , predicateForWorkoutEffortSamplesRelatedToWorkout_activity
  , objectType
  , sampleType
  , predicate
  , initSelector
  , objectTypeSelector
  , predicateForActivitySummariesBetweenStartDateComponents_endDateComponentsSelector
  , predicateForActivitySummaryWithDateComponentsSelector
  , predicateForCategorySamplesEqualToValuesSelector
  , predicateForCategorySamplesWithOperatorType_valueSelector
  , predicateForClinicalRecordsFromSource_FHIRResourceType_identifierSelector
  , predicateForClinicalRecordsWithFHIRResourceTypeSelector
  , predicateForElectrocardiogramsWithClassificationSelector
  , predicateForElectrocardiogramsWithSymptomsStatusSelector
  , predicateForMedicationDoseEventWithMedicationConceptIdentifierSelector
  , predicateForMedicationDoseEventWithMedicationConceptIdentifiersSelector
  , predicateForMedicationDoseEventWithScheduledDateSelector
  , predicateForMedicationDoseEventWithScheduledDatesSelector
  , predicateForMedicationDoseEventWithScheduledStartDate_endDateSelector
  , predicateForMedicationDoseEventWithStatusSelector
  , predicateForMedicationDoseEventWithStatusesSelector
  , predicateForObjectWithUUIDSelector
  , predicateForObjectsAssociatedWithElectrocardiogramSelector
  , predicateForObjectsFromDevicesSelector
  , predicateForObjectsFromSourceRevisionsSelector
  , predicateForObjectsFromSourceSelector
  , predicateForObjectsFromSourcesSelector
  , predicateForObjectsFromWorkoutSelector
  , predicateForObjectsWithDeviceProperty_allowedValuesSelector
  , predicateForObjectsWithMetadataKeySelector
  , predicateForObjectsWithMetadataKey_allowedValuesSelector
  , predicateForObjectsWithMetadataKey_operatorType_valueSelector
  , predicateForObjectsWithNoCorrelationSelector
  , predicateForObjectsWithUUIDsSelector
  , predicateForQuantitySamplesWithOperatorType_quantitySelector
  , predicateForSamplesWithStartDate_endDate_optionsSelector
  , predicateForStatesOfMindWithAssociationSelector
  , predicateForStatesOfMindWithKindSelector
  , predicateForStatesOfMindWithLabelSelector
  , predicateForStatesOfMindWithValence_operatorTypeSelector
  , predicateForUserAnnotatedMedicationsWithHasScheduleSelector
  , predicateForUserAnnotatedMedicationsWithIsArchivedSelector
  , predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateIntervalSelector
  , predicateForWorkoutActivitiesWithOperatorType_durationSelector
  , predicateForWorkoutActivitiesWithOperatorType_quantityType_averageQuantitySelector
  , predicateForWorkoutActivitiesWithOperatorType_quantityType_maximumQuantitySelector
  , predicateForWorkoutActivitiesWithOperatorType_quantityType_minimumQuantitySelector
  , predicateForWorkoutActivitiesWithOperatorType_quantityType_sumQuantitySelector
  , predicateForWorkoutActivitiesWithStartDate_endDate_optionsSelector
  , predicateForWorkoutActivitiesWithWorkoutActivityTypeSelector
  , predicateForWorkoutEffortSamplesRelatedToWorkout_activitySelector
  , predicateForWorkoutsWithActivityPredicateSelector
  , predicateForWorkoutsWithOperatorType_durationSelector
  , predicateForWorkoutsWithOperatorType_quantityType_averageQuantitySelector
  , predicateForWorkoutsWithOperatorType_quantityType_maximumQuantitySelector
  , predicateForWorkoutsWithOperatorType_quantityType_minimumQuantitySelector
  , predicateForWorkoutsWithOperatorType_quantityType_sumQuantitySelector
  , predicateForWorkoutsWithOperatorType_totalDistanceSelector
  , predicateForWorkoutsWithOperatorType_totalEnergyBurnedSelector
  , predicateForWorkoutsWithOperatorType_totalFlightsClimbedSelector
  , predicateForWorkoutsWithOperatorType_totalSwimmingStrokeCountSelector
  , predicateForWorkoutsWithWorkoutActivityTypeSelector
  , predicateSelector
  , sampleTypeSelector

  -- * Enum types
  , HKElectrocardiogramClassification(HKElectrocardiogramClassification)
  , pattern HKElectrocardiogramClassificationNotSet
  , pattern HKElectrocardiogramClassificationSinusRhythm
  , pattern HKElectrocardiogramClassificationAtrialFibrillation
  , pattern HKElectrocardiogramClassificationInconclusiveLowHeartRate
  , pattern HKElectrocardiogramClassificationInconclusiveHighHeartRate
  , pattern HKElectrocardiogramClassificationInconclusivePoorReading
  , pattern HKElectrocardiogramClassificationInconclusiveOther
  , pattern HKElectrocardiogramClassificationUnrecognized
  , HKElectrocardiogramSymptomsStatus(HKElectrocardiogramSymptomsStatus)
  , pattern HKElectrocardiogramSymptomsStatusNotSet
  , pattern HKElectrocardiogramSymptomsStatusNone
  , pattern HKElectrocardiogramSymptomsStatusPresent
  , HKMedicationDoseEventLogStatus(HKMedicationDoseEventLogStatus)
  , pattern HKMedicationDoseEventLogStatusNotInteracted
  , pattern HKMedicationDoseEventLogStatusNotificationNotSent
  , pattern HKMedicationDoseEventLogStatusSnoozed
  , pattern HKMedicationDoseEventLogStatusTaken
  , pattern HKMedicationDoseEventLogStatusSkipped
  , pattern HKMedicationDoseEventLogStatusNotLogged
  , HKQueryOptions(HKQueryOptions)
  , pattern HKQueryOptionNone
  , pattern HKQueryOptionStrictStartDate
  , pattern HKQueryOptionStrictEndDate
  , HKStateOfMindAssociation(HKStateOfMindAssociation)
  , pattern HKStateOfMindAssociationCommunity
  , pattern HKStateOfMindAssociationCurrentEvents
  , pattern HKStateOfMindAssociationDating
  , pattern HKStateOfMindAssociationEducation
  , pattern HKStateOfMindAssociationFamily
  , pattern HKStateOfMindAssociationFitness
  , pattern HKStateOfMindAssociationFriends
  , pattern HKStateOfMindAssociationHealth
  , pattern HKStateOfMindAssociationHobbies
  , pattern HKStateOfMindAssociationIdentity
  , pattern HKStateOfMindAssociationMoney
  , pattern HKStateOfMindAssociationPartner
  , pattern HKStateOfMindAssociationSelfCare
  , pattern HKStateOfMindAssociationSpirituality
  , pattern HKStateOfMindAssociationTasks
  , pattern HKStateOfMindAssociationTravel
  , pattern HKStateOfMindAssociationWork
  , pattern HKStateOfMindAssociationWeather
  , HKStateOfMindKind(HKStateOfMindKind)
  , pattern HKStateOfMindKindMomentaryEmotion
  , pattern HKStateOfMindKindDailyMood
  , HKStateOfMindLabel(HKStateOfMindLabel)
  , pattern HKStateOfMindLabelAmazed
  , pattern HKStateOfMindLabelAmused
  , pattern HKStateOfMindLabelAngry
  , pattern HKStateOfMindLabelAnxious
  , pattern HKStateOfMindLabelAshamed
  , pattern HKStateOfMindLabelBrave
  , pattern HKStateOfMindLabelCalm
  , pattern HKStateOfMindLabelContent
  , pattern HKStateOfMindLabelDisappointed
  , pattern HKStateOfMindLabelDiscouraged
  , pattern HKStateOfMindLabelDisgusted
  , pattern HKStateOfMindLabelEmbarrassed
  , pattern HKStateOfMindLabelExcited
  , pattern HKStateOfMindLabelFrustrated
  , pattern HKStateOfMindLabelGrateful
  , pattern HKStateOfMindLabelGuilty
  , pattern HKStateOfMindLabelHappy
  , pattern HKStateOfMindLabelHopeless
  , pattern HKStateOfMindLabelIrritated
  , pattern HKStateOfMindLabelJealous
  , pattern HKStateOfMindLabelJoyful
  , pattern HKStateOfMindLabelLonely
  , pattern HKStateOfMindLabelPassionate
  , pattern HKStateOfMindLabelPeaceful
  , pattern HKStateOfMindLabelProud
  , pattern HKStateOfMindLabelRelieved
  , pattern HKStateOfMindLabelSad
  , pattern HKStateOfMindLabelScared
  , pattern HKStateOfMindLabelStressed
  , pattern HKStateOfMindLabelSurprised
  , pattern HKStateOfMindLabelWorried
  , pattern HKStateOfMindLabelAnnoyed
  , pattern HKStateOfMindLabelConfident
  , pattern HKStateOfMindLabelDrained
  , pattern HKStateOfMindLabelHopeful
  , pattern HKStateOfMindLabelIndifferent
  , pattern HKStateOfMindLabelOverwhelmed
  , pattern HKStateOfMindLabelSatisfied
  , HKWorkoutActivityType(HKWorkoutActivityType)
  , pattern HKWorkoutActivityTypeAmericanFootball
  , pattern HKWorkoutActivityTypeArchery
  , pattern HKWorkoutActivityTypeAustralianFootball
  , pattern HKWorkoutActivityTypeBadminton
  , pattern HKWorkoutActivityTypeBaseball
  , pattern HKWorkoutActivityTypeBasketball
  , pattern HKWorkoutActivityTypeBowling
  , pattern HKWorkoutActivityTypeBoxing
  , pattern HKWorkoutActivityTypeClimbing
  , pattern HKWorkoutActivityTypeCricket
  , pattern HKWorkoutActivityTypeCrossTraining
  , pattern HKWorkoutActivityTypeCurling
  , pattern HKWorkoutActivityTypeCycling
  , pattern HKWorkoutActivityTypeDance
  , pattern HKWorkoutActivityTypeDanceInspiredTraining
  , pattern HKWorkoutActivityTypeElliptical
  , pattern HKWorkoutActivityTypeEquestrianSports
  , pattern HKWorkoutActivityTypeFencing
  , pattern HKWorkoutActivityTypeFishing
  , pattern HKWorkoutActivityTypeFunctionalStrengthTraining
  , pattern HKWorkoutActivityTypeGolf
  , pattern HKWorkoutActivityTypeGymnastics
  , pattern HKWorkoutActivityTypeHandball
  , pattern HKWorkoutActivityTypeHiking
  , pattern HKWorkoutActivityTypeHockey
  , pattern HKWorkoutActivityTypeHunting
  , pattern HKWorkoutActivityTypeLacrosse
  , pattern HKWorkoutActivityTypeMartialArts
  , pattern HKWorkoutActivityTypeMindAndBody
  , pattern HKWorkoutActivityTypeMixedMetabolicCardioTraining
  , pattern HKWorkoutActivityTypePaddleSports
  , pattern HKWorkoutActivityTypePlay
  , pattern HKWorkoutActivityTypePreparationAndRecovery
  , pattern HKWorkoutActivityTypeRacquetball
  , pattern HKWorkoutActivityTypeRowing
  , pattern HKWorkoutActivityTypeRugby
  , pattern HKWorkoutActivityTypeRunning
  , pattern HKWorkoutActivityTypeSailing
  , pattern HKWorkoutActivityTypeSkatingSports
  , pattern HKWorkoutActivityTypeSnowSports
  , pattern HKWorkoutActivityTypeSoccer
  , pattern HKWorkoutActivityTypeSoftball
  , pattern HKWorkoutActivityTypeSquash
  , pattern HKWorkoutActivityTypeStairClimbing
  , pattern HKWorkoutActivityTypeSurfingSports
  , pattern HKWorkoutActivityTypeSwimming
  , pattern HKWorkoutActivityTypeTableTennis
  , pattern HKWorkoutActivityTypeTennis
  , pattern HKWorkoutActivityTypeTrackAndField
  , pattern HKWorkoutActivityTypeTraditionalStrengthTraining
  , pattern HKWorkoutActivityTypeVolleyball
  , pattern HKWorkoutActivityTypeWalking
  , pattern HKWorkoutActivityTypeWaterFitness
  , pattern HKWorkoutActivityTypeWaterPolo
  , pattern HKWorkoutActivityTypeWaterSports
  , pattern HKWorkoutActivityTypeWrestling
  , pattern HKWorkoutActivityTypeYoga
  , pattern HKWorkoutActivityTypeBarre
  , pattern HKWorkoutActivityTypeCoreTraining
  , pattern HKWorkoutActivityTypeCrossCountrySkiing
  , pattern HKWorkoutActivityTypeDownhillSkiing
  , pattern HKWorkoutActivityTypeFlexibility
  , pattern HKWorkoutActivityTypeHighIntensityIntervalTraining
  , pattern HKWorkoutActivityTypeJumpRope
  , pattern HKWorkoutActivityTypeKickboxing
  , pattern HKWorkoutActivityTypePilates
  , pattern HKWorkoutActivityTypeSnowboarding
  , pattern HKWorkoutActivityTypeStairs
  , pattern HKWorkoutActivityTypeStepTraining
  , pattern HKWorkoutActivityTypeWheelchairWalkPace
  , pattern HKWorkoutActivityTypeWheelchairRunPace
  , pattern HKWorkoutActivityTypeTaiChi
  , pattern HKWorkoutActivityTypeMixedCardio
  , pattern HKWorkoutActivityTypeHandCycling
  , pattern HKWorkoutActivityTypeDiscSports
  , pattern HKWorkoutActivityTypeFitnessGaming
  , pattern HKWorkoutActivityTypeCardioDance
  , pattern HKWorkoutActivityTypeSocialDance
  , pattern HKWorkoutActivityTypePickleball
  , pattern HKWorkoutActivityTypeCooldown
  , pattern HKWorkoutActivityTypeSwimBikeRun
  , pattern HKWorkoutActivityTypeTransition
  , pattern HKWorkoutActivityTypeUnderwaterDiving
  , pattern HKWorkoutActivityTypeOther
  , NSPredicateOperatorType(NSPredicateOperatorType)
  , pattern NSLessThanPredicateOperatorType
  , pattern NSLessThanOrEqualToPredicateOperatorType
  , pattern NSGreaterThanPredicateOperatorType
  , pattern NSGreaterThanOrEqualToPredicateOperatorType
  , pattern NSEqualToPredicateOperatorType
  , pattern NSNotEqualToPredicateOperatorType
  , pattern NSMatchesPredicateOperatorType
  , pattern NSLikePredicateOperatorType
  , pattern NSBeginsWithPredicateOperatorType
  , pattern NSEndsWithPredicateOperatorType
  , pattern NSInPredicateOperatorType
  , pattern NSCustomSelectorPredicateOperatorType
  , pattern NSContainsPredicateOperatorType
  , pattern NSBetweenPredicateOperatorType

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsHKQuery hkQuery => hkQuery -> IO (Id HKQuery)
init_ hkQuery =
  sendOwnedMessage hkQuery initSelector

-- | predicateForUserAnnotatedMedicationsWithIsArchived:
--
-- Creates a predicate for use with HKUserAnnotatedMedicationQuery.
--
-- Creates a query predicate that matches HKUserAnnotatedMedication objects that have the archived status specified.
--
-- @isArchived@ — The archived status of the medication. Ex: True will match medications in the archived section in the Health App.
--
-- ObjC selector: @+ predicateForUserAnnotatedMedicationsWithIsArchived:@
predicateForUserAnnotatedMedicationsWithIsArchived :: Bool -> IO (Id NSPredicate)
predicateForUserAnnotatedMedicationsWithIsArchived isArchived =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForUserAnnotatedMedicationsWithIsArchivedSelector isArchived

-- | predicateForUserAnnotatedMedicationsWithHasSchedule:
--
-- Creates a predicate for use with HKUserAnnotatedMedicationQuery.
--
-- Creates a query predicate that matches HKUserAnnotatedMedication objects that match the schedule status specified.
--
-- @hasSchedule@ — The schedule status of the medication. Ex: True will match medications that have a reminders schedule set up in the Health App.
--
-- ObjC selector: @+ predicateForUserAnnotatedMedicationsWithHasSchedule:@
predicateForUserAnnotatedMedicationsWithHasSchedule :: Bool -> IO (Id NSPredicate)
predicateForUserAnnotatedMedicationsWithHasSchedule hasSchedule =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForUserAnnotatedMedicationsWithHasScheduleSelector hasSchedule

-- | predicateForMedicationDoseEventWithStatus:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKMedicationDoseEvent samples that have the status specified.
--
-- @status@ — The logged status of the medication dose event to match.
--
-- ObjC selector: @+ predicateForMedicationDoseEventWithStatus:@
predicateForMedicationDoseEventWithStatus :: HKMedicationDoseEventLogStatus -> IO (Id NSPredicate)
predicateForMedicationDoseEventWithStatus status =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForMedicationDoseEventWithStatusSelector status

-- | predicateForMedicationDoseEventWithStatuses:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKMedicationDoseEvent samples that have any of the statuses specified.
--
-- @statuses@ — The logged statuses of the medication dose event to match.
--
-- ObjC selector: @+ predicateForMedicationDoseEventWithStatuses:@
predicateForMedicationDoseEventWithStatuses :: IsNSSet statuses => statuses -> IO (Id NSPredicate)
predicateForMedicationDoseEventWithStatuses statuses =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForMedicationDoseEventWithStatusesSelector (toNSSet statuses)

-- | predicateForMedicationDoseEventWithScheduledDate:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKMedicationDoseEvent samples that have the exact scheduled date specified.
--
-- @scheduledDate@ — The exact scheduled date of the medication dose event to match.
--
-- ObjC selector: @+ predicateForMedicationDoseEventWithScheduledDate:@
predicateForMedicationDoseEventWithScheduledDate :: IsNSDate scheduledDate => scheduledDate -> IO (Id NSPredicate)
predicateForMedicationDoseEventWithScheduledDate scheduledDate =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForMedicationDoseEventWithScheduledDateSelector (toNSDate scheduledDate)

-- | predicateForMedicationDoseEventWithScheduledDates:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKMedicationDoseEvent samples that have any of the exact scheduled dates specified.
--
-- @scheduledDates@ — The exact scheduled dates of any medication dose event to match.
--
-- ObjC selector: @+ predicateForMedicationDoseEventWithScheduledDates:@
predicateForMedicationDoseEventWithScheduledDates :: IsNSSet scheduledDates => scheduledDates -> IO (Id NSPredicate)
predicateForMedicationDoseEventWithScheduledDates scheduledDates =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForMedicationDoseEventWithScheduledDatesSelector (toNSSet scheduledDates)

-- | predicateForMedicationDoseEventWithScheduledStartDate:endDate:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKMedicationDoseEvent samples that have a scheduled date within a window of scheduled times. If nil is provided to either parameter, the respective side of the window is unbound.
--
-- @startDate@ — The beginning of the window for scheduled dates of any medication dose event to match.
--
-- @endDate@ — The beginning of the window for scheduled dates of any medication dose event to match.
--
-- ObjC selector: @+ predicateForMedicationDoseEventWithScheduledStartDate:endDate:@
predicateForMedicationDoseEventWithScheduledStartDate_endDate :: (IsNSDate startDate, IsNSDate endDate) => startDate -> endDate -> IO (Id NSPredicate)
predicateForMedicationDoseEventWithScheduledStartDate_endDate startDate endDate =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForMedicationDoseEventWithScheduledStartDate_endDateSelector (toNSDate startDate) (toNSDate endDate)

-- | predicateForMedicationDoseEventWithMedicationConceptIdentifier:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKMedicationDoseEvent samples that match a medication's concept identifier.
--
-- @medicationConceptIdentifier@ — The identifier of the medication that a dose event was created for.
--
-- ObjC selector: @+ predicateForMedicationDoseEventWithMedicationConceptIdentifier:@
predicateForMedicationDoseEventWithMedicationConceptIdentifier :: IsHKHealthConceptIdentifier medicationConceptIdentifier => medicationConceptIdentifier -> IO (Id NSPredicate)
predicateForMedicationDoseEventWithMedicationConceptIdentifier medicationConceptIdentifier =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForMedicationDoseEventWithMedicationConceptIdentifierSelector (toHKHealthConceptIdentifier medicationConceptIdentifier)

-- | predicateForMedicationDoseEventWithMedicationConceptIdentifiers:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKMedicationDoseEvent samples generated by any medication in a set of medication concept identifiers.
--
-- @medicationConceptIdentifiers@ — Any identifier of a medication that a dose event was created for.
--
-- ObjC selector: @+ predicateForMedicationDoseEventWithMedicationConceptIdentifiers:@
predicateForMedicationDoseEventWithMedicationConceptIdentifiers :: IsNSSet medicationConceptIdentifiers => medicationConceptIdentifiers -> IO (Id NSPredicate)
predicateForMedicationDoseEventWithMedicationConceptIdentifiers medicationConceptIdentifiers =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForMedicationDoseEventWithMedicationConceptIdentifiersSelector (toNSSet medicationConceptIdentifiers)

-- | predicateForStatesOfMindWithValence:operatorType:
--
-- Creates a predicate for use with HKStateOfMind
--
-- Creates a query predicate that matches HKStateOfMind samples that have a valence property matching the operator type and valence.
--
-- @valence@ — The value to be compared against.
--
-- @operatorType@ — The comparison operator type for the expression.
--
-- ObjC selector: @+ predicateForStatesOfMindWithValence:operatorType:@
predicateForStatesOfMindWithValence_operatorType :: CDouble -> NSPredicateOperatorType -> IO (Id NSPredicate)
predicateForStatesOfMindWithValence_operatorType valence operatorType =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForStatesOfMindWithValence_operatorTypeSelector valence operatorType

-- | predicateForStatesOfMindWithKind:
--
-- Creates a predicate for use with HKStateOfMind
--
-- Creates a query predicate that matches HKStateOfMind samples that have the specified kind of feeling type.
--
-- @kind@ — The kind of feeling type to be compared against.
--
-- ObjC selector: @+ predicateForStatesOfMindWithKind:@
predicateForStatesOfMindWithKind :: HKStateOfMindKind -> IO (Id NSPredicate)
predicateForStatesOfMindWithKind kind =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForStatesOfMindWithKindSelector kind

-- | predicateForStatesOfMindWithLabel:
--
-- Creates a predicate for use with HKStateOfMind
--
-- Creates a query predicate that matches HKStateOfMind samples that have the specified label.
--
-- @label@ — The label to be compared against.
--
-- ObjC selector: @+ predicateForStatesOfMindWithLabel:@
predicateForStatesOfMindWithLabel :: HKStateOfMindLabel -> IO (Id NSPredicate)
predicateForStatesOfMindWithLabel label =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForStatesOfMindWithLabelSelector label

-- | predicateForStatesOfMindWithAssociation:
--
-- Creates a predicate for use with HKStateOfMind
--
-- Creates a query predicate that matches HKStateOfMind samples that have the specified association.
--
-- @association@ — The association to be compared against.
--
-- ObjC selector: @+ predicateForStatesOfMindWithAssociation:@
predicateForStatesOfMindWithAssociation :: HKStateOfMindAssociation -> IO (Id NSPredicate)
predicateForStatesOfMindWithAssociation association =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForStatesOfMindWithAssociationSelector association

-- | predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateInterval:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a predicate that matches HKVerifiableClinicalRecords with a relevant date within a date interval.
--
-- @dateInterval@ — The date interval that the record's relevant date is in.
--
-- ObjC selector: @+ predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateInterval:@
predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateInterval :: IsNSDateInterval dateInterval => dateInterval -> IO (Id NSPredicate)
predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateInterval dateInterval =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateIntervalSelector (toNSDateInterval dateInterval)

-- | predicateForElectrocardiogramsWithClassification:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKElectrocardiograms with a specific classification.
--
-- @classification@ — The classification for the electrocardiogram.
--
-- ObjC selector: @+ predicateForElectrocardiogramsWithClassification:@
predicateForElectrocardiogramsWithClassification :: HKElectrocardiogramClassification -> IO (Id NSPredicate)
predicateForElectrocardiogramsWithClassification classification =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForElectrocardiogramsWithClassificationSelector classification

-- | predicateForElectrocardiogramsWithSymptomsStatus:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKElectrocardiograms with a specificied symptoms status.
--
-- @symptomsStatus@ — The symptoms status for the electrocardiogram.
--
-- ObjC selector: @+ predicateForElectrocardiogramsWithSymptomsStatus:@
predicateForElectrocardiogramsWithSymptomsStatus :: HKElectrocardiogramSymptomsStatus -> IO (Id NSPredicate)
predicateForElectrocardiogramsWithSymptomsStatus symptomsStatus =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForElectrocardiogramsWithSymptomsStatusSelector symptomsStatus

-- | predicateForClinicalRecordsWithFHIRResourceType:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKClinicalRecords with a specific FHIR resource type.
--
-- @resourceType@ — The FHIR resource type.
--
-- ObjC selector: @+ predicateForClinicalRecordsWithFHIRResourceType:@
predicateForClinicalRecordsWithFHIRResourceType :: IsNSString resourceType => resourceType -> IO (Id NSPredicate)
predicateForClinicalRecordsWithFHIRResourceType resourceType =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForClinicalRecordsWithFHIRResourceTypeSelector (toNSString resourceType)

-- | predicateForClinicalRecordsFromSource:withFHIRResourceType:identifier:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKClinicalRecords for a given source, FHIR resource type, and FHIR identifier.
--
-- @source@ — The source.
--
-- @resourceType@ — The FHIR resource type.
--
-- @identifier@ — The FHIR identifier.
--
-- ObjC selector: @+ predicateForClinicalRecordsFromSource:FHIRResourceType:identifier:@
predicateForClinicalRecordsFromSource_FHIRResourceType_identifier :: (IsHKSource source, IsNSString resourceType, IsNSString identifier) => source -> resourceType -> identifier -> IO (Id NSPredicate)
predicateForClinicalRecordsFromSource_FHIRResourceType_identifier source resourceType identifier =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForClinicalRecordsFromSource_FHIRResourceType_identifierSelector (toHKSource source) (toNSString resourceType) (toNSString identifier)

-- | predicateForActivitySummaryWithDateComponents:
--
-- Creates a predicate for use with HKActivitySummaryQuery
--
-- Creates a query predicate that matches HKActivitySummaries with the given date components.
--
-- @dateComponents@ — The date components of the activity summary. These date components should contain era, year, month,                and day components in the gregorian calendar.
--
-- ObjC selector: @+ predicateForActivitySummaryWithDateComponents:@
predicateForActivitySummaryWithDateComponents :: IsNSDateComponents dateComponents => dateComponents -> IO (Id NSPredicate)
predicateForActivitySummaryWithDateComponents dateComponents =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForActivitySummaryWithDateComponentsSelector (toNSDateComponents dateComponents)

-- | predicateForActivitySummariesBetweenStartDateComponents:endDateComponents:
--
-- Creates a predicate for use with HKActivitySummaryQuery
--
-- Creates a query predicate that matches HKActivitySummaries that fall between the given date components.
--
-- @startDateComponents@ — The date components that define the beginning of the range. These date components should contain                 era, year, month, and day components in the gregorian calendar.
--
-- @endDateComponents@ — The date components that define the end of the range. These date components should contain era,                 year, month, and day components in the gregorian calendar.
--
-- ObjC selector: @+ predicateForActivitySummariesBetweenStartDateComponents:endDateComponents:@
predicateForActivitySummariesBetweenStartDateComponents_endDateComponents :: (IsNSDateComponents startDateComponents, IsNSDateComponents endDateComponents) => startDateComponents -> endDateComponents -> IO (Id NSPredicate)
predicateForActivitySummariesBetweenStartDateComponents_endDateComponents startDateComponents endDateComponents =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForActivitySummariesBetweenStartDateComponents_endDateComponentsSelector (toNSDateComponents startDateComponents) (toNSDateComponents endDateComponents)

-- | predicateForWorkoutActivitiesWithWorkoutActivityType:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkoutActivity objects with the given HKWorkoutActivityType.                The resulting predicate should be wrapped using predicateForWorkoutsWithActivityPredicate: before being used in a query.
--
-- @workoutActivityType@ — The HKWorkoutActivity type of the workout
--
-- ObjC selector: @+ predicateForWorkoutActivitiesWithWorkoutActivityType:@
predicateForWorkoutActivitiesWithWorkoutActivityType :: HKWorkoutActivityType -> IO (Id NSPredicate)
predicateForWorkoutActivitiesWithWorkoutActivityType workoutActivityType =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForWorkoutActivitiesWithWorkoutActivityTypeSelector workoutActivityType

-- | predicateForWorkoutActivitiesWithOperatorType:duration:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkoutActivity objects by the given operator type and duration.                The resulting predicate should be wrapped using predicateForWorkoutsWithActivityPredicate: before being used in a query.
--
-- @operatorType@ — The operator type for the expression.
--
-- @duration@ — The value that the workout's duration is being compared to. It is the right hand side of the                                expression.
--
-- ObjC selector: @+ predicateForWorkoutActivitiesWithOperatorType:duration:@
predicateForWorkoutActivitiesWithOperatorType_duration :: NSPredicateOperatorType -> CDouble -> IO (Id NSPredicate)
predicateForWorkoutActivitiesWithOperatorType_duration operatorType duration =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForWorkoutActivitiesWithOperatorType_durationSelector operatorType duration

-- | predicateForWorkoutActivitiesWithStartDate:endDate:options:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkoutActivity objects with a startDate and an endDate that lie inside of a                given time interval. The resulting predicate should be wrapped using predicateForWorkoutsWithActivityPredicate:                before being used in a query.
--
-- @startDate@ — The start date of the predicate's time interval.
--
-- @endDate@ — The end date of the predicate's time interval.
--
-- @options@ — The rules for how a activity's time interval overlaps with the predicate's time interval.
--
-- ObjC selector: @+ predicateForWorkoutActivitiesWithStartDate:endDate:options:@
predicateForWorkoutActivitiesWithStartDate_endDate_options :: (IsNSDate startDate, IsNSDate endDate) => startDate -> endDate -> HKQueryOptions -> IO (Id NSPredicate)
predicateForWorkoutActivitiesWithStartDate_endDate_options startDate endDate options =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForWorkoutActivitiesWithStartDate_endDate_optionsSelector (toNSDate startDate) (toNSDate endDate) options

-- | predicateForWorkoutActivitiesWithOperatorType:quantityType:sumQuantity:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkoutActivity objetcs by the given operator type and sumQuantity in the                statistics for the specified type. The resulting predicate should be wrapped using predicateForWorkoutsWithActivityPredicate:                before being used in a query.
--
-- @operatorType@ — The operator type for the expression.
--
-- @quantityType@ — The quantity type to compare statistics for. Should be a cumulative quantity type.
--
-- @sumQuantity@ — The sum value that the activity statistics are being compared to. The unit for this value should                                match the allowed values for the quantityType.
--
-- ObjC selector: @+ predicateForWorkoutActivitiesWithOperatorType:quantityType:sumQuantity:@
predicateForWorkoutActivitiesWithOperatorType_quantityType_sumQuantity :: (IsHKQuantityType quantityType, IsHKQuantity sumQuantity) => NSPredicateOperatorType -> quantityType -> sumQuantity -> IO (Id NSPredicate)
predicateForWorkoutActivitiesWithOperatorType_quantityType_sumQuantity operatorType quantityType sumQuantity =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForWorkoutActivitiesWithOperatorType_quantityType_sumQuantitySelector operatorType (toHKQuantityType quantityType) (toHKQuantity sumQuantity)

-- | predicateForWorkoutActivitiesWithOperatorType:quantityType:minimumQuantity:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkoutActivity objetcs  by the given operator type and minimumQuantity in the                statistics for the specified type. The resulting predicate should be wrapped using predicateForWorkoutsWithActivityPredicate:                before being used in a query.
--
-- @operatorType@ — The operator type for the expression.
--
-- @quantityType@ — The quantity type to compare statistics for. Should be a discrete quantity type.
--
-- @minimumQuantity@ — The minumum value that the activty statistics are being compared to. The unit for this value should                                    match the allowed values for the quantityType.
--
-- ObjC selector: @+ predicateForWorkoutActivitiesWithOperatorType:quantityType:minimumQuantity:@
predicateForWorkoutActivitiesWithOperatorType_quantityType_minimumQuantity :: (IsHKQuantityType quantityType, IsHKQuantity minimumQuantity) => NSPredicateOperatorType -> quantityType -> minimumQuantity -> IO (Id NSPredicate)
predicateForWorkoutActivitiesWithOperatorType_quantityType_minimumQuantity operatorType quantityType minimumQuantity =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForWorkoutActivitiesWithOperatorType_quantityType_minimumQuantitySelector operatorType (toHKQuantityType quantityType) (toHKQuantity minimumQuantity)

-- | predicateForWorkoutActivitiesWithOperatorType:quantityType:maximumQuantity:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkoutActivity objetcs by the given operator type and maximumQuantity in the                statistics for the specified type. The resulting predicate should be wrapped using predicateForWorkoutsWithActivityPredicate:                before being used in a query.
--
-- @operatorType@ — The operator type for the expression.
--
-- @quantityType@ — The quantity type to compare statistics for. Should be a discrete quantity type.
--
-- @maximumQuantity@ — The maximum value that the activity statistics are being compared to. The unit for this value should                                    match the allowed values for the quantityType.
--
-- ObjC selector: @+ predicateForWorkoutActivitiesWithOperatorType:quantityType:maximumQuantity:@
predicateForWorkoutActivitiesWithOperatorType_quantityType_maximumQuantity :: (IsHKQuantityType quantityType, IsHKQuantity maximumQuantity) => NSPredicateOperatorType -> quantityType -> maximumQuantity -> IO (Id NSPredicate)
predicateForWorkoutActivitiesWithOperatorType_quantityType_maximumQuantity operatorType quantityType maximumQuantity =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForWorkoutActivitiesWithOperatorType_quantityType_maximumQuantitySelector operatorType (toHKQuantityType quantityType) (toHKQuantity maximumQuantity)

-- | predicateForWorkoutActivitiesWithOperatorType:quantityType:averageQuantity:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkoutActivity objetcs by the given operator type and averageQuantity in the                statistics for the specified type. The resulting predicate should be wrapped using predicateForWorkoutsWithActivityPredicate:                before being used in a query.
--
-- @operatorType@ — The operator type for the expression.
--
-- @quantityType@ — The quantity type to compare statistics for. Should be a discrete quantity type.
--
-- @averageQuantity@ — The average value that the activity statistics are being compared to. The unit for this value should                                    match the allowed values for the quantityType.
--
-- ObjC selector: @+ predicateForWorkoutActivitiesWithOperatorType:quantityType:averageQuantity:@
predicateForWorkoutActivitiesWithOperatorType_quantityType_averageQuantity :: (IsHKQuantityType quantityType, IsHKQuantity averageQuantity) => NSPredicateOperatorType -> quantityType -> averageQuantity -> IO (Id NSPredicate)
predicateForWorkoutActivitiesWithOperatorType_quantityType_averageQuantity operatorType quantityType averageQuantity =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForWorkoutActivitiesWithOperatorType_quantityType_averageQuantitySelector operatorType (toHKQuantityType quantityType) (toHKQuantity averageQuantity)

-- | predicateForWorkoutsWithActivityPredicate:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches workouts containing an activity matching the passed predicate.
--
-- @activityPredicate@ — The predicate on the activities of the workout
--
-- ObjC selector: @+ predicateForWorkoutsWithActivityPredicate:@
predicateForWorkoutsWithActivityPredicate :: IsNSPredicate activityPredicate => activityPredicate -> IO (Id NSPredicate)
predicateForWorkoutsWithActivityPredicate activityPredicate =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForWorkoutsWithActivityPredicateSelector (toNSPredicate activityPredicate)

-- | predicateForWorkoutsWithWorkoutActivityType:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkouts with the given HKWorkoutActivityType.
--
-- @workoutActivityType@ — The HKWorkoutActivity type of the workout
--
-- ObjC selector: @+ predicateForWorkoutsWithWorkoutActivityType:@
predicateForWorkoutsWithWorkoutActivityType :: HKWorkoutActivityType -> IO (Id NSPredicate)
predicateForWorkoutsWithWorkoutActivityType workoutActivityType =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForWorkoutsWithWorkoutActivityTypeSelector workoutActivityType

-- | predicateForWorkoutsWithOperatorType:duration:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkouts by the given operator type and duration
--
-- @operatorType@ — The operator type for the expression.
--
-- @duration@ — The value that the workout's duration is being compared to. It is the right hand side of the                                expression.
--
-- ObjC selector: @+ predicateForWorkoutsWithOperatorType:duration:@
predicateForWorkoutsWithOperatorType_duration :: NSPredicateOperatorType -> CDouble -> IO (Id NSPredicate)
predicateForWorkoutsWithOperatorType_duration operatorType duration =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForWorkoutsWithOperatorType_durationSelector operatorType duration

-- | predicateForWorkoutsWithOperatorType:totalEnergyBurned:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkouts by the given operator type and totalEnergyBurned
--
-- @operatorType@ — The operator type for the expression.
--
-- @totalEnergyBurned@ — The value that the workout's totalEnergyBurned is being compared to. It is the right hand side of the                                    expression. The unit for this value should be of type Energy.
--
-- ObjC selector: @+ predicateForWorkoutsWithOperatorType:totalEnergyBurned:@
predicateForWorkoutsWithOperatorType_totalEnergyBurned :: IsHKQuantity totalEnergyBurned => NSPredicateOperatorType -> totalEnergyBurned -> IO (Id NSPredicate)
predicateForWorkoutsWithOperatorType_totalEnergyBurned operatorType totalEnergyBurned =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForWorkoutsWithOperatorType_totalEnergyBurnedSelector operatorType (toHKQuantity totalEnergyBurned)

-- | predicateForWorkoutsWithOperatorType:totalDistance:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkouts by the given operator type and totalEnergyBurned
--
-- @operatorType@ — The operator type for the expression.
--
-- @totalDistance@ — The value that the workout's totalEnergyBurned is being compared to. It is the right hand side of the                                expression. The unit for this value should be of type Distance.
--
-- ObjC selector: @+ predicateForWorkoutsWithOperatorType:totalDistance:@
predicateForWorkoutsWithOperatorType_totalDistance :: IsHKQuantity totalDistance => NSPredicateOperatorType -> totalDistance -> IO (Id NSPredicate)
predicateForWorkoutsWithOperatorType_totalDistance operatorType totalDistance =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForWorkoutsWithOperatorType_totalDistanceSelector operatorType (toHKQuantity totalDistance)

-- | predicateForWorkoutsWithOperatorType:totalSwimmingStrokeCount:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkouts by the given operator type and totalSwimmingStrokeCount
--
-- @operatorType@ — The operator type for the expression.
--
-- @totalSwimmingStrokeCount@ — The value that the workout's totalSwimmingStrokeCount is being compared to.                                            It is the right hand side of the expression. The unit for this value should                                            be of type Count.
--
-- ObjC selector: @+ predicateForWorkoutsWithOperatorType:totalSwimmingStrokeCount:@
predicateForWorkoutsWithOperatorType_totalSwimmingStrokeCount :: IsHKQuantity totalSwimmingStrokeCount => NSPredicateOperatorType -> totalSwimmingStrokeCount -> IO (Id NSPredicate)
predicateForWorkoutsWithOperatorType_totalSwimmingStrokeCount operatorType totalSwimmingStrokeCount =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForWorkoutsWithOperatorType_totalSwimmingStrokeCountSelector operatorType (toHKQuantity totalSwimmingStrokeCount)

-- | predicateForWorkoutsWithOperatorType:totalFlightsClimbed:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkouts by the given operator type and totalFlightsClimbed
--
-- @operatorType@ — The operator type for the expression.
--
-- @totalFlightsClimbed@ — The value that the workout's totalFlightsClimbed is being compared to.                                            It is the right hand side of the expression. The unit for this value should                                            be of type Count.
--
-- ObjC selector: @+ predicateForWorkoutsWithOperatorType:totalFlightsClimbed:@
predicateForWorkoutsWithOperatorType_totalFlightsClimbed :: IsHKQuantity totalFlightsClimbed => NSPredicateOperatorType -> totalFlightsClimbed -> IO (Id NSPredicate)
predicateForWorkoutsWithOperatorType_totalFlightsClimbed operatorType totalFlightsClimbed =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForWorkoutsWithOperatorType_totalFlightsClimbedSelector operatorType (toHKQuantity totalFlightsClimbed)

-- | predicateForWorkoutsWithOperatorType:quantityType:sumQuantity:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkouts by the given operator type and sumQuantity in the statistics for                the specified type.
--
-- @operatorType@ — The operator type for the expression.
--
-- @quantityType@ — The quantity type to compare statistics for. Should be a cumulative quantity type.
--
-- @sumQuantity@ — The sum value that the workout statistics are being compared to. The unit for this value should                                match the allowed values for the quantityType.
--
-- ObjC selector: @+ predicateForWorkoutsWithOperatorType:quantityType:sumQuantity:@
predicateForWorkoutsWithOperatorType_quantityType_sumQuantity :: (IsHKQuantityType quantityType, IsHKQuantity sumQuantity) => NSPredicateOperatorType -> quantityType -> sumQuantity -> IO (Id NSPredicate)
predicateForWorkoutsWithOperatorType_quantityType_sumQuantity operatorType quantityType sumQuantity =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForWorkoutsWithOperatorType_quantityType_sumQuantitySelector operatorType (toHKQuantityType quantityType) (toHKQuantity sumQuantity)

-- | predicateForWorkoutsWithOperatorType:quantityType:minimumQuantity:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkouts by the given operator type and minimumQuantity in the statistics                for the specified type.
--
-- @operatorType@ — The operator type for the expression.
--
-- @quantityType@ — The quantity type to compare statistics for. Should be a discrete quantity type.
--
-- @minimumQuantity@ — The minumum value that the workout statistics are being compared to. The unit for this value should                                    match the allowed values for the quantityType.
--
-- ObjC selector: @+ predicateForWorkoutsWithOperatorType:quantityType:minimumQuantity:@
predicateForWorkoutsWithOperatorType_quantityType_minimumQuantity :: (IsHKQuantityType quantityType, IsHKQuantity minimumQuantity) => NSPredicateOperatorType -> quantityType -> minimumQuantity -> IO (Id NSPredicate)
predicateForWorkoutsWithOperatorType_quantityType_minimumQuantity operatorType quantityType minimumQuantity =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForWorkoutsWithOperatorType_quantityType_minimumQuantitySelector operatorType (toHKQuantityType quantityType) (toHKQuantity minimumQuantity)

-- | predicateForWorkoutsWithOperatorType:quantityType:maximumQuantity:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkouts by the given operator type and maximumQuantity in the statistics                for the specified type.
--
-- @operatorType@ — The operator type for the expression.
--
-- @quantityType@ — The quantity type to compare statistics for. Should be a discrete quantity type.
--
-- @maximumQuantity@ — The maximum value that the workout statistics are being compared to. The unit for this value should                                    match the allowed values for the quantityType.
--
-- ObjC selector: @+ predicateForWorkoutsWithOperatorType:quantityType:maximumQuantity:@
predicateForWorkoutsWithOperatorType_quantityType_maximumQuantity :: (IsHKQuantityType quantityType, IsHKQuantity maximumQuantity) => NSPredicateOperatorType -> quantityType -> maximumQuantity -> IO (Id NSPredicate)
predicateForWorkoutsWithOperatorType_quantityType_maximumQuantity operatorType quantityType maximumQuantity =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForWorkoutsWithOperatorType_quantityType_maximumQuantitySelector operatorType (toHKQuantityType quantityType) (toHKQuantity maximumQuantity)

-- | predicateForWorkoutsWithOperatorType:quantityType:averageQuantity:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches HKWorkouts by the given operator type and averageQuantity in the statistics                for the specified type.
--
-- @operatorType@ — The operator type for the expression.
--
-- @quantityType@ — The quantity type to compare statistics for. Should be a discrete quantity type.
--
-- @averageQuantity@ — The average value that the workout statistics are being compared to. The unit for this value should                                    match the allowed values for the quantityType.
--
-- ObjC selector: @+ predicateForWorkoutsWithOperatorType:quantityType:averageQuantity:@
predicateForWorkoutsWithOperatorType_quantityType_averageQuantity :: (IsHKQuantityType quantityType, IsHKQuantity averageQuantity) => NSPredicateOperatorType -> quantityType -> averageQuantity -> IO (Id NSPredicate)
predicateForWorkoutsWithOperatorType_quantityType_averageQuantity operatorType quantityType averageQuantity =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForWorkoutsWithOperatorType_quantityType_averageQuantitySelector operatorType (toHKQuantityType quantityType) (toHKQuantity averageQuantity)

-- | @+ predicateForCategorySamplesWithOperatorType:value:@
predicateForCategorySamplesWithOperatorType_value :: NSPredicateOperatorType -> CLong -> IO (Id NSPredicate)
predicateForCategorySamplesWithOperatorType_value operatorType value =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForCategorySamplesWithOperatorType_valueSelector operatorType value

-- | predicateForCategorySamplesEqualToValues:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches all specified category values.
--
-- ObjC selector: @+ predicateForCategorySamplesEqualToValues:@
predicateForCategorySamplesEqualToValues :: IsNSSet values => values -> IO (Id NSPredicate)
predicateForCategorySamplesEqualToValues values =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForCategorySamplesEqualToValuesSelector (toNSSet values)

-- | predicateForQuantitySamplesWithOperatorType:quantity:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches quantity samples with values that match the expression formed by                the given operator and quantity.
--
-- @operatorType@ — The operator type for the expression.
--
-- @quantity@ — The quantity that the sample's quantity is being compared to. It is the right hand side                                of the expression.
--
-- ObjC selector: @+ predicateForQuantitySamplesWithOperatorType:quantity:@
predicateForQuantitySamplesWithOperatorType_quantity :: IsHKQuantity quantity => NSPredicateOperatorType -> quantity -> IO (Id NSPredicate)
predicateForQuantitySamplesWithOperatorType_quantity operatorType quantity =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForQuantitySamplesWithOperatorType_quantitySelector operatorType (toHKQuantity quantity)

-- | predicateForSamplesWithStartDate:endDate:options:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches samples with a startDate and an endDate that lie inside of a                given time interval.
--
-- @startDate@ — The start date of the predicate's time interval.
--
-- @endDate@ — The end date of the predicate's time interval.
--
-- @options@ — The rules for how a sample's time interval overlaps with the predicate's time interval.
--
-- ObjC selector: @+ predicateForSamplesWithStartDate:endDate:options:@
predicateForSamplesWithStartDate_endDate_options :: (IsNSDate startDate, IsNSDate endDate) => startDate -> endDate -> HKQueryOptions -> IO (Id NSPredicate)
predicateForSamplesWithStartDate_endDate_options startDate endDate options =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForSamplesWithStartDate_endDate_optionsSelector (toNSDate startDate) (toNSDate endDate) options

-- | predicateForObjectsWithMetadataKey:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches objects with metadata that contains a given key.
--
-- @key@ — The metadata key.
--
-- ObjC selector: @+ predicateForObjectsWithMetadataKey:@
predicateForObjectsWithMetadataKey :: IsNSString key => key -> IO (Id NSPredicate)
predicateForObjectsWithMetadataKey key =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForObjectsWithMetadataKeySelector (toNSString key)

-- | predicateForObjectsWithMetadataKey:allowedValues:
--
-- Creates a predicate for use with HKQuery subclasses
--
-- Creates a query predicate that matches objects with metadata containing a value the matches one of the                given values for the given key.
--
-- @key@ — The metadata key.
--
-- @allowedValues@ — The list of values that the metadata value can be equal to.
--
-- ObjC selector: @+ predicateForObjectsWithMetadataKey:allowedValues:@
predicateForObjectsWithMetadataKey_allowedValues :: (IsNSString key, IsNSArray allowedValues) => key -> allowedValues -> IO (Id NSPredicate)
predicateForObjectsWithMetadataKey_allowedValues key allowedValues =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForObjectsWithMetadataKey_allowedValuesSelector (toNSString key) (toNSArray allowedValues)

-- | predicateForObjectsWithMetadataKey:operatorType:value:
--
-- Creates a predicate for use with HKQuery subclasses
--
-- Creates a query predicate that matches objects with a value for a given metadata key matches the given                operator type and value.
--
-- @key@ — The metadata key.
--
-- @operatorType@ — The comparison operator type for the expression.
--
-- @value@ — The value to be compared against.
--
-- ObjC selector: @+ predicateForObjectsWithMetadataKey:operatorType:value:@
predicateForObjectsWithMetadataKey_operatorType_value :: IsNSString key => key -> NSPredicateOperatorType -> RawId -> IO (Id NSPredicate)
predicateForObjectsWithMetadataKey_operatorType_value key operatorType value =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForObjectsWithMetadataKey_operatorType_valueSelector (toNSString key) operatorType value

-- | predicateForObjectsFromSource:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches objects saved by a given source.
--
-- @source@ — The source.
--
-- ObjC selector: @+ predicateForObjectsFromSource:@
predicateForObjectsFromSource :: IsHKSource source => source -> IO (Id NSPredicate)
predicateForObjectsFromSource source =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForObjectsFromSourceSelector (toHKSource source)

-- | predicateForObjectsFromSources:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches objects saved by any of the given sources.
--
-- @sources@ — The list of sources.
--
-- ObjC selector: @+ predicateForObjectsFromSources:@
predicateForObjectsFromSources :: IsNSSet sources => sources -> IO (Id NSPredicate)
predicateForObjectsFromSources sources =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForObjectsFromSourcesSelector (toNSSet sources)

-- | predicateForObjectsFromSourceRevisions:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches objects saved by any of the specified HKSourceRevisions.
--
-- @sourceRevisions@ — The list of source revisions.
--
-- ObjC selector: @+ predicateForObjectsFromSourceRevisions:@
predicateForObjectsFromSourceRevisions :: IsNSSet sourceRevisions => sourceRevisions -> IO (Id NSPredicate)
predicateForObjectsFromSourceRevisions sourceRevisions =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForObjectsFromSourceRevisionsSelector (toNSSet sourceRevisions)

-- | predicateForObjectsFromDevices:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches objects associated with any of the given devices. All properties                of each HKDevice are considered in the query and must match exactly, including nil values. To perform                 searches based on specific device properties, use predicateForObjectsWithDeviceProperty:allowedValues:.
--
-- @devices@ — The set of devices that generated data.
--
-- ObjC selector: @+ predicateForObjectsFromDevices:@
predicateForObjectsFromDevices :: IsNSSet devices => devices -> IO (Id NSPredicate)
predicateForObjectsFromDevices devices =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForObjectsFromDevicesSelector (toNSSet devices)

-- | predicateForObjectsWithDeviceProperty:allowedValues:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches objects associated with an HKDevice with the specified device                property matching any value included in allowedValues. To query for samples with devices that match all                 the properties of an HKDevice, use predicateForObjectsFromDevices.
--
-- @key@ — The device property key. (See HKDevice.h)
--
-- @allowedValues@ — The set of values for which the device property can match. An empty set will match all                devices whose property value is nil.
--
-- ObjC selector: @+ predicateForObjectsWithDeviceProperty:allowedValues:@
predicateForObjectsWithDeviceProperty_allowedValues :: (IsNSString key, IsNSSet allowedValues) => key -> allowedValues -> IO (Id NSPredicate)
predicateForObjectsWithDeviceProperty_allowedValues key allowedValues =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForObjectsWithDeviceProperty_allowedValuesSelector (toNSString key) (toNSSet allowedValues)

-- | predicateForObjectWithUUID:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches the object saved with a particular UUID.
--
-- @UUID@ — The UUID of the object.
--
-- ObjC selector: @+ predicateForObjectWithUUID:@
predicateForObjectWithUUID :: IsNSUUID uuid => uuid -> IO (Id NSPredicate)
predicateForObjectWithUUID uuid =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForObjectWithUUIDSelector (toNSUUID uuid)

-- | predicateForObjectsWithUUIDs:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches the objects saved with one of the given UUIDs.
--
-- @UUIDs@ — The set of NSUUIDs.
--
-- ObjC selector: @+ predicateForObjectsWithUUIDs:@
predicateForObjectsWithUUIDs :: IsNSSet uuiDs => uuiDs -> IO (Id NSPredicate)
predicateForObjectsWithUUIDs uuiDs =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForObjectsWithUUIDsSelector (toNSSet uuiDs)

-- | predicateForObjectsNoCorrelation
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches the objects that are not associated with an HKCorrelation.
--
-- ObjC selector: @+ predicateForObjectsWithNoCorrelation@
predicateForObjectsWithNoCorrelation :: IO (Id NSPredicate)
predicateForObjectsWithNoCorrelation  =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForObjectsWithNoCorrelationSelector

-- | predicateForObjectsFromWorkout:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches the objects that have been added to the given workout.
--
-- @workout@ — The HKWorkout that the object was added to.
--
-- ObjC selector: @+ predicateForObjectsFromWorkout:@
predicateForObjectsFromWorkout :: IsHKWorkout workout => workout -> IO (Id NSPredicate)
predicateForObjectsFromWorkout workout =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForObjectsFromWorkoutSelector (toHKWorkout workout)

-- | predicateForObjectsAssociatedWithElectrocardiogram:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches the objects that have been added to the given electrocardiogram
--
-- @electrocardiogram@ — The HKElectrocardiogram that the object was added to.
--
-- ObjC selector: @+ predicateForObjectsAssociatedWithElectrocardiogram:@
predicateForObjectsAssociatedWithElectrocardiogram :: IsHKElectrocardiogram electrocardiogram => electrocardiogram -> IO (Id NSPredicate)
predicateForObjectsAssociatedWithElectrocardiogram electrocardiogram =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForObjectsAssociatedWithElectrocardiogramSelector (toHKElectrocardiogram electrocardiogram)

-- | predicateForWorkoutEffortSamplesRelatedToWorkout:
--
-- Creates a predicate for use with HKQuery subclasses.
--
-- Creates a query predicate that matches Workout Effort samples that have been related to the given workout
--
-- @workout@ — The HKWorkout that the object is related to.
--
-- @activity@ — The HKWorkoutActivity that the object is related to.
--
-- ObjC selector: @+ predicateForWorkoutEffortSamplesRelatedToWorkout:activity:@
predicateForWorkoutEffortSamplesRelatedToWorkout_activity :: (IsHKWorkout workout, IsHKWorkoutActivity activity) => workout -> activity -> IO (Id NSPredicate)
predicateForWorkoutEffortSamplesRelatedToWorkout_activity workout activity =
  do
    cls' <- getRequiredClass "HKQuery"
    sendClassMessage cls' predicateForWorkoutEffortSamplesRelatedToWorkout_activitySelector (toHKWorkout workout) (toHKWorkoutActivity activity)

-- | @- objectType@
objectType :: IsHKQuery hkQuery => hkQuery -> IO (Id HKObjectType)
objectType hkQuery =
  sendMessage hkQuery objectTypeSelector

-- | @- sampleType@
sampleType :: IsHKQuery hkQuery => hkQuery -> IO (Id HKSampleType)
sampleType hkQuery =
  sendMessage hkQuery sampleTypeSelector

-- | @- predicate@
predicate :: IsHKQuery hkQuery => hkQuery -> IO (Id NSPredicate)
predicate hkQuery =
  sendMessage hkQuery predicateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKQuery)
initSelector = mkSelector "init"

-- | @Selector@ for @predicateForUserAnnotatedMedicationsWithIsArchived:@
predicateForUserAnnotatedMedicationsWithIsArchivedSelector :: Selector '[Bool] (Id NSPredicate)
predicateForUserAnnotatedMedicationsWithIsArchivedSelector = mkSelector "predicateForUserAnnotatedMedicationsWithIsArchived:"

-- | @Selector@ for @predicateForUserAnnotatedMedicationsWithHasSchedule:@
predicateForUserAnnotatedMedicationsWithHasScheduleSelector :: Selector '[Bool] (Id NSPredicate)
predicateForUserAnnotatedMedicationsWithHasScheduleSelector = mkSelector "predicateForUserAnnotatedMedicationsWithHasSchedule:"

-- | @Selector@ for @predicateForMedicationDoseEventWithStatus:@
predicateForMedicationDoseEventWithStatusSelector :: Selector '[HKMedicationDoseEventLogStatus] (Id NSPredicate)
predicateForMedicationDoseEventWithStatusSelector = mkSelector "predicateForMedicationDoseEventWithStatus:"

-- | @Selector@ for @predicateForMedicationDoseEventWithStatuses:@
predicateForMedicationDoseEventWithStatusesSelector :: Selector '[Id NSSet] (Id NSPredicate)
predicateForMedicationDoseEventWithStatusesSelector = mkSelector "predicateForMedicationDoseEventWithStatuses:"

-- | @Selector@ for @predicateForMedicationDoseEventWithScheduledDate:@
predicateForMedicationDoseEventWithScheduledDateSelector :: Selector '[Id NSDate] (Id NSPredicate)
predicateForMedicationDoseEventWithScheduledDateSelector = mkSelector "predicateForMedicationDoseEventWithScheduledDate:"

-- | @Selector@ for @predicateForMedicationDoseEventWithScheduledDates:@
predicateForMedicationDoseEventWithScheduledDatesSelector :: Selector '[Id NSSet] (Id NSPredicate)
predicateForMedicationDoseEventWithScheduledDatesSelector = mkSelector "predicateForMedicationDoseEventWithScheduledDates:"

-- | @Selector@ for @predicateForMedicationDoseEventWithScheduledStartDate:endDate:@
predicateForMedicationDoseEventWithScheduledStartDate_endDateSelector :: Selector '[Id NSDate, Id NSDate] (Id NSPredicate)
predicateForMedicationDoseEventWithScheduledStartDate_endDateSelector = mkSelector "predicateForMedicationDoseEventWithScheduledStartDate:endDate:"

-- | @Selector@ for @predicateForMedicationDoseEventWithMedicationConceptIdentifier:@
predicateForMedicationDoseEventWithMedicationConceptIdentifierSelector :: Selector '[Id HKHealthConceptIdentifier] (Id NSPredicate)
predicateForMedicationDoseEventWithMedicationConceptIdentifierSelector = mkSelector "predicateForMedicationDoseEventWithMedicationConceptIdentifier:"

-- | @Selector@ for @predicateForMedicationDoseEventWithMedicationConceptIdentifiers:@
predicateForMedicationDoseEventWithMedicationConceptIdentifiersSelector :: Selector '[Id NSSet] (Id NSPredicate)
predicateForMedicationDoseEventWithMedicationConceptIdentifiersSelector = mkSelector "predicateForMedicationDoseEventWithMedicationConceptIdentifiers:"

-- | @Selector@ for @predicateForStatesOfMindWithValence:operatorType:@
predicateForStatesOfMindWithValence_operatorTypeSelector :: Selector '[CDouble, NSPredicateOperatorType] (Id NSPredicate)
predicateForStatesOfMindWithValence_operatorTypeSelector = mkSelector "predicateForStatesOfMindWithValence:operatorType:"

-- | @Selector@ for @predicateForStatesOfMindWithKind:@
predicateForStatesOfMindWithKindSelector :: Selector '[HKStateOfMindKind] (Id NSPredicate)
predicateForStatesOfMindWithKindSelector = mkSelector "predicateForStatesOfMindWithKind:"

-- | @Selector@ for @predicateForStatesOfMindWithLabel:@
predicateForStatesOfMindWithLabelSelector :: Selector '[HKStateOfMindLabel] (Id NSPredicate)
predicateForStatesOfMindWithLabelSelector = mkSelector "predicateForStatesOfMindWithLabel:"

-- | @Selector@ for @predicateForStatesOfMindWithAssociation:@
predicateForStatesOfMindWithAssociationSelector :: Selector '[HKStateOfMindAssociation] (Id NSPredicate)
predicateForStatesOfMindWithAssociationSelector = mkSelector "predicateForStatesOfMindWithAssociation:"

-- | @Selector@ for @predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateInterval:@
predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateIntervalSelector :: Selector '[Id NSDateInterval] (Id NSPredicate)
predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateIntervalSelector = mkSelector "predicateForVerifiableClinicalRecordsWithRelevantDateWithinDateInterval:"

-- | @Selector@ for @predicateForElectrocardiogramsWithClassification:@
predicateForElectrocardiogramsWithClassificationSelector :: Selector '[HKElectrocardiogramClassification] (Id NSPredicate)
predicateForElectrocardiogramsWithClassificationSelector = mkSelector "predicateForElectrocardiogramsWithClassification:"

-- | @Selector@ for @predicateForElectrocardiogramsWithSymptomsStatus:@
predicateForElectrocardiogramsWithSymptomsStatusSelector :: Selector '[HKElectrocardiogramSymptomsStatus] (Id NSPredicate)
predicateForElectrocardiogramsWithSymptomsStatusSelector = mkSelector "predicateForElectrocardiogramsWithSymptomsStatus:"

-- | @Selector@ for @predicateForClinicalRecordsWithFHIRResourceType:@
predicateForClinicalRecordsWithFHIRResourceTypeSelector :: Selector '[Id NSString] (Id NSPredicate)
predicateForClinicalRecordsWithFHIRResourceTypeSelector = mkSelector "predicateForClinicalRecordsWithFHIRResourceType:"

-- | @Selector@ for @predicateForClinicalRecordsFromSource:FHIRResourceType:identifier:@
predicateForClinicalRecordsFromSource_FHIRResourceType_identifierSelector :: Selector '[Id HKSource, Id NSString, Id NSString] (Id NSPredicate)
predicateForClinicalRecordsFromSource_FHIRResourceType_identifierSelector = mkSelector "predicateForClinicalRecordsFromSource:FHIRResourceType:identifier:"

-- | @Selector@ for @predicateForActivitySummaryWithDateComponents:@
predicateForActivitySummaryWithDateComponentsSelector :: Selector '[Id NSDateComponents] (Id NSPredicate)
predicateForActivitySummaryWithDateComponentsSelector = mkSelector "predicateForActivitySummaryWithDateComponents:"

-- | @Selector@ for @predicateForActivitySummariesBetweenStartDateComponents:endDateComponents:@
predicateForActivitySummariesBetweenStartDateComponents_endDateComponentsSelector :: Selector '[Id NSDateComponents, Id NSDateComponents] (Id NSPredicate)
predicateForActivitySummariesBetweenStartDateComponents_endDateComponentsSelector = mkSelector "predicateForActivitySummariesBetweenStartDateComponents:endDateComponents:"

-- | @Selector@ for @predicateForWorkoutActivitiesWithWorkoutActivityType:@
predicateForWorkoutActivitiesWithWorkoutActivityTypeSelector :: Selector '[HKWorkoutActivityType] (Id NSPredicate)
predicateForWorkoutActivitiesWithWorkoutActivityTypeSelector = mkSelector "predicateForWorkoutActivitiesWithWorkoutActivityType:"

-- | @Selector@ for @predicateForWorkoutActivitiesWithOperatorType:duration:@
predicateForWorkoutActivitiesWithOperatorType_durationSelector :: Selector '[NSPredicateOperatorType, CDouble] (Id NSPredicate)
predicateForWorkoutActivitiesWithOperatorType_durationSelector = mkSelector "predicateForWorkoutActivitiesWithOperatorType:duration:"

-- | @Selector@ for @predicateForWorkoutActivitiesWithStartDate:endDate:options:@
predicateForWorkoutActivitiesWithStartDate_endDate_optionsSelector :: Selector '[Id NSDate, Id NSDate, HKQueryOptions] (Id NSPredicate)
predicateForWorkoutActivitiesWithStartDate_endDate_optionsSelector = mkSelector "predicateForWorkoutActivitiesWithStartDate:endDate:options:"

-- | @Selector@ for @predicateForWorkoutActivitiesWithOperatorType:quantityType:sumQuantity:@
predicateForWorkoutActivitiesWithOperatorType_quantityType_sumQuantitySelector :: Selector '[NSPredicateOperatorType, Id HKQuantityType, Id HKQuantity] (Id NSPredicate)
predicateForWorkoutActivitiesWithOperatorType_quantityType_sumQuantitySelector = mkSelector "predicateForWorkoutActivitiesWithOperatorType:quantityType:sumQuantity:"

-- | @Selector@ for @predicateForWorkoutActivitiesWithOperatorType:quantityType:minimumQuantity:@
predicateForWorkoutActivitiesWithOperatorType_quantityType_minimumQuantitySelector :: Selector '[NSPredicateOperatorType, Id HKQuantityType, Id HKQuantity] (Id NSPredicate)
predicateForWorkoutActivitiesWithOperatorType_quantityType_minimumQuantitySelector = mkSelector "predicateForWorkoutActivitiesWithOperatorType:quantityType:minimumQuantity:"

-- | @Selector@ for @predicateForWorkoutActivitiesWithOperatorType:quantityType:maximumQuantity:@
predicateForWorkoutActivitiesWithOperatorType_quantityType_maximumQuantitySelector :: Selector '[NSPredicateOperatorType, Id HKQuantityType, Id HKQuantity] (Id NSPredicate)
predicateForWorkoutActivitiesWithOperatorType_quantityType_maximumQuantitySelector = mkSelector "predicateForWorkoutActivitiesWithOperatorType:quantityType:maximumQuantity:"

-- | @Selector@ for @predicateForWorkoutActivitiesWithOperatorType:quantityType:averageQuantity:@
predicateForWorkoutActivitiesWithOperatorType_quantityType_averageQuantitySelector :: Selector '[NSPredicateOperatorType, Id HKQuantityType, Id HKQuantity] (Id NSPredicate)
predicateForWorkoutActivitiesWithOperatorType_quantityType_averageQuantitySelector = mkSelector "predicateForWorkoutActivitiesWithOperatorType:quantityType:averageQuantity:"

-- | @Selector@ for @predicateForWorkoutsWithActivityPredicate:@
predicateForWorkoutsWithActivityPredicateSelector :: Selector '[Id NSPredicate] (Id NSPredicate)
predicateForWorkoutsWithActivityPredicateSelector = mkSelector "predicateForWorkoutsWithActivityPredicate:"

-- | @Selector@ for @predicateForWorkoutsWithWorkoutActivityType:@
predicateForWorkoutsWithWorkoutActivityTypeSelector :: Selector '[HKWorkoutActivityType] (Id NSPredicate)
predicateForWorkoutsWithWorkoutActivityTypeSelector = mkSelector "predicateForWorkoutsWithWorkoutActivityType:"

-- | @Selector@ for @predicateForWorkoutsWithOperatorType:duration:@
predicateForWorkoutsWithOperatorType_durationSelector :: Selector '[NSPredicateOperatorType, CDouble] (Id NSPredicate)
predicateForWorkoutsWithOperatorType_durationSelector = mkSelector "predicateForWorkoutsWithOperatorType:duration:"

-- | @Selector@ for @predicateForWorkoutsWithOperatorType:totalEnergyBurned:@
predicateForWorkoutsWithOperatorType_totalEnergyBurnedSelector :: Selector '[NSPredicateOperatorType, Id HKQuantity] (Id NSPredicate)
predicateForWorkoutsWithOperatorType_totalEnergyBurnedSelector = mkSelector "predicateForWorkoutsWithOperatorType:totalEnergyBurned:"

-- | @Selector@ for @predicateForWorkoutsWithOperatorType:totalDistance:@
predicateForWorkoutsWithOperatorType_totalDistanceSelector :: Selector '[NSPredicateOperatorType, Id HKQuantity] (Id NSPredicate)
predicateForWorkoutsWithOperatorType_totalDistanceSelector = mkSelector "predicateForWorkoutsWithOperatorType:totalDistance:"

-- | @Selector@ for @predicateForWorkoutsWithOperatorType:totalSwimmingStrokeCount:@
predicateForWorkoutsWithOperatorType_totalSwimmingStrokeCountSelector :: Selector '[NSPredicateOperatorType, Id HKQuantity] (Id NSPredicate)
predicateForWorkoutsWithOperatorType_totalSwimmingStrokeCountSelector = mkSelector "predicateForWorkoutsWithOperatorType:totalSwimmingStrokeCount:"

-- | @Selector@ for @predicateForWorkoutsWithOperatorType:totalFlightsClimbed:@
predicateForWorkoutsWithOperatorType_totalFlightsClimbedSelector :: Selector '[NSPredicateOperatorType, Id HKQuantity] (Id NSPredicate)
predicateForWorkoutsWithOperatorType_totalFlightsClimbedSelector = mkSelector "predicateForWorkoutsWithOperatorType:totalFlightsClimbed:"

-- | @Selector@ for @predicateForWorkoutsWithOperatorType:quantityType:sumQuantity:@
predicateForWorkoutsWithOperatorType_quantityType_sumQuantitySelector :: Selector '[NSPredicateOperatorType, Id HKQuantityType, Id HKQuantity] (Id NSPredicate)
predicateForWorkoutsWithOperatorType_quantityType_sumQuantitySelector = mkSelector "predicateForWorkoutsWithOperatorType:quantityType:sumQuantity:"

-- | @Selector@ for @predicateForWorkoutsWithOperatorType:quantityType:minimumQuantity:@
predicateForWorkoutsWithOperatorType_quantityType_minimumQuantitySelector :: Selector '[NSPredicateOperatorType, Id HKQuantityType, Id HKQuantity] (Id NSPredicate)
predicateForWorkoutsWithOperatorType_quantityType_minimumQuantitySelector = mkSelector "predicateForWorkoutsWithOperatorType:quantityType:minimumQuantity:"

-- | @Selector@ for @predicateForWorkoutsWithOperatorType:quantityType:maximumQuantity:@
predicateForWorkoutsWithOperatorType_quantityType_maximumQuantitySelector :: Selector '[NSPredicateOperatorType, Id HKQuantityType, Id HKQuantity] (Id NSPredicate)
predicateForWorkoutsWithOperatorType_quantityType_maximumQuantitySelector = mkSelector "predicateForWorkoutsWithOperatorType:quantityType:maximumQuantity:"

-- | @Selector@ for @predicateForWorkoutsWithOperatorType:quantityType:averageQuantity:@
predicateForWorkoutsWithOperatorType_quantityType_averageQuantitySelector :: Selector '[NSPredicateOperatorType, Id HKQuantityType, Id HKQuantity] (Id NSPredicate)
predicateForWorkoutsWithOperatorType_quantityType_averageQuantitySelector = mkSelector "predicateForWorkoutsWithOperatorType:quantityType:averageQuantity:"

-- | @Selector@ for @predicateForCategorySamplesWithOperatorType:value:@
predicateForCategorySamplesWithOperatorType_valueSelector :: Selector '[NSPredicateOperatorType, CLong] (Id NSPredicate)
predicateForCategorySamplesWithOperatorType_valueSelector = mkSelector "predicateForCategorySamplesWithOperatorType:value:"

-- | @Selector@ for @predicateForCategorySamplesEqualToValues:@
predicateForCategorySamplesEqualToValuesSelector :: Selector '[Id NSSet] (Id NSPredicate)
predicateForCategorySamplesEqualToValuesSelector = mkSelector "predicateForCategorySamplesEqualToValues:"

-- | @Selector@ for @predicateForQuantitySamplesWithOperatorType:quantity:@
predicateForQuantitySamplesWithOperatorType_quantitySelector :: Selector '[NSPredicateOperatorType, Id HKQuantity] (Id NSPredicate)
predicateForQuantitySamplesWithOperatorType_quantitySelector = mkSelector "predicateForQuantitySamplesWithOperatorType:quantity:"

-- | @Selector@ for @predicateForSamplesWithStartDate:endDate:options:@
predicateForSamplesWithStartDate_endDate_optionsSelector :: Selector '[Id NSDate, Id NSDate, HKQueryOptions] (Id NSPredicate)
predicateForSamplesWithStartDate_endDate_optionsSelector = mkSelector "predicateForSamplesWithStartDate:endDate:options:"

-- | @Selector@ for @predicateForObjectsWithMetadataKey:@
predicateForObjectsWithMetadataKeySelector :: Selector '[Id NSString] (Id NSPredicate)
predicateForObjectsWithMetadataKeySelector = mkSelector "predicateForObjectsWithMetadataKey:"

-- | @Selector@ for @predicateForObjectsWithMetadataKey:allowedValues:@
predicateForObjectsWithMetadataKey_allowedValuesSelector :: Selector '[Id NSString, Id NSArray] (Id NSPredicate)
predicateForObjectsWithMetadataKey_allowedValuesSelector = mkSelector "predicateForObjectsWithMetadataKey:allowedValues:"

-- | @Selector@ for @predicateForObjectsWithMetadataKey:operatorType:value:@
predicateForObjectsWithMetadataKey_operatorType_valueSelector :: Selector '[Id NSString, NSPredicateOperatorType, RawId] (Id NSPredicate)
predicateForObjectsWithMetadataKey_operatorType_valueSelector = mkSelector "predicateForObjectsWithMetadataKey:operatorType:value:"

-- | @Selector@ for @predicateForObjectsFromSource:@
predicateForObjectsFromSourceSelector :: Selector '[Id HKSource] (Id NSPredicate)
predicateForObjectsFromSourceSelector = mkSelector "predicateForObjectsFromSource:"

-- | @Selector@ for @predicateForObjectsFromSources:@
predicateForObjectsFromSourcesSelector :: Selector '[Id NSSet] (Id NSPredicate)
predicateForObjectsFromSourcesSelector = mkSelector "predicateForObjectsFromSources:"

-- | @Selector@ for @predicateForObjectsFromSourceRevisions:@
predicateForObjectsFromSourceRevisionsSelector :: Selector '[Id NSSet] (Id NSPredicate)
predicateForObjectsFromSourceRevisionsSelector = mkSelector "predicateForObjectsFromSourceRevisions:"

-- | @Selector@ for @predicateForObjectsFromDevices:@
predicateForObjectsFromDevicesSelector :: Selector '[Id NSSet] (Id NSPredicate)
predicateForObjectsFromDevicesSelector = mkSelector "predicateForObjectsFromDevices:"

-- | @Selector@ for @predicateForObjectsWithDeviceProperty:allowedValues:@
predicateForObjectsWithDeviceProperty_allowedValuesSelector :: Selector '[Id NSString, Id NSSet] (Id NSPredicate)
predicateForObjectsWithDeviceProperty_allowedValuesSelector = mkSelector "predicateForObjectsWithDeviceProperty:allowedValues:"

-- | @Selector@ for @predicateForObjectWithUUID:@
predicateForObjectWithUUIDSelector :: Selector '[Id NSUUID] (Id NSPredicate)
predicateForObjectWithUUIDSelector = mkSelector "predicateForObjectWithUUID:"

-- | @Selector@ for @predicateForObjectsWithUUIDs:@
predicateForObjectsWithUUIDsSelector :: Selector '[Id NSSet] (Id NSPredicate)
predicateForObjectsWithUUIDsSelector = mkSelector "predicateForObjectsWithUUIDs:"

-- | @Selector@ for @predicateForObjectsWithNoCorrelation@
predicateForObjectsWithNoCorrelationSelector :: Selector '[] (Id NSPredicate)
predicateForObjectsWithNoCorrelationSelector = mkSelector "predicateForObjectsWithNoCorrelation"

-- | @Selector@ for @predicateForObjectsFromWorkout:@
predicateForObjectsFromWorkoutSelector :: Selector '[Id HKWorkout] (Id NSPredicate)
predicateForObjectsFromWorkoutSelector = mkSelector "predicateForObjectsFromWorkout:"

-- | @Selector@ for @predicateForObjectsAssociatedWithElectrocardiogram:@
predicateForObjectsAssociatedWithElectrocardiogramSelector :: Selector '[Id HKElectrocardiogram] (Id NSPredicate)
predicateForObjectsAssociatedWithElectrocardiogramSelector = mkSelector "predicateForObjectsAssociatedWithElectrocardiogram:"

-- | @Selector@ for @predicateForWorkoutEffortSamplesRelatedToWorkout:activity:@
predicateForWorkoutEffortSamplesRelatedToWorkout_activitySelector :: Selector '[Id HKWorkout, Id HKWorkoutActivity] (Id NSPredicate)
predicateForWorkoutEffortSamplesRelatedToWorkout_activitySelector = mkSelector "predicateForWorkoutEffortSamplesRelatedToWorkout:activity:"

-- | @Selector@ for @objectType@
objectTypeSelector :: Selector '[] (Id HKObjectType)
objectTypeSelector = mkSelector "objectType"

-- | @Selector@ for @sampleType@
sampleTypeSelector :: Selector '[] (Id HKSampleType)
sampleTypeSelector = mkSelector "sampleType"

-- | @Selector@ for @predicate@
predicateSelector :: Selector '[] (Id NSPredicate)
predicateSelector = mkSelector "predicate"

