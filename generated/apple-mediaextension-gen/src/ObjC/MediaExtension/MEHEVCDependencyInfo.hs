{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MEHEVCDependencyInfo
--
-- Provides information about the HEVC dependency attributes of a sample.
--
-- An instance of this class is returned by MESampleCursor property hevcDependencyInfo.
--
-- Generated bindings for @MEHEVCDependencyInfo@.
module ObjC.MediaExtension.MEHEVCDependencyInfo
  ( MEHEVCDependencyInfo
  , IsMEHEVCDependencyInfo(..)
  , temporalSubLayerAccess
  , setTemporalSubLayerAccess
  , stepwiseTemporalSubLayerAccess
  , setStepwiseTemporalSubLayerAccess
  , syncSampleNALUnitType
  , setSyncSampleNALUnitType
  , temporalLevel
  , setTemporalLevel
  , profileSpace
  , setProfileSpace
  , tierFlag
  , setTierFlag
  , profileIndex
  , setProfileIndex
  , profileCompatibilityFlags
  , setProfileCompatibilityFlags
  , constraintIndicatorFlags
  , setConstraintIndicatorFlags
  , levelIndex
  , setLevelIndex
  , constraintIndicatorFlagsSelector
  , levelIndexSelector
  , profileCompatibilityFlagsSelector
  , profileIndexSelector
  , profileSpaceSelector
  , setConstraintIndicatorFlagsSelector
  , setLevelIndexSelector
  , setProfileCompatibilityFlagsSelector
  , setProfileIndexSelector
  , setProfileSpaceSelector
  , setStepwiseTemporalSubLayerAccessSelector
  , setSyncSampleNALUnitTypeSelector
  , setTemporalLevelSelector
  , setTemporalSubLayerAccessSelector
  , setTierFlagSelector
  , stepwiseTemporalSubLayerAccessSelector
  , syncSampleNALUnitTypeSelector
  , temporalLevelSelector
  , temporalSubLayerAccessSelector
  , tierFlagSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | temporalSubLayerAccess
--
-- YES if the sample is an HEVC 'TSA' picture, NO otherwise.
--
-- Maps to the kCMSampleAttachmentKey_HEVCTemporalSubLayerAccess sample buffer attachment.
--
-- ObjC selector: @- temporalSubLayerAccess@
temporalSubLayerAccess :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> IO Bool
temporalSubLayerAccess mehevcDependencyInfo =
  sendMessage mehevcDependencyInfo temporalSubLayerAccessSelector

-- | temporalSubLayerAccess
--
-- YES if the sample is an HEVC 'TSA' picture, NO otherwise.
--
-- Maps to the kCMSampleAttachmentKey_HEVCTemporalSubLayerAccess sample buffer attachment.
--
-- ObjC selector: @- setTemporalSubLayerAccess:@
setTemporalSubLayerAccess :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> Bool -> IO ()
setTemporalSubLayerAccess mehevcDependencyInfo value =
  sendMessage mehevcDependencyInfo setTemporalSubLayerAccessSelector value

-- | stepwiseTemporalSubLayerAccess
--
-- YES if the sample is an HEVC 'STSA' picture, NO otherwise.
--
-- Maps to the kCMSampleAttachmentKey_HEVCStepwiseTemporalSubLayerAccess sample buffer attachment.
--
-- ObjC selector: @- stepwiseTemporalSubLayerAccess@
stepwiseTemporalSubLayerAccess :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> IO Bool
stepwiseTemporalSubLayerAccess mehevcDependencyInfo =
  sendMessage mehevcDependencyInfo stepwiseTemporalSubLayerAccessSelector

-- | stepwiseTemporalSubLayerAccess
--
-- YES if the sample is an HEVC 'STSA' picture, NO otherwise.
--
-- Maps to the kCMSampleAttachmentKey_HEVCStepwiseTemporalSubLayerAccess sample buffer attachment.
--
-- ObjC selector: @- setStepwiseTemporalSubLayerAccess:@
setStepwiseTemporalSubLayerAccess :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> Bool -> IO ()
setStepwiseTemporalSubLayerAccess mehevcDependencyInfo value =
  sendMessage mehevcDependencyInfo setStepwiseTemporalSubLayerAccessSelector value

-- | syncSampleNALUnitType
--
-- The NAL unit type for HEVC 'sync' sample groups, or -1 if this information is not available.
--
-- Maps to the kCMSampleAttachmentKey_HEVCSyncSampleNALUnitType sample buffer attachment.
--
-- ObjC selector: @- syncSampleNALUnitType@
syncSampleNALUnitType :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> IO CShort
syncSampleNALUnitType mehevcDependencyInfo =
  sendMessage mehevcDependencyInfo syncSampleNALUnitTypeSelector

-- | syncSampleNALUnitType
--
-- The NAL unit type for HEVC 'sync' sample groups, or -1 if this information is not available.
--
-- Maps to the kCMSampleAttachmentKey_HEVCSyncSampleNALUnitType sample buffer attachment.
--
-- ObjC selector: @- setSyncSampleNALUnitType:@
setSyncSampleNALUnitType :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> CShort -> IO ()
setSyncSampleNALUnitType mehevcDependencyInfo value =
  sendMessage mehevcDependencyInfo setSyncSampleNALUnitTypeSelector value

-- | temporalLevel
--
-- The HEVC temporal level, or -1 if this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_TemporalLevel sample buffer attachment.
--
-- ObjC selector: @- temporalLevel@
temporalLevel :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> IO CShort
temporalLevel mehevcDependencyInfo =
  sendMessage mehevcDependencyInfo temporalLevelSelector

-- | temporalLevel
--
-- The HEVC temporal level, or -1 if this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_TemporalLevel sample buffer attachment.
--
-- ObjC selector: @- setTemporalLevel:@
setTemporalLevel :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> CShort -> IO ()
setTemporalLevel mehevcDependencyInfo value =
  sendMessage mehevcDependencyInfo setTemporalLevelSelector value

-- | profileSpace
--
-- The HEVC profile space, or -1 if this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_ProfileSpace sample buffer attachment.
--
-- ObjC selector: @- profileSpace@
profileSpace :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> IO CShort
profileSpace mehevcDependencyInfo =
  sendMessage mehevcDependencyInfo profileSpaceSelector

-- | profileSpace
--
-- The HEVC profile space, or -1 if this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_ProfileSpace sample buffer attachment.
--
-- ObjC selector: @- setProfileSpace:@
setProfileSpace :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> CShort -> IO ()
setProfileSpace mehevcDependencyInfo value =
  sendMessage mehevcDependencyInfo setProfileSpaceSelector value

-- | tierFlag
--
-- The HEVC tier level flag, or -1 if this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_TierFlag sample buffer attachment.
--
-- ObjC selector: @- tierFlag@
tierFlag :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> IO CShort
tierFlag mehevcDependencyInfo =
  sendMessage mehevcDependencyInfo tierFlagSelector

-- | tierFlag
--
-- The HEVC tier level flag, or -1 if this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_TierFlag sample buffer attachment.
--
-- ObjC selector: @- setTierFlag:@
setTierFlag :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> CShort -> IO ()
setTierFlag mehevcDependencyInfo value =
  sendMessage mehevcDependencyInfo setTierFlagSelector value

-- | profileIndex
--
-- The HEVC profile index, or -1 if this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_ProfileIndex sample buffer attachment.
--
-- ObjC selector: @- profileIndex@
profileIndex :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> IO CShort
profileIndex mehevcDependencyInfo =
  sendMessage mehevcDependencyInfo profileIndexSelector

-- | profileIndex
--
-- The HEVC profile index, or -1 if this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_ProfileIndex sample buffer attachment.
--
-- ObjC selector: @- setProfileIndex:@
setProfileIndex :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> CShort -> IO ()
setProfileIndex mehevcDependencyInfo value =
  sendMessage mehevcDependencyInfo setProfileIndexSelector value

-- | profileCompatibilityFlags
--
-- The HEVC profile compatibility flags (4 bytes), or nil of this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_ProfileCompatibilityFlags sample buffer attachment.
--
-- ObjC selector: @- profileCompatibilityFlags@
profileCompatibilityFlags :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> IO (Id NSData)
profileCompatibilityFlags mehevcDependencyInfo =
  sendMessage mehevcDependencyInfo profileCompatibilityFlagsSelector

-- | profileCompatibilityFlags
--
-- The HEVC profile compatibility flags (4 bytes), or nil of this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_ProfileCompatibilityFlags sample buffer attachment.
--
-- ObjC selector: @- setProfileCompatibilityFlags:@
setProfileCompatibilityFlags :: (IsMEHEVCDependencyInfo mehevcDependencyInfo, IsNSData value) => mehevcDependencyInfo -> value -> IO ()
setProfileCompatibilityFlags mehevcDependencyInfo value =
  sendMessage mehevcDependencyInfo setProfileCompatibilityFlagsSelector (toNSData value)

-- | constraintIndicatorFlags
--
-- The HEVC constraint indicator flags (6 bytes), or nil of this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_ConstraintIndicatorFlags sample buffer attachment.
--
-- ObjC selector: @- constraintIndicatorFlags@
constraintIndicatorFlags :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> IO (Id NSData)
constraintIndicatorFlags mehevcDependencyInfo =
  sendMessage mehevcDependencyInfo constraintIndicatorFlagsSelector

-- | constraintIndicatorFlags
--
-- The HEVC constraint indicator flags (6 bytes), or nil of this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_ConstraintIndicatorFlags sample buffer attachment.
--
-- ObjC selector: @- setConstraintIndicatorFlags:@
setConstraintIndicatorFlags :: (IsMEHEVCDependencyInfo mehevcDependencyInfo, IsNSData value) => mehevcDependencyInfo -> value -> IO ()
setConstraintIndicatorFlags mehevcDependencyInfo value =
  sendMessage mehevcDependencyInfo setConstraintIndicatorFlagsSelector (toNSData value)

-- | levelIndex
--
-- The HEVC level index, or -1 if this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_LevelIndex sample buffer attachment.
--
-- ObjC selector: @- levelIndex@
levelIndex :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> IO CShort
levelIndex mehevcDependencyInfo =
  sendMessage mehevcDependencyInfo levelIndexSelector

-- | levelIndex
--
-- The HEVC level index, or -1 if this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_LevelIndex sample buffer attachment.
--
-- ObjC selector: @- setLevelIndex:@
setLevelIndex :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> CShort -> IO ()
setLevelIndex mehevcDependencyInfo value =
  sendMessage mehevcDependencyInfo setLevelIndexSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @temporalSubLayerAccess@
temporalSubLayerAccessSelector :: Selector '[] Bool
temporalSubLayerAccessSelector = mkSelector "temporalSubLayerAccess"

-- | @Selector@ for @setTemporalSubLayerAccess:@
setTemporalSubLayerAccessSelector :: Selector '[Bool] ()
setTemporalSubLayerAccessSelector = mkSelector "setTemporalSubLayerAccess:"

-- | @Selector@ for @stepwiseTemporalSubLayerAccess@
stepwiseTemporalSubLayerAccessSelector :: Selector '[] Bool
stepwiseTemporalSubLayerAccessSelector = mkSelector "stepwiseTemporalSubLayerAccess"

-- | @Selector@ for @setStepwiseTemporalSubLayerAccess:@
setStepwiseTemporalSubLayerAccessSelector :: Selector '[Bool] ()
setStepwiseTemporalSubLayerAccessSelector = mkSelector "setStepwiseTemporalSubLayerAccess:"

-- | @Selector@ for @syncSampleNALUnitType@
syncSampleNALUnitTypeSelector :: Selector '[] CShort
syncSampleNALUnitTypeSelector = mkSelector "syncSampleNALUnitType"

-- | @Selector@ for @setSyncSampleNALUnitType:@
setSyncSampleNALUnitTypeSelector :: Selector '[CShort] ()
setSyncSampleNALUnitTypeSelector = mkSelector "setSyncSampleNALUnitType:"

-- | @Selector@ for @temporalLevel@
temporalLevelSelector :: Selector '[] CShort
temporalLevelSelector = mkSelector "temporalLevel"

-- | @Selector@ for @setTemporalLevel:@
setTemporalLevelSelector :: Selector '[CShort] ()
setTemporalLevelSelector = mkSelector "setTemporalLevel:"

-- | @Selector@ for @profileSpace@
profileSpaceSelector :: Selector '[] CShort
profileSpaceSelector = mkSelector "profileSpace"

-- | @Selector@ for @setProfileSpace:@
setProfileSpaceSelector :: Selector '[CShort] ()
setProfileSpaceSelector = mkSelector "setProfileSpace:"

-- | @Selector@ for @tierFlag@
tierFlagSelector :: Selector '[] CShort
tierFlagSelector = mkSelector "tierFlag"

-- | @Selector@ for @setTierFlag:@
setTierFlagSelector :: Selector '[CShort] ()
setTierFlagSelector = mkSelector "setTierFlag:"

-- | @Selector@ for @profileIndex@
profileIndexSelector :: Selector '[] CShort
profileIndexSelector = mkSelector "profileIndex"

-- | @Selector@ for @setProfileIndex:@
setProfileIndexSelector :: Selector '[CShort] ()
setProfileIndexSelector = mkSelector "setProfileIndex:"

-- | @Selector@ for @profileCompatibilityFlags@
profileCompatibilityFlagsSelector :: Selector '[] (Id NSData)
profileCompatibilityFlagsSelector = mkSelector "profileCompatibilityFlags"

-- | @Selector@ for @setProfileCompatibilityFlags:@
setProfileCompatibilityFlagsSelector :: Selector '[Id NSData] ()
setProfileCompatibilityFlagsSelector = mkSelector "setProfileCompatibilityFlags:"

-- | @Selector@ for @constraintIndicatorFlags@
constraintIndicatorFlagsSelector :: Selector '[] (Id NSData)
constraintIndicatorFlagsSelector = mkSelector "constraintIndicatorFlags"

-- | @Selector@ for @setConstraintIndicatorFlags:@
setConstraintIndicatorFlagsSelector :: Selector '[Id NSData] ()
setConstraintIndicatorFlagsSelector = mkSelector "setConstraintIndicatorFlags:"

-- | @Selector@ for @levelIndex@
levelIndexSelector :: Selector '[] CShort
levelIndexSelector = mkSelector "levelIndex"

-- | @Selector@ for @setLevelIndex:@
setLevelIndexSelector :: Selector '[CShort] ()
setLevelIndexSelector = mkSelector "setLevelIndex:"

