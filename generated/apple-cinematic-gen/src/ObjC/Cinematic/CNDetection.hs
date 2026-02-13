{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A cinematic detection of a subject.
--
-- Specifies the type, distance (as disparity), bounds (as a normalized rectangle), and time (as CMTime) of the detection. Detections obtained from the cinematic script include a detectionID that can be used to track the detection over time. Some types of detections also include a detectionGroupID that associates related detections (e.g. the face and torso of the same person).
--
-- Generated bindings for @CNDetection@.
module ObjC.Cinematic.CNDetection
  ( CNDetection
  , IsCNDetection(..)
  , isValidDetectionID
  , isValidDetectionGroupID
  , accessibilityLabelForDetectionType
  , init_
  , new
  , detectionType
  , focusDisparity
  , detectionID
  , detectionGroupID
  , accessibilityLabelForDetectionTypeSelector
  , detectionGroupIDSelector
  , detectionIDSelector
  , detectionTypeSelector
  , focusDisparitySelector
  , initSelector
  , isValidDetectionGroupIDSelector
  , isValidDetectionIDSelector
  , newSelector

  -- * Enum types
  , CNDetectionType(CNDetectionType)
  , pattern CNDetectionTypeUnknown
  , pattern CNDetectionTypeHumanFace
  , pattern CNDetectionTypeHumanHead
  , pattern CNDetectionTypeHumanTorso
  , pattern CNDetectionTypeCatBody
  , pattern CNDetectionTypeDogBody
  , pattern CNDetectionTypeCatHead
  , pattern CNDetectionTypeDogHead
  , pattern CNDetectionTypeSportsBall
  , pattern CNDetectionTypeAutoFocus
  , pattern CNDetectionTypeFixedFocus
  , pattern CNDetectionTypeCustom

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Cinematic.Internal.Classes
import ObjC.Cinematic.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Determine whether a given detectionID is valid
--
-- ObjC selector: @+ isValidDetectionID:@
isValidDetectionID :: CLong -> IO Bool
isValidDetectionID detectionID =
  do
    cls' <- getRequiredClass "CNDetection"
    sendClassMessage cls' isValidDetectionIDSelector detectionID

-- | Determine whether a given detectionGroupID is valid
--
-- ObjC selector: @+ isValidDetectionGroupID:@
isValidDetectionGroupID :: CLong -> IO Bool
isValidDetectionGroupID detectionGroupID =
  do
    cls' <- getRequiredClass "CNDetection"
    sendClassMessage cls' isValidDetectionGroupIDSelector detectionGroupID

-- | A localized accessibility label converting a specific detection type into a broad category (person, pet, etc.).
--
-- ObjC selector: @+ accessibilityLabelForDetectionType:@
accessibilityLabelForDetectionType :: CNDetectionType -> IO (Id NSString)
accessibilityLabelForDetectionType detectionType =
  do
    cls' <- getRequiredClass "CNDetection"
    sendClassMessage cls' accessibilityLabelForDetectionTypeSelector detectionType

-- | @- init@
init_ :: IsCNDetection cnDetection => cnDetection -> IO (Id CNDetection)
init_ cnDetection =
  sendOwnedMessage cnDetection initSelector

-- | @+ new@
new :: IO (Id CNDetection)
new  =
  do
    cls' <- getRequiredClass "CNDetection"
    sendOwnedClassMessage cls' newSelector

-- | The type of object that was detected (face, torso, cat, dog, etc.)
--
-- ObjC selector: @- detectionType@
detectionType :: IsCNDetection cnDetection => cnDetection -> IO CNDetectionType
detectionType cnDetection =
  sendMessage cnDetection detectionTypeSelector

-- | The disparity to use in order to focus on the object. If the disparity is unknown, use the class method to find it: @disparityInNormalizedRect:sourceDisparity:detectionType:priorDisparity:@.
--
-- ObjC selector: @- focusDisparity@
focusDisparity :: IsCNDetection cnDetection => cnDetection -> IO CFloat
focusDisparity cnDetection =
  sendMessage cnDetection focusDisparitySelector

-- | An unique identifier assigned by the cinematic script to all detections of the same subject and detection type across time. If you build a custom detection track, the detectionID will be assigned when you add it to the script.
--
-- ObjC selector: @- detectionID@
detectionID :: IsCNDetection cnDetection => cnDetection -> IO CLong
detectionID cnDetection =
  sendMessage cnDetection detectionIDSelector

-- | An unique identifier assigned by the cinematic script to all detections of the same subject and related detection types across time. For example, the face/torso detections of the same person are assigned the same detectionGroupID.
--
-- ObjC selector: @- detectionGroupID@
detectionGroupID :: IsCNDetection cnDetection => cnDetection -> IO CLong
detectionGroupID cnDetection =
  sendMessage cnDetection detectionGroupIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isValidDetectionID:@
isValidDetectionIDSelector :: Selector '[CLong] Bool
isValidDetectionIDSelector = mkSelector "isValidDetectionID:"

-- | @Selector@ for @isValidDetectionGroupID:@
isValidDetectionGroupIDSelector :: Selector '[CLong] Bool
isValidDetectionGroupIDSelector = mkSelector "isValidDetectionGroupID:"

-- | @Selector@ for @accessibilityLabelForDetectionType:@
accessibilityLabelForDetectionTypeSelector :: Selector '[CNDetectionType] (Id NSString)
accessibilityLabelForDetectionTypeSelector = mkSelector "accessibilityLabelForDetectionType:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CNDetection)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CNDetection)
newSelector = mkSelector "new"

-- | @Selector@ for @detectionType@
detectionTypeSelector :: Selector '[] CNDetectionType
detectionTypeSelector = mkSelector "detectionType"

-- | @Selector@ for @focusDisparity@
focusDisparitySelector :: Selector '[] CFloat
focusDisparitySelector = mkSelector "focusDisparity"

-- | @Selector@ for @detectionID@
detectionIDSelector :: Selector '[] CLong
detectionIDSelector = mkSelector "detectionID"

-- | @Selector@ for @detectionGroupID@
detectionGroupIDSelector :: Selector '[] CLong
detectionGroupIDSelector = mkSelector "detectionGroupID"

