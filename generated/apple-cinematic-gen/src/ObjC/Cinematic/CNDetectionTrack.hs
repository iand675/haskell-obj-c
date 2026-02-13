{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Abstract class representing a series of detections of the same subject over time.
--
-- Generated bindings for @CNDetectionTrack@.
module ObjC.Cinematic.CNDetectionTrack
  ( CNDetectionTrack
  , IsCNDetectionTrack(..)
  , init_
  , new
  , detectionType
  , detectionID
  , detectionGroupID
  , userCreated
  , discrete
  , detectionGroupIDSelector
  , detectionIDSelector
  , detectionTypeSelector
  , discreteSelector
  , initSelector
  , newSelector
  , userCreatedSelector

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

-- | @- init@
init_ :: IsCNDetectionTrack cnDetectionTrack => cnDetectionTrack -> IO (Id CNDetectionTrack)
init_ cnDetectionTrack =
  sendOwnedMessage cnDetectionTrack initSelector

-- | @+ new@
new :: IO (Id CNDetectionTrack)
new  =
  do
    cls' <- getRequiredClass "CNDetectionTrack"
    sendOwnedClassMessage cls' newSelector

-- | The type of subject detected by this detection track.
--
-- ObjC selector: @- detectionType@
detectionType :: IsCNDetectionTrack cnDetectionTrack => cnDetectionTrack -> IO CNDetectionType
detectionType cnDetectionTrack =
  sendMessage cnDetectionTrack detectionTypeSelector

-- | The detectionID of the subject detected during this track; unique within a cinematic script.
--
-- ObjC selector: @- detectionID@
detectionID :: IsCNDetectionTrack cnDetectionTrack => cnDetectionTrack -> IO CLong
detectionID cnDetectionTrack =
  sendMessage cnDetectionTrack detectionIDSelector

-- | The detectionGroupID of the subject detected by the track.
--
-- The detectionGroupID can be used to associate related detections such as the face and torso of the same person.
--
-- ObjC selector: @- detectionGroupID@
detectionGroupID :: IsCNDetectionTrack cnDetectionTrack => cnDetectionTrack -> IO CLong
detectionGroupID cnDetectionTrack =
  sendMessage cnDetectionTrack detectionGroupIDSelector

-- | Whether this detection track was created by the client.
--
-- ObjC selector: @- userCreated@
userCreated :: IsCNDetectionTrack cnDetectionTrack => cnDetectionTrack -> IO Bool
userCreated cnDetectionTrack =
  sendMessage cnDetectionTrack userCreatedSelector

-- | Whether this detection track has discrete detections (otherwise continuous).
--
-- A discrete detection track will return detections only at the specific times a detection occurs. A continuous detection track will return a detection for any requested time and an empty array for time ranges.
--
-- ObjC selector: @- discrete@
discrete :: IsCNDetectionTrack cnDetectionTrack => cnDetectionTrack -> IO Bool
discrete cnDetectionTrack =
  sendMessage cnDetectionTrack discreteSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CNDetectionTrack)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CNDetectionTrack)
newSelector = mkSelector "new"

-- | @Selector@ for @detectionType@
detectionTypeSelector :: Selector '[] CNDetectionType
detectionTypeSelector = mkSelector "detectionType"

-- | @Selector@ for @detectionID@
detectionIDSelector :: Selector '[] CLong
detectionIDSelector = mkSelector "detectionID"

-- | @Selector@ for @detectionGroupID@
detectionGroupIDSelector :: Selector '[] CLong
detectionGroupIDSelector = mkSelector "detectionGroupID"

-- | @Selector@ for @userCreated@
userCreatedSelector :: Selector '[] Bool
userCreatedSelector = mkSelector "userCreated"

-- | @Selector@ for @discrete@
discreteSelector :: Selector '[] Bool
discreteSelector = mkSelector "discrete"

