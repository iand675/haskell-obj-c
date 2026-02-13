{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INReservationAction@.
module ObjC.Intents.INReservationAction
  ( INReservationAction
  , IsINReservationAction(..)
  , init_
  , initWithType_validDuration_userActivity
  , type_
  , validDuration
  , userActivity
  , initSelector
  , initWithType_validDuration_userActivitySelector
  , typeSelector
  , userActivitySelector
  , validDurationSelector

  -- * Enum types
  , INReservationActionType(INReservationActionType)
  , pattern INReservationActionTypeUnknown
  , pattern INReservationActionTypeCheckIn

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINReservationAction inReservationAction => inReservationAction -> IO (Id INReservationAction)
init_ inReservationAction =
  sendOwnedMessage inReservationAction initSelector

-- | @- initWithType:validDuration:userActivity:@
initWithType_validDuration_userActivity :: (IsINReservationAction inReservationAction, IsINDateComponentsRange validDuration, IsNSUserActivity userActivity) => inReservationAction -> INReservationActionType -> validDuration -> userActivity -> IO (Id INReservationAction)
initWithType_validDuration_userActivity inReservationAction type_ validDuration userActivity =
  sendOwnedMessage inReservationAction initWithType_validDuration_userActivitySelector type_ (toINDateComponentsRange validDuration) (toNSUserActivity userActivity)

-- | @- type@
type_ :: IsINReservationAction inReservationAction => inReservationAction -> IO INReservationActionType
type_ inReservationAction =
  sendMessage inReservationAction typeSelector

-- | @- validDuration@
validDuration :: IsINReservationAction inReservationAction => inReservationAction -> IO (Id INDateComponentsRange)
validDuration inReservationAction =
  sendMessage inReservationAction validDurationSelector

-- | @- userActivity@
userActivity :: IsINReservationAction inReservationAction => inReservationAction -> IO (Id NSUserActivity)
userActivity inReservationAction =
  sendMessage inReservationAction userActivitySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INReservationAction)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithType:validDuration:userActivity:@
initWithType_validDuration_userActivitySelector :: Selector '[INReservationActionType, Id INDateComponentsRange, Id NSUserActivity] (Id INReservationAction)
initWithType_validDuration_userActivitySelector = mkSelector "initWithType:validDuration:userActivity:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] INReservationActionType
typeSelector = mkSelector "type"

-- | @Selector@ for @validDuration@
validDurationSelector :: Selector '[] (Id INDateComponentsRange)
validDurationSelector = mkSelector "validDuration"

-- | @Selector@ for @userActivity@
userActivitySelector :: Selector '[] (Id NSUserActivity)
userActivitySelector = mkSelector "userActivity"

