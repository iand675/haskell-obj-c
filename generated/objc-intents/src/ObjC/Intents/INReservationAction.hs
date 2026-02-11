{-# LANGUAGE PatternSynonyms #-}
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
  , validDurationSelector
  , userActivitySelector

  -- * Enum types
  , INReservationActionType(INReservationActionType)
  , pattern INReservationActionTypeUnknown
  , pattern INReservationActionTypeCheckIn

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
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINReservationAction inReservationAction => inReservationAction -> IO (Id INReservationAction)
init_ inReservationAction  =
  sendMsg inReservationAction (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithType:validDuration:userActivity:@
initWithType_validDuration_userActivity :: (IsINReservationAction inReservationAction, IsINDateComponentsRange validDuration, IsNSUserActivity userActivity) => inReservationAction -> INReservationActionType -> validDuration -> userActivity -> IO (Id INReservationAction)
initWithType_validDuration_userActivity inReservationAction  type_ validDuration userActivity =
withObjCPtr validDuration $ \raw_validDuration ->
  withObjCPtr userActivity $ \raw_userActivity ->
      sendMsg inReservationAction (mkSelector "initWithType:validDuration:userActivity:") (retPtr retVoid) [argCLong (coerce type_), argPtr (castPtr raw_validDuration :: Ptr ()), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- type@
type_ :: IsINReservationAction inReservationAction => inReservationAction -> IO INReservationActionType
type_ inReservationAction  =
  fmap (coerce :: CLong -> INReservationActionType) $ sendMsg inReservationAction (mkSelector "type") retCLong []

-- | @- validDuration@
validDuration :: IsINReservationAction inReservationAction => inReservationAction -> IO (Id INDateComponentsRange)
validDuration inReservationAction  =
  sendMsg inReservationAction (mkSelector "validDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- userActivity@
userActivity :: IsINReservationAction inReservationAction => inReservationAction -> IO (Id NSUserActivity)
userActivity inReservationAction  =
  sendMsg inReservationAction (mkSelector "userActivity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithType:validDuration:userActivity:@
initWithType_validDuration_userActivitySelector :: Selector
initWithType_validDuration_userActivitySelector = mkSelector "initWithType:validDuration:userActivity:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @validDuration@
validDurationSelector :: Selector
validDurationSelector = mkSelector "validDuration"

-- | @Selector@ for @userActivity@
userActivitySelector :: Selector
userActivitySelector = mkSelector "userActivity"

