{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMHeadphoneActivityManager@.
module ObjC.CoreMotion.CMHeadphoneActivityManager
  ( CMHeadphoneActivityManager
  , IsCMHeadphoneActivityManager(..)
  , authorizationStatus
  , startActivityUpdatesToQueue_withHandler
  , stopActivityUpdates
  , startStatusUpdatesToQueue_withHandler
  , stopStatusUpdates
  , activityAvailable
  , activityActive
  , statusAvailable
  , statusActive
  , activityActiveSelector
  , activityAvailableSelector
  , authorizationStatusSelector
  , startActivityUpdatesToQueue_withHandlerSelector
  , startStatusUpdatesToQueue_withHandlerSelector
  , statusActiveSelector
  , statusAvailableSelector
  , stopActivityUpdatesSelector
  , stopStatusUpdatesSelector

  -- * Enum types
  , CMAuthorizationStatus(CMAuthorizationStatus)
  , pattern CMAuthorizationStatusNotDetermined
  , pattern CMAuthorizationStatusRestricted
  , pattern CMAuthorizationStatusDenied
  , pattern CMAuthorizationStatusAuthorized

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.CoreMotion.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ authorizationStatus@
authorizationStatus :: IO CMAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "CMHeadphoneActivityManager"
    sendClassMessage cls' authorizationStatusSelector

-- | @- startActivityUpdatesToQueue:withHandler:@
startActivityUpdatesToQueue_withHandler :: (IsCMHeadphoneActivityManager cmHeadphoneActivityManager, IsNSOperationQueue queue) => cmHeadphoneActivityManager -> queue -> Ptr () -> IO ()
startActivityUpdatesToQueue_withHandler cmHeadphoneActivityManager queue handler =
  sendMessage cmHeadphoneActivityManager startActivityUpdatesToQueue_withHandlerSelector (toNSOperationQueue queue) handler

-- | @- stopActivityUpdates@
stopActivityUpdates :: IsCMHeadphoneActivityManager cmHeadphoneActivityManager => cmHeadphoneActivityManager -> IO ()
stopActivityUpdates cmHeadphoneActivityManager =
  sendMessage cmHeadphoneActivityManager stopActivityUpdatesSelector

-- | @- startStatusUpdatesToQueue:withHandler:@
startStatusUpdatesToQueue_withHandler :: (IsCMHeadphoneActivityManager cmHeadphoneActivityManager, IsNSOperationQueue queue) => cmHeadphoneActivityManager -> queue -> Ptr () -> IO ()
startStatusUpdatesToQueue_withHandler cmHeadphoneActivityManager queue handler =
  sendMessage cmHeadphoneActivityManager startStatusUpdatesToQueue_withHandlerSelector (toNSOperationQueue queue) handler

-- | @- stopStatusUpdates@
stopStatusUpdates :: IsCMHeadphoneActivityManager cmHeadphoneActivityManager => cmHeadphoneActivityManager -> IO ()
stopStatusUpdates cmHeadphoneActivityManager =
  sendMessage cmHeadphoneActivityManager stopStatusUpdatesSelector

-- | @- activityAvailable@
activityAvailable :: IsCMHeadphoneActivityManager cmHeadphoneActivityManager => cmHeadphoneActivityManager -> IO Bool
activityAvailable cmHeadphoneActivityManager =
  sendMessage cmHeadphoneActivityManager activityAvailableSelector

-- | @- activityActive@
activityActive :: IsCMHeadphoneActivityManager cmHeadphoneActivityManager => cmHeadphoneActivityManager -> IO Bool
activityActive cmHeadphoneActivityManager =
  sendMessage cmHeadphoneActivityManager activityActiveSelector

-- | @- statusAvailable@
statusAvailable :: IsCMHeadphoneActivityManager cmHeadphoneActivityManager => cmHeadphoneActivityManager -> IO Bool
statusAvailable cmHeadphoneActivityManager =
  sendMessage cmHeadphoneActivityManager statusAvailableSelector

-- | @- statusActive@
statusActive :: IsCMHeadphoneActivityManager cmHeadphoneActivityManager => cmHeadphoneActivityManager -> IO Bool
statusActive cmHeadphoneActivityManager =
  sendMessage cmHeadphoneActivityManager statusActiveSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector '[] CMAuthorizationStatus
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @startActivityUpdatesToQueue:withHandler:@
startActivityUpdatesToQueue_withHandlerSelector :: Selector '[Id NSOperationQueue, Ptr ()] ()
startActivityUpdatesToQueue_withHandlerSelector = mkSelector "startActivityUpdatesToQueue:withHandler:"

-- | @Selector@ for @stopActivityUpdates@
stopActivityUpdatesSelector :: Selector '[] ()
stopActivityUpdatesSelector = mkSelector "stopActivityUpdates"

-- | @Selector@ for @startStatusUpdatesToQueue:withHandler:@
startStatusUpdatesToQueue_withHandlerSelector :: Selector '[Id NSOperationQueue, Ptr ()] ()
startStatusUpdatesToQueue_withHandlerSelector = mkSelector "startStatusUpdatesToQueue:withHandler:"

-- | @Selector@ for @stopStatusUpdates@
stopStatusUpdatesSelector :: Selector '[] ()
stopStatusUpdatesSelector = mkSelector "stopStatusUpdates"

-- | @Selector@ for @activityAvailable@
activityAvailableSelector :: Selector '[] Bool
activityAvailableSelector = mkSelector "activityAvailable"

-- | @Selector@ for @activityActive@
activityActiveSelector :: Selector '[] Bool
activityActiveSelector = mkSelector "activityActive"

-- | @Selector@ for @statusAvailable@
statusAvailableSelector :: Selector '[] Bool
statusAvailableSelector = mkSelector "statusAvailable"

-- | @Selector@ for @statusActive@
statusActiveSelector :: Selector '[] Bool
statusActiveSelector = mkSelector "statusActive"

