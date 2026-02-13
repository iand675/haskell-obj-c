{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMMotionActivityManager@.
module ObjC.CoreMotion.CMMotionActivityManager
  ( CMMotionActivityManager
  , IsCMMotionActivityManager(..)
  , isActivityAvailable
  , authorizationStatus
  , queryActivityStartingFromDate_toDate_toQueue_withHandler
  , startActivityUpdatesToQueue_withHandler
  , stopActivityUpdates
  , authorizationStatusSelector
  , isActivityAvailableSelector
  , queryActivityStartingFromDate_toDate_toQueue_withHandlerSelector
  , startActivityUpdatesToQueue_withHandlerSelector
  , stopActivityUpdatesSelector

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

-- | @+ isActivityAvailable@
isActivityAvailable :: IO Bool
isActivityAvailable  =
  do
    cls' <- getRequiredClass "CMMotionActivityManager"
    sendClassMessage cls' isActivityAvailableSelector

-- | @+ authorizationStatus@
authorizationStatus :: IO CMAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "CMMotionActivityManager"
    sendClassMessage cls' authorizationStatusSelector

-- | @- queryActivityStartingFromDate:toDate:toQueue:withHandler:@
queryActivityStartingFromDate_toDate_toQueue_withHandler :: (IsCMMotionActivityManager cmMotionActivityManager, IsNSDate start, IsNSDate end, IsNSOperationQueue queue) => cmMotionActivityManager -> start -> end -> queue -> Ptr () -> IO ()
queryActivityStartingFromDate_toDate_toQueue_withHandler cmMotionActivityManager start end queue handler =
  sendMessage cmMotionActivityManager queryActivityStartingFromDate_toDate_toQueue_withHandlerSelector (toNSDate start) (toNSDate end) (toNSOperationQueue queue) handler

-- | @- startActivityUpdatesToQueue:withHandler:@
startActivityUpdatesToQueue_withHandler :: (IsCMMotionActivityManager cmMotionActivityManager, IsNSOperationQueue queue) => cmMotionActivityManager -> queue -> Ptr () -> IO ()
startActivityUpdatesToQueue_withHandler cmMotionActivityManager queue handler =
  sendMessage cmMotionActivityManager startActivityUpdatesToQueue_withHandlerSelector (toNSOperationQueue queue) handler

-- | @- stopActivityUpdates@
stopActivityUpdates :: IsCMMotionActivityManager cmMotionActivityManager => cmMotionActivityManager -> IO ()
stopActivityUpdates cmMotionActivityManager =
  sendMessage cmMotionActivityManager stopActivityUpdatesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isActivityAvailable@
isActivityAvailableSelector :: Selector '[] Bool
isActivityAvailableSelector = mkSelector "isActivityAvailable"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector '[] CMAuthorizationStatus
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @queryActivityStartingFromDate:toDate:toQueue:withHandler:@
queryActivityStartingFromDate_toDate_toQueue_withHandlerSelector :: Selector '[Id NSDate, Id NSDate, Id NSOperationQueue, Ptr ()] ()
queryActivityStartingFromDate_toDate_toQueue_withHandlerSelector = mkSelector "queryActivityStartingFromDate:toDate:toQueue:withHandler:"

-- | @Selector@ for @startActivityUpdatesToQueue:withHandler:@
startActivityUpdatesToQueue_withHandlerSelector :: Selector '[Id NSOperationQueue, Ptr ()] ()
startActivityUpdatesToQueue_withHandlerSelector = mkSelector "startActivityUpdatesToQueue:withHandler:"

-- | @Selector@ for @stopActivityUpdates@
stopActivityUpdatesSelector :: Selector '[] ()
stopActivityUpdatesSelector = mkSelector "stopActivityUpdates"

