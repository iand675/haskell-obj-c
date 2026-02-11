{-# LANGUAGE PatternSynonyms #-}
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
  , isActivityAvailableSelector
  , authorizationStatusSelector
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

import ObjC.CoreMotion.Internal.Classes
import ObjC.CoreMotion.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ isActivityAvailable@
isActivityAvailable :: IO Bool
isActivityAvailable  =
  do
    cls' <- getRequiredClass "CMMotionActivityManager"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isActivityAvailable") retCULong []

-- | @+ authorizationStatus@
authorizationStatus :: IO CMAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "CMMotionActivityManager"
    fmap (coerce :: CLong -> CMAuthorizationStatus) $ sendClassMsg cls' (mkSelector "authorizationStatus") retCLong []

-- | @- queryActivityStartingFromDate:toDate:toQueue:withHandler:@
queryActivityStartingFromDate_toDate_toQueue_withHandler :: (IsCMMotionActivityManager cmMotionActivityManager, IsNSDate start, IsNSDate end, IsNSOperationQueue queue) => cmMotionActivityManager -> start -> end -> queue -> Ptr () -> IO ()
queryActivityStartingFromDate_toDate_toQueue_withHandler cmMotionActivityManager  start end queue handler =
withObjCPtr start $ \raw_start ->
  withObjCPtr end $ \raw_end ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg cmMotionActivityManager (mkSelector "queryActivityStartingFromDate:toDate:toQueue:withHandler:") retVoid [argPtr (castPtr raw_start :: Ptr ()), argPtr (castPtr raw_end :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- startActivityUpdatesToQueue:withHandler:@
startActivityUpdatesToQueue_withHandler :: (IsCMMotionActivityManager cmMotionActivityManager, IsNSOperationQueue queue) => cmMotionActivityManager -> queue -> Ptr () -> IO ()
startActivityUpdatesToQueue_withHandler cmMotionActivityManager  queue handler =
withObjCPtr queue $ \raw_queue ->
    sendMsg cmMotionActivityManager (mkSelector "startActivityUpdatesToQueue:withHandler:") retVoid [argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- stopActivityUpdates@
stopActivityUpdates :: IsCMMotionActivityManager cmMotionActivityManager => cmMotionActivityManager -> IO ()
stopActivityUpdates cmMotionActivityManager  =
  sendMsg cmMotionActivityManager (mkSelector "stopActivityUpdates") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isActivityAvailable@
isActivityAvailableSelector :: Selector
isActivityAvailableSelector = mkSelector "isActivityAvailable"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @queryActivityStartingFromDate:toDate:toQueue:withHandler:@
queryActivityStartingFromDate_toDate_toQueue_withHandlerSelector :: Selector
queryActivityStartingFromDate_toDate_toQueue_withHandlerSelector = mkSelector "queryActivityStartingFromDate:toDate:toQueue:withHandler:"

-- | @Selector@ for @startActivityUpdatesToQueue:withHandler:@
startActivityUpdatesToQueue_withHandlerSelector :: Selector
startActivityUpdatesToQueue_withHandlerSelector = mkSelector "startActivityUpdatesToQueue:withHandler:"

-- | @Selector@ for @stopActivityUpdates@
stopActivityUpdatesSelector :: Selector
stopActivityUpdatesSelector = mkSelector "stopActivityUpdates"

