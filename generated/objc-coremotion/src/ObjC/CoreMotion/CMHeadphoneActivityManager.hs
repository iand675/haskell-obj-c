{-# LANGUAGE PatternSynonyms #-}
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
  , authorizationStatusSelector
  , startActivityUpdatesToQueue_withHandlerSelector
  , stopActivityUpdatesSelector
  , startStatusUpdatesToQueue_withHandlerSelector
  , stopStatusUpdatesSelector
  , activityAvailableSelector
  , activityActiveSelector
  , statusAvailableSelector
  , statusActiveSelector

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

-- | @+ authorizationStatus@
authorizationStatus :: IO CMAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "CMHeadphoneActivityManager"
    fmap (coerce :: CLong -> CMAuthorizationStatus) $ sendClassMsg cls' (mkSelector "authorizationStatus") retCLong []

-- | @- startActivityUpdatesToQueue:withHandler:@
startActivityUpdatesToQueue_withHandler :: (IsCMHeadphoneActivityManager cmHeadphoneActivityManager, IsNSOperationQueue queue) => cmHeadphoneActivityManager -> queue -> Ptr () -> IO ()
startActivityUpdatesToQueue_withHandler cmHeadphoneActivityManager  queue handler =
withObjCPtr queue $ \raw_queue ->
    sendMsg cmHeadphoneActivityManager (mkSelector "startActivityUpdatesToQueue:withHandler:") retVoid [argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- stopActivityUpdates@
stopActivityUpdates :: IsCMHeadphoneActivityManager cmHeadphoneActivityManager => cmHeadphoneActivityManager -> IO ()
stopActivityUpdates cmHeadphoneActivityManager  =
  sendMsg cmHeadphoneActivityManager (mkSelector "stopActivityUpdates") retVoid []

-- | @- startStatusUpdatesToQueue:withHandler:@
startStatusUpdatesToQueue_withHandler :: (IsCMHeadphoneActivityManager cmHeadphoneActivityManager, IsNSOperationQueue queue) => cmHeadphoneActivityManager -> queue -> Ptr () -> IO ()
startStatusUpdatesToQueue_withHandler cmHeadphoneActivityManager  queue handler =
withObjCPtr queue $ \raw_queue ->
    sendMsg cmHeadphoneActivityManager (mkSelector "startStatusUpdatesToQueue:withHandler:") retVoid [argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- stopStatusUpdates@
stopStatusUpdates :: IsCMHeadphoneActivityManager cmHeadphoneActivityManager => cmHeadphoneActivityManager -> IO ()
stopStatusUpdates cmHeadphoneActivityManager  =
  sendMsg cmHeadphoneActivityManager (mkSelector "stopStatusUpdates") retVoid []

-- | @- activityAvailable@
activityAvailable :: IsCMHeadphoneActivityManager cmHeadphoneActivityManager => cmHeadphoneActivityManager -> IO Bool
activityAvailable cmHeadphoneActivityManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmHeadphoneActivityManager (mkSelector "activityAvailable") retCULong []

-- | @- activityActive@
activityActive :: IsCMHeadphoneActivityManager cmHeadphoneActivityManager => cmHeadphoneActivityManager -> IO Bool
activityActive cmHeadphoneActivityManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmHeadphoneActivityManager (mkSelector "activityActive") retCULong []

-- | @- statusAvailable@
statusAvailable :: IsCMHeadphoneActivityManager cmHeadphoneActivityManager => cmHeadphoneActivityManager -> IO Bool
statusAvailable cmHeadphoneActivityManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmHeadphoneActivityManager (mkSelector "statusAvailable") retCULong []

-- | @- statusActive@
statusActive :: IsCMHeadphoneActivityManager cmHeadphoneActivityManager => cmHeadphoneActivityManager -> IO Bool
statusActive cmHeadphoneActivityManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmHeadphoneActivityManager (mkSelector "statusActive") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @startActivityUpdatesToQueue:withHandler:@
startActivityUpdatesToQueue_withHandlerSelector :: Selector
startActivityUpdatesToQueue_withHandlerSelector = mkSelector "startActivityUpdatesToQueue:withHandler:"

-- | @Selector@ for @stopActivityUpdates@
stopActivityUpdatesSelector :: Selector
stopActivityUpdatesSelector = mkSelector "stopActivityUpdates"

-- | @Selector@ for @startStatusUpdatesToQueue:withHandler:@
startStatusUpdatesToQueue_withHandlerSelector :: Selector
startStatusUpdatesToQueue_withHandlerSelector = mkSelector "startStatusUpdatesToQueue:withHandler:"

-- | @Selector@ for @stopStatusUpdates@
stopStatusUpdatesSelector :: Selector
stopStatusUpdatesSelector = mkSelector "stopStatusUpdates"

-- | @Selector@ for @activityAvailable@
activityAvailableSelector :: Selector
activityAvailableSelector = mkSelector "activityAvailable"

-- | @Selector@ for @activityActive@
activityActiveSelector :: Selector
activityActiveSelector = mkSelector "activityActive"

-- | @Selector@ for @statusAvailable@
statusAvailableSelector :: Selector
statusAvailableSelector = mkSelector "statusAvailable"

-- | @Selector@ for @statusActive@
statusActiveSelector :: Selector
statusActiveSelector = mkSelector "statusActive"

