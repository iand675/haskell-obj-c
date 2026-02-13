{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMAltimeter@.
module ObjC.CoreMotion.CMAltimeter
  ( CMAltimeter
  , IsCMAltimeter(..)
  , isRelativeAltitudeAvailable
  , authorizationStatus
  , startRelativeAltitudeUpdatesToQueue_withHandler
  , stopRelativeAltitudeUpdates
  , isAbsoluteAltitudeAvailable
  , startAbsoluteAltitudeUpdatesToQueue_withHandler
  , stopAbsoluteAltitudeUpdates
  , authorizationStatusSelector
  , isAbsoluteAltitudeAvailableSelector
  , isRelativeAltitudeAvailableSelector
  , startAbsoluteAltitudeUpdatesToQueue_withHandlerSelector
  , startRelativeAltitudeUpdatesToQueue_withHandlerSelector
  , stopAbsoluteAltitudeUpdatesSelector
  , stopRelativeAltitudeUpdatesSelector

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

-- | @+ isRelativeAltitudeAvailable@
isRelativeAltitudeAvailable :: IO Bool
isRelativeAltitudeAvailable  =
  do
    cls' <- getRequiredClass "CMAltimeter"
    sendClassMessage cls' isRelativeAltitudeAvailableSelector

-- | @+ authorizationStatus@
authorizationStatus :: IO CMAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "CMAltimeter"
    sendClassMessage cls' authorizationStatusSelector

-- | @- startRelativeAltitudeUpdatesToQueue:withHandler:@
startRelativeAltitudeUpdatesToQueue_withHandler :: (IsCMAltimeter cmAltimeter, IsNSOperationQueue queue) => cmAltimeter -> queue -> Ptr () -> IO ()
startRelativeAltitudeUpdatesToQueue_withHandler cmAltimeter queue handler =
  sendMessage cmAltimeter startRelativeAltitudeUpdatesToQueue_withHandlerSelector (toNSOperationQueue queue) handler

-- | @- stopRelativeAltitudeUpdates@
stopRelativeAltitudeUpdates :: IsCMAltimeter cmAltimeter => cmAltimeter -> IO ()
stopRelativeAltitudeUpdates cmAltimeter =
  sendMessage cmAltimeter stopRelativeAltitudeUpdatesSelector

-- | @+ isAbsoluteAltitudeAvailable@
isAbsoluteAltitudeAvailable :: IO Bool
isAbsoluteAltitudeAvailable  =
  do
    cls' <- getRequiredClass "CMAltimeter"
    sendClassMessage cls' isAbsoluteAltitudeAvailableSelector

-- | @- startAbsoluteAltitudeUpdatesToQueue:withHandler:@
startAbsoluteAltitudeUpdatesToQueue_withHandler :: (IsCMAltimeter cmAltimeter, IsNSOperationQueue queue) => cmAltimeter -> queue -> Ptr () -> IO ()
startAbsoluteAltitudeUpdatesToQueue_withHandler cmAltimeter queue handler =
  sendMessage cmAltimeter startAbsoluteAltitudeUpdatesToQueue_withHandlerSelector (toNSOperationQueue queue) handler

-- | @- stopAbsoluteAltitudeUpdates@
stopAbsoluteAltitudeUpdates :: IsCMAltimeter cmAltimeter => cmAltimeter -> IO ()
stopAbsoluteAltitudeUpdates cmAltimeter =
  sendMessage cmAltimeter stopAbsoluteAltitudeUpdatesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isRelativeAltitudeAvailable@
isRelativeAltitudeAvailableSelector :: Selector '[] Bool
isRelativeAltitudeAvailableSelector = mkSelector "isRelativeAltitudeAvailable"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector '[] CMAuthorizationStatus
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @startRelativeAltitudeUpdatesToQueue:withHandler:@
startRelativeAltitudeUpdatesToQueue_withHandlerSelector :: Selector '[Id NSOperationQueue, Ptr ()] ()
startRelativeAltitudeUpdatesToQueue_withHandlerSelector = mkSelector "startRelativeAltitudeUpdatesToQueue:withHandler:"

-- | @Selector@ for @stopRelativeAltitudeUpdates@
stopRelativeAltitudeUpdatesSelector :: Selector '[] ()
stopRelativeAltitudeUpdatesSelector = mkSelector "stopRelativeAltitudeUpdates"

-- | @Selector@ for @isAbsoluteAltitudeAvailable@
isAbsoluteAltitudeAvailableSelector :: Selector '[] Bool
isAbsoluteAltitudeAvailableSelector = mkSelector "isAbsoluteAltitudeAvailable"

-- | @Selector@ for @startAbsoluteAltitudeUpdatesToQueue:withHandler:@
startAbsoluteAltitudeUpdatesToQueue_withHandlerSelector :: Selector '[Id NSOperationQueue, Ptr ()] ()
startAbsoluteAltitudeUpdatesToQueue_withHandlerSelector = mkSelector "startAbsoluteAltitudeUpdatesToQueue:withHandler:"

-- | @Selector@ for @stopAbsoluteAltitudeUpdates@
stopAbsoluteAltitudeUpdatesSelector :: Selector '[] ()
stopAbsoluteAltitudeUpdatesSelector = mkSelector "stopAbsoluteAltitudeUpdates"

