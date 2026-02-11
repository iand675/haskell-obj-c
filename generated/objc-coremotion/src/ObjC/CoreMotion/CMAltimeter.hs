{-# LANGUAGE PatternSynonyms #-}
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
  , isRelativeAltitudeAvailableSelector
  , authorizationStatusSelector
  , startRelativeAltitudeUpdatesToQueue_withHandlerSelector
  , stopRelativeAltitudeUpdatesSelector
  , isAbsoluteAltitudeAvailableSelector
  , startAbsoluteAltitudeUpdatesToQueue_withHandlerSelector
  , stopAbsoluteAltitudeUpdatesSelector

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

-- | @+ isRelativeAltitudeAvailable@
isRelativeAltitudeAvailable :: IO Bool
isRelativeAltitudeAvailable  =
  do
    cls' <- getRequiredClass "CMAltimeter"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isRelativeAltitudeAvailable") retCULong []

-- | @+ authorizationStatus@
authorizationStatus :: IO CMAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "CMAltimeter"
    fmap (coerce :: CLong -> CMAuthorizationStatus) $ sendClassMsg cls' (mkSelector "authorizationStatus") retCLong []

-- | @- startRelativeAltitudeUpdatesToQueue:withHandler:@
startRelativeAltitudeUpdatesToQueue_withHandler :: (IsCMAltimeter cmAltimeter, IsNSOperationQueue queue) => cmAltimeter -> queue -> Ptr () -> IO ()
startRelativeAltitudeUpdatesToQueue_withHandler cmAltimeter  queue handler =
withObjCPtr queue $ \raw_queue ->
    sendMsg cmAltimeter (mkSelector "startRelativeAltitudeUpdatesToQueue:withHandler:") retVoid [argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- stopRelativeAltitudeUpdates@
stopRelativeAltitudeUpdates :: IsCMAltimeter cmAltimeter => cmAltimeter -> IO ()
stopRelativeAltitudeUpdates cmAltimeter  =
  sendMsg cmAltimeter (mkSelector "stopRelativeAltitudeUpdates") retVoid []

-- | @+ isAbsoluteAltitudeAvailable@
isAbsoluteAltitudeAvailable :: IO Bool
isAbsoluteAltitudeAvailable  =
  do
    cls' <- getRequiredClass "CMAltimeter"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isAbsoluteAltitudeAvailable") retCULong []

-- | @- startAbsoluteAltitudeUpdatesToQueue:withHandler:@
startAbsoluteAltitudeUpdatesToQueue_withHandler :: (IsCMAltimeter cmAltimeter, IsNSOperationQueue queue) => cmAltimeter -> queue -> Ptr () -> IO ()
startAbsoluteAltitudeUpdatesToQueue_withHandler cmAltimeter  queue handler =
withObjCPtr queue $ \raw_queue ->
    sendMsg cmAltimeter (mkSelector "startAbsoluteAltitudeUpdatesToQueue:withHandler:") retVoid [argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- stopAbsoluteAltitudeUpdates@
stopAbsoluteAltitudeUpdates :: IsCMAltimeter cmAltimeter => cmAltimeter -> IO ()
stopAbsoluteAltitudeUpdates cmAltimeter  =
  sendMsg cmAltimeter (mkSelector "stopAbsoluteAltitudeUpdates") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isRelativeAltitudeAvailable@
isRelativeAltitudeAvailableSelector :: Selector
isRelativeAltitudeAvailableSelector = mkSelector "isRelativeAltitudeAvailable"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @startRelativeAltitudeUpdatesToQueue:withHandler:@
startRelativeAltitudeUpdatesToQueue_withHandlerSelector :: Selector
startRelativeAltitudeUpdatesToQueue_withHandlerSelector = mkSelector "startRelativeAltitudeUpdatesToQueue:withHandler:"

-- | @Selector@ for @stopRelativeAltitudeUpdates@
stopRelativeAltitudeUpdatesSelector :: Selector
stopRelativeAltitudeUpdatesSelector = mkSelector "stopRelativeAltitudeUpdates"

-- | @Selector@ for @isAbsoluteAltitudeAvailable@
isAbsoluteAltitudeAvailableSelector :: Selector
isAbsoluteAltitudeAvailableSelector = mkSelector "isAbsoluteAltitudeAvailable"

-- | @Selector@ for @startAbsoluteAltitudeUpdatesToQueue:withHandler:@
startAbsoluteAltitudeUpdatesToQueue_withHandlerSelector :: Selector
startAbsoluteAltitudeUpdatesToQueue_withHandlerSelector = mkSelector "startAbsoluteAltitudeUpdatesToQueue:withHandler:"

-- | @Selector@ for @stopAbsoluteAltitudeUpdates@
stopAbsoluteAltitudeUpdatesSelector :: Selector
stopAbsoluteAltitudeUpdatesSelector = mkSelector "stopAbsoluteAltitudeUpdates"

