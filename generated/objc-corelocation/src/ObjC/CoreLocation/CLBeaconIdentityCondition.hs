{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLBeaconIdentityCondition@.
module ObjC.CoreLocation.CLBeaconIdentityCondition
  ( CLBeaconIdentityCondition
  , IsCLBeaconIdentityCondition(..)
  , initWithUUID
  , initWithUUID_major
  , initWithUUID_major_minor
  , uuid
  , initWithUUIDSelector
  , initWithUUID_majorSelector
  , initWithUUID_major_minorSelector
  , uuidSelector


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

import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithUUID:@
initWithUUID :: (IsCLBeaconIdentityCondition clBeaconIdentityCondition, IsNSUUID uuid) => clBeaconIdentityCondition -> uuid -> IO (Id CLBeaconIdentityCondition)
initWithUUID clBeaconIdentityCondition  uuid =
withObjCPtr uuid $ \raw_uuid ->
    sendMsg clBeaconIdentityCondition (mkSelector "initWithUUID:") (retPtr retVoid) [argPtr (castPtr raw_uuid :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithUUID:major:@
initWithUUID_major :: (IsCLBeaconIdentityCondition clBeaconIdentityCondition, IsNSUUID uuid) => clBeaconIdentityCondition -> uuid -> CUShort -> IO (Id CLBeaconIdentityCondition)
initWithUUID_major clBeaconIdentityCondition  uuid major =
withObjCPtr uuid $ \raw_uuid ->
    sendMsg clBeaconIdentityCondition (mkSelector "initWithUUID:major:") (retPtr retVoid) [argPtr (castPtr raw_uuid :: Ptr ()), argCUInt (fromIntegral major)] >>= ownedObject . castPtr

-- | @- initWithUUID:major:minor:@
initWithUUID_major_minor :: (IsCLBeaconIdentityCondition clBeaconIdentityCondition, IsNSUUID uuid) => clBeaconIdentityCondition -> uuid -> CUShort -> CUShort -> IO (Id CLBeaconIdentityCondition)
initWithUUID_major_minor clBeaconIdentityCondition  uuid major minor =
withObjCPtr uuid $ \raw_uuid ->
    sendMsg clBeaconIdentityCondition (mkSelector "initWithUUID:major:minor:") (retPtr retVoid) [argPtr (castPtr raw_uuid :: Ptr ()), argCUInt (fromIntegral major), argCUInt (fromIntegral minor)] >>= ownedObject . castPtr

-- | @- UUID@
uuid :: IsCLBeaconIdentityCondition clBeaconIdentityCondition => clBeaconIdentityCondition -> IO (Id NSUUID)
uuid clBeaconIdentityCondition  =
  sendMsg clBeaconIdentityCondition (mkSelector "UUID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUUID:@
initWithUUIDSelector :: Selector
initWithUUIDSelector = mkSelector "initWithUUID:"

-- | @Selector@ for @initWithUUID:major:@
initWithUUID_majorSelector :: Selector
initWithUUID_majorSelector = mkSelector "initWithUUID:major:"

-- | @Selector@ for @initWithUUID:major:minor:@
initWithUUID_major_minorSelector :: Selector
initWithUUID_major_minorSelector = mkSelector "initWithUUID:major:minor:"

-- | @Selector@ for @UUID@
uuidSelector :: Selector
uuidSelector = mkSelector "UUID"

