{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUUID@.
module ObjC.Foundation.NSUUID
  ( NSUUID
  , IsNSUUID(..)
  , uuid
  , init_
  , initWithUUIDString
  , initWithUUIDBytes
  , getUUIDBytes
  , compare_
  , uuidString
  , uuidSelector
  , initSelector
  , initWithUUIDStringSelector
  , initWithUUIDBytesSelector
  , getUUIDBytesSelector
  , compareSelector
  , uuidStringSelector

  -- * Enum types
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ UUID@
uuid :: IO (Id NSUUID)
uuid  =
  do
    cls' <- getRequiredClass "NSUUID"
    sendClassMsg cls' (mkSelector "UUID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSUUID nsuuid => nsuuid -> IO (Id NSUUID)
init_ nsuuid  =
  sendMsg nsuuid (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithUUIDString:@
initWithUUIDString :: (IsNSUUID nsuuid, IsNSString string) => nsuuid -> string -> IO (Id NSUUID)
initWithUUIDString nsuuid  string =
withObjCPtr string $ \raw_string ->
    sendMsg nsuuid (mkSelector "initWithUUIDString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithUUIDBytes:@
initWithUUIDBytes :: IsNSUUID nsuuid => nsuuid -> Const (Ptr CUChar) -> IO (Id NSUUID)
initWithUUIDBytes nsuuid  bytes =
  sendMsg nsuuid (mkSelector "initWithUUIDBytes:") (retPtr retVoid) [argPtr (unConst bytes)] >>= ownedObject . castPtr

-- | @- getUUIDBytes:@
getUUIDBytes :: IsNSUUID nsuuid => nsuuid -> Ptr CUChar -> IO ()
getUUIDBytes nsuuid  uuid =
  sendMsg nsuuid (mkSelector "getUUIDBytes:") retVoid [argPtr uuid]

-- | @- compare:@
compare_ :: (IsNSUUID nsuuid, IsNSUUID otherUUID) => nsuuid -> otherUUID -> IO NSComparisonResult
compare_ nsuuid  otherUUID =
withObjCPtr otherUUID $ \raw_otherUUID ->
    fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg nsuuid (mkSelector "compare:") retCLong [argPtr (castPtr raw_otherUUID :: Ptr ())]

-- | @- UUIDString@
uuidString :: IsNSUUID nsuuid => nsuuid -> IO (Id NSString)
uuidString nsuuid  =
  sendMsg nsuuid (mkSelector "UUIDString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @UUID@
uuidSelector :: Selector
uuidSelector = mkSelector "UUID"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithUUIDString:@
initWithUUIDStringSelector :: Selector
initWithUUIDStringSelector = mkSelector "initWithUUIDString:"

-- | @Selector@ for @initWithUUIDBytes:@
initWithUUIDBytesSelector :: Selector
initWithUUIDBytesSelector = mkSelector "initWithUUIDBytes:"

-- | @Selector@ for @getUUIDBytes:@
getUUIDBytesSelector :: Selector
getUUIDBytesSelector = mkSelector "getUUIDBytes:"

-- | @Selector@ for @compare:@
compareSelector :: Selector
compareSelector = mkSelector "compare:"

-- | @Selector@ for @UUIDString@
uuidStringSelector :: Selector
uuidStringSelector = mkSelector "UUIDString"

