{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , compareSelector
  , getUUIDBytesSelector
  , initSelector
  , initWithUUIDBytesSelector
  , initWithUUIDStringSelector
  , uuidSelector
  , uuidStringSelector

  -- * Enum types
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ UUID@
uuid :: IO (Id NSUUID)
uuid  =
  do
    cls' <- getRequiredClass "NSUUID"
    sendClassMessage cls' uuidSelector

-- | @- init@
init_ :: IsNSUUID nsuuid => nsuuid -> IO (Id NSUUID)
init_ nsuuid =
  sendOwnedMessage nsuuid initSelector

-- | @- initWithUUIDString:@
initWithUUIDString :: (IsNSUUID nsuuid, IsNSString string) => nsuuid -> string -> IO (Id NSUUID)
initWithUUIDString nsuuid string =
  sendOwnedMessage nsuuid initWithUUIDStringSelector (toNSString string)

-- | @- initWithUUIDBytes:@
initWithUUIDBytes :: IsNSUUID nsuuid => nsuuid -> Const (Ptr CUChar) -> IO (Id NSUUID)
initWithUUIDBytes nsuuid bytes =
  sendOwnedMessage nsuuid initWithUUIDBytesSelector bytes

-- | @- getUUIDBytes:@
getUUIDBytes :: IsNSUUID nsuuid => nsuuid -> Ptr CUChar -> IO ()
getUUIDBytes nsuuid uuid =
  sendMessage nsuuid getUUIDBytesSelector uuid

-- | @- compare:@
compare_ :: (IsNSUUID nsuuid, IsNSUUID otherUUID) => nsuuid -> otherUUID -> IO NSComparisonResult
compare_ nsuuid otherUUID =
  sendMessage nsuuid compareSelector (toNSUUID otherUUID)

-- | @- UUIDString@
uuidString :: IsNSUUID nsuuid => nsuuid -> IO (Id NSString)
uuidString nsuuid =
  sendMessage nsuuid uuidStringSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @UUID@
uuidSelector :: Selector '[] (Id NSUUID)
uuidSelector = mkSelector "UUID"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSUUID)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithUUIDString:@
initWithUUIDStringSelector :: Selector '[Id NSString] (Id NSUUID)
initWithUUIDStringSelector = mkSelector "initWithUUIDString:"

-- | @Selector@ for @initWithUUIDBytes:@
initWithUUIDBytesSelector :: Selector '[Const (Ptr CUChar)] (Id NSUUID)
initWithUUIDBytesSelector = mkSelector "initWithUUIDBytes:"

-- | @Selector@ for @getUUIDBytes:@
getUUIDBytesSelector :: Selector '[Ptr CUChar] ()
getUUIDBytesSelector = mkSelector "getUUIDBytes:"

-- | @Selector@ for @compare:@
compareSelector :: Selector '[Id NSUUID] NSComparisonResult
compareSelector = mkSelector "compare:"

-- | @Selector@ for @UUIDString@
uuidStringSelector :: Selector '[] (Id NSString)
uuidStringSelector = mkSelector "UUIDString"

