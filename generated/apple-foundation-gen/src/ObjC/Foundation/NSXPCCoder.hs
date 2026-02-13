{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSXPCCoder@.
module ObjC.Foundation.NSXPCCoder
  ( NSXPCCoder
  , IsNSXPCCoder(..)
  , encodeXPCObject_forKey
  , decodeXPCObjectOfType_forKey
  , userInfo
  , setUserInfo
  , connection
  , connectionSelector
  , decodeXPCObjectOfType_forKeySelector
  , encodeXPCObject_forKeySelector
  , setUserInfoSelector
  , userInfoSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- encodeXPCObject:forKey:@
encodeXPCObject_forKey :: (IsNSXPCCoder nsxpcCoder, IsNSObject xpcObject, IsNSString key) => nsxpcCoder -> xpcObject -> key -> IO ()
encodeXPCObject_forKey nsxpcCoder xpcObject key =
  sendMessage nsxpcCoder encodeXPCObject_forKeySelector (toNSObject xpcObject) (toNSString key)

-- | @- decodeXPCObjectOfType:forKey:@
decodeXPCObjectOfType_forKey :: (IsNSXPCCoder nsxpcCoder, IsNSString key) => nsxpcCoder -> RawId -> key -> IO (Id NSObject)
decodeXPCObjectOfType_forKey nsxpcCoder type_ key =
  sendMessage nsxpcCoder decodeXPCObjectOfType_forKeySelector type_ (toNSString key)

-- | @- userInfo@
userInfo :: IsNSXPCCoder nsxpcCoder => nsxpcCoder -> IO RawId
userInfo nsxpcCoder =
  sendMessage nsxpcCoder userInfoSelector

-- | @- setUserInfo:@
setUserInfo :: IsNSXPCCoder nsxpcCoder => nsxpcCoder -> RawId -> IO ()
setUserInfo nsxpcCoder value =
  sendMessage nsxpcCoder setUserInfoSelector value

-- | @- connection@
connection :: IsNSXPCCoder nsxpcCoder => nsxpcCoder -> IO (Id NSXPCConnection)
connection nsxpcCoder =
  sendMessage nsxpcCoder connectionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @encodeXPCObject:forKey:@
encodeXPCObject_forKeySelector :: Selector '[Id NSObject, Id NSString] ()
encodeXPCObject_forKeySelector = mkSelector "encodeXPCObject:forKey:"

-- | @Selector@ for @decodeXPCObjectOfType:forKey:@
decodeXPCObjectOfType_forKeySelector :: Selector '[RawId, Id NSString] (Id NSObject)
decodeXPCObjectOfType_forKeySelector = mkSelector "decodeXPCObjectOfType:forKey:"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] RawId
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector '[RawId] ()
setUserInfoSelector = mkSelector "setUserInfo:"

-- | @Selector@ for @connection@
connectionSelector :: Selector '[] (Id NSXPCConnection)
connectionSelector = mkSelector "connection"

