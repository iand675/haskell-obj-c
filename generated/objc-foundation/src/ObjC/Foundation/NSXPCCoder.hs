{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSXPCCoder@.
module ObjC.Foundation.NSXPCCoder
  ( NSXPCCoder
  , IsNSXPCCoder(..)
  , encodeXPCObject_forKey
  , decodeXPCObjectOfType_forKey
  , connection
  , encodeXPCObject_forKeySelector
  , decodeXPCObjectOfType_forKeySelector
  , connectionSelector


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

-- | @- encodeXPCObject:forKey:@
encodeXPCObject_forKey :: (IsNSXPCCoder nsxpcCoder, IsNSObject xpcObject, IsNSString key) => nsxpcCoder -> xpcObject -> key -> IO ()
encodeXPCObject_forKey nsxpcCoder  xpcObject key =
withObjCPtr xpcObject $ \raw_xpcObject ->
  withObjCPtr key $ \raw_key ->
      sendMsg nsxpcCoder (mkSelector "encodeXPCObject:forKey:") retVoid [argPtr (castPtr raw_xpcObject :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- decodeXPCObjectOfType:forKey:@
decodeXPCObjectOfType_forKey :: (IsNSXPCCoder nsxpcCoder, IsNSString key) => nsxpcCoder -> RawId -> key -> IO (Id NSObject)
decodeXPCObjectOfType_forKey nsxpcCoder  type_ key =
withObjCPtr key $ \raw_key ->
    sendMsg nsxpcCoder (mkSelector "decodeXPCObjectOfType:forKey:") (retPtr retVoid) [argPtr (castPtr (unRawId type_) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @- connection@
connection :: IsNSXPCCoder nsxpcCoder => nsxpcCoder -> IO (Id NSXPCConnection)
connection nsxpcCoder  =
  sendMsg nsxpcCoder (mkSelector "connection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @encodeXPCObject:forKey:@
encodeXPCObject_forKeySelector :: Selector
encodeXPCObject_forKeySelector = mkSelector "encodeXPCObject:forKey:"

-- | @Selector@ for @decodeXPCObjectOfType:forKey:@
decodeXPCObjectOfType_forKeySelector :: Selector
decodeXPCObjectOfType_forKeySelector = mkSelector "decodeXPCObjectOfType:forKey:"

-- | @Selector@ for @connection@
connectionSelector :: Selector
connectionSelector = mkSelector "connection"

