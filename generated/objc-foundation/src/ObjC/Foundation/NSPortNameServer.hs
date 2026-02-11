{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPortNameServer@.
module ObjC.Foundation.NSPortNameServer
  ( NSPortNameServer
  , IsNSPortNameServer(..)
  , systemDefaultPortNameServer
  , portForName
  , portForName_host
  , registerPort_name
  , removePortForName
  , systemDefaultPortNameServerSelector
  , portForNameSelector
  , portForName_hostSelector
  , registerPort_nameSelector
  , removePortForNameSelector


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

-- | @+ systemDefaultPortNameServer@
systemDefaultPortNameServer :: IO (Id NSPortNameServer)
systemDefaultPortNameServer  =
  do
    cls' <- getRequiredClass "NSPortNameServer"
    sendClassMsg cls' (mkSelector "systemDefaultPortNameServer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- portForName:@
portForName :: (IsNSPortNameServer nsPortNameServer, IsNSString name) => nsPortNameServer -> name -> IO (Id NSPort)
portForName nsPortNameServer  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsPortNameServer (mkSelector "portForName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- portForName:host:@
portForName_host :: (IsNSPortNameServer nsPortNameServer, IsNSString name, IsNSString host) => nsPortNameServer -> name -> host -> IO (Id NSPort)
portForName_host nsPortNameServer  name host =
withObjCPtr name $ \raw_name ->
  withObjCPtr host $ \raw_host ->
      sendMsg nsPortNameServer (mkSelector "portForName:host:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_host :: Ptr ())] >>= retainedObject . castPtr

-- | @- registerPort:name:@
registerPort_name :: (IsNSPortNameServer nsPortNameServer, IsNSPort port, IsNSString name) => nsPortNameServer -> port -> name -> IO Bool
registerPort_name nsPortNameServer  port name =
withObjCPtr port $ \raw_port ->
  withObjCPtr name $ \raw_name ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPortNameServer (mkSelector "registerPort:name:") retCULong [argPtr (castPtr raw_port :: Ptr ()), argPtr (castPtr raw_name :: Ptr ())]

-- | @- removePortForName:@
removePortForName :: (IsNSPortNameServer nsPortNameServer, IsNSString name) => nsPortNameServer -> name -> IO Bool
removePortForName nsPortNameServer  name =
withObjCPtr name $ \raw_name ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPortNameServer (mkSelector "removePortForName:") retCULong [argPtr (castPtr raw_name :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @systemDefaultPortNameServer@
systemDefaultPortNameServerSelector :: Selector
systemDefaultPortNameServerSelector = mkSelector "systemDefaultPortNameServer"

-- | @Selector@ for @portForName:@
portForNameSelector :: Selector
portForNameSelector = mkSelector "portForName:"

-- | @Selector@ for @portForName:host:@
portForName_hostSelector :: Selector
portForName_hostSelector = mkSelector "portForName:host:"

-- | @Selector@ for @registerPort:name:@
registerPort_nameSelector :: Selector
registerPort_nameSelector = mkSelector "registerPort:name:"

-- | @Selector@ for @removePortForName:@
removePortForNameSelector :: Selector
removePortForNameSelector = mkSelector "removePortForName:"

