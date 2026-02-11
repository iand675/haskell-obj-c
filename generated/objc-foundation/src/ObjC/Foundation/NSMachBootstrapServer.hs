{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMachBootstrapServer@.
module ObjC.Foundation.NSMachBootstrapServer
  ( NSMachBootstrapServer
  , IsNSMachBootstrapServer(..)
  , sharedInstance
  , portForName
  , portForName_host
  , registerPort_name
  , servicePortWithName
  , sharedInstanceSelector
  , portForNameSelector
  , portForName_hostSelector
  , registerPort_nameSelector
  , servicePortWithNameSelector


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

-- | @+ sharedInstance@
sharedInstance :: IO RawId
sharedInstance  =
  do
    cls' <- getRequiredClass "NSMachBootstrapServer"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "sharedInstance") (retPtr retVoid) []

-- | @- portForName:@
portForName :: (IsNSMachBootstrapServer nsMachBootstrapServer, IsNSString name) => nsMachBootstrapServer -> name -> IO (Id NSPort)
portForName nsMachBootstrapServer  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsMachBootstrapServer (mkSelector "portForName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- portForName:host:@
portForName_host :: (IsNSMachBootstrapServer nsMachBootstrapServer, IsNSString name, IsNSString host) => nsMachBootstrapServer -> name -> host -> IO (Id NSPort)
portForName_host nsMachBootstrapServer  name host =
withObjCPtr name $ \raw_name ->
  withObjCPtr host $ \raw_host ->
      sendMsg nsMachBootstrapServer (mkSelector "portForName:host:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_host :: Ptr ())] >>= retainedObject . castPtr

-- | @- registerPort:name:@
registerPort_name :: (IsNSMachBootstrapServer nsMachBootstrapServer, IsNSPort port, IsNSString name) => nsMachBootstrapServer -> port -> name -> IO Bool
registerPort_name nsMachBootstrapServer  port name =
withObjCPtr port $ \raw_port ->
  withObjCPtr name $ \raw_name ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMachBootstrapServer (mkSelector "registerPort:name:") retCULong [argPtr (castPtr raw_port :: Ptr ()), argPtr (castPtr raw_name :: Ptr ())]

-- | @- servicePortWithName:@
servicePortWithName :: (IsNSMachBootstrapServer nsMachBootstrapServer, IsNSString name) => nsMachBootstrapServer -> name -> IO (Id NSPort)
servicePortWithName nsMachBootstrapServer  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsMachBootstrapServer (mkSelector "servicePortWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedInstance@
sharedInstanceSelector :: Selector
sharedInstanceSelector = mkSelector "sharedInstance"

-- | @Selector@ for @portForName:@
portForNameSelector :: Selector
portForNameSelector = mkSelector "portForName:"

-- | @Selector@ for @portForName:host:@
portForName_hostSelector :: Selector
portForName_hostSelector = mkSelector "portForName:host:"

-- | @Selector@ for @registerPort:name:@
registerPort_nameSelector :: Selector
registerPort_nameSelector = mkSelector "registerPort:name:"

-- | @Selector@ for @servicePortWithName:@
servicePortWithNameSelector :: Selector
servicePortWithNameSelector = mkSelector "servicePortWithName:"

