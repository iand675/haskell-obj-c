{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMessagePortNameServer@.
module ObjC.Foundation.NSMessagePortNameServer
  ( NSMessagePortNameServer
  , IsNSMessagePortNameServer(..)
  , sharedInstance
  , portForName
  , portForName_host
  , sharedInstanceSelector
  , portForNameSelector
  , portForName_hostSelector


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
    cls' <- getRequiredClass "NSMessagePortNameServer"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "sharedInstance") (retPtr retVoid) []

-- | @- portForName:@
portForName :: (IsNSMessagePortNameServer nsMessagePortNameServer, IsNSString name) => nsMessagePortNameServer -> name -> IO (Id NSPort)
portForName nsMessagePortNameServer  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsMessagePortNameServer (mkSelector "portForName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- portForName:host:@
portForName_host :: (IsNSMessagePortNameServer nsMessagePortNameServer, IsNSString name, IsNSString host) => nsMessagePortNameServer -> name -> host -> IO (Id NSPort)
portForName_host nsMessagePortNameServer  name host =
withObjCPtr name $ \raw_name ->
  withObjCPtr host $ \raw_host ->
      sendMsg nsMessagePortNameServer (mkSelector "portForName:host:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_host :: Ptr ())] >>= retainedObject . castPtr

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

