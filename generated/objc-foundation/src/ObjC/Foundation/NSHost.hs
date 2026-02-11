{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | DEPRECATION NOTICE
--
-- If you’re using @NSHost@ to resolve DNS names so that you can connect to a service, switch to a connect-by-name API, for example, @nw_connection@.
--
-- If you have other DNS resolution needs, switch to <dns_sd.h>.
--
-- Generated bindings for @NSHost@.
module ObjC.Foundation.NSHost
  ( NSHost
  , IsNSHost(..)
  , currentHost
  , hostWithName
  , hostWithAddress
  , isEqualToHost
  , setHostCacheEnabled
  , isHostCacheEnabled
  , flushHostCache
  , name
  , names
  , address
  , addresses
  , currentHostSelector
  , hostWithNameSelector
  , hostWithAddressSelector
  , isEqualToHostSelector
  , setHostCacheEnabledSelector
  , isHostCacheEnabledSelector
  , flushHostCacheSelector
  , nameSelector
  , namesSelector
  , addressSelector
  , addressesSelector


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

-- | @+ currentHost@
currentHost :: IO (Id NSHost)
currentHost  =
  do
    cls' <- getRequiredClass "NSHost"
    sendClassMsg cls' (mkSelector "currentHost") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ hostWithName:@
hostWithName :: IsNSString name => name -> IO (Id NSHost)
hostWithName name =
  do
    cls' <- getRequiredClass "NSHost"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "hostWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @+ hostWithAddress:@
hostWithAddress :: IsNSString address => address -> IO (Id NSHost)
hostWithAddress address =
  do
    cls' <- getRequiredClass "NSHost"
    withObjCPtr address $ \raw_address ->
      sendClassMsg cls' (mkSelector "hostWithAddress:") (retPtr retVoid) [argPtr (castPtr raw_address :: Ptr ())] >>= retainedObject . castPtr

-- | @- isEqualToHost:@
isEqualToHost :: (IsNSHost nsHost, IsNSHost aHost) => nsHost -> aHost -> IO Bool
isEqualToHost nsHost  aHost =
withObjCPtr aHost $ \raw_aHost ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsHost (mkSelector "isEqualToHost:") retCULong [argPtr (castPtr raw_aHost :: Ptr ())]

-- | @+ setHostCacheEnabled:@
setHostCacheEnabled :: Bool -> IO ()
setHostCacheEnabled flag =
  do
    cls' <- getRequiredClass "NSHost"
    sendClassMsg cls' (mkSelector "setHostCacheEnabled:") retVoid [argCULong (if flag then 1 else 0)]

-- | @+ isHostCacheEnabled@
isHostCacheEnabled :: IO Bool
isHostCacheEnabled  =
  do
    cls' <- getRequiredClass "NSHost"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isHostCacheEnabled") retCULong []

-- | @+ flushHostCache@
flushHostCache :: IO ()
flushHostCache  =
  do
    cls' <- getRequiredClass "NSHost"
    sendClassMsg cls' (mkSelector "flushHostCache") retVoid []

-- | @- name@
name :: IsNSHost nsHost => nsHost -> IO (Id NSString)
name nsHost  =
  sendMsg nsHost (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- names@
names :: IsNSHost nsHost => nsHost -> IO (Id NSArray)
names nsHost  =
  sendMsg nsHost (mkSelector "names") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- address@
address :: IsNSHost nsHost => nsHost -> IO (Id NSString)
address nsHost  =
  sendMsg nsHost (mkSelector "address") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- addresses@
addresses :: IsNSHost nsHost => nsHost -> IO (Id NSArray)
addresses nsHost  =
  sendMsg nsHost (mkSelector "addresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentHost@
currentHostSelector :: Selector
currentHostSelector = mkSelector "currentHost"

-- | @Selector@ for @hostWithName:@
hostWithNameSelector :: Selector
hostWithNameSelector = mkSelector "hostWithName:"

-- | @Selector@ for @hostWithAddress:@
hostWithAddressSelector :: Selector
hostWithAddressSelector = mkSelector "hostWithAddress:"

-- | @Selector@ for @isEqualToHost:@
isEqualToHostSelector :: Selector
isEqualToHostSelector = mkSelector "isEqualToHost:"

-- | @Selector@ for @setHostCacheEnabled:@
setHostCacheEnabledSelector :: Selector
setHostCacheEnabledSelector = mkSelector "setHostCacheEnabled:"

-- | @Selector@ for @isHostCacheEnabled@
isHostCacheEnabledSelector :: Selector
isHostCacheEnabledSelector = mkSelector "isHostCacheEnabled"

-- | @Selector@ for @flushHostCache@
flushHostCacheSelector :: Selector
flushHostCacheSelector = mkSelector "flushHostCache"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @names@
namesSelector :: Selector
namesSelector = mkSelector "names"

-- | @Selector@ for @address@
addressSelector :: Selector
addressSelector = mkSelector "address"

-- | @Selector@ for @addresses@
addressesSelector :: Selector
addressesSelector = mkSelector "addresses"

