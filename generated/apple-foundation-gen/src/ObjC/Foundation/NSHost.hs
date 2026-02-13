{-# LANGUAGE DataKinds #-}
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
  , localizedName
  , addressSelector
  , addressesSelector
  , currentHostSelector
  , flushHostCacheSelector
  , hostWithAddressSelector
  , hostWithNameSelector
  , isEqualToHostSelector
  , isHostCacheEnabledSelector
  , localizedNameSelector
  , nameSelector
  , namesSelector
  , setHostCacheEnabledSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ currentHost@
currentHost :: IO (Id NSHost)
currentHost  =
  do
    cls' <- getRequiredClass "NSHost"
    sendClassMessage cls' currentHostSelector

-- | @+ hostWithName:@
hostWithName :: IsNSString name => name -> IO (Id NSHost)
hostWithName name =
  do
    cls' <- getRequiredClass "NSHost"
    sendClassMessage cls' hostWithNameSelector (toNSString name)

-- | @+ hostWithAddress:@
hostWithAddress :: IsNSString address => address -> IO (Id NSHost)
hostWithAddress address =
  do
    cls' <- getRequiredClass "NSHost"
    sendClassMessage cls' hostWithAddressSelector (toNSString address)

-- | @- isEqualToHost:@
isEqualToHost :: (IsNSHost nsHost, IsNSHost aHost) => nsHost -> aHost -> IO Bool
isEqualToHost nsHost aHost =
  sendMessage nsHost isEqualToHostSelector (toNSHost aHost)

-- | @+ setHostCacheEnabled:@
setHostCacheEnabled :: Bool -> IO ()
setHostCacheEnabled flag =
  do
    cls' <- getRequiredClass "NSHost"
    sendClassMessage cls' setHostCacheEnabledSelector flag

-- | @+ isHostCacheEnabled@
isHostCacheEnabled :: IO Bool
isHostCacheEnabled  =
  do
    cls' <- getRequiredClass "NSHost"
    sendClassMessage cls' isHostCacheEnabledSelector

-- | @+ flushHostCache@
flushHostCache :: IO ()
flushHostCache  =
  do
    cls' <- getRequiredClass "NSHost"
    sendClassMessage cls' flushHostCacheSelector

-- | @- name@
name :: IsNSHost nsHost => nsHost -> IO (Id NSString)
name nsHost =
  sendMessage nsHost nameSelector

-- | @- names@
names :: IsNSHost nsHost => nsHost -> IO (Id NSArray)
names nsHost =
  sendMessage nsHost namesSelector

-- | @- address@
address :: IsNSHost nsHost => nsHost -> IO (Id NSString)
address nsHost =
  sendMessage nsHost addressSelector

-- | @- addresses@
addresses :: IsNSHost nsHost => nsHost -> IO (Id NSArray)
addresses nsHost =
  sendMessage nsHost addressesSelector

-- | @- localizedName@
localizedName :: IsNSHost nsHost => nsHost -> IO (Id NSString)
localizedName nsHost =
  sendMessage nsHost localizedNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentHost@
currentHostSelector :: Selector '[] (Id NSHost)
currentHostSelector = mkSelector "currentHost"

-- | @Selector@ for @hostWithName:@
hostWithNameSelector :: Selector '[Id NSString] (Id NSHost)
hostWithNameSelector = mkSelector "hostWithName:"

-- | @Selector@ for @hostWithAddress:@
hostWithAddressSelector :: Selector '[Id NSString] (Id NSHost)
hostWithAddressSelector = mkSelector "hostWithAddress:"

-- | @Selector@ for @isEqualToHost:@
isEqualToHostSelector :: Selector '[Id NSHost] Bool
isEqualToHostSelector = mkSelector "isEqualToHost:"

-- | @Selector@ for @setHostCacheEnabled:@
setHostCacheEnabledSelector :: Selector '[Bool] ()
setHostCacheEnabledSelector = mkSelector "setHostCacheEnabled:"

-- | @Selector@ for @isHostCacheEnabled@
isHostCacheEnabledSelector :: Selector '[] Bool
isHostCacheEnabledSelector = mkSelector "isHostCacheEnabled"

-- | @Selector@ for @flushHostCache@
flushHostCacheSelector :: Selector '[] ()
flushHostCacheSelector = mkSelector "flushHostCache"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @names@
namesSelector :: Selector '[] (Id NSArray)
namesSelector = mkSelector "names"

-- | @Selector@ for @address@
addressSelector :: Selector '[] (Id NSString)
addressSelector = mkSelector "address"

-- | @Selector@ for @addresses@
addressesSelector :: Selector '[] (Id NSArray)
addressesSelector = mkSelector "addresses"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector '[] (Id NSString)
localizedNameSelector = mkSelector "localizedName"

