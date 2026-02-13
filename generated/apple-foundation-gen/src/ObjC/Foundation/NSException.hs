{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSException@.
module ObjC.Foundation.NSException
  ( NSException
  , IsNSException(..)
  , exceptionWithName_reason_userInfo
  , initWithName_reason_userInfo
  , raise
  , raise_format
  , raise_format_arguments
  , name
  , reason
  , userInfo
  , callStackReturnAddresses
  , callStackSymbols
  , callStackReturnAddressesSelector
  , callStackSymbolsSelector
  , exceptionWithName_reason_userInfoSelector
  , initWithName_reason_userInfoSelector
  , nameSelector
  , raiseSelector
  , raise_formatSelector
  , raise_format_argumentsSelector
  , reasonSelector
  , userInfoSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ exceptionWithName:reason:userInfo:@
exceptionWithName_reason_userInfo :: (IsNSString name, IsNSString reason, IsNSDictionary userInfo) => name -> reason -> userInfo -> IO (Id NSException)
exceptionWithName_reason_userInfo name reason userInfo =
  do
    cls' <- getRequiredClass "NSException"
    sendClassMessage cls' exceptionWithName_reason_userInfoSelector (toNSString name) (toNSString reason) (toNSDictionary userInfo)

-- | @- initWithName:reason:userInfo:@
initWithName_reason_userInfo :: (IsNSException nsException, IsNSString aName, IsNSString aReason, IsNSDictionary aUserInfo) => nsException -> aName -> aReason -> aUserInfo -> IO (Id NSException)
initWithName_reason_userInfo nsException aName aReason aUserInfo =
  sendOwnedMessage nsException initWithName_reason_userInfoSelector (toNSString aName) (toNSString aReason) (toNSDictionary aUserInfo)

-- | @- raise@
raise :: IsNSException nsException => nsException -> IO ()
raise nsException =
  sendMessage nsException raiseSelector

-- | @+ raise:format:@
raise_format :: (IsNSString name, IsNSString format) => name -> format -> IO ()
raise_format name format =
  do
    cls' <- getRequiredClass "NSException"
    sendClassMessage cls' raise_formatSelector (toNSString name) (toNSString format)

-- | @+ raise:format:arguments:@
raise_format_arguments :: (IsNSString name, IsNSString format) => name -> format -> RawId -> IO ()
raise_format_arguments name format argList =
  do
    cls' <- getRequiredClass "NSException"
    sendClassMessage cls' raise_format_argumentsSelector (toNSString name) (toNSString format) argList

-- | @- name@
name :: IsNSException nsException => nsException -> IO (Id NSString)
name nsException =
  sendMessage nsException nameSelector

-- | @- reason@
reason :: IsNSException nsException => nsException -> IO (Id NSString)
reason nsException =
  sendMessage nsException reasonSelector

-- | @- userInfo@
userInfo :: IsNSException nsException => nsException -> IO (Id NSDictionary)
userInfo nsException =
  sendMessage nsException userInfoSelector

-- | @- callStackReturnAddresses@
callStackReturnAddresses :: IsNSException nsException => nsException -> IO (Id NSArray)
callStackReturnAddresses nsException =
  sendMessage nsException callStackReturnAddressesSelector

-- | @- callStackSymbols@
callStackSymbols :: IsNSException nsException => nsException -> IO (Id NSArray)
callStackSymbols nsException =
  sendMessage nsException callStackSymbolsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @exceptionWithName:reason:userInfo:@
exceptionWithName_reason_userInfoSelector :: Selector '[Id NSString, Id NSString, Id NSDictionary] (Id NSException)
exceptionWithName_reason_userInfoSelector = mkSelector "exceptionWithName:reason:userInfo:"

-- | @Selector@ for @initWithName:reason:userInfo:@
initWithName_reason_userInfoSelector :: Selector '[Id NSString, Id NSString, Id NSDictionary] (Id NSException)
initWithName_reason_userInfoSelector = mkSelector "initWithName:reason:userInfo:"

-- | @Selector@ for @raise@
raiseSelector :: Selector '[] ()
raiseSelector = mkSelector "raise"

-- | @Selector@ for @raise:format:@
raise_formatSelector :: Selector '[Id NSString, Id NSString] ()
raise_formatSelector = mkSelector "raise:format:"

-- | @Selector@ for @raise:format:arguments:@
raise_format_argumentsSelector :: Selector '[Id NSString, Id NSString, RawId] ()
raise_format_argumentsSelector = mkSelector "raise:format:arguments:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @reason@
reasonSelector :: Selector '[] (Id NSString)
reasonSelector = mkSelector "reason"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @callStackReturnAddresses@
callStackReturnAddressesSelector :: Selector '[] (Id NSArray)
callStackReturnAddressesSelector = mkSelector "callStackReturnAddresses"

-- | @Selector@ for @callStackSymbols@
callStackSymbolsSelector :: Selector '[] (Id NSArray)
callStackSymbolsSelector = mkSelector "callStackSymbols"

