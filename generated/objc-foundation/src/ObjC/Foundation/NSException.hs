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
  , exceptionWithName_reason_userInfoSelector
  , initWithName_reason_userInfoSelector
  , raiseSelector
  , raise_formatSelector
  , raise_format_argumentsSelector
  , nameSelector
  , reasonSelector
  , userInfoSelector
  , callStackReturnAddressesSelector
  , callStackSymbolsSelector


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

-- | @+ exceptionWithName:reason:userInfo:@
exceptionWithName_reason_userInfo :: (IsNSString name, IsNSString reason, IsNSDictionary userInfo) => name -> reason -> userInfo -> IO (Id NSException)
exceptionWithName_reason_userInfo name reason userInfo =
  do
    cls' <- getRequiredClass "NSException"
    withObjCPtr name $ \raw_name ->
      withObjCPtr reason $ \raw_reason ->
        withObjCPtr userInfo $ \raw_userInfo ->
          sendClassMsg cls' (mkSelector "exceptionWithName:reason:userInfo:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_reason :: Ptr ()), argPtr (castPtr raw_userInfo :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithName:reason:userInfo:@
initWithName_reason_userInfo :: (IsNSException nsException, IsNSString aName, IsNSString aReason, IsNSDictionary aUserInfo) => nsException -> aName -> aReason -> aUserInfo -> IO (Id NSException)
initWithName_reason_userInfo nsException  aName aReason aUserInfo =
withObjCPtr aName $ \raw_aName ->
  withObjCPtr aReason $ \raw_aReason ->
    withObjCPtr aUserInfo $ \raw_aUserInfo ->
        sendMsg nsException (mkSelector "initWithName:reason:userInfo:") (retPtr retVoid) [argPtr (castPtr raw_aName :: Ptr ()), argPtr (castPtr raw_aReason :: Ptr ()), argPtr (castPtr raw_aUserInfo :: Ptr ())] >>= ownedObject . castPtr

-- | @- raise@
raise :: IsNSException nsException => nsException -> IO ()
raise nsException  =
  sendMsg nsException (mkSelector "raise") retVoid []

-- | @+ raise:format:@
raise_format :: (IsNSString name, IsNSString format) => name -> format -> IO ()
raise_format name format =
  do
    cls' <- getRequiredClass "NSException"
    withObjCPtr name $ \raw_name ->
      withObjCPtr format $ \raw_format ->
        sendClassMsg cls' (mkSelector "raise:format:") retVoid [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_format :: Ptr ())]

-- | @+ raise:format:arguments:@
raise_format_arguments :: (IsNSString name, IsNSString format) => name -> format -> RawId -> IO ()
raise_format_arguments name format argList =
  do
    cls' <- getRequiredClass "NSException"
    withObjCPtr name $ \raw_name ->
      withObjCPtr format $ \raw_format ->
        sendClassMsg cls' (mkSelector "raise:format:arguments:") retVoid [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr (unRawId argList) :: Ptr ())]

-- | @- name@
name :: IsNSException nsException => nsException -> IO (Id NSString)
name nsException  =
  sendMsg nsException (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- reason@
reason :: IsNSException nsException => nsException -> IO (Id NSString)
reason nsException  =
  sendMsg nsException (mkSelector "reason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- userInfo@
userInfo :: IsNSException nsException => nsException -> IO (Id NSDictionary)
userInfo nsException  =
  sendMsg nsException (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- callStackReturnAddresses@
callStackReturnAddresses :: IsNSException nsException => nsException -> IO (Id NSArray)
callStackReturnAddresses nsException  =
  sendMsg nsException (mkSelector "callStackReturnAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- callStackSymbols@
callStackSymbols :: IsNSException nsException => nsException -> IO (Id NSArray)
callStackSymbols nsException  =
  sendMsg nsException (mkSelector "callStackSymbols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @exceptionWithName:reason:userInfo:@
exceptionWithName_reason_userInfoSelector :: Selector
exceptionWithName_reason_userInfoSelector = mkSelector "exceptionWithName:reason:userInfo:"

-- | @Selector@ for @initWithName:reason:userInfo:@
initWithName_reason_userInfoSelector :: Selector
initWithName_reason_userInfoSelector = mkSelector "initWithName:reason:userInfo:"

-- | @Selector@ for @raise@
raiseSelector :: Selector
raiseSelector = mkSelector "raise"

-- | @Selector@ for @raise:format:@
raise_formatSelector :: Selector
raise_formatSelector = mkSelector "raise:format:"

-- | @Selector@ for @raise:format:arguments:@
raise_format_argumentsSelector :: Selector
raise_format_argumentsSelector = mkSelector "raise:format:arguments:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @reason@
reasonSelector :: Selector
reasonSelector = mkSelector "reason"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @callStackReturnAddresses@
callStackReturnAddressesSelector :: Selector
callStackReturnAddressesSelector = mkSelector "callStackReturnAddresses"

-- | @Selector@ for @callStackSymbols@
callStackSymbolsSelector :: Selector
callStackSymbolsSelector = mkSelector "callStackSymbols"

