{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSError@.
module ObjC.Foundation.NSError
  ( NSError
  , IsNSError(..)
  , initWithDomain_code_userInfo
  , errorWithDomain_code_userInfo
  , setUserInfoValueProviderForDomain_provider
  , userInfoValueProviderForDomain
  , domain
  , code
  , userInfo
  , localizedDescription
  , localizedFailureReason
  , localizedRecoverySuggestion
  , localizedRecoveryOptions
  , recoveryAttempter
  , helpAnchor
  , underlyingErrors
  , initWithDomain_code_userInfoSelector
  , errorWithDomain_code_userInfoSelector
  , setUserInfoValueProviderForDomain_providerSelector
  , userInfoValueProviderForDomainSelector
  , domainSelector
  , codeSelector
  , userInfoSelector
  , localizedDescriptionSelector
  , localizedFailureReasonSelector
  , localizedRecoverySuggestionSelector
  , localizedRecoveryOptionsSelector
  , recoveryAttempterSelector
  , helpAnchorSelector
  , underlyingErrorsSelector


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

-- | @- initWithDomain:code:userInfo:@
initWithDomain_code_userInfo :: (IsNSError nsError, IsNSString domain, IsNSDictionary dict) => nsError -> domain -> CLong -> dict -> IO (Id NSError)
initWithDomain_code_userInfo nsError  domain code dict =
withObjCPtr domain $ \raw_domain ->
  withObjCPtr dict $ \raw_dict ->
      sendMsg nsError (mkSelector "initWithDomain:code:userInfo:") (retPtr retVoid) [argPtr (castPtr raw_domain :: Ptr ()), argCLong (fromIntegral code), argPtr (castPtr raw_dict :: Ptr ())] >>= ownedObject . castPtr

-- | @+ errorWithDomain:code:userInfo:@
errorWithDomain_code_userInfo :: (IsNSString domain, IsNSDictionary dict) => domain -> CLong -> dict -> IO (Id NSError)
errorWithDomain_code_userInfo domain code dict =
  do
    cls' <- getRequiredClass "NSError"
    withObjCPtr domain $ \raw_domain ->
      withObjCPtr dict $ \raw_dict ->
        sendClassMsg cls' (mkSelector "errorWithDomain:code:userInfo:") (retPtr retVoid) [argPtr (castPtr raw_domain :: Ptr ()), argCLong (fromIntegral code), argPtr (castPtr raw_dict :: Ptr ())] >>= retainedObject . castPtr

-- | @+ setUserInfoValueProviderForDomain:provider:@
setUserInfoValueProviderForDomain_provider :: IsNSString errorDomain => errorDomain -> RawId -> IO ()
setUserInfoValueProviderForDomain_provider errorDomain provider =
  do
    cls' <- getRequiredClass "NSError"
    withObjCPtr errorDomain $ \raw_errorDomain ->
      sendClassMsg cls' (mkSelector "setUserInfoValueProviderForDomain:provider:") retVoid [argPtr (castPtr raw_errorDomain :: Ptr ()), argPtr (castPtr (unRawId provider) :: Ptr ())]

-- | @+ userInfoValueProviderForDomain:@
userInfoValueProviderForDomain :: IsNSString errorDomain => errorDomain -> IO RawId
userInfoValueProviderForDomain errorDomain =
  do
    cls' <- getRequiredClass "NSError"
    withObjCPtr errorDomain $ \raw_errorDomain ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "userInfoValueProviderForDomain:") (retPtr retVoid) [argPtr (castPtr raw_errorDomain :: Ptr ())]

-- | @- domain@
domain :: IsNSError nsError => nsError -> IO (Id NSString)
domain nsError  =
  sendMsg nsError (mkSelector "domain") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- code@
code :: IsNSError nsError => nsError -> IO CLong
code nsError  =
  sendMsg nsError (mkSelector "code") retCLong []

-- | @- userInfo@
userInfo :: IsNSError nsError => nsError -> IO (Id NSDictionary)
userInfo nsError  =
  sendMsg nsError (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizedDescription@
localizedDescription :: IsNSError nsError => nsError -> IO (Id NSString)
localizedDescription nsError  =
  sendMsg nsError (mkSelector "localizedDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizedFailureReason@
localizedFailureReason :: IsNSError nsError => nsError -> IO (Id NSString)
localizedFailureReason nsError  =
  sendMsg nsError (mkSelector "localizedFailureReason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizedRecoverySuggestion@
localizedRecoverySuggestion :: IsNSError nsError => nsError -> IO (Id NSString)
localizedRecoverySuggestion nsError  =
  sendMsg nsError (mkSelector "localizedRecoverySuggestion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizedRecoveryOptions@
localizedRecoveryOptions :: IsNSError nsError => nsError -> IO (Id NSArray)
localizedRecoveryOptions nsError  =
  sendMsg nsError (mkSelector "localizedRecoveryOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- recoveryAttempter@
recoveryAttempter :: IsNSError nsError => nsError -> IO RawId
recoveryAttempter nsError  =
  fmap (RawId . castPtr) $ sendMsg nsError (mkSelector "recoveryAttempter") (retPtr retVoid) []

-- | @- helpAnchor@
helpAnchor :: IsNSError nsError => nsError -> IO (Id NSString)
helpAnchor nsError  =
  sendMsg nsError (mkSelector "helpAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- underlyingErrors@
underlyingErrors :: IsNSError nsError => nsError -> IO (Id NSArray)
underlyingErrors nsError  =
  sendMsg nsError (mkSelector "underlyingErrors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDomain:code:userInfo:@
initWithDomain_code_userInfoSelector :: Selector
initWithDomain_code_userInfoSelector = mkSelector "initWithDomain:code:userInfo:"

-- | @Selector@ for @errorWithDomain:code:userInfo:@
errorWithDomain_code_userInfoSelector :: Selector
errorWithDomain_code_userInfoSelector = mkSelector "errorWithDomain:code:userInfo:"

-- | @Selector@ for @setUserInfoValueProviderForDomain:provider:@
setUserInfoValueProviderForDomain_providerSelector :: Selector
setUserInfoValueProviderForDomain_providerSelector = mkSelector "setUserInfoValueProviderForDomain:provider:"

-- | @Selector@ for @userInfoValueProviderForDomain:@
userInfoValueProviderForDomainSelector :: Selector
userInfoValueProviderForDomainSelector = mkSelector "userInfoValueProviderForDomain:"

-- | @Selector@ for @domain@
domainSelector :: Selector
domainSelector = mkSelector "domain"

-- | @Selector@ for @code@
codeSelector :: Selector
codeSelector = mkSelector "code"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @localizedFailureReason@
localizedFailureReasonSelector :: Selector
localizedFailureReasonSelector = mkSelector "localizedFailureReason"

-- | @Selector@ for @localizedRecoverySuggestion@
localizedRecoverySuggestionSelector :: Selector
localizedRecoverySuggestionSelector = mkSelector "localizedRecoverySuggestion"

-- | @Selector@ for @localizedRecoveryOptions@
localizedRecoveryOptionsSelector :: Selector
localizedRecoveryOptionsSelector = mkSelector "localizedRecoveryOptions"

-- | @Selector@ for @recoveryAttempter@
recoveryAttempterSelector :: Selector
recoveryAttempterSelector = mkSelector "recoveryAttempter"

-- | @Selector@ for @helpAnchor@
helpAnchorSelector :: Selector
helpAnchorSelector = mkSelector "helpAnchor"

-- | @Selector@ for @underlyingErrors@
underlyingErrorsSelector :: Selector
underlyingErrorsSelector = mkSelector "underlyingErrors"

