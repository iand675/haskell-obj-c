{-# LANGUAGE DataKinds #-}
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
  , codeSelector
  , domainSelector
  , errorWithDomain_code_userInfoSelector
  , helpAnchorSelector
  , initWithDomain_code_userInfoSelector
  , localizedDescriptionSelector
  , localizedFailureReasonSelector
  , localizedRecoveryOptionsSelector
  , localizedRecoverySuggestionSelector
  , recoveryAttempterSelector
  , setUserInfoValueProviderForDomain_providerSelector
  , underlyingErrorsSelector
  , userInfoSelector
  , userInfoValueProviderForDomainSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithDomain:code:userInfo:@
initWithDomain_code_userInfo :: (IsNSError nsError, IsNSString domain, IsNSDictionary dict) => nsError -> domain -> CLong -> dict -> IO (Id NSError)
initWithDomain_code_userInfo nsError domain code dict =
  sendOwnedMessage nsError initWithDomain_code_userInfoSelector (toNSString domain) code (toNSDictionary dict)

-- | @+ errorWithDomain:code:userInfo:@
errorWithDomain_code_userInfo :: (IsNSString domain, IsNSDictionary dict) => domain -> CLong -> dict -> IO (Id NSError)
errorWithDomain_code_userInfo domain code dict =
  do
    cls' <- getRequiredClass "NSError"
    sendClassMessage cls' errorWithDomain_code_userInfoSelector (toNSString domain) code (toNSDictionary dict)

-- | @+ setUserInfoValueProviderForDomain:provider:@
setUserInfoValueProviderForDomain_provider :: IsNSString errorDomain => errorDomain -> RawId -> IO ()
setUserInfoValueProviderForDomain_provider errorDomain provider =
  do
    cls' <- getRequiredClass "NSError"
    sendClassMessage cls' setUserInfoValueProviderForDomain_providerSelector (toNSString errorDomain) provider

-- | @+ userInfoValueProviderForDomain:@
userInfoValueProviderForDomain :: IsNSString errorDomain => errorDomain -> IO RawId
userInfoValueProviderForDomain errorDomain =
  do
    cls' <- getRequiredClass "NSError"
    sendClassMessage cls' userInfoValueProviderForDomainSelector (toNSString errorDomain)

-- | @- domain@
domain :: IsNSError nsError => nsError -> IO (Id NSString)
domain nsError =
  sendMessage nsError domainSelector

-- | @- code@
code :: IsNSError nsError => nsError -> IO CLong
code nsError =
  sendMessage nsError codeSelector

-- | @- userInfo@
userInfo :: IsNSError nsError => nsError -> IO (Id NSDictionary)
userInfo nsError =
  sendMessage nsError userInfoSelector

-- | @- localizedDescription@
localizedDescription :: IsNSError nsError => nsError -> IO (Id NSString)
localizedDescription nsError =
  sendMessage nsError localizedDescriptionSelector

-- | @- localizedFailureReason@
localizedFailureReason :: IsNSError nsError => nsError -> IO (Id NSString)
localizedFailureReason nsError =
  sendMessage nsError localizedFailureReasonSelector

-- | @- localizedRecoverySuggestion@
localizedRecoverySuggestion :: IsNSError nsError => nsError -> IO (Id NSString)
localizedRecoverySuggestion nsError =
  sendMessage nsError localizedRecoverySuggestionSelector

-- | @- localizedRecoveryOptions@
localizedRecoveryOptions :: IsNSError nsError => nsError -> IO (Id NSArray)
localizedRecoveryOptions nsError =
  sendMessage nsError localizedRecoveryOptionsSelector

-- | @- recoveryAttempter@
recoveryAttempter :: IsNSError nsError => nsError -> IO RawId
recoveryAttempter nsError =
  sendMessage nsError recoveryAttempterSelector

-- | @- helpAnchor@
helpAnchor :: IsNSError nsError => nsError -> IO (Id NSString)
helpAnchor nsError =
  sendMessage nsError helpAnchorSelector

-- | @- underlyingErrors@
underlyingErrors :: IsNSError nsError => nsError -> IO (Id NSArray)
underlyingErrors nsError =
  sendMessage nsError underlyingErrorsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDomain:code:userInfo:@
initWithDomain_code_userInfoSelector :: Selector '[Id NSString, CLong, Id NSDictionary] (Id NSError)
initWithDomain_code_userInfoSelector = mkSelector "initWithDomain:code:userInfo:"

-- | @Selector@ for @errorWithDomain:code:userInfo:@
errorWithDomain_code_userInfoSelector :: Selector '[Id NSString, CLong, Id NSDictionary] (Id NSError)
errorWithDomain_code_userInfoSelector = mkSelector "errorWithDomain:code:userInfo:"

-- | @Selector@ for @setUserInfoValueProviderForDomain:provider:@
setUserInfoValueProviderForDomain_providerSelector :: Selector '[Id NSString, RawId] ()
setUserInfoValueProviderForDomain_providerSelector = mkSelector "setUserInfoValueProviderForDomain:provider:"

-- | @Selector@ for @userInfoValueProviderForDomain:@
userInfoValueProviderForDomainSelector :: Selector '[Id NSString] RawId
userInfoValueProviderForDomainSelector = mkSelector "userInfoValueProviderForDomain:"

-- | @Selector@ for @domain@
domainSelector :: Selector '[] (Id NSString)
domainSelector = mkSelector "domain"

-- | @Selector@ for @code@
codeSelector :: Selector '[] CLong
codeSelector = mkSelector "code"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector '[] (Id NSString)
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @localizedFailureReason@
localizedFailureReasonSelector :: Selector '[] (Id NSString)
localizedFailureReasonSelector = mkSelector "localizedFailureReason"

-- | @Selector@ for @localizedRecoverySuggestion@
localizedRecoverySuggestionSelector :: Selector '[] (Id NSString)
localizedRecoverySuggestionSelector = mkSelector "localizedRecoverySuggestion"

-- | @Selector@ for @localizedRecoveryOptions@
localizedRecoveryOptionsSelector :: Selector '[] (Id NSArray)
localizedRecoveryOptionsSelector = mkSelector "localizedRecoveryOptions"

-- | @Selector@ for @recoveryAttempter@
recoveryAttempterSelector :: Selector '[] RawId
recoveryAttempterSelector = mkSelector "recoveryAttempter"

-- | @Selector@ for @helpAnchor@
helpAnchorSelector :: Selector '[] (Id NSString)
helpAnchorSelector = mkSelector "helpAnchor"

-- | @Selector@ for @underlyingErrors@
underlyingErrorsSelector :: Selector '[] (Id NSArray)
underlyingErrorsSelector = mkSelector "underlyingErrors"

