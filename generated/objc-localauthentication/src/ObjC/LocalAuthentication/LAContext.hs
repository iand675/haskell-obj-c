{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class that represents an authentication context.
--
-- This context can be used for evaluating policies.
--
-- See: LAPolicy
--
-- Generated bindings for @LAContext@.
module ObjC.LocalAuthentication.LAContext
  ( LAContext
  , IsLAContext(..)
  , canEvaluatePolicy_error
  , evaluatePolicy_localizedReason_reply
  , invalidate
  , setCredential_type
  , isCredentialSet
  , evaluateAccessControl_operation_localizedReason_reply
  , localizedFallbackTitle
  , setLocalizedFallbackTitle
  , maxBiometryFailures
  , setMaxBiometryFailures
  , localizedCancelTitle
  , setLocalizedCancelTitle
  , touchIDAuthenticationAllowableReuseDuration
  , setTouchIDAuthenticationAllowableReuseDuration
  , localizedReason
  , setLocalizedReason
  , interactionNotAllowed
  , setInteractionNotAllowed
  , biometryType
  , evaluatedPolicyDomainState
  , domainState
  , canEvaluatePolicy_errorSelector
  , evaluatePolicy_localizedReason_replySelector
  , invalidateSelector
  , setCredential_typeSelector
  , isCredentialSetSelector
  , evaluateAccessControl_operation_localizedReason_replySelector
  , localizedFallbackTitleSelector
  , setLocalizedFallbackTitleSelector
  , maxBiometryFailuresSelector
  , setMaxBiometryFailuresSelector
  , localizedCancelTitleSelector
  , setLocalizedCancelTitleSelector
  , touchIDAuthenticationAllowableReuseDurationSelector
  , setTouchIDAuthenticationAllowableReuseDurationSelector
  , localizedReasonSelector
  , setLocalizedReasonSelector
  , interactionNotAllowedSelector
  , setInteractionNotAllowedSelector
  , biometryTypeSelector
  , evaluatedPolicyDomainStateSelector
  , domainStateSelector

  -- * Enum types
  , LAAccessControlOperation(LAAccessControlOperation)
  , pattern LAAccessControlOperationCreateItem
  , pattern LAAccessControlOperationUseItem
  , pattern LAAccessControlOperationCreateKey
  , pattern LAAccessControlOperationUseKeySign
  , pattern LAAccessControlOperationUseKeyDecrypt
  , pattern LAAccessControlOperationUseKeyKeyExchange
  , LABiometryType(LABiometryType)
  , pattern LABiometryTypeNone
  , pattern LABiometryNone
  , pattern LABiometryTypeTouchID
  , pattern LABiometryTypeFaceID
  , pattern LABiometryTypeOpticID
  , LACredentialType(LACredentialType)
  , pattern LACredentialTypeApplicationPassword
  , pattern LACredentialTypeSmartCardPIN
  , LAPolicy(LAPolicy)
  , pattern LAPolicyDeviceOwnerAuthenticationWithBiometrics
  , pattern LAPolicyDeviceOwnerAuthentication
  , pattern LAPolicyDeviceOwnerAuthenticationWithCompanion
  , pattern LAPolicyDeviceOwnerAuthenticationWithBiometricsOrCompanion
  , pattern LAPolicyDeviceOwnerAuthenticationWithWristDetection
  , pattern LAPolicyDeviceOwnerAuthenticationWithWatch
  , pattern LAPolicyDeviceOwnerAuthenticationWithBiometricsOrWatch

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

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.LocalAuthentication.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Determines if a particular policy can be evaluated.
--
-- Policies can have certain requirements which, when not satisfied, would always cause             the policy evaluation to fail - e.g. a passcode set, a fingerprint             enrolled with Touch ID or a face set up with Face ID. This method allows easy checking             for such conditions.
--
-- Applications should consume the returned value immediately and avoid relying on it             for an extensive period of time. At least, it is guaranteed to stay valid until the             application enters background.
--
-- Warning: Do not call this method in the reply block of evaluatePolicy:reply: because it could             lead to a deadlock.
--
-- @policy@ — Policy for which the preflight check should be run.
--
-- @error@ — Optional output parameter which is set to nil if the policy can be evaluated, or it              contains error information if policy evaluation is not possible.
--
-- Returns: YES if the policy can be evaluated, NO otherwise.
--
-- ObjC selector: @- canEvaluatePolicy:error:@
canEvaluatePolicy_error :: (IsLAContext laContext, IsNSError error_) => laContext -> LAPolicy -> error_ -> IO Bool
canEvaluatePolicy_error laContext  policy error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg laContext (mkSelector "canEvaluatePolicy:error:") retCULong [argCLong (coerce policy), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Evaluates the specified policy.
--
-- Policy evaluation may involve prompting user for various kinds of interaction             or authentication. Actual behavior is dependent on evaluated policy, device type,             and can be affected by installed configuration profiles.
--
-- Be sure to keep a strong reference to the context while the evaluation is in progress.             Otherwise, an evaluation would be canceled when the context is being deallocated.
--
-- The method does not block. Instead, the caller must provide a reply block to be             called asynchronously when evaluation finishes. The block is executed on a private             queue internal to the framework in an unspecified threading context. Other than that,             no guarantee is made about which queue, thread, or run-loop the block is executed on.
--
-- Implications of successful policy evaluation are policy specific. In general, this             operation is not idempotent. Policy evaluation may fail for various reasons, including             user cancel, system cancel and others, see LAError codes.
--
-- @policy@ — Policy to be evaluated.
--
-- @reply@ — Reply block that is executed when policy evaluation finishes.              success Reply parameter that is YES if the policy has been evaluated successfully or                      NO if the evaluation failed.              error Reply parameter that is nil if the policy has been evaluated successfully, or it                    contains error information about the evaluation failure.
--
-- @localizedReason@ — Application reason for authentication. This string must be provided in correct                        localization and should be short and clear. It will be eventually displayed in                        the authentication dialog as a part of the following string:                        "<appname>" is trying to <localized reason>.
--
-- For example, if the app name is "TestApp" and localizedReason is passed "access                        the hidden records", then the authentication prompt will read:                        "TestApp" is trying to access the hidden records.
--
-- Warning: localizedReason parameter is mandatory and the call will throw NSInvalidArgumentException if          nil or empty string is specified.
--
-- See: LAError
--
-- Typical error codes returned by this call are:
--
-- LAErrorUserFallback if user tapped the fallback button
--
-- LAErrorUserCancel if user has tapped the Cancel button
--
-- LAErrorSystemCancel if some system event interrupted the evaluation (e.g. Home button pressed).
--
-- ObjC selector: @- evaluatePolicy:localizedReason:reply:@
evaluatePolicy_localizedReason_reply :: (IsLAContext laContext, IsNSString localizedReason) => laContext -> LAPolicy -> localizedReason -> Ptr () -> IO ()
evaluatePolicy_localizedReason_reply laContext  policy localizedReason reply =
withObjCPtr localizedReason $ \raw_localizedReason ->
    sendMsg laContext (mkSelector "evaluatePolicy:localizedReason:reply:") retVoid [argCLong (coerce policy), argPtr (castPtr raw_localizedReason :: Ptr ()), argPtr (castPtr reply :: Ptr ())]

-- | Invalidates the context.
--
-- The context is invalidated automatically when it is (auto)released. This method             allows invalidating it manually while it is still in scope.
--
-- Invalidation terminates any existing policy evaluation and the respective call will             fail with LAErrorAppCancel. After the context has been invalidated, it can not be             used for policy evaluation and an attempt to do so will fail with LAErrorInvalidContext.
--
-- Invalidating a context that has been already invalidated has no effect.
--
-- ObjC selector: @- invalidate@
invalidate :: IsLAContext laContext => laContext -> IO ()
invalidate laContext  =
  sendMsg laContext (mkSelector "invalidate") retVoid []

-- | Sets a credential to this context.
--
-- Some policies allow to bind application-provided credential with them.             This method allows credential to be passed to the right context.
--
-- @credential@ — Credential to be used with subsequent calls. Setting this parameter to nil will remove                   any existing credential of the specified type.
--
-- @type@ — Type of the provided credential.
--
-- Returns: YES if the credential was set successfully, NO otherwise.
--
-- ObjC selector: @- setCredential:type:@
setCredential_type :: (IsLAContext laContext, IsNSData credential) => laContext -> credential -> LACredentialType -> IO Bool
setCredential_type laContext  credential type_ =
withObjCPtr credential $ \raw_credential ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg laContext (mkSelector "setCredential:type:") retCULong [argPtr (castPtr raw_credential :: Ptr ()), argCLong (coerce type_)]

-- | Reveals if credential was set with this context.
--
-- @type@ — Type of credential we are asking for.
--
-- Returns: YES on success, NO otherwise.
--
-- ObjC selector: @- isCredentialSet:@
isCredentialSet :: IsLAContext laContext => laContext -> LACredentialType -> IO Bool
isCredentialSet laContext  type_ =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg laContext (mkSelector "isCredentialSet:") retCULong [argCLong (coerce type_)]

-- | Evaluates access control object for the specified operation.
--
-- Access control evaluation may involve prompting user for various kinds of interaction             or authentication. Actual behavior is dependent on evaluated access control, device type,             and can be affected by installed configuration profiles.
--
-- Be sure to keep a strong reference to the context while the evaluation is in progress.             Otherwise, an evaluation would be canceled when the context is being deallocated.
--
-- The method does not block. Instead, the caller must provide a reply block to be             called asynchronously when evaluation finishes. The block is executed on a private             queue internal to the framework in an unspecified threading context. Other than that,             no guarantee is made about which queue, thread, or run-loop the block is executed on.
--
-- After successful access control evaluation, the LAContext can be used with keychain operations,             so that they do not require user to authenticate.
--
-- Access control evaluation may fail for various reasons, including user cancel, system cancel             and others, see LAError codes.
--
-- @accessControl@ — Access control object that is typically created by SecAccessControlCreateWithFlags.
--
-- @operation@ — Type of operation the access control will be used with.
--
-- @localizedReason@ — Application reason for authentication. This string must be provided in correct                        localization and should be short and clear. It will be eventually displayed in                        the authentication dialog as a part of the following string:                        "<appname>" is trying to <localized reason>.
--
-- For example, if the app name is "TestApp" and localizedReason is passed "access                        the hidden records", then the authentication prompt will read:                        "TestApp" is trying to access the hidden records.
--
-- @reply@ — Reply block that is executed when access control evaluation finishes.              success Reply parameter that is YES if the access control has been evaluated successfully or                      NO if the evaluation failed.              error Reply parameter that is nil if the access control has been evaluated successfully, or                    it contains error information about the evaluation failure.
--
-- Warning: localizedReason parameter is mandatory and the call will throw NSInvalidArgumentException if          nil or empty string is specified.
--
-- ObjC selector: @- evaluateAccessControl:operation:localizedReason:reply:@
evaluateAccessControl_operation_localizedReason_reply :: (IsLAContext laContext, IsNSString localizedReason) => laContext -> Ptr () -> LAAccessControlOperation -> localizedReason -> Ptr () -> IO ()
evaluateAccessControl_operation_localizedReason_reply laContext  accessControl operation localizedReason reply =
withObjCPtr localizedReason $ \raw_localizedReason ->
    sendMsg laContext (mkSelector "evaluateAccessControl:operation:localizedReason:reply:") retVoid [argPtr accessControl, argCLong (coerce operation), argPtr (castPtr raw_localizedReason :: Ptr ()), argPtr (castPtr reply :: Ptr ())]

-- | Fallback button title.
--
-- Allows fallback button title customization. If set to empty string, the button will be hidden.             A default title "Use Password…" is used when this property is left nil.
--
-- ObjC selector: @- localizedFallbackTitle@
localizedFallbackTitle :: IsLAContext laContext => laContext -> IO (Id NSString)
localizedFallbackTitle laContext  =
  sendMsg laContext (mkSelector "localizedFallbackTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Fallback button title.
--
-- Allows fallback button title customization. If set to empty string, the button will be hidden.             A default title "Use Password…" is used when this property is left nil.
--
-- ObjC selector: @- setLocalizedFallbackTitle:@
setLocalizedFallbackTitle :: (IsLAContext laContext, IsNSString value) => laContext -> value -> IO ()
setLocalizedFallbackTitle laContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg laContext (mkSelector "setLocalizedFallbackTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | This property is deprecated and setting it has no effect.
--
-- ObjC selector: @- maxBiometryFailures@
maxBiometryFailures :: IsLAContext laContext => laContext -> IO (Id NSNumber)
maxBiometryFailures laContext  =
  sendMsg laContext (mkSelector "maxBiometryFailures") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | This property is deprecated and setting it has no effect.
--
-- ObjC selector: @- setMaxBiometryFailures:@
setMaxBiometryFailures :: (IsLAContext laContext, IsNSNumber value) => laContext -> value -> IO ()
setMaxBiometryFailures laContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg laContext (mkSelector "setMaxBiometryFailures:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Cancel button title.
--
-- Allows cancel button title customization. A default title "Cancel" is used when             this property is left nil or is set to empty string.
--
-- ObjC selector: @- localizedCancelTitle@
localizedCancelTitle :: IsLAContext laContext => laContext -> IO (Id NSString)
localizedCancelTitle laContext  =
  sendMsg laContext (mkSelector "localizedCancelTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Cancel button title.
--
-- Allows cancel button title customization. A default title "Cancel" is used when             this property is left nil or is set to empty string.
--
-- ObjC selector: @- setLocalizedCancelTitle:@
setLocalizedCancelTitle :: (IsLAContext laContext, IsNSString value) => laContext -> value -> IO ()
setLocalizedCancelTitle laContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg laContext (mkSelector "setLocalizedCancelTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Time interval for accepting a successful Touch ID or Face ID device unlock (on the lock screen) from the past.
--
-- This property can be set with a time interval in seconds. If the device was successfully unlocked by             biometry within this time interval, then biometric authentication on this context will succeed             automatically and the reply block will be called without prompting user for Touch ID or Face ID.
--
-- The default value is 0, meaning that no previous biometric unlock can be reused.
--
-- This property is meant only for reusing biometric matches from the device lock screen.             It does not allow reusing previous biometric matches in application or between applications.
--
-- The maximum supported interval is 5 minutes and setting the value beyond 5 minutes does not increase             the accepted interval.
--
-- See: LATouchIDAuthenticationMaximumAllowableReuseDuration
--
-- ObjC selector: @- touchIDAuthenticationAllowableReuseDuration@
touchIDAuthenticationAllowableReuseDuration :: IsLAContext laContext => laContext -> IO CDouble
touchIDAuthenticationAllowableReuseDuration laContext  =
  sendMsg laContext (mkSelector "touchIDAuthenticationAllowableReuseDuration") retCDouble []

-- | Time interval for accepting a successful Touch ID or Face ID device unlock (on the lock screen) from the past.
--
-- This property can be set with a time interval in seconds. If the device was successfully unlocked by             biometry within this time interval, then biometric authentication on this context will succeed             automatically and the reply block will be called without prompting user for Touch ID or Face ID.
--
-- The default value is 0, meaning that no previous biometric unlock can be reused.
--
-- This property is meant only for reusing biometric matches from the device lock screen.             It does not allow reusing previous biometric matches in application or between applications.
--
-- The maximum supported interval is 5 minutes and setting the value beyond 5 minutes does not increase             the accepted interval.
--
-- See: LATouchIDAuthenticationMaximumAllowableReuseDuration
--
-- ObjC selector: @- setTouchIDAuthenticationAllowableReuseDuration:@
setTouchIDAuthenticationAllowableReuseDuration :: IsLAContext laContext => laContext -> CDouble -> IO ()
setTouchIDAuthenticationAllowableReuseDuration laContext  value =
  sendMsg laContext (mkSelector "setTouchIDAuthenticationAllowableReuseDuration:") retVoid [argCDouble (fromIntegral value)]

-- | Allows setting the default localized authentication reason on context.
--
-- A localized string from this property is displayed in the authentication UI if the caller didn't specify             its own authentication reason (e.g. a keychain operation with kSecUseAuthenticationContext). This property             is ignored if the authentication reason was provided by caller.
--
-- ObjC selector: @- localizedReason@
localizedReason :: IsLAContext laContext => laContext -> IO (Id NSString)
localizedReason laContext  =
  sendMsg laContext (mkSelector "localizedReason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Allows setting the default localized authentication reason on context.
--
-- A localized string from this property is displayed in the authentication UI if the caller didn't specify             its own authentication reason (e.g. a keychain operation with kSecUseAuthenticationContext). This property             is ignored if the authentication reason was provided by caller.
--
-- ObjC selector: @- setLocalizedReason:@
setLocalizedReason :: (IsLAContext laContext, IsNSString value) => laContext -> value -> IO ()
setLocalizedReason laContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg laContext (mkSelector "setLocalizedReason:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Allows running authentication in non-interactive mode.
--
-- If the context is used in a keychain query by the means of kSecUseAuthenticationContext,             then setting this property to YES has the same effect as passing kSecUseNoAuthenticationUI             in the query, i.e. the keychain call will eventually fail with errSecInteractionNotAllowed             instead of displaying the authentication UI.
--
-- If this property is used with a LocalAuthentication evaluation, it will eventually fail with             LAErrorNotInteractive instead of displaying the authentication UI.
--
-- ObjC selector: @- interactionNotAllowed@
interactionNotAllowed :: IsLAContext laContext => laContext -> IO Bool
interactionNotAllowed laContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg laContext (mkSelector "interactionNotAllowed") retCULong []

-- | Allows running authentication in non-interactive mode.
--
-- If the context is used in a keychain query by the means of kSecUseAuthenticationContext,             then setting this property to YES has the same effect as passing kSecUseNoAuthenticationUI             in the query, i.e. the keychain call will eventually fail with errSecInteractionNotAllowed             instead of displaying the authentication UI.
--
-- If this property is used with a LocalAuthentication evaluation, it will eventually fail with             LAErrorNotInteractive instead of displaying the authentication UI.
--
-- ObjC selector: @- setInteractionNotAllowed:@
setInteractionNotAllowed :: IsLAContext laContext => laContext -> Bool -> IO ()
setInteractionNotAllowed laContext  value =
  sendMsg laContext (mkSelector "setInteractionNotAllowed:") retVoid [argCULong (if value then 1 else 0)]

-- | Indicates the type of the biometry supported by the device.
--
-- ObjC selector: @- biometryType@
biometryType :: IsLAContext laContext => laContext -> IO LABiometryType
biometryType laContext  =
  fmap (coerce :: CLong -> LABiometryType) $ sendMsg laContext (mkSelector "biometryType") retCLong []

-- | Contains policy domain state.
--
-- This property is set only when evaluatePolicy is called and succesful Touch ID or Face ID authentication              was performed, or when canEvaluatePolicy succeeds for a biometric policy.              It stays nil for all other cases.              If biometric database was modified (fingers or faces were removed or added), evaluatedPolicyDomainState              data will change. Nature of such database changes cannot be determined              but comparing data of evaluatedPolicyDomainState after different evaluatePolicy              will reveal the fact database was changed between calls.
--
-- Warning: Please note that the value returned by this property can change exceptionally between major OS versions even if          the state of biometry has not changed.
--
-- ObjC selector: @- evaluatedPolicyDomainState@
evaluatedPolicyDomainState :: IsLAContext laContext => laContext -> IO (Id NSData)
evaluatedPolicyDomainState laContext  =
  sendMsg laContext (mkSelector "evaluatedPolicyDomainState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Contains authentication domain state.
--
-- ObjC selector: @- domainState@
domainState :: IsLAContext laContext => laContext -> IO (Id LADomainState)
domainState laContext  =
  sendMsg laContext (mkSelector "domainState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canEvaluatePolicy:error:@
canEvaluatePolicy_errorSelector :: Selector
canEvaluatePolicy_errorSelector = mkSelector "canEvaluatePolicy:error:"

-- | @Selector@ for @evaluatePolicy:localizedReason:reply:@
evaluatePolicy_localizedReason_replySelector :: Selector
evaluatePolicy_localizedReason_replySelector = mkSelector "evaluatePolicy:localizedReason:reply:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @setCredential:type:@
setCredential_typeSelector :: Selector
setCredential_typeSelector = mkSelector "setCredential:type:"

-- | @Selector@ for @isCredentialSet:@
isCredentialSetSelector :: Selector
isCredentialSetSelector = mkSelector "isCredentialSet:"

-- | @Selector@ for @evaluateAccessControl:operation:localizedReason:reply:@
evaluateAccessControl_operation_localizedReason_replySelector :: Selector
evaluateAccessControl_operation_localizedReason_replySelector = mkSelector "evaluateAccessControl:operation:localizedReason:reply:"

-- | @Selector@ for @localizedFallbackTitle@
localizedFallbackTitleSelector :: Selector
localizedFallbackTitleSelector = mkSelector "localizedFallbackTitle"

-- | @Selector@ for @setLocalizedFallbackTitle:@
setLocalizedFallbackTitleSelector :: Selector
setLocalizedFallbackTitleSelector = mkSelector "setLocalizedFallbackTitle:"

-- | @Selector@ for @maxBiometryFailures@
maxBiometryFailuresSelector :: Selector
maxBiometryFailuresSelector = mkSelector "maxBiometryFailures"

-- | @Selector@ for @setMaxBiometryFailures:@
setMaxBiometryFailuresSelector :: Selector
setMaxBiometryFailuresSelector = mkSelector "setMaxBiometryFailures:"

-- | @Selector@ for @localizedCancelTitle@
localizedCancelTitleSelector :: Selector
localizedCancelTitleSelector = mkSelector "localizedCancelTitle"

-- | @Selector@ for @setLocalizedCancelTitle:@
setLocalizedCancelTitleSelector :: Selector
setLocalizedCancelTitleSelector = mkSelector "setLocalizedCancelTitle:"

-- | @Selector@ for @touchIDAuthenticationAllowableReuseDuration@
touchIDAuthenticationAllowableReuseDurationSelector :: Selector
touchIDAuthenticationAllowableReuseDurationSelector = mkSelector "touchIDAuthenticationAllowableReuseDuration"

-- | @Selector@ for @setTouchIDAuthenticationAllowableReuseDuration:@
setTouchIDAuthenticationAllowableReuseDurationSelector :: Selector
setTouchIDAuthenticationAllowableReuseDurationSelector = mkSelector "setTouchIDAuthenticationAllowableReuseDuration:"

-- | @Selector@ for @localizedReason@
localizedReasonSelector :: Selector
localizedReasonSelector = mkSelector "localizedReason"

-- | @Selector@ for @setLocalizedReason:@
setLocalizedReasonSelector :: Selector
setLocalizedReasonSelector = mkSelector "setLocalizedReason:"

-- | @Selector@ for @interactionNotAllowed@
interactionNotAllowedSelector :: Selector
interactionNotAllowedSelector = mkSelector "interactionNotAllowed"

-- | @Selector@ for @setInteractionNotAllowed:@
setInteractionNotAllowedSelector :: Selector
setInteractionNotAllowedSelector = mkSelector "setInteractionNotAllowed:"

-- | @Selector@ for @biometryType@
biometryTypeSelector :: Selector
biometryTypeSelector = mkSelector "biometryType"

-- | @Selector@ for @evaluatedPolicyDomainState@
evaluatedPolicyDomainStateSelector :: Selector
evaluatedPolicyDomainStateSelector = mkSelector "evaluatedPolicyDomainState"

-- | @Selector@ for @domainState@
domainStateSelector :: Selector
domainStateSelector = mkSelector "domainState"

