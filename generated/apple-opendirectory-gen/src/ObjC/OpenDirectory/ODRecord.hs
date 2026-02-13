{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ODRecord
--
-- This class is used to read, update and modify records within the directory
--
-- This class is used to read, update and modify records within the directory.  outError is optional parameter,                 nil can be passed if error details are not needed.
--
-- Generated bindings for @ODRecord@.
module ObjC.OpenDirectory.ODRecord
  ( ODRecord
  , IsODRecord(..)
  , setNodeCredentials_password_error
  , setNodeCredentialsWithRecordType_authenticationType_authenticationItems_continueItems_context_error
  , setNodeCredentialsUsingKerberosCache_error
  , passwordPolicyAndReturnError
  , verifyPassword_error
  , verifyExtendedWithAuthenticationType_authenticationItems_continueItems_context_error
  , changePassword_toPassword_error
  , synchronizeAndReturnError
  , recordDetailsForAttributes_error
  , valuesForAttribute_error
  , setValue_forAttribute_error
  , removeValuesForAttribute_error
  , addValue_toAttribute_error
  , removeValue_fromAttribute_error
  , deleteRecordAndReturnError
  , policiesAndReturnError
  , effectivePoliciesAndReturnError
  , supportedPoliciesAndReturnError
  , setPolicies_error
  , setPolicy_value_error
  , removePolicy_error
  , addAccountPolicy_toCategory_error
  , removeAccountPolicy_fromCategory_error
  , setAccountPolicies_error
  , accountPoliciesAndReturnError
  , authenticationAllowedAndReturnError
  , passwordChangeAllowed_error
  , willPasswordExpire
  , willAuthenticationsExpire
  , addMemberRecord_error
  , removeMemberRecord_error
  , isMemberRecord_error
  , recordType
  , recordName
  , secondsUntilPasswordExpires
  , secondsUntilAuthenticationsExpire
  , accountPoliciesAndReturnErrorSelector
  , addAccountPolicy_toCategory_errorSelector
  , addMemberRecord_errorSelector
  , addValue_toAttribute_errorSelector
  , authenticationAllowedAndReturnErrorSelector
  , changePassword_toPassword_errorSelector
  , deleteRecordAndReturnErrorSelector
  , effectivePoliciesAndReturnErrorSelector
  , isMemberRecord_errorSelector
  , passwordChangeAllowed_errorSelector
  , passwordPolicyAndReturnErrorSelector
  , policiesAndReturnErrorSelector
  , recordDetailsForAttributes_errorSelector
  , recordNameSelector
  , recordTypeSelector
  , removeAccountPolicy_fromCategory_errorSelector
  , removeMemberRecord_errorSelector
  , removePolicy_errorSelector
  , removeValue_fromAttribute_errorSelector
  , removeValuesForAttribute_errorSelector
  , secondsUntilAuthenticationsExpireSelector
  , secondsUntilPasswordExpiresSelector
  , setAccountPolicies_errorSelector
  , setNodeCredentialsUsingKerberosCache_errorSelector
  , setNodeCredentialsWithRecordType_authenticationType_authenticationItems_continueItems_context_errorSelector
  , setNodeCredentials_password_errorSelector
  , setPolicies_errorSelector
  , setPolicy_value_errorSelector
  , setValue_forAttribute_errorSelector
  , supportedPoliciesAndReturnErrorSelector
  , synchronizeAndReturnErrorSelector
  , valuesForAttribute_errorSelector
  , verifyExtendedWithAuthenticationType_authenticationItems_continueItems_context_errorSelector
  , verifyPassword_errorSelector
  , willAuthenticationsExpireSelector
  , willPasswordExpireSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.OpenDirectory.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | setNodeCredentials:password:error:
--
-- Similar to calling -[ODNode setCredentials:] except credentials are only set for this particular                record's node
--
-- Sets the credentials if necessary on the ODNode referenced by this ODRecord.  Very similar to                calling -[ODNode setCredentials:] except other records referencing the underlying node will not get                authenticated, therefore inadvertant changes cannot occur.  If all records referencing a particular                 node need to be updated, then use -[ODNode setCredentials:] on the original node instead.  If the                node is already authenticated with the same name and password, it will be a NOOP call.  The original                ODNode held by an ODRecord will be released when the credentials are changed for the connection                associated with the record.  outError is optional parameter, nil can be passed if error details are not needed.
--
-- ObjC selector: @- setNodeCredentials:password:error:@
setNodeCredentials_password_error :: (IsODRecord odRecord, IsNSString inUsername, IsNSString inPassword, IsNSError outError) => odRecord -> inUsername -> inPassword -> outError -> IO Bool
setNodeCredentials_password_error odRecord inUsername inPassword outError =
  sendMessage odRecord setNodeCredentials_password_errorSelector (toNSString inUsername) (toNSString inPassword) (toNSError outError)

-- | setNodeCredentialsWithRecordType:authenticationType:authenticationItems:continueItems:context:error:
--
-- Similar to calling -[ODNode setCredentialsWithRecordType:] except credentials are only set for this particular record's                node
--
-- Allows the caller to use other types of authentications that are available in OpenDirectory, that may                require response-request loops, etc.  Not all OD plugins will support this call, look for                 kODErrorCredentialsMethodNotSupported in outError.  Same behavior as ODRecordSetNodeCredentials.  outError 				is optional parameter, nil can be passed if error details are not needed.
--
-- ObjC selector: @- setNodeCredentialsWithRecordType:authenticationType:authenticationItems:continueItems:context:error:@
setNodeCredentialsWithRecordType_authenticationType_authenticationItems_continueItems_context_error :: (IsODRecord odRecord, IsNSString inRecordType, IsNSString inType, IsNSArray inItems, IsNSArray outItems, IsNSError outError) => odRecord -> inRecordType -> inType -> inItems -> outItems -> Ptr (Maybe RawId) -> outError -> IO Bool
setNodeCredentialsWithRecordType_authenticationType_authenticationItems_continueItems_context_error odRecord inRecordType inType inItems outItems outContext outError =
  sendMessage odRecord setNodeCredentialsWithRecordType_authenticationType_authenticationItems_continueItems_context_errorSelector (toNSString inRecordType) (toNSString inType) (toNSArray inItems) (toNSArray outItems) outContext (toNSError outError)

-- | setNodeCredentialsUsingKerberosCache:error:
--
-- Unsupported method.
--
-- Unsupported method.
--
-- ObjC selector: @- setNodeCredentialsUsingKerberosCache:error:@
setNodeCredentialsUsingKerberosCache_error :: (IsODRecord odRecord, IsNSString inCacheName, IsNSError outError) => odRecord -> inCacheName -> outError -> IO Bool
setNodeCredentialsUsingKerberosCache_error odRecord inCacheName outError =
  sendMessage odRecord setNodeCredentialsUsingKerberosCache_errorSelector (toNSString inCacheName) (toNSError outError)

-- | passwordPolicyAndReturnError:
--
-- Returns a dictionary containing the password policy for the record if available.
--
-- Returns a dictionary containing the password policy for the record if available.  If no policy for record                nil will be returned.  outError is optional parameter, nil can be passed if error details are not needed.
--
-- ObjC selector: @- passwordPolicyAndReturnError:@
passwordPolicyAndReturnError :: (IsODRecord odRecord, IsNSError outError) => odRecord -> outError -> IO (Id NSDictionary)
passwordPolicyAndReturnError odRecord outError =
  sendMessage odRecord passwordPolicyAndReturnErrorSelector (toNSError outError)

-- | verifyPassword:error:
--
-- Verifies the password provided is valid for the record
--
-- Verifies the password provided is valid for the record.  outError is optional parameter, nil can be passed if                 error details are not needed.
--
-- ObjC selector: @- verifyPassword:error:@
verifyPassword_error :: (IsODRecord odRecord, IsNSString inPassword, IsNSError outError) => odRecord -> inPassword -> outError -> IO Bool
verifyPassword_error odRecord inPassword outError =
  sendMessage odRecord verifyPassword_errorSelector (toNSString inPassword) (toNSError outError)

-- | verifyExtendedWithAuthenticationType:authenticationItems:continueItems:context:error:
--
-- Allows use of other OpenDirectory types of authentications
--
-- Allows the caller to use other types of authentications that are available in OpenDirectory, that may                 require response-request loops, etc.  A bool with the result of the operation.                  If it fails, outError can be checked for more specific error.  Some ODNodes may not support the call                so an error code of kODErrorCredentialsMethodNotSupported may be returned.  outError is optional                 parameter, nil can be passed if error details are not needed.
--
-- ObjC selector: @- verifyExtendedWithAuthenticationType:authenticationItems:continueItems:context:error:@
verifyExtendedWithAuthenticationType_authenticationItems_continueItems_context_error :: (IsODRecord odRecord, IsNSString inType, IsNSArray inItems, IsNSArray outItems, IsNSError outError) => odRecord -> inType -> inItems -> outItems -> Ptr (Maybe RawId) -> outError -> IO Bool
verifyExtendedWithAuthenticationType_authenticationItems_continueItems_context_error odRecord inType inItems outItems outContext outError =
  sendMessage odRecord verifyExtendedWithAuthenticationType_authenticationItems_continueItems_context_errorSelector (toNSString inType) (toNSArray inItems) (toNSArray outItems) outContext (toNSError outError)

-- | changePassword:toPassword:error:
--
-- Changes the password for a record
--
-- Changes the password for a record.  The oldPassword can be nil if password is being set assuming the appropriate                privileges are in place.  outError is optional parameter, nil can be passed if error details are not needed.
--
-- ObjC selector: @- changePassword:toPassword:error:@
changePassword_toPassword_error :: (IsODRecord odRecord, IsNSString oldPassword, IsNSString newPassword, IsNSError outError) => odRecord -> oldPassword -> newPassword -> outError -> IO Bool
changePassword_toPassword_error odRecord oldPassword newPassword outError =
  sendMessage odRecord changePassword_toPassword_errorSelector (toNSString oldPassword) (toNSString newPassword) (toNSError outError)

-- | synchronizeAndReturnError:
--
-- Synchronizes the record from the Directory in order to get current data and/or commit pending changes
--
-- Synchronizes the record from the Directory in order to get current data.  Any previously fetched attributes                will be re-fetch from the Directory.  This will not re-fetch the entire record, unless the entire record                has been accessed.  Additionally, any changes made to the record will be committed to the directory,                if the node does not do immediate commits.  outError is optional parameter, nil can be passed if error details                are not needed.
--
-- ObjC selector: @- synchronizeAndReturnError:@
synchronizeAndReturnError :: (IsODRecord odRecord, IsNSError outError) => odRecord -> outError -> IO Bool
synchronizeAndReturnError odRecord outError =
  sendMessage odRecord synchronizeAndReturnErrorSelector (toNSError outError)

-- | recordDetailsForAttributes:error:
--
-- Returns the attributes and values in the form of a key-value pair set.
--
-- Returns the attributes and values in the form of a key-value pair set for this record.  The key is a                 NSString of the attribute name (e.g., kODAttributeTypeRecordName, etc.) and the value is an NSArray                of either NSData or NSString depending on the type of data.  Binary data will be returned as NSData.                If nil is passed, then all currently retrieved attributes will be returned.  outError is optional parameter,                 nil can be passed if error details are not needed.
--
-- ObjC selector: @- recordDetailsForAttributes:error:@
recordDetailsForAttributes_error :: (IsODRecord odRecord, IsNSArray inAttributes, IsNSError outError) => odRecord -> inAttributes -> outError -> IO (Id NSDictionary)
recordDetailsForAttributes_error odRecord inAttributes outError =
  sendMessage odRecord recordDetailsForAttributes_errorSelector (toNSArray inAttributes) (toNSError outError)

-- | valuesForAttribute:error:
--
-- Returns an NSArray of NSString or NSData values of the attribute
--
-- Returns an NSArray of NSString or NSData depending on the type of data.  Binary data will be                 returned as NSData.  outError is optional parameter, nil can be passed if error details are not needed.
--
-- ObjC selector: @- valuesForAttribute:error:@
valuesForAttribute_error :: (IsODRecord odRecord, IsNSString inAttribute, IsNSError outError) => odRecord -> inAttribute -> outError -> IO (Id NSArray)
valuesForAttribute_error odRecord inAttribute outError =
  sendMessage odRecord valuesForAttribute_errorSelector (toNSString inAttribute) (toNSError outError)

-- | setValue:forAttribute:error:
--
-- Will take a mixture of NSData or NSString or an NSArray of either type when setting the values of an attribute
--
-- Will take a mixture of NSData or NSString or an NSArray of either type when setting the values of an attribute.				outError is optional parameter, nil can be passed if error details are not needed.
--
-- ObjC selector: @- setValue:forAttribute:error:@
setValue_forAttribute_error :: (IsODRecord odRecord, IsNSString inAttribute, IsNSError outError) => odRecord -> RawId -> inAttribute -> outError -> IO Bool
setValue_forAttribute_error odRecord inValueOrValues inAttribute outError =
  sendMessage odRecord setValue_forAttribute_errorSelector inValueOrValues (toNSString inAttribute) (toNSError outError)

-- | removeValuesForAttribute:error:
--
-- Removes all the values for an attribute.
--
-- Removes all the values for an attribute.  outError is optional parameter, nil can be passed if                 error details are not needed.
--
-- ObjC selector: @- removeValuesForAttribute:error:@
removeValuesForAttribute_error :: (IsODRecord odRecord, IsNSString inAttribute, IsNSError outError) => odRecord -> inAttribute -> outError -> IO Bool
removeValuesForAttribute_error odRecord inAttribute outError =
  sendMessage odRecord removeValuesForAttribute_errorSelector (toNSString inAttribute) (toNSError outError)

-- | addValue:toAttribute:error:
--
-- Will add a value to an attribute
--
-- Will add a value to an attribute.  Should be either NSData or NSString type.  outError is optional                 parameter, nil can be passed if error details are not needed.
--
-- ObjC selector: @- addValue:toAttribute:error:@
addValue_toAttribute_error :: (IsODRecord odRecord, IsNSString inAttribute, IsNSError outError) => odRecord -> RawId -> inAttribute -> outError -> IO Bool
addValue_toAttribute_error odRecord inValue inAttribute outError =
  sendMessage odRecord addValue_toAttribute_errorSelector inValue (toNSString inAttribute) (toNSError outError)

-- | removeValue:fromAttribute:error:
--
-- Will remove a value from an attribute
--
-- Will remove a value from an attribute.  Should be either NSData or NSString type.  outError is optional                 parameter, nil can be passed if error details are not needed.
--
-- ObjC selector: @- removeValue:fromAttribute:error:@
removeValue_fromAttribute_error :: (IsODRecord odRecord, IsNSString inAttribute, IsNSError outError) => odRecord -> RawId -> inAttribute -> outError -> IO Bool
removeValue_fromAttribute_error odRecord inValue inAttribute outError =
  sendMessage odRecord removeValue_fromAttribute_errorSelector inValue (toNSString inAttribute) (toNSError outError)

-- | deleteRecordAndReturnError:
--
-- Deletes the record from the node and invalidates the record.
--
-- Deletes the record from the node and invalidates the record.  The ODRecord should be                released after deletion.  outError is optional parameter, nil can be passed if error details are not needed.
--
-- ObjC selector: @- deleteRecordAndReturnError:@
deleteRecordAndReturnError :: (IsODRecord odRecord, IsNSError outError) => odRecord -> outError -> IO Bool
deleteRecordAndReturnError odRecord outError =
  sendMessage odRecord deleteRecordAndReturnErrorSelector (toNSError outError)

-- | policiesAndReturnError:
--
-- This will copy any policies configured for the record.
--
-- This will copy any policies configured for the record.
--
-- ObjC selector: @- policiesAndReturnError:@
policiesAndReturnError :: (IsODRecord odRecord, IsNSError error_) => odRecord -> error_ -> IO (Id NSDictionary)
policiesAndReturnError odRecord error_ =
  sendMessage odRecord policiesAndReturnErrorSelector (toNSError error_)

-- | effectivePoliciesAndReturnError:
--
-- This will copy any policies configured for the record.
--
-- This will copy any policies configured for the record.
--
-- ObjC selector: @- effectivePoliciesAndReturnError:@
effectivePoliciesAndReturnError :: (IsODRecord odRecord, IsNSError error_) => odRecord -> error_ -> IO (Id NSDictionary)
effectivePoliciesAndReturnError odRecord error_ =
  sendMessage odRecord effectivePoliciesAndReturnErrorSelector (toNSError error_)

-- | supportedPoliciesAndReturnError:
--
-- This will return a dictionary of supported policies.
--
-- This will return a dictionary of supported policies, if appropriate, the value will be the maximum value allowed                for the policy in question.  For example, if password history is available, it will state how much history is                supported.
--
-- ObjC selector: @- supportedPoliciesAndReturnError:@
supportedPoliciesAndReturnError :: (IsODRecord odRecord, IsNSError error_) => odRecord -> error_ -> IO (Id NSDictionary)
supportedPoliciesAndReturnError odRecord error_ =
  sendMessage odRecord supportedPoliciesAndReturnErrorSelector (toNSError error_)

-- | setPolicies:error:
--
-- This will set the policy for the record.
--
-- This will set the policy for the record.  Policies are evaluated in combination with node-level policies.
--
-- ObjC selector: @- setPolicies:error:@
setPolicies_error :: (IsODRecord odRecord, IsNSDictionary policies, IsNSError error_) => odRecord -> policies -> error_ -> IO Bool
setPolicies_error odRecord policies error_ =
  sendMessage odRecord setPolicies_errorSelector (toNSDictionary policies) (toNSError error_)

-- | setPolicy:value:error:
--
-- This will set a specific policy setting for the record.
--
-- This will set a specific policy setting for the record.
--
-- ObjC selector: @- setPolicy:value:error:@
setPolicy_value_error :: (IsODRecord odRecord, IsNSString policy, IsNSError error_) => odRecord -> policy -> RawId -> error_ -> IO Bool
setPolicy_value_error odRecord policy value error_ =
  sendMessage odRecord setPolicy_value_errorSelector (toNSString policy) value (toNSError error_)

-- | removePolicy:error:
--
-- This will remove a specific policy setting from the record.
--
-- This will remove a specific policy setting from the record.
--
-- ObjC selector: @- removePolicy:error:@
removePolicy_error :: (IsODRecord odRecord, IsNSString policy, IsNSError error_) => odRecord -> policy -> error_ -> IO Bool
removePolicy_error odRecord policy error_ =
  sendMessage odRecord removePolicy_errorSelector (toNSString policy) (toNSError error_)

-- | addAccountPolicy:toCategory:error:
--
-- This will add a specific policy to the specific category for the record.
--
-- This will add a specific policy to the specific category for the record.                The specified policy will be applied, in combination with any                node policies, to the specified record when policies are evaluated.
--
-- @policy@ — a dictionary containing the specific policy to be added.                The dictionary may contain the following keys:                    kODPolicyKeyIdentifier a required key identifying the policy.                    kODPolicyKeyParameters an optional key containing a dictionary of                        parameters that can be used for informational purposes or in                        the policy format string.                    kODPolicyKeyContent a required key specifying the policy,                        from which a predicate will be created for evaluating                        the policy.
--
-- @category@ — a valid ODPolicyCategoryType to which the policy will be added.
--
-- @error@ — an optional NSError reference for error details.
--
-- Returns: a BOOL which signifies if the policy addition succeeded, otherwise error is set.
--
-- ObjC selector: @- addAccountPolicy:toCategory:error:@
addAccountPolicy_toCategory_error :: (IsODRecord odRecord, IsNSDictionary policy, IsNSString category, IsNSError error_) => odRecord -> policy -> category -> error_ -> IO Bool
addAccountPolicy_toCategory_error odRecord policy category error_ =
  sendMessage odRecord addAccountPolicy_toCategory_errorSelector (toNSDictionary policy) (toNSString category) (toNSError error_)

-- | removeAccountPolicy:fromCategory:error:
--
-- This will remove a specific policy from the specific category for the record.
--
-- This will remove a specific policy from the specific category for the record.
--
-- @policy@ — a dictionary containing the specific policy to be                removed, with the same format as described in addAccountPolicy.
--
-- @category@ — a valid ODPolicyCategoryType from which the policy will be removed.
--
-- @error@ — an optional NSError reference for error details.
--
-- Returns: a BOOL which signifies if the policy removal succeeded, otherwise error is set.
--
-- ObjC selector: @- removeAccountPolicy:fromCategory:error:@
removeAccountPolicy_fromCategory_error :: (IsODRecord odRecord, IsNSDictionary policy, IsNSString category, IsNSError error_) => odRecord -> policy -> category -> error_ -> IO Bool
removeAccountPolicy_fromCategory_error odRecord policy category error_ =
  sendMessage odRecord removeAccountPolicy_fromCategory_errorSelector (toNSDictionary policy) (toNSString category) (toNSError error_)

-- | setAccountPolicies:error:
--
-- This will set the policies for the record.
--
-- This will set the policies for the record, replacing any                existing policies.  All of the policies in the set will be                applied to the record when policies are evaluated.
--
-- @policies@ — a dictionary containing all of the policies to be set                for the node.  The dictionary may contain the following keys:                    kODPolicyCategoryAuthentication an optional key with a value                        of an array of policy dictionaries that specify when                        authentications should be allowed.                    kODPolicyCategoryPasswordContent an optional key with a                        value of an array of policy dictionaries the specify the                        required content of passwords.                     kODPolicyCategoryPasswordChange an optional key with a value                        of an array of policy dictionaries that specify when                        passwords are required to be changed.
--
-- @error@ — an optional NSError reference for error details.
--
-- Returns: a BOOL which signifies if the policy set succeeded, otherwise error is set.
--
-- ObjC selector: @- setAccountPolicies:error:@
setAccountPolicies_error :: (IsODRecord odRecord, IsNSDictionary policies, IsNSError error_) => odRecord -> policies -> error_ -> IO Bool
setAccountPolicies_error odRecord policies error_ =
  sendMessage odRecord setAccountPolicies_errorSelector (toNSDictionary policies) (toNSError error_)

-- | accountPoliciesAndReturnError:
--
-- Returns a dictionary containing any policies configured for the record.
--
-- Returns a dictionary containing any policies configured for the record.                Does not include any policies set for the node.
--
-- Returns a dictionary containing any policies configured for the record.
--
-- @error@ — an optional NSError reference for error details.
--
-- Returns: a NSDictionary containing all currently set policies.  The                format of the dictionary is the same as described in                setAccountPolicies.
--
-- ObjC selector: @- accountPoliciesAndReturnError:@
accountPoliciesAndReturnError :: (IsODRecord odRecord, IsNSError error_) => odRecord -> error_ -> IO (Id NSDictionary)
accountPoliciesAndReturnError odRecord error_ =
  sendMessage odRecord accountPoliciesAndReturnErrorSelector (toNSError error_)

-- | authenticationAllowedAndReturnError:
--
-- Determines if policies allow the account to authenticate.
--
-- Determines if policies allow the account to authenticate.                Authentication and password change policies are evaluated.                Record-level and node-level policies are evaluated in                combination, with record-level taking precedence over node-level                policies.  The failure of any single policy will deny the                authentication.
--
-- This check is only definitive at the time it was requested. The                policy or the environment could change before the authentication                is actually requested.  Errors from the authentication request                should be consulted.
--
-- It is not necessary to call this function when calling                verifyPassword or verifyPasswordExtended since those methods                perform the same policy evaluation.
--
-- @error@ — an optional NSError reference for error details.
--
-- Returns: a bool which signifies if the authentication is allowed, otherwise error is set.
--
-- ObjC selector: @- authenticationAllowedAndReturnError:@
authenticationAllowedAndReturnError :: (IsODRecord odRecord, IsNSError error_) => odRecord -> error_ -> IO Bool
authenticationAllowedAndReturnError odRecord error_ =
  sendMessage odRecord authenticationAllowedAndReturnErrorSelector (toNSError error_)

-- | passwordChangeAllowed:error:
--
-- Determines if policies allow the password change.
--
-- Determines if policies allow the password change.  Password                content policies are evaluated. Record-level and node-level                policies are evaluated in combination, with record-level taking                precedence over node-level policies.  The failure of any single                policy will deny the password change.
--
-- This check is only definitive at the time it was requested. The                policy or the environment could change before the password change                is actually requested.  Errors from the password change request                should be consulted.
--
-- @newPassword@ — contains the password to be evaluated.
--
-- @error@ — an optional NSError reference for error details.
--
-- Returns: a BOOL which signifies if the password change is allowed, otherwise error is set.
--
-- ObjC selector: @- passwordChangeAllowed:error:@
passwordChangeAllowed_error :: (IsODRecord odRecord, IsNSString newPassword, IsNSError error_) => odRecord -> newPassword -> error_ -> IO Bool
passwordChangeAllowed_error odRecord newPassword error_ =
  sendMessage odRecord passwordChangeAllowed_errorSelector (toNSString newPassword) (toNSError error_)

-- | willPasswordExpire:
--
-- Determines if the password will expire within the specified time.
--
-- Determines if the password will expire (i.e. need to be changed)                between now and the specified number of seconds in the future.                 Password change policies are evaluated.  Record-level and                node-level policies are evaluated in combination, with                record-level taking precedence over node-level policies.
--
-- @willExpireIn@ — the number of seconds from the current time to be                used as the upper-bound for the password expiration period.
--
-- Returns: a BOOL which signifies if the password will expire within the                specified time.
--
-- ObjC selector: @- willPasswordExpire:@
willPasswordExpire :: IsODRecord odRecord => odRecord -> CULong -> IO Bool
willPasswordExpire odRecord willExpireIn =
  sendMessage odRecord willPasswordExpireSelector willExpireIn

-- | willAuthenticationsExpire:
--
-- Determines if authentications will expire within the specified time.
--
-- Determines if authentications will expire (i.e. session and/or                account expires) between now and the specified number of seconds                in the future.  Authentication policies are evaluated.                Record-level and node-level policies are evaluated in                combination, with record-level taking precedence over node-level                policies.
--
-- @willExpireIn@ — the number of seconds from the current time to be                used as the upper-bound for the authentication expiration period.
--
-- Returns: a BOOL which signifies if authentications will expire within the                specified time.
--
-- ObjC selector: @- willAuthenticationsExpire:@
willAuthenticationsExpire :: IsODRecord odRecord => odRecord -> CULong -> IO Bool
willAuthenticationsExpire odRecord willExpireIn =
  sendMessage odRecord willAuthenticationsExpireSelector willExpireIn

-- | addMemberRecord:error:
--
-- Will add the record as a member of the group record
--
-- Will add the record as a member of the group record.  An error will be returned if the record is not                a group record.  Additionally, if the member record is not an appropriate type allowed as part of a group                an error will be returned.  outError is optional parameter, nil can be passed if error details are not needed.
--
-- ObjC selector: @- addMemberRecord:error:@
addMemberRecord_error :: (IsODRecord odRecord, IsODRecord inRecord, IsNSError outError) => odRecord -> inRecord -> outError -> IO Bool
addMemberRecord_error odRecord inRecord outError =
  sendMessage odRecord addMemberRecord_errorSelector (toODRecord inRecord) (toNSError outError)

-- | removeMemberRecord:error:
--
-- Will remove the record as a member from the group record
--
-- Will remove the record as a member from the group record. An error will be returned if the record is not                a group record.  Additionally, if the member record is not an appropriate type allowed as part of a group                an error will be returned.  outError is optional parameter, nil can be passed if error details are not needed.
--
-- ObjC selector: @- removeMemberRecord:error:@
removeMemberRecord_error :: (IsODRecord odRecord, IsODRecord inRecord, IsNSError outError) => odRecord -> inRecord -> outError -> IO Bool
removeMemberRecord_error odRecord inRecord outError =
  sendMessage odRecord removeMemberRecord_errorSelector (toODRecord inRecord) (toNSError outError)

-- | isMemberRecord:error:
--
-- Will use membership APIs to determine if inRecord is a member of the group
--
-- Will use membership APIs to determine if inRecord is a member of the group.  If the receiving                object is not a group then NO will still be returned.  outError is optional parameter, nil can be passed if                 error details are not needed.
--
-- ObjC selector: @- isMemberRecord:error:@
isMemberRecord_error :: (IsODRecord odRecord, IsODRecord inRecord, IsNSError outError) => odRecord -> inRecord -> outError -> IO Bool
isMemberRecord_error odRecord inRecord outError =
  sendMessage odRecord isMemberRecord_errorSelector (toODRecord inRecord) (toNSError outError)

-- | recordType
--
-- Type of the record.
--
-- The record type.
--
-- ObjC selector: @- recordType@
recordType :: IsODRecord odRecord => odRecord -> IO (Id NSString)
recordType odRecord =
  sendMessage odRecord recordTypeSelector

-- | recordName
--
-- Name of the record.
--
-- This is the official record name.
--
-- ObjC selector: @- recordName@
recordName :: IsODRecord odRecord => odRecord -> IO (Id NSString)
recordName odRecord =
  sendMessage odRecord recordNameSelector

-- | secondsUntilPasswordExpires
--
-- Determines how many seconds until the password expires.
--
-- Determines how many seconds until the password expires (i.e.                needs changing).  Password change policies are evaluated.                Record-level and node-level policies are evaluated in                combination, with record-level taking precedence over node-level                policies.
--
-- Returns: the number of seconds until the password expires.  If multiple                policies will cause the password to expire, the soonest                expiration time is returned.  If already expired,                kODExpirationTimeExpired is returned.  If there are no password                change policies, kODExpirationTimeNeverExpires is returned.
--
-- ObjC selector: @- secondsUntilPasswordExpires@
secondsUntilPasswordExpires :: IsODRecord odRecord => odRecord -> IO CLong
secondsUntilPasswordExpires odRecord =
  sendMessage odRecord secondsUntilPasswordExpiresSelector

-- | secondsUntilAuthenticationsExpire
--
-- Determines how many seconds until authentications expire.
--
-- Determines how many seconds until authentications expire (i.e.                session and/or account expires). Authentication policies are                evaluated.   Record-level and node-level policies are evaluated                in combination, with record-level taking precedence over                node-level policies.
--
-- Returns: the number of seconds until authentications expire.  If multiple                policies will cause authentications to expire, the soonest                expiration time is returned. If already expired,                kODExpirationTimeExpired is returned.  If there are no                authentication policies controlling expiration,                kODExpirationTimeNeverExpires is returned.
--
-- ObjC selector: @- secondsUntilAuthenticationsExpire@
secondsUntilAuthenticationsExpire :: IsODRecord odRecord => odRecord -> IO CLong
secondsUntilAuthenticationsExpire odRecord =
  sendMessage odRecord secondsUntilAuthenticationsExpireSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setNodeCredentials:password:error:@
setNodeCredentials_password_errorSelector :: Selector '[Id NSString, Id NSString, Id NSError] Bool
setNodeCredentials_password_errorSelector = mkSelector "setNodeCredentials:password:error:"

-- | @Selector@ for @setNodeCredentialsWithRecordType:authenticationType:authenticationItems:continueItems:context:error:@
setNodeCredentialsWithRecordType_authenticationType_authenticationItems_continueItems_context_errorSelector :: Selector '[Id NSString, Id NSString, Id NSArray, Id NSArray, Ptr (Maybe RawId), Id NSError] Bool
setNodeCredentialsWithRecordType_authenticationType_authenticationItems_continueItems_context_errorSelector = mkSelector "setNodeCredentialsWithRecordType:authenticationType:authenticationItems:continueItems:context:error:"

-- | @Selector@ for @setNodeCredentialsUsingKerberosCache:error:@
setNodeCredentialsUsingKerberosCache_errorSelector :: Selector '[Id NSString, Id NSError] Bool
setNodeCredentialsUsingKerberosCache_errorSelector = mkSelector "setNodeCredentialsUsingKerberosCache:error:"

-- | @Selector@ for @passwordPolicyAndReturnError:@
passwordPolicyAndReturnErrorSelector :: Selector '[Id NSError] (Id NSDictionary)
passwordPolicyAndReturnErrorSelector = mkSelector "passwordPolicyAndReturnError:"

-- | @Selector@ for @verifyPassword:error:@
verifyPassword_errorSelector :: Selector '[Id NSString, Id NSError] Bool
verifyPassword_errorSelector = mkSelector "verifyPassword:error:"

-- | @Selector@ for @verifyExtendedWithAuthenticationType:authenticationItems:continueItems:context:error:@
verifyExtendedWithAuthenticationType_authenticationItems_continueItems_context_errorSelector :: Selector '[Id NSString, Id NSArray, Id NSArray, Ptr (Maybe RawId), Id NSError] Bool
verifyExtendedWithAuthenticationType_authenticationItems_continueItems_context_errorSelector = mkSelector "verifyExtendedWithAuthenticationType:authenticationItems:continueItems:context:error:"

-- | @Selector@ for @changePassword:toPassword:error:@
changePassword_toPassword_errorSelector :: Selector '[Id NSString, Id NSString, Id NSError] Bool
changePassword_toPassword_errorSelector = mkSelector "changePassword:toPassword:error:"

-- | @Selector@ for @synchronizeAndReturnError:@
synchronizeAndReturnErrorSelector :: Selector '[Id NSError] Bool
synchronizeAndReturnErrorSelector = mkSelector "synchronizeAndReturnError:"

-- | @Selector@ for @recordDetailsForAttributes:error:@
recordDetailsForAttributes_errorSelector :: Selector '[Id NSArray, Id NSError] (Id NSDictionary)
recordDetailsForAttributes_errorSelector = mkSelector "recordDetailsForAttributes:error:"

-- | @Selector@ for @valuesForAttribute:error:@
valuesForAttribute_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSArray)
valuesForAttribute_errorSelector = mkSelector "valuesForAttribute:error:"

-- | @Selector@ for @setValue:forAttribute:error:@
setValue_forAttribute_errorSelector :: Selector '[RawId, Id NSString, Id NSError] Bool
setValue_forAttribute_errorSelector = mkSelector "setValue:forAttribute:error:"

-- | @Selector@ for @removeValuesForAttribute:error:@
removeValuesForAttribute_errorSelector :: Selector '[Id NSString, Id NSError] Bool
removeValuesForAttribute_errorSelector = mkSelector "removeValuesForAttribute:error:"

-- | @Selector@ for @addValue:toAttribute:error:@
addValue_toAttribute_errorSelector :: Selector '[RawId, Id NSString, Id NSError] Bool
addValue_toAttribute_errorSelector = mkSelector "addValue:toAttribute:error:"

-- | @Selector@ for @removeValue:fromAttribute:error:@
removeValue_fromAttribute_errorSelector :: Selector '[RawId, Id NSString, Id NSError] Bool
removeValue_fromAttribute_errorSelector = mkSelector "removeValue:fromAttribute:error:"

-- | @Selector@ for @deleteRecordAndReturnError:@
deleteRecordAndReturnErrorSelector :: Selector '[Id NSError] Bool
deleteRecordAndReturnErrorSelector = mkSelector "deleteRecordAndReturnError:"

-- | @Selector@ for @policiesAndReturnError:@
policiesAndReturnErrorSelector :: Selector '[Id NSError] (Id NSDictionary)
policiesAndReturnErrorSelector = mkSelector "policiesAndReturnError:"

-- | @Selector@ for @effectivePoliciesAndReturnError:@
effectivePoliciesAndReturnErrorSelector :: Selector '[Id NSError] (Id NSDictionary)
effectivePoliciesAndReturnErrorSelector = mkSelector "effectivePoliciesAndReturnError:"

-- | @Selector@ for @supportedPoliciesAndReturnError:@
supportedPoliciesAndReturnErrorSelector :: Selector '[Id NSError] (Id NSDictionary)
supportedPoliciesAndReturnErrorSelector = mkSelector "supportedPoliciesAndReturnError:"

-- | @Selector@ for @setPolicies:error:@
setPolicies_errorSelector :: Selector '[Id NSDictionary, Id NSError] Bool
setPolicies_errorSelector = mkSelector "setPolicies:error:"

-- | @Selector@ for @setPolicy:value:error:@
setPolicy_value_errorSelector :: Selector '[Id NSString, RawId, Id NSError] Bool
setPolicy_value_errorSelector = mkSelector "setPolicy:value:error:"

-- | @Selector@ for @removePolicy:error:@
removePolicy_errorSelector :: Selector '[Id NSString, Id NSError] Bool
removePolicy_errorSelector = mkSelector "removePolicy:error:"

-- | @Selector@ for @addAccountPolicy:toCategory:error:@
addAccountPolicy_toCategory_errorSelector :: Selector '[Id NSDictionary, Id NSString, Id NSError] Bool
addAccountPolicy_toCategory_errorSelector = mkSelector "addAccountPolicy:toCategory:error:"

-- | @Selector@ for @removeAccountPolicy:fromCategory:error:@
removeAccountPolicy_fromCategory_errorSelector :: Selector '[Id NSDictionary, Id NSString, Id NSError] Bool
removeAccountPolicy_fromCategory_errorSelector = mkSelector "removeAccountPolicy:fromCategory:error:"

-- | @Selector@ for @setAccountPolicies:error:@
setAccountPolicies_errorSelector :: Selector '[Id NSDictionary, Id NSError] Bool
setAccountPolicies_errorSelector = mkSelector "setAccountPolicies:error:"

-- | @Selector@ for @accountPoliciesAndReturnError:@
accountPoliciesAndReturnErrorSelector :: Selector '[Id NSError] (Id NSDictionary)
accountPoliciesAndReturnErrorSelector = mkSelector "accountPoliciesAndReturnError:"

-- | @Selector@ for @authenticationAllowedAndReturnError:@
authenticationAllowedAndReturnErrorSelector :: Selector '[Id NSError] Bool
authenticationAllowedAndReturnErrorSelector = mkSelector "authenticationAllowedAndReturnError:"

-- | @Selector@ for @passwordChangeAllowed:error:@
passwordChangeAllowed_errorSelector :: Selector '[Id NSString, Id NSError] Bool
passwordChangeAllowed_errorSelector = mkSelector "passwordChangeAllowed:error:"

-- | @Selector@ for @willPasswordExpire:@
willPasswordExpireSelector :: Selector '[CULong] Bool
willPasswordExpireSelector = mkSelector "willPasswordExpire:"

-- | @Selector@ for @willAuthenticationsExpire:@
willAuthenticationsExpireSelector :: Selector '[CULong] Bool
willAuthenticationsExpireSelector = mkSelector "willAuthenticationsExpire:"

-- | @Selector@ for @addMemberRecord:error:@
addMemberRecord_errorSelector :: Selector '[Id ODRecord, Id NSError] Bool
addMemberRecord_errorSelector = mkSelector "addMemberRecord:error:"

-- | @Selector@ for @removeMemberRecord:error:@
removeMemberRecord_errorSelector :: Selector '[Id ODRecord, Id NSError] Bool
removeMemberRecord_errorSelector = mkSelector "removeMemberRecord:error:"

-- | @Selector@ for @isMemberRecord:error:@
isMemberRecord_errorSelector :: Selector '[Id ODRecord, Id NSError] Bool
isMemberRecord_errorSelector = mkSelector "isMemberRecord:error:"

-- | @Selector@ for @recordType@
recordTypeSelector :: Selector '[] (Id NSString)
recordTypeSelector = mkSelector "recordType"

-- | @Selector@ for @recordName@
recordNameSelector :: Selector '[] (Id NSString)
recordNameSelector = mkSelector "recordName"

-- | @Selector@ for @secondsUntilPasswordExpires@
secondsUntilPasswordExpiresSelector :: Selector '[] CLong
secondsUntilPasswordExpiresSelector = mkSelector "secondsUntilPasswordExpires"

-- | @Selector@ for @secondsUntilAuthenticationsExpire@
secondsUntilAuthenticationsExpireSelector :: Selector '[] CLong
secondsUntilAuthenticationsExpireSelector = mkSelector "secondsUntilAuthenticationsExpire"

