{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ODNode
--
-- This class is used to work with OpenDirectory nodes.
--
-- OpenDirectory uses nodes to represent different sources of directory information, via the local disk, LDAP, etc.
--
-- Generated bindings for @ODNode@.
module ObjC.OpenDirectory.ODNode
  ( ODNode
  , IsODNode(..)
  , nodeWithSession_type_error
  , nodeWithSession_name_error
  , initWithSession_type_error
  , initWithSession_name_error
  , subnodeNamesAndReturnError
  , unreachableSubnodeNamesAndReturnError
  , nodeDetailsForKeys_error
  , supportedRecordTypesAndReturnError
  , supportedAttributesForRecordType_error
  , setCredentialsWithRecordType_recordName_password_error
  , setCredentialsWithRecordType_authenticationType_authenticationItems_continueItems_context_error
  , setCredentialsUsingKerberosCache_error
  , createRecordWithRecordType_name_attributes_error
  , recordWithRecordType_name_attributes_error
  , customCall_sendData_error
  , customFunction_payload_error
  , policiesAndReturnError
  , supportedPoliciesAndReturnError
  , setPolicies_error
  , setPolicy_value_error
  , removePolicy_error
  , addAccountPolicy_toCategory_error
  , removeAccountPolicy_fromCategory_error
  , setAccountPolicies_error
  , accountPoliciesAndReturnError
  , passwordContentCheck_forRecordName_error
  , nodeName
  , configuration
  , nodeWithSession_type_errorSelector
  , nodeWithSession_name_errorSelector
  , initWithSession_type_errorSelector
  , initWithSession_name_errorSelector
  , subnodeNamesAndReturnErrorSelector
  , unreachableSubnodeNamesAndReturnErrorSelector
  , nodeDetailsForKeys_errorSelector
  , supportedRecordTypesAndReturnErrorSelector
  , supportedAttributesForRecordType_errorSelector
  , setCredentialsWithRecordType_recordName_password_errorSelector
  , setCredentialsWithRecordType_authenticationType_authenticationItems_continueItems_context_errorSelector
  , setCredentialsUsingKerberosCache_errorSelector
  , createRecordWithRecordType_name_attributes_errorSelector
  , recordWithRecordType_name_attributes_errorSelector
  , customCall_sendData_errorSelector
  , customFunction_payload_errorSelector
  , policiesAndReturnErrorSelector
  , supportedPoliciesAndReturnErrorSelector
  , setPolicies_errorSelector
  , setPolicy_value_errorSelector
  , removePolicy_errorSelector
  , addAccountPolicy_toCategory_errorSelector
  , removeAccountPolicy_fromCategory_errorSelector
  , setAccountPolicies_errorSelector
  , accountPoliciesAndReturnErrorSelector
  , passwordContentCheck_forRecordName_errorSelector
  , nodeNameSelector
  , configurationSelector


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

import ObjC.OpenDirectory.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | nodeWithSession:type:error:
--
-- Create an autoreleased ODNode of the given type, optionally in a specific session.
--
-- Autoreleased instance of an ODNode with a provided ODSession and ODNodeType.  outError is                 optional parameter, nil can be passed if error details are not needed.
--
-- ObjC selector: @+ nodeWithSession:type:error:@
nodeWithSession_type_error :: (IsODSession inSession, IsNSError outError) => inSession -> CUInt -> outError -> IO (Id ODNode)
nodeWithSession_type_error inSession inType outError =
  do
    cls' <- getRequiredClass "ODNode"
    withObjCPtr inSession $ \raw_inSession ->
      withObjCPtr outError $ \raw_outError ->
        sendClassMsg cls' (mkSelector "nodeWithSession:type:error:") (retPtr retVoid) [argPtr (castPtr raw_inSession :: Ptr ()), argCUInt (fromIntegral inType), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | nodeWithSession:name:error:
--
-- Create an autoreleased ODNode with the given name, optionally in a specific session.
--
-- autoreleased instance of an ODNode with a provided ODSession and node name.  outError is                 optional parameter, nil can be passed if error details are not needed.
--
-- ObjC selector: @+ nodeWithSession:name:error:@
nodeWithSession_name_error :: (IsODSession inSession, IsNSString inName, IsNSError outError) => inSession -> inName -> outError -> IO (Id ODNode)
nodeWithSession_name_error inSession inName outError =
  do
    cls' <- getRequiredClass "ODNode"
    withObjCPtr inSession $ \raw_inSession ->
      withObjCPtr inName $ \raw_inName ->
        withObjCPtr outError $ \raw_outError ->
          sendClassMsg cls' (mkSelector "nodeWithSession:name:error:") (retPtr retVoid) [argPtr (castPtr raw_inSession :: Ptr ()), argPtr (castPtr raw_inName :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | initWithSession:type:error:
--
-- Initialize an ODNode instance of the given type, optionally in a specific session.
--
-- initialize instance of an ODNode with a provided ODSession and ODNodeType.  outError is                 optional parameter, nil can be passed if error details are not needed.
--
-- ObjC selector: @- initWithSession:type:error:@
initWithSession_type_error :: (IsODNode odNode, IsODSession inSession, IsNSError outError) => odNode -> inSession -> CUInt -> outError -> IO (Id ODNode)
initWithSession_type_error odNode  inSession inType outError =
withObjCPtr inSession $ \raw_inSession ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg odNode (mkSelector "initWithSession:type:error:") (retPtr retVoid) [argPtr (castPtr raw_inSession :: Ptr ()), argCUInt (fromIntegral inType), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | initWithSession:name:error:
--
-- Initialize an ODNode instance with the given name, optionally in a specific session.
--
-- initialize instance of an ODNode with a provided ODSession and node name.  outError is optional                parameter, nil can be passed if error details are not needed.
--
-- ObjC selector: @- initWithSession:name:error:@
initWithSession_name_error :: (IsODNode odNode, IsODSession inSession, IsNSString inName, IsNSError outError) => odNode -> inSession -> inName -> outError -> IO (Id ODNode)
initWithSession_name_error odNode  inSession inName outError =
withObjCPtr inSession $ \raw_inSession ->
  withObjCPtr inName $ \raw_inName ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg odNode (mkSelector "initWithSession:name:error:") (retPtr retVoid) [argPtr (castPtr raw_inSession :: Ptr ()), argPtr (castPtr raw_inName :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | subnodeNamesAndReturnError:
--
-- Returns NSArray of node names for this node, which may contain sub-nodes or search policy nodes
--
-- Returns NSArray of node names for this node, which may contain sub-nodes or search policy nodes.                Commonly used with Search policy nodes.  outError is optional parameter, nil can be passed if error                details are not needed.
--
-- ObjC selector: @- subnodeNamesAndReturnError:@
subnodeNamesAndReturnError :: (IsODNode odNode, IsNSError outError) => odNode -> outError -> IO (Id NSArray)
subnodeNamesAndReturnError odNode  outError =
withObjCPtr outError $ \raw_outError ->
    sendMsg odNode (mkSelector "subnodeNamesAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | unreachableSubnodeNamesAndReturnError:
--
-- Will return NSArray of names of subnodes that are not currently reachable.
--
-- Will return NSArray of names of subnodes that are not currently reachable.  Commonly used with Search policy                 nodes to determine if any nodes are currently unreachable, but may also return other subnodes if the                OpenDirectory plugin supports.  outError is optional parameter, nil can be passed if error details are not needed.
--
-- ObjC selector: @- unreachableSubnodeNamesAndReturnError:@
unreachableSubnodeNamesAndReturnError :: (IsODNode odNode, IsNSError outError) => odNode -> outError -> IO (Id NSArray)
unreachableSubnodeNamesAndReturnError odNode  outError =
withObjCPtr outError $ \raw_outError ->
    sendMsg odNode (mkSelector "unreachableSubnodeNamesAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | nodeDetails:error:
--
-- Returns a dictionary of information about the instance of ODNode
--
-- Returns a dictionary of information about the instance of ODNode.  Details such as Trust information                (kODAttributeTypeTrustInformation) or other Node details can be retrieved.  outError is optional parameter,                nil can be passed if error details are not needed.
--
-- ObjC selector: @- nodeDetailsForKeys:error:@
nodeDetailsForKeys_error :: (IsODNode odNode, IsNSArray inKeys, IsNSError outError) => odNode -> inKeys -> outError -> IO (Id NSDictionary)
nodeDetailsForKeys_error odNode  inKeys outError =
withObjCPtr inKeys $ \raw_inKeys ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg odNode (mkSelector "nodeDetailsForKeys:error:") (retPtr retVoid) [argPtr (castPtr raw_inKeys :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | supportedRecordTypesAndReturnError:
--
-- Returns a NSArray of the record types supported by this node.
--
-- Returns a NSArray of the record types supported by this node.  If node does not support the check                then all possible types will be returned.  outError is optional parameter, nil can be passed if error details                are not needed.
--
-- ObjC selector: @- supportedRecordTypesAndReturnError:@
supportedRecordTypesAndReturnError :: (IsODNode odNode, IsNSError outError) => odNode -> outError -> IO (Id NSArray)
supportedRecordTypesAndReturnError odNode  outError =
withObjCPtr outError $ \raw_outError ->
    sendMsg odNode (mkSelector "supportedRecordTypesAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | supportedAttributesForRecordType:error:
--
-- Will return a list of attribute types supported for that attribute if possible
--
-- Will return a list of attribute types supported for that attribute if possible.  If no specific                types are available, then all possible values will be returned instead.  outError is optional parameter,                nil can be passed if error details are not needed.
--
-- ObjC selector: @- supportedAttributesForRecordType:error:@
supportedAttributesForRecordType_error :: (IsODNode odNode, IsNSString inRecordType, IsNSError outError) => odNode -> inRecordType -> outError -> IO (Id NSArray)
supportedAttributesForRecordType_error odNode  inRecordType outError =
withObjCPtr inRecordType $ \raw_inRecordType ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg odNode (mkSelector "supportedAttributesForRecordType:error:") (retPtr retVoid) [argPtr (castPtr raw_inRecordType :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | setCredentialsWithRecordType:recordName:password:error:
--
-- Sets the credentials for interaction with the ODNode
--
-- Sets the credentials for interaction with the ODNode.  Record references, etc. will use these credentials                to query or change data.  Setting the credentials on a node referenced by other OD object types will                change the credentials for all for all references.  outError is optional parameter, nil can be passed if error                details are not needed.
--
-- ObjC selector: @- setCredentialsWithRecordType:recordName:password:error:@
setCredentialsWithRecordType_recordName_password_error :: (IsODNode odNode, IsNSString inRecordType, IsNSString inRecordName, IsNSString inPassword, IsNSError outError) => odNode -> inRecordType -> inRecordName -> inPassword -> outError -> IO Bool
setCredentialsWithRecordType_recordName_password_error odNode  inRecordType inRecordName inPassword outError =
withObjCPtr inRecordType $ \raw_inRecordType ->
  withObjCPtr inRecordName $ \raw_inRecordName ->
    withObjCPtr inPassword $ \raw_inPassword ->
      withObjCPtr outError $ \raw_outError ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg odNode (mkSelector "setCredentialsWithRecordType:recordName:password:error:") retCULong [argPtr (castPtr raw_inRecordType :: Ptr ()), argPtr (castPtr raw_inRecordName :: Ptr ()), argPtr (castPtr raw_inPassword :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | setCredentialsWithRecordType:authType:authItems:outAuthItems:context:error:
--
-- Allows use of other OpenDirectory types of authentications to set the credentials for an ODNode
--
-- Allows the caller to use other types of authentications that are available in OpenDirectory, that may                require response-request loops, etc.  Not all OD plugins will support this call, look for                 kODErrorCredentialsMethodNotSupported in outError.  outError is optional parameter, nil can be passed if 				error details is not needed.
--
-- ObjC selector: @- setCredentialsWithRecordType:authenticationType:authenticationItems:continueItems:context:error:@
setCredentialsWithRecordType_authenticationType_authenticationItems_continueItems_context_error :: (IsODNode odNode, IsNSString inRecordType, IsNSString inType, IsNSArray inItems, IsNSArray outItems, IsNSError outError) => odNode -> inRecordType -> inType -> inItems -> outItems -> Ptr (Maybe RawId) -> outError -> IO Bool
setCredentialsWithRecordType_authenticationType_authenticationItems_continueItems_context_error odNode  inRecordType inType inItems outItems outContext outError =
withObjCPtr inRecordType $ \raw_inRecordType ->
  withObjCPtr inType $ \raw_inType ->
    withObjCPtr inItems $ \raw_inItems ->
      withObjCPtr outItems $ \raw_outItems ->
        withObjCPtr outError $ \raw_outError ->
            fmap ((/= 0) :: CULong -> Bool) $ sendMsg odNode (mkSelector "setCredentialsWithRecordType:authenticationType:authenticationItems:continueItems:context:error:") retCULong [argPtr (castPtr raw_inRecordType :: Ptr ()), argPtr (castPtr raw_inType :: Ptr ()), argPtr (castPtr raw_inItems :: Ptr ()), argPtr (castPtr raw_outItems :: Ptr ()), argPtr outContext, argPtr (castPtr raw_outError :: Ptr ())]

-- | setCredentialsUsingKerberosCache:error:
--
-- Unsupported method.
--
-- Unsupported method.
--
-- ObjC selector: @- setCredentialsUsingKerberosCache:error:@
setCredentialsUsingKerberosCache_error :: (IsODNode odNode, IsNSString inCacheName, IsNSError outError) => odNode -> inCacheName -> outError -> IO Bool
setCredentialsUsingKerberosCache_error odNode  inCacheName outError =
withObjCPtr inCacheName $ \raw_inCacheName ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg odNode (mkSelector "setCredentialsUsingKerberosCache:error:") retCULong [argPtr (castPtr raw_inCacheName :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | createRecordWithRecordType:name:attributes:error:
--
-- Creates a record in this node, using the given name and attributes.
--
-- Takes all the provided attributes and type to create an entire record.  The function will assign a                UUID to the record automatically.  This UUID can be overwritten by the client by passing with the                other attributes.  inAttributes is optional, nil can be passed if no other attributes are to be set.
--
-- ObjC selector: @- createRecordWithRecordType:name:attributes:error:@
createRecordWithRecordType_name_attributes_error :: (IsODNode odNode, IsNSString inRecordType, IsNSString inRecordName, IsNSDictionary inAttributes, IsNSError outError) => odNode -> inRecordType -> inRecordName -> inAttributes -> outError -> IO (Id ODRecord)
createRecordWithRecordType_name_attributes_error odNode  inRecordType inRecordName inAttributes outError =
withObjCPtr inRecordType $ \raw_inRecordType ->
  withObjCPtr inRecordName $ \raw_inRecordName ->
    withObjCPtr inAttributes $ \raw_inAttributes ->
      withObjCPtr outError $ \raw_outError ->
          sendMsg odNode (mkSelector "createRecordWithRecordType:name:attributes:error:") (retPtr retVoid) [argPtr (castPtr raw_inRecordType :: Ptr ()), argPtr (castPtr raw_inRecordName :: Ptr ()), argPtr (castPtr raw_inAttributes :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | recordWithRecordType:name:attributes:error:
--
-- Returns an ODRecord object that references the requested type and name
--
-- Returns an ODRecord object that references the requested type and name.  The record will have cached the                attributes requested.  Further attributes can be requested via ODRecord APIs.  For performance it is best                to ask for as many attributes that are needed as possible up front.
--
-- ObjC selector: @- recordWithRecordType:name:attributes:error:@
recordWithRecordType_name_attributes_error :: (IsODNode odNode, IsNSString inRecordType, IsNSString inRecordName, IsNSError outError) => odNode -> inRecordType -> inRecordName -> RawId -> outError -> IO (Id ODRecord)
recordWithRecordType_name_attributes_error odNode  inRecordType inRecordName inAttributes outError =
withObjCPtr inRecordType $ \raw_inRecordType ->
  withObjCPtr inRecordName $ \raw_inRecordName ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg odNode (mkSelector "recordWithRecordType:name:attributes:error:") (retPtr retVoid) [argPtr (castPtr raw_inRecordType :: Ptr ()), argPtr (castPtr raw_inRecordName :: Ptr ()), argPtr (castPtr (unRawId inAttributes) :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | customCall:sendData:error:
--
-- Sends a custom code to the node; input and output data formats are specific to the call.
--
-- Sends a custom code to the node; input and output data formats are specific to the call.  outError is                 optional parameter, nil can be passed if error details are not needed.
--
-- ObjC selector: @- customCall:sendData:error:@
customCall_sendData_error :: (IsODNode odNode, IsNSData inSendData, IsNSError outError) => odNode -> CLong -> inSendData -> outError -> IO (Id NSData)
customCall_sendData_error odNode  inCustomCode inSendData outError =
withObjCPtr inSendData $ \raw_inSendData ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg odNode (mkSelector "customCall:sendData:error:") (retPtr retVoid) [argCLong (fromIntegral inCustomCode), argPtr (castPtr raw_inSendData :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | customFunction:payload:error:
--
-- Sends a custom function call to the node; data is a type specific to the call.
--
-- Sends a custom function call to the node; data is a type specific to the call.  'error' is an                optional parameter therefore nil can be passed if error details are not needed.  Return type is				defined by the custom function requested.
--
-- ObjC selector: @- customFunction:payload:error:@
customFunction_payload_error :: (IsODNode odNode, IsNSString function, IsNSError error_) => odNode -> function -> RawId -> error_ -> IO RawId
customFunction_payload_error odNode  function payload error_ =
withObjCPtr function $ \raw_function ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap (RawId . castPtr) $ sendMsg odNode (mkSelector "customFunction:payload:error:") (retPtr retVoid) [argPtr (castPtr raw_function :: Ptr ()), argPtr (castPtr (unRawId payload) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | policiesAndReturnError:
--
-- This will copy any policies configured for the node.
--
-- This will copy any policies configured for the node.
--
-- ObjC selector: @- policiesAndReturnError:@
policiesAndReturnError :: (IsODNode odNode, IsNSError error_) => odNode -> error_ -> IO (Id NSDictionary)
policiesAndReturnError odNode  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg odNode (mkSelector "policiesAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | supportedPoliciesAndReturnError:
--
-- This will return a dictionary of supported policies.
--
-- This will return a dictionary of supported policies, if appropriate, the value will be the maximum value allowed                for the policy in question.  For example, if password history is available, it will state how much history is                supported.
--
-- ObjC selector: @- supportedPoliciesAndReturnError:@
supportedPoliciesAndReturnError :: (IsODNode odNode, IsNSError error_) => odNode -> error_ -> IO (Id NSDictionary)
supportedPoliciesAndReturnError odNode  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg odNode (mkSelector "supportedPoliciesAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | setPolicies:error:
--
-- This will set the policy for the node.
--
-- This will set the policy for the node.  Policies are evaluated in combination with record-level policies.
--
-- ObjC selector: @- setPolicies:error:@
setPolicies_error :: (IsODNode odNode, IsNSDictionary policies, IsNSError error_) => odNode -> policies -> error_ -> IO Bool
setPolicies_error odNode  policies error_ =
withObjCPtr policies $ \raw_policies ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg odNode (mkSelector "setPolicies:error:") retCULong [argPtr (castPtr raw_policies :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | setPolicy:value:error:
--
-- This will set a specific policy setting for the node.
--
-- This will set a specific policy setting for the node.
--
-- ObjC selector: @- setPolicy:value:error:@
setPolicy_value_error :: (IsODNode odNode, IsNSString policy, IsNSError error_) => odNode -> policy -> RawId -> error_ -> IO Bool
setPolicy_value_error odNode  policy value error_ =
withObjCPtr policy $ \raw_policy ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg odNode (mkSelector "setPolicy:value:error:") retCULong [argPtr (castPtr raw_policy :: Ptr ()), argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | removePolicy:value:error:
--
-- This will remove a specific policy setting from the node.
--
-- This will remove a specific policy setting from the node.
--
-- ObjC selector: @- removePolicy:error:@
removePolicy_error :: (IsODNode odNode, IsNSString policy, IsNSError error_) => odNode -> policy -> error_ -> IO Bool
removePolicy_error odNode  policy error_ =
withObjCPtr policy $ \raw_policy ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg odNode (mkSelector "removePolicy:error:") retCULong [argPtr (castPtr raw_policy :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | addAccountPolicy:toCategory:error:
--
-- This will add an account policy to the node for the specified category.
--
-- This will add an account policy to the node for the specified category.                The specified policy will be applied to all users in the                specified node when policies are evaluated.
--
-- @policy@ — a dictionary containing the specific policy to be added.                The dictionary may contain the following keys:                    kODPolicyKeyIdentifier a required key identifying the policy.                    kODPolicyKeyParameters an optional key containing a dictionary of                        parameters that can be used for informational purposes or in                        the policy format string.                    kODPolicyKeyContent a required key specifying the policy,                        from which a predicate will be created for evaluating                        the policy.
--
-- @category@ — a valid ODPolicyCategoryType to which the specified policy will be added.
--
-- @error@ — an optional NSError reference for error details.
--
-- Returns: a BOOL which signifies if the policy addition succeeded, otherwise error is set.
--
-- ObjC selector: @- addAccountPolicy:toCategory:error:@
addAccountPolicy_toCategory_error :: (IsODNode odNode, IsNSDictionary policy, IsNSString category, IsNSError error_) => odNode -> policy -> category -> error_ -> IO Bool
addAccountPolicy_toCategory_error odNode  policy category error_ =
withObjCPtr policy $ \raw_policy ->
  withObjCPtr category $ \raw_category ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg odNode (mkSelector "addAccountPolicy:toCategory:error:") retCULong [argPtr (castPtr raw_policy :: Ptr ()), argPtr (castPtr raw_category :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | removeAccountPolicy:fromCategory:error:
--
-- This will remove an account policy from the node for the specified category.
--
-- This will remove an account policy from the node for the specified category.
--
-- @policy@ — a dictionary containing the specific policy to be                removed, with the same format as described in addAccountPolicy.
--
-- @category@ — a valid ODPolicyCategoryType from which the specified policy will be removed.
--
-- @error@ — an optional NSError reference for error details.
--
-- Returns: a BOOL which signifies if the policy removal succeeded, otherwise error is set.
--
-- ObjC selector: @- removeAccountPolicy:fromCategory:error:@
removeAccountPolicy_fromCategory_error :: (IsODNode odNode, IsNSDictionary policy, IsNSString category, IsNSError error_) => odNode -> policy -> category -> error_ -> IO Bool
removeAccountPolicy_fromCategory_error odNode  policy category error_ =
withObjCPtr policy $ \raw_policy ->
  withObjCPtr category $ \raw_category ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg odNode (mkSelector "removeAccountPolicy:fromCategory:error:") retCULong [argPtr (castPtr raw_policy :: Ptr ()), argPtr (castPtr raw_category :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | setAccountPolicies:error:
--
-- This will set the policies for the node.
--
-- This will set the policies for the node, replacing any existing                policies.  All of the policies in the set will be applied to all                users in the specified node when policies are evaluated.
--
-- @policies@ — a dictionary containing all of the policies to be set                for the node.  The dictionary may contain the following keys:                    kODPolicyCategoryAuthentication an optional key with a value                        of an array of policy dictionaries that specify when                        authentications should be allowed.                    kODPolicyCategoryPasswordContent an optional key with a                        value of an array of policy dictionaries the specify the                        required content of passwords.                     kODPolicyCategoryPasswordChange an optional key with a value                    of an array of policy dictionaries that specify when                    passwords are required to be changed.
--
-- @error@ — an optional NSError reference for error details.
--
-- Returns: a BOOL which signifies if the policy set succeeded, otherwise error is set.
--
-- ObjC selector: @- setAccountPolicies:error:@
setAccountPolicies_error :: (IsODNode odNode, IsNSDictionary policies, IsNSError error_) => odNode -> policies -> error_ -> IO Bool
setAccountPolicies_error odNode  policies error_ =
withObjCPtr policies $ \raw_policies ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg odNode (mkSelector "setAccountPolicies:error:") retCULong [argPtr (castPtr raw_policies :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | accountPoliciesAndReturnError:
--
-- Returns a dictionary containing any policies configured for the node.
--
-- Returns a dictionary containing any policies configured for the node.
--
-- @error@ — an optional NSError reference for error details.
--
-- Returns: an NSDictionary containing all currently set policies.  The                format of the dictionary is the same as described in                setAccountPolicies.
--
-- ObjC selector: @- accountPoliciesAndReturnError:@
accountPoliciesAndReturnError :: (IsODNode odNode, IsNSError error_) => odNode -> error_ -> IO (Id NSDictionary)
accountPoliciesAndReturnError odNode  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg odNode (mkSelector "accountPoliciesAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | passwordContentCheck:forRecordName:error:
--
-- Validates a password against the node's password content policies.
--
-- Validates a password against the node's password content policies.                The node's password content policies will be evaluated to                determine if the password is acceptable.  May be used prior to                creating the record.
--
-- This check is only definitive at the time it was requested. The                policy or the environment could change before the password change                is actually requested.  Errors from the password change request                should be consulted.
--
-- @password@ — the password to be evaluated against the content policies.
--
-- @recordName@ — the name of the record.
--
-- @error@ — an optional NSError reference for error details.
--
-- Returns: a bool which signifies if the password passes all content policies, otherwise error is set.
--
-- ObjC selector: @- passwordContentCheck:forRecordName:error:@
passwordContentCheck_forRecordName_error :: (IsODNode odNode, IsNSString password, IsNSString recordName, IsNSError error_) => odNode -> password -> recordName -> error_ -> IO Bool
passwordContentCheck_forRecordName_error odNode  password recordName error_ =
withObjCPtr password $ \raw_password ->
  withObjCPtr recordName $ \raw_recordName ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg odNode (mkSelector "passwordContentCheck:forRecordName:error:") retCULong [argPtr (castPtr raw_password :: Ptr ()), argPtr (castPtr raw_recordName :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | nodeName
--
-- The node name.
--
-- The node name, corresponding to its path in OpenDirectory.
--
-- ObjC selector: @- nodeName@
nodeName :: IsODNode odNode => odNode -> IO (Id NSString)
nodeName odNode  =
  sendMsg odNode (mkSelector "nodeName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | configuration
--
-- Returns an ODConfiguration object for the node.
--
-- Returns an ODConfiguration object for the node.
--
-- ObjC selector: @- configuration@
configuration :: IsODNode odNode => odNode -> IO (Id ODConfiguration)
configuration odNode  =
  sendMsg odNode (mkSelector "configuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSession:type:error:@
nodeWithSession_type_errorSelector :: Selector
nodeWithSession_type_errorSelector = mkSelector "nodeWithSession:type:error:"

-- | @Selector@ for @nodeWithSession:name:error:@
nodeWithSession_name_errorSelector :: Selector
nodeWithSession_name_errorSelector = mkSelector "nodeWithSession:name:error:"

-- | @Selector@ for @initWithSession:type:error:@
initWithSession_type_errorSelector :: Selector
initWithSession_type_errorSelector = mkSelector "initWithSession:type:error:"

-- | @Selector@ for @initWithSession:name:error:@
initWithSession_name_errorSelector :: Selector
initWithSession_name_errorSelector = mkSelector "initWithSession:name:error:"

-- | @Selector@ for @subnodeNamesAndReturnError:@
subnodeNamesAndReturnErrorSelector :: Selector
subnodeNamesAndReturnErrorSelector = mkSelector "subnodeNamesAndReturnError:"

-- | @Selector@ for @unreachableSubnodeNamesAndReturnError:@
unreachableSubnodeNamesAndReturnErrorSelector :: Selector
unreachableSubnodeNamesAndReturnErrorSelector = mkSelector "unreachableSubnodeNamesAndReturnError:"

-- | @Selector@ for @nodeDetailsForKeys:error:@
nodeDetailsForKeys_errorSelector :: Selector
nodeDetailsForKeys_errorSelector = mkSelector "nodeDetailsForKeys:error:"

-- | @Selector@ for @supportedRecordTypesAndReturnError:@
supportedRecordTypesAndReturnErrorSelector :: Selector
supportedRecordTypesAndReturnErrorSelector = mkSelector "supportedRecordTypesAndReturnError:"

-- | @Selector@ for @supportedAttributesForRecordType:error:@
supportedAttributesForRecordType_errorSelector :: Selector
supportedAttributesForRecordType_errorSelector = mkSelector "supportedAttributesForRecordType:error:"

-- | @Selector@ for @setCredentialsWithRecordType:recordName:password:error:@
setCredentialsWithRecordType_recordName_password_errorSelector :: Selector
setCredentialsWithRecordType_recordName_password_errorSelector = mkSelector "setCredentialsWithRecordType:recordName:password:error:"

-- | @Selector@ for @setCredentialsWithRecordType:authenticationType:authenticationItems:continueItems:context:error:@
setCredentialsWithRecordType_authenticationType_authenticationItems_continueItems_context_errorSelector :: Selector
setCredentialsWithRecordType_authenticationType_authenticationItems_continueItems_context_errorSelector = mkSelector "setCredentialsWithRecordType:authenticationType:authenticationItems:continueItems:context:error:"

-- | @Selector@ for @setCredentialsUsingKerberosCache:error:@
setCredentialsUsingKerberosCache_errorSelector :: Selector
setCredentialsUsingKerberosCache_errorSelector = mkSelector "setCredentialsUsingKerberosCache:error:"

-- | @Selector@ for @createRecordWithRecordType:name:attributes:error:@
createRecordWithRecordType_name_attributes_errorSelector :: Selector
createRecordWithRecordType_name_attributes_errorSelector = mkSelector "createRecordWithRecordType:name:attributes:error:"

-- | @Selector@ for @recordWithRecordType:name:attributes:error:@
recordWithRecordType_name_attributes_errorSelector :: Selector
recordWithRecordType_name_attributes_errorSelector = mkSelector "recordWithRecordType:name:attributes:error:"

-- | @Selector@ for @customCall:sendData:error:@
customCall_sendData_errorSelector :: Selector
customCall_sendData_errorSelector = mkSelector "customCall:sendData:error:"

-- | @Selector@ for @customFunction:payload:error:@
customFunction_payload_errorSelector :: Selector
customFunction_payload_errorSelector = mkSelector "customFunction:payload:error:"

-- | @Selector@ for @policiesAndReturnError:@
policiesAndReturnErrorSelector :: Selector
policiesAndReturnErrorSelector = mkSelector "policiesAndReturnError:"

-- | @Selector@ for @supportedPoliciesAndReturnError:@
supportedPoliciesAndReturnErrorSelector :: Selector
supportedPoliciesAndReturnErrorSelector = mkSelector "supportedPoliciesAndReturnError:"

-- | @Selector@ for @setPolicies:error:@
setPolicies_errorSelector :: Selector
setPolicies_errorSelector = mkSelector "setPolicies:error:"

-- | @Selector@ for @setPolicy:value:error:@
setPolicy_value_errorSelector :: Selector
setPolicy_value_errorSelector = mkSelector "setPolicy:value:error:"

-- | @Selector@ for @removePolicy:error:@
removePolicy_errorSelector :: Selector
removePolicy_errorSelector = mkSelector "removePolicy:error:"

-- | @Selector@ for @addAccountPolicy:toCategory:error:@
addAccountPolicy_toCategory_errorSelector :: Selector
addAccountPolicy_toCategory_errorSelector = mkSelector "addAccountPolicy:toCategory:error:"

-- | @Selector@ for @removeAccountPolicy:fromCategory:error:@
removeAccountPolicy_fromCategory_errorSelector :: Selector
removeAccountPolicy_fromCategory_errorSelector = mkSelector "removeAccountPolicy:fromCategory:error:"

-- | @Selector@ for @setAccountPolicies:error:@
setAccountPolicies_errorSelector :: Selector
setAccountPolicies_errorSelector = mkSelector "setAccountPolicies:error:"

-- | @Selector@ for @accountPoliciesAndReturnError:@
accountPoliciesAndReturnErrorSelector :: Selector
accountPoliciesAndReturnErrorSelector = mkSelector "accountPoliciesAndReturnError:"

-- | @Selector@ for @passwordContentCheck:forRecordName:error:@
passwordContentCheck_forRecordName_errorSelector :: Selector
passwordContentCheck_forRecordName_errorSelector = mkSelector "passwordContentCheck:forRecordName:error:"

-- | @Selector@ for @nodeName@
nodeNameSelector :: Selector
nodeNameSelector = mkSelector "nodeName"

-- | @Selector@ for @configuration@
configurationSelector :: Selector
configurationSelector = mkSelector "configuration"

