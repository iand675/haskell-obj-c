{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ODSession
--
-- Class for working with OpenDirectory sessions.
--
-- Class for working with OpenDirectory sessions.
--
-- Generated bindings for @ODSession@.
module ObjC.OpenDirectory.ODSession
  ( ODSession
  , IsODSession(..)
  , defaultSession
  , sessionWithOptions_error
  , initWithOptions_error
  , nodeNamesAndReturnError
  , configurationAuthorizationAllowingUserInteraction_error
  , configurationForNodename
  , addConfiguration_authorization_error
  , deleteConfiguration_authorization_error
  , deleteConfigurationWithNodename_authorization_error
  , configurationTemplateNames
  , mappingTemplateNames
  , defaultSessionSelector
  , sessionWithOptions_errorSelector
  , initWithOptions_errorSelector
  , nodeNamesAndReturnErrorSelector
  , configurationAuthorizationAllowingUserInteraction_errorSelector
  , configurationForNodenameSelector
  , addConfiguration_authorization_errorSelector
  , deleteConfiguration_authorization_errorSelector
  , deleteConfigurationWithNodename_authorization_errorSelector
  , configurationTemplateNamesSelector
  , mappingTemplateNamesSelector


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
import ObjC.SecurityFoundation.Internal.Classes

-- | defaultSession
--
-- Returns a shared instance of a local ODSession
--
-- Returns a shared instance of a local ODSession.  This can be used for most situations unless                more control is needed over the session.
--
-- ObjC selector: @+ defaultSession@
defaultSession :: IO (Id ODSession)
defaultSession  =
  do
    cls' <- getRequiredClass "ODSession"
    sendClassMsg cls' (mkSelector "defaultSession") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sessionWithOptions:error:
--
-- Creates an autoreleased instance of ODSession directed over Proxy to another host
--
-- Creates an autoreleased instance of ODSession directed over Proxy to another host.  nil                can be passed for no options. outError is optional parameter, nil can be passed if error                details are not needed.  Options include:
--
-- If proxy is required then a dictionary with keys should be:                            Key                             Value                    ODSessionProxyAddress        NSString(hostname or IP)                    ODSessionProxyPort           NSNumber(IP port, should not be set as it will default)                    ODSessionProxyUsername       NSString(username)                    ODSessionProxyPassword       NSString(password)
--
-- ObjC selector: @+ sessionWithOptions:error:@
sessionWithOptions_error :: (IsNSDictionary inOptions, IsNSError outError) => inOptions -> outError -> IO (Id ODSession)
sessionWithOptions_error inOptions outError =
  do
    cls' <- getRequiredClass "ODSession"
    withObjCPtr inOptions $ \raw_inOptions ->
      withObjCPtr outError $ \raw_outError ->
        sendClassMsg cls' (mkSelector "sessionWithOptions:error:") (retPtr retVoid) [argPtr (castPtr raw_inOptions :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | initWithOptions:error:
--
-- Creates an instance of ODSession directed over Proxy to another host
--
-- Creates an instance of ODSession directed over Proxy to another host.  nil can be                passed for no options. outError is optional parameter, nil can be passed if error                details are not needed. Options include:
--
-- If proxy is required then a dictionary with keys should be:                            Key                             Value                    ODSessionProxyAddress        NSString(hostname or IP)                    ODSessionProxyPort           NSNumber(IP port, should not be set as it will default)                    ODSessionProxyUsername       NSString(username)                    ODSessionProxyPassword       NSString(password)
--
-- ObjC selector: @- initWithOptions:error:@
initWithOptions_error :: (IsODSession odSession, IsNSDictionary inOptions, IsNSError outError) => odSession -> inOptions -> outError -> IO (Id ODSession)
initWithOptions_error odSession  inOptions outError =
withObjCPtr inOptions $ \raw_inOptions ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg odSession (mkSelector "initWithOptions:error:") (retPtr retVoid) [argPtr (castPtr raw_inOptions :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | nodeNamesAndReturnError:
--
-- Returns the node names that are registered on this ODSession
--
-- Returns the node names that are registered on this ODSession.  outError can be nil if                error details are not needed.
--
-- ObjC selector: @- nodeNamesAndReturnError:@
nodeNamesAndReturnError :: (IsODSession odSession, IsNSError outError) => odSession -> outError -> IO (Id NSArray)
nodeNamesAndReturnError odSession  outError =
withObjCPtr outError $ \raw_outError ->
    sendMsg odSession (mkSelector "nodeNamesAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | configurationAuthorizationAllowingUserInteraction:
--
-- Returns an authorization appropriate for managing configurations.
--
-- Returns an authorization appropriate for managing configurations.  If a proxy session is in use this method will return nil and no error.
--
-- ObjC selector: @- configurationAuthorizationAllowingUserInteraction:error:@
configurationAuthorizationAllowingUserInteraction_error :: (IsODSession odSession, IsNSError error_) => odSession -> Bool -> error_ -> IO (Id SFAuthorization)
configurationAuthorizationAllowingUserInteraction_error odSession  allowInteraction error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg odSession (mkSelector "configurationAuthorizationAllowingUserInteraction:error:") (retPtr retVoid) [argCULong (if allowInteraction then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | configurationForNodename:
--
-- Reads the configuration for a given nodename.
--
-- Reads the configuration for a given nodename.
--
-- ObjC selector: @- configurationForNodename:@
configurationForNodename :: (IsODSession odSession, IsNSString nodename) => odSession -> nodename -> IO (Id ODConfiguration)
configurationForNodename odSession  nodename =
withObjCPtr nodename $ \raw_nodename ->
    sendMsg odSession (mkSelector "configurationForNodename:") (retPtr retVoid) [argPtr (castPtr raw_nodename :: Ptr ())] >>= retainedObject . castPtr

-- | addConfiguration:authorization:error:
--
-- Adds a new configuration to the existing ODSession.
--
-- Adds a new configuration to the existing ODSession.  An SFAuthorization can be provided if necessary.
--
-- ObjC selector: @- addConfiguration:authorization:error:@
addConfiguration_authorization_error :: (IsODSession odSession, IsODConfiguration configuration, IsSFAuthorization authorization, IsNSError error_) => odSession -> configuration -> authorization -> error_ -> IO Bool
addConfiguration_authorization_error odSession  configuration authorization error_ =
withObjCPtr configuration $ \raw_configuration ->
  withObjCPtr authorization $ \raw_authorization ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg odSession (mkSelector "addConfiguration:authorization:error:") retCULong [argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr raw_authorization :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | deleteConfiguration:authorization:error:
--
-- Deletes an existing configuration from the ODSession.
--
-- Deletes an existing configuration from the ODSession.  An authorization can be provided if necessary.
--
-- ObjC selector: @- deleteConfiguration:authorization:error:@
deleteConfiguration_authorization_error :: (IsODSession odSession, IsODConfiguration configuration, IsSFAuthorization authorization, IsNSError error_) => odSession -> configuration -> authorization -> error_ -> IO Bool
deleteConfiguration_authorization_error odSession  configuration authorization error_ =
withObjCPtr configuration $ \raw_configuration ->
  withObjCPtr authorization $ \raw_authorization ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg odSession (mkSelector "deleteConfiguration:authorization:error:") retCULong [argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr raw_authorization :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | deleteConfigurationWithNodename:authorization:error:
--
-- Deletes an existing configuration from the ODSession.
--
-- Deletes an existing configuration from the ODSession.  An authorization can be provided if necessary.
--
-- ObjC selector: @- deleteConfigurationWithNodename:authorization:error:@
deleteConfigurationWithNodename_authorization_error :: (IsODSession odSession, IsNSString nodename, IsSFAuthorization authorization, IsNSError error_) => odSession -> nodename -> authorization -> error_ -> IO Bool
deleteConfigurationWithNodename_authorization_error odSession  nodename authorization error_ =
withObjCPtr nodename $ \raw_nodename ->
  withObjCPtr authorization $ \raw_authorization ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg odSession (mkSelector "deleteConfigurationWithNodename:authorization:error:") retCULong [argPtr (castPtr raw_nodename :: Ptr ()), argPtr (castPtr raw_authorization :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | configurationTemplateNames
--
-- Returns a list of names as NSStrings for all available configuration templates.
--
-- Returns a list of names as NSStrings for all available configuration templates.  Configuration templates have pre-configured modules and/or mappings.  Useful for re-using existing configurations that may change with operating system without changing the actual configuration.
--
-- ObjC selector: @- configurationTemplateNames@
configurationTemplateNames :: IsODSession odSession => odSession -> IO (Id NSArray)
configurationTemplateNames odSession  =
  sendMsg odSession (mkSelector "configurationTemplateNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mappingTemplateNames
--
-- Returns a list names as NSStrings for all available mapping templates.
--
-- Returns a list names as NSStrings for all available mapping templates.  Mapping templates have pre-configured record/attribute mappings.  Useful if a configuration uses a common layout of mappings for a type of server.
--
-- ObjC selector: @- mappingTemplateNames@
mappingTemplateNames :: IsODSession odSession => odSession -> IO (Id NSArray)
mappingTemplateNames odSession  =
  sendMsg odSession (mkSelector "mappingTemplateNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultSession@
defaultSessionSelector :: Selector
defaultSessionSelector = mkSelector "defaultSession"

-- | @Selector@ for @sessionWithOptions:error:@
sessionWithOptions_errorSelector :: Selector
sessionWithOptions_errorSelector = mkSelector "sessionWithOptions:error:"

-- | @Selector@ for @initWithOptions:error:@
initWithOptions_errorSelector :: Selector
initWithOptions_errorSelector = mkSelector "initWithOptions:error:"

-- | @Selector@ for @nodeNamesAndReturnError:@
nodeNamesAndReturnErrorSelector :: Selector
nodeNamesAndReturnErrorSelector = mkSelector "nodeNamesAndReturnError:"

-- | @Selector@ for @configurationAuthorizationAllowingUserInteraction:error:@
configurationAuthorizationAllowingUserInteraction_errorSelector :: Selector
configurationAuthorizationAllowingUserInteraction_errorSelector = mkSelector "configurationAuthorizationAllowingUserInteraction:error:"

-- | @Selector@ for @configurationForNodename:@
configurationForNodenameSelector :: Selector
configurationForNodenameSelector = mkSelector "configurationForNodename:"

-- | @Selector@ for @addConfiguration:authorization:error:@
addConfiguration_authorization_errorSelector :: Selector
addConfiguration_authorization_errorSelector = mkSelector "addConfiguration:authorization:error:"

-- | @Selector@ for @deleteConfiguration:authorization:error:@
deleteConfiguration_authorization_errorSelector :: Selector
deleteConfiguration_authorization_errorSelector = mkSelector "deleteConfiguration:authorization:error:"

-- | @Selector@ for @deleteConfigurationWithNodename:authorization:error:@
deleteConfigurationWithNodename_authorization_errorSelector :: Selector
deleteConfigurationWithNodename_authorization_errorSelector = mkSelector "deleteConfigurationWithNodename:authorization:error:"

-- | @Selector@ for @configurationTemplateNames@
configurationTemplateNamesSelector :: Selector
configurationTemplateNamesSelector = mkSelector "configurationTemplateNames"

-- | @Selector@ for @mappingTemplateNames@
mappingTemplateNamesSelector :: Selector
mappingTemplateNamesSelector = mkSelector "mappingTemplateNames"

