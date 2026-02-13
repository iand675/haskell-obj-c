{-# LANGUAGE DataKinds #-}
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
  , addConfiguration_authorization_errorSelector
  , configurationAuthorizationAllowingUserInteraction_errorSelector
  , configurationForNodenameSelector
  , configurationTemplateNamesSelector
  , defaultSessionSelector
  , deleteConfigurationWithNodename_authorization_errorSelector
  , deleteConfiguration_authorization_errorSelector
  , initWithOptions_errorSelector
  , mappingTemplateNamesSelector
  , nodeNamesAndReturnErrorSelector
  , sessionWithOptions_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' defaultSessionSelector

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
    sendClassMessage cls' sessionWithOptions_errorSelector (toNSDictionary inOptions) (toNSError outError)

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
initWithOptions_error odSession inOptions outError =
  sendOwnedMessage odSession initWithOptions_errorSelector (toNSDictionary inOptions) (toNSError outError)

-- | nodeNamesAndReturnError:
--
-- Returns the node names that are registered on this ODSession
--
-- Returns the node names that are registered on this ODSession.  outError can be nil if                error details are not needed.
--
-- ObjC selector: @- nodeNamesAndReturnError:@
nodeNamesAndReturnError :: (IsODSession odSession, IsNSError outError) => odSession -> outError -> IO (Id NSArray)
nodeNamesAndReturnError odSession outError =
  sendMessage odSession nodeNamesAndReturnErrorSelector (toNSError outError)

-- | configurationAuthorizationAllowingUserInteraction:
--
-- Returns an authorization appropriate for managing configurations.
--
-- Returns an authorization appropriate for managing configurations.  If a proxy session is in use this method will return nil and no error.
--
-- ObjC selector: @- configurationAuthorizationAllowingUserInteraction:error:@
configurationAuthorizationAllowingUserInteraction_error :: (IsODSession odSession, IsNSError error_) => odSession -> Bool -> error_ -> IO (Id SFAuthorization)
configurationAuthorizationAllowingUserInteraction_error odSession allowInteraction error_ =
  sendMessage odSession configurationAuthorizationAllowingUserInteraction_errorSelector allowInteraction (toNSError error_)

-- | configurationForNodename:
--
-- Reads the configuration for a given nodename.
--
-- Reads the configuration for a given nodename.
--
-- ObjC selector: @- configurationForNodename:@
configurationForNodename :: (IsODSession odSession, IsNSString nodename) => odSession -> nodename -> IO (Id ODConfiguration)
configurationForNodename odSession nodename =
  sendMessage odSession configurationForNodenameSelector (toNSString nodename)

-- | addConfiguration:authorization:error:
--
-- Adds a new configuration to the existing ODSession.
--
-- Adds a new configuration to the existing ODSession.  An SFAuthorization can be provided if necessary.
--
-- ObjC selector: @- addConfiguration:authorization:error:@
addConfiguration_authorization_error :: (IsODSession odSession, IsODConfiguration configuration, IsSFAuthorization authorization, IsNSError error_) => odSession -> configuration -> authorization -> error_ -> IO Bool
addConfiguration_authorization_error odSession configuration authorization error_ =
  sendMessage odSession addConfiguration_authorization_errorSelector (toODConfiguration configuration) (toSFAuthorization authorization) (toNSError error_)

-- | deleteConfiguration:authorization:error:
--
-- Deletes an existing configuration from the ODSession.
--
-- Deletes an existing configuration from the ODSession.  An authorization can be provided if necessary.
--
-- ObjC selector: @- deleteConfiguration:authorization:error:@
deleteConfiguration_authorization_error :: (IsODSession odSession, IsODConfiguration configuration, IsSFAuthorization authorization, IsNSError error_) => odSession -> configuration -> authorization -> error_ -> IO Bool
deleteConfiguration_authorization_error odSession configuration authorization error_ =
  sendMessage odSession deleteConfiguration_authorization_errorSelector (toODConfiguration configuration) (toSFAuthorization authorization) (toNSError error_)

-- | deleteConfigurationWithNodename:authorization:error:
--
-- Deletes an existing configuration from the ODSession.
--
-- Deletes an existing configuration from the ODSession.  An authorization can be provided if necessary.
--
-- ObjC selector: @- deleteConfigurationWithNodename:authorization:error:@
deleteConfigurationWithNodename_authorization_error :: (IsODSession odSession, IsNSString nodename, IsSFAuthorization authorization, IsNSError error_) => odSession -> nodename -> authorization -> error_ -> IO Bool
deleteConfigurationWithNodename_authorization_error odSession nodename authorization error_ =
  sendMessage odSession deleteConfigurationWithNodename_authorization_errorSelector (toNSString nodename) (toSFAuthorization authorization) (toNSError error_)

-- | configurationTemplateNames
--
-- Returns a list of names as NSStrings for all available configuration templates.
--
-- Returns a list of names as NSStrings for all available configuration templates.  Configuration templates have pre-configured modules and/or mappings.  Useful for re-using existing configurations that may change with operating system without changing the actual configuration.
--
-- ObjC selector: @- configurationTemplateNames@
configurationTemplateNames :: IsODSession odSession => odSession -> IO (Id NSArray)
configurationTemplateNames odSession =
  sendMessage odSession configurationTemplateNamesSelector

-- | mappingTemplateNames
--
-- Returns a list names as NSStrings for all available mapping templates.
--
-- Returns a list names as NSStrings for all available mapping templates.  Mapping templates have pre-configured record/attribute mappings.  Useful if a configuration uses a common layout of mappings for a type of server.
--
-- ObjC selector: @- mappingTemplateNames@
mappingTemplateNames :: IsODSession odSession => odSession -> IO (Id NSArray)
mappingTemplateNames odSession =
  sendMessage odSession mappingTemplateNamesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultSession@
defaultSessionSelector :: Selector '[] (Id ODSession)
defaultSessionSelector = mkSelector "defaultSession"

-- | @Selector@ for @sessionWithOptions:error:@
sessionWithOptions_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id ODSession)
sessionWithOptions_errorSelector = mkSelector "sessionWithOptions:error:"

-- | @Selector@ for @initWithOptions:error:@
initWithOptions_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id ODSession)
initWithOptions_errorSelector = mkSelector "initWithOptions:error:"

-- | @Selector@ for @nodeNamesAndReturnError:@
nodeNamesAndReturnErrorSelector :: Selector '[Id NSError] (Id NSArray)
nodeNamesAndReturnErrorSelector = mkSelector "nodeNamesAndReturnError:"

-- | @Selector@ for @configurationAuthorizationAllowingUserInteraction:error:@
configurationAuthorizationAllowingUserInteraction_errorSelector :: Selector '[Bool, Id NSError] (Id SFAuthorization)
configurationAuthorizationAllowingUserInteraction_errorSelector = mkSelector "configurationAuthorizationAllowingUserInteraction:error:"

-- | @Selector@ for @configurationForNodename:@
configurationForNodenameSelector :: Selector '[Id NSString] (Id ODConfiguration)
configurationForNodenameSelector = mkSelector "configurationForNodename:"

-- | @Selector@ for @addConfiguration:authorization:error:@
addConfiguration_authorization_errorSelector :: Selector '[Id ODConfiguration, Id SFAuthorization, Id NSError] Bool
addConfiguration_authorization_errorSelector = mkSelector "addConfiguration:authorization:error:"

-- | @Selector@ for @deleteConfiguration:authorization:error:@
deleteConfiguration_authorization_errorSelector :: Selector '[Id ODConfiguration, Id SFAuthorization, Id NSError] Bool
deleteConfiguration_authorization_errorSelector = mkSelector "deleteConfiguration:authorization:error:"

-- | @Selector@ for @deleteConfigurationWithNodename:authorization:error:@
deleteConfigurationWithNodename_authorization_errorSelector :: Selector '[Id NSString, Id SFAuthorization, Id NSError] Bool
deleteConfigurationWithNodename_authorization_errorSelector = mkSelector "deleteConfigurationWithNodename:authorization:error:"

-- | @Selector@ for @configurationTemplateNames@
configurationTemplateNamesSelector :: Selector '[] (Id NSArray)
configurationTemplateNamesSelector = mkSelector "configurationTemplateNames"

-- | @Selector@ for @mappingTemplateNames@
mappingTemplateNamesSelector :: Selector '[] (Id NSArray)
mappingTemplateNamesSelector = mkSelector "mappingTemplateNames"

