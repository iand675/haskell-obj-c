{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An SMAppService is used to control helper executables that live inside of an app's main bundle.
--
-- For SMAppServices initialized as LoginItems, the register and unregister APIs provide a replacement for SMLoginItemSetEnabled.
--
-- Apps that use SMAppService APIs must be code signed.
--
-- For SMAppServices initialized as LaunchAgents, the register and unregister APIs provide a replacement for installing plists in ~/Library/LaunchAgents or /Library/LaunchAgents.
--
-- For SMAppServices initialized as LaunchDaemons, the register and unregister APIs provide a replacement for installing plists in /Library/LaunchDaemons. Apps that contain LaunchDaemons must be notarized.
--
-- Legacy LaunchDaemons installed in /Library/LaunchDaemons will continue to be bootstrapped without explicit approval in System Settings since writing to /Library is protected  with filesystem permissions.
--
-- If an app updates either the plist or the executable for a LaunchAgent or LaunchDaemon, the SMAppService must be re-registered or it may not launch. It is recommended to also call unregister before re-registering if the executable has been changed.
--
-- Generated bindings for @SMAppService@.
module ObjC.ServiceManagement.SMAppService
  ( SMAppService
  , IsSMAppService(..)
  , loginItemServiceWithIdentifier
  , agentServiceWithPlistName
  , daemonServiceWithPlistName
  , registerAndReturnError
  , unregisterAndReturnError
  , unregisterWithCompletionHandler
  , statusForLegacyURL
  , openSystemSettingsLoginItems
  , status
  , loginItemServiceWithIdentifierSelector
  , agentServiceWithPlistNameSelector
  , daemonServiceWithPlistNameSelector
  , registerAndReturnErrorSelector
  , unregisterAndReturnErrorSelector
  , unregisterWithCompletionHandlerSelector
  , statusForLegacyURLSelector
  , openSystemSettingsLoginItemsSelector
  , statusSelector

  -- * Enum types
  , SMAppServiceStatus(SMAppServiceStatus)
  , pattern SMAppServiceStatusNotRegistered
  , pattern SMAppServiceStatusEnabled
  , pattern SMAppServiceStatusRequiresApproval
  , pattern SMAppServiceStatusNotFound

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

import ObjC.ServiceManagement.Internal.Classes
import ObjC.ServiceManagement.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | loginItemServiceWithIdentifier
--
-- Initializes a SMAppService for a LoginItem corresponding to the bundle with the specified identifier.
--
-- @identifier@ — The bundle identifier of the helper application
--
-- The identifier must correspond to the bundle identifier for a LoginItem that lives in the calling app's Contents/Library/LoginItems directory
--
-- ObjC selector: @+ loginItemServiceWithIdentifier:@
loginItemServiceWithIdentifier :: IsNSString identifier => identifier -> IO (Id SMAppService)
loginItemServiceWithIdentifier identifier =
  do
    cls' <- getRequiredClass "SMAppService"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "loginItemServiceWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | agentServiceWithPlistName
--
-- Initializes a SMAppService with a LaunchAgent with the specified plist name.
--
-- @plistName@ — The name of the plist corresponding to the SMAppService.
--
-- The plistName must correspond to a plist in the calling app's Contents/Library/LaunchAgents directory
--
-- In addition to the standard launchd.plist keys, plists registered with SMAppService may use the BundleProgram launchd plist key to specify an app bundle relative path for the executable. This key allows apps to support a user relocating the app bundle after installation.
--
-- ObjC selector: @+ agentServiceWithPlistName:@
agentServiceWithPlistName :: IsNSString plistName => plistName -> IO (Id SMAppService)
agentServiceWithPlistName plistName =
  do
    cls' <- getRequiredClass "SMAppService"
    withObjCPtr plistName $ \raw_plistName ->
      sendClassMsg cls' (mkSelector "agentServiceWithPlistName:") (retPtr retVoid) [argPtr (castPtr raw_plistName :: Ptr ())] >>= retainedObject . castPtr

-- | daemonServiceWithPlistName
--
-- Initializes a SMAppService with a LaunchDaemon with the specified plist name.
--
-- @plistName@ — The name of the plist corresponding to the SMAppService.
--
-- The plistName must correspond to a plist in the calling app's Contents/Library/LaunchDaemons directory
--
-- In addition to the standard launchd.plist keys, plists registered with SMAppService may use the BundleProgram launchd plist key to specify an app bundle relative path for the executable. This key allows apps to support a user relocating the app bundle after installation.
--
-- For a LaunchDaemon to be bootstrapped during boot, the containing application must be accessible before a user logs in. For applications that intend to register LaunchDaemons, it is recommended that the application bundle live in /Applications
--
-- ObjC selector: @+ daemonServiceWithPlistName:@
daemonServiceWithPlistName :: IsNSString plistName => plistName -> IO (Id SMAppService)
daemonServiceWithPlistName plistName =
  do
    cls' <- getRequiredClass "SMAppService"
    withObjCPtr plistName $ \raw_plistName ->
      sendClassMsg cls' (mkSelector "daemonServiceWithPlistName:") (retPtr retVoid) [argPtr (castPtr raw_plistName :: Ptr ())] >>= retainedObject . castPtr

-- | registerAndReturnError
--
-- Registers the service such that it may begin launching subject to user consent
--
-- @error@ — Upon unsuccessful return, a new NSError object describing the error. Upon successful return, this argument is set to NULL. This argument may be NULL.
--
-- Returns: YES if the service was successfully registered, otherwise NO.
--
-- If the service corresponds to a LoginItem bundle, the helper will be started immediately and on subsequent logins. If the helper crashes or exits non-zero it will be relaunched.
--
-- If the service corresponds to the main application, the application will be launched on subsequent logins.
--
-- If the service corresponds to a LaunchAgent, the LaunchAgent is immediately bootstrapped and may begin running. In addition LaunchAgents registered with this API will be bootstrapped on each subsequent login.
--
-- If an app desires to register a LaunchAgent for multiple users, the API must be called once per user while the desired user is running the app. LaunchAgents cannot be registered from outside a user context using this API.
--
-- If the service corresponds to a LaunchDaemon, the LaunchDaemon will not be bootstrapped until an admin approves the LaunchDaemon in System Settings. LaunchDaemons registered with this API and approved by an admin will be bootstrapped on each subsequent boot.
--
-- If the service is already registered, this API will return error kSMErrorAlreadyRegistered
--
-- If the service is not approved by the user, this API will return error kSMErrorLaunchDeniedByUser
--
-- If the app bundle is not properly code signed, this API will return error kSMErrorInvalidSignature
--
-- See: SMAppService:unregisterAndReturnError
--
-- ObjC selector: @- registerAndReturnError:@
registerAndReturnError :: (IsSMAppService smAppService, IsNSError error_) => smAppService -> error_ -> IO Bool
registerAndReturnError smAppService  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg smAppService (mkSelector "registerAndReturnError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | unregisterAndReturnError
--
-- Unregisters the service such that it will no longer be launched by the system.
--
-- @error@ — Upon unsuccessful return, a new NSError object describing the error. Upon successful return, this argument is set to NULL. This argument may be NULL.
--
-- Returns: YES if the service was successfully unregistered, otherwise NO.
--
-- If the service corresponds to a LoginItem, LaunchAgent, or LaunchDaemon and the service is currently running it will be killed. The unregister call will not wait for the service to be reaped.
--
-- If the service corresponds to the main application, it will continue running, but will still be unregistered to prevent future launches at login.
--
-- This is the opposite operation of SMAppService:register
--
-- If the service is already unregistered, this API will return error kSMErrorJobNotFound
--
-- See: SMAppService:registerAndReturnError
--
-- ObjC selector: @- unregisterAndReturnError:@
unregisterAndReturnError :: (IsSMAppService smAppService, IsNSError error_) => smAppService -> error_ -> IO Bool
unregisterAndReturnError smAppService  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg smAppService (mkSelector "unregisterAndReturnError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | unregisterWithCompletionHandler
--
-- Unregisters the service such that it will no longer be launched by the system.
--
-- @handler@ — The completion handler block to be invoked with the result of the unregister operation. This handler will be invoked on libdispatch's default target queue
--
-- If the service corresponds to a LoginItem, LaunchAgent, or LaunchDaemon and the service is currently running it will be killed. The unregister call will not wait for the service to be killed and will return promptly. The completion handler will be invoked after the running process has been killed if successful or will be invoked whenever an error occurs. After the completion handler has been invoked it is safe to re-register the service.
--
-- If the service corresponds to the main application, it will continue running, but will still be unregistered to prevent future launches at login.
--
-- This is the opposite operation of SMAppService:register
--
-- This is the asynchronous variant of SMAppService:unregisterAndReturnError
--
-- If the service is already unregistered, this API will return error kSMErrorJobNotFound
--
-- See: SMAppService:unregisterAndReturnError
--
-- ObjC selector: @- unregisterWithCompletionHandler:@
unregisterWithCompletionHandler :: IsSMAppService smAppService => smAppService -> Ptr () -> IO ()
unregisterWithCompletionHandler smAppService  handler =
  sendMsg smAppService (mkSelector "unregisterWithCompletionHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | @+ statusForLegacyURL:@
statusForLegacyURL :: IsNSURL url => url -> IO SMAppServiceStatus
statusForLegacyURL url =
  do
    cls' <- getRequiredClass "SMAppService"
    withObjCPtr url $ \raw_url ->
      fmap (coerce :: CLong -> SMAppServiceStatus) $ sendClassMsg cls' (mkSelector "statusForLegacyURL:") retCLong [argPtr (castPtr raw_url :: Ptr ())]

-- | openSystemSettingsLoginItems
--
-- Opens System Settings to the Login Items panel
--
-- This API is intended for apps to call whenever they present a prompt to the user and the user confirms that they want to enable the app's helpers again. The app can call this API to help the user navigate to the appropriate panel in System Settings.
--
-- ObjC selector: @+ openSystemSettingsLoginItems@
openSystemSettingsLoginItems :: IO ()
openSystemSettingsLoginItems  =
  do
    cls' <- getRequiredClass "SMAppService"
    sendClassMsg cls' (mkSelector "openSystemSettingsLoginItems") retVoid []

-- | status
--
-- Returns the status for the service
--
-- The status API can be used to check what selection a user has made regarding allowing the service to launch.
--
-- If the user has denied execution, the return value will be SMAppServiceRequiresApproval.
--
-- If the service has been unregistered, the return value will be SMAppServiceNotRegistered
--
-- ObjC selector: @- status@
status :: IsSMAppService smAppService => smAppService -> IO SMAppServiceStatus
status smAppService  =
  fmap (coerce :: CLong -> SMAppServiceStatus) $ sendMsg smAppService (mkSelector "status") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loginItemServiceWithIdentifier:@
loginItemServiceWithIdentifierSelector :: Selector
loginItemServiceWithIdentifierSelector = mkSelector "loginItemServiceWithIdentifier:"

-- | @Selector@ for @agentServiceWithPlistName:@
agentServiceWithPlistNameSelector :: Selector
agentServiceWithPlistNameSelector = mkSelector "agentServiceWithPlistName:"

-- | @Selector@ for @daemonServiceWithPlistName:@
daemonServiceWithPlistNameSelector :: Selector
daemonServiceWithPlistNameSelector = mkSelector "daemonServiceWithPlistName:"

-- | @Selector@ for @registerAndReturnError:@
registerAndReturnErrorSelector :: Selector
registerAndReturnErrorSelector = mkSelector "registerAndReturnError:"

-- | @Selector@ for @unregisterAndReturnError:@
unregisterAndReturnErrorSelector :: Selector
unregisterAndReturnErrorSelector = mkSelector "unregisterAndReturnError:"

-- | @Selector@ for @unregisterWithCompletionHandler:@
unregisterWithCompletionHandlerSelector :: Selector
unregisterWithCompletionHandlerSelector = mkSelector "unregisterWithCompletionHandler:"

-- | @Selector@ for @statusForLegacyURL:@
statusForLegacyURLSelector :: Selector
statusForLegacyURLSelector = mkSelector "statusForLegacyURL:"

-- | @Selector@ for @openSystemSettingsLoginItems@
openSystemSettingsLoginItemsSelector :: Selector
openSystemSettingsLoginItemsSelector = mkSelector "openSystemSettingsLoginItems"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

