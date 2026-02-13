{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @EPDeveloperTool@.
module ObjC.ExecutionPolicy.EPDeveloperTool
  ( EPDeveloperTool
  , IsEPDeveloperTool(..)
  , init_
  , requestDeveloperToolAccessWithCompletionHandler
  , authorizationStatus
  , authorizationStatusSelector
  , initSelector
  , requestDeveloperToolAccessWithCompletionHandlerSelector

  -- * Enum types
  , EPDeveloperToolStatus(EPDeveloperToolStatus)
  , pattern EPDeveloperToolStatusNotDetermined
  , pattern EPDeveloperToolStatusRestricted
  , pattern EPDeveloperToolStatusDenied
  , pattern EPDeveloperToolStatusAuthorized

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ExecutionPolicy.Internal.Classes
import ObjC.ExecutionPolicy.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initializes the object to manage the lifetime of the XPC connection.
--
-- The XPC connection remains for the lifecycle of the object and deallocation is required to trigger the teardown of the XPC connection.
--
-- ObjC selector: @- init@
init_ :: IsEPDeveloperTool epDeveloperTool => epDeveloperTool -> IO (Id EPDeveloperTool)
init_ epDeveloperTool =
  sendOwnedMessage epDeveloperTool initSelector

-- | Checks whether developer tool privileges are already available and if not  populates an entry in Settings for user approval.
--
-- This method does not show any UI to the user or guide them towards Settings for approval, if necessary.
--
-- - Parameter handler: A block called asynchronously with whether the privilege is available.
--
-- > New info > Concurrency Note: You can call this method from synchronous code using a completion handler, > as shown on this page, or you can call it as an asynchronous method that has the > following declaration: > > ```swift > func requestAccess() async -> Bool > ``` > > For information about concurrency and asynchronous code in Swift, see <doc://com.apple.documentation/documentation/swift/calling-objective-c-apis-asynchronously>.
--
-- ObjC selector: @- requestDeveloperToolAccessWithCompletionHandler:@
requestDeveloperToolAccessWithCompletionHandler :: IsEPDeveloperTool epDeveloperTool => epDeveloperTool -> Ptr () -> IO ()
requestDeveloperToolAccessWithCompletionHandler epDeveloperTool handler =
  sendMessage epDeveloperTool requestDeveloperToolAccessWithCompletionHandlerSelector handler

-- | The current authorization status of the current process. - Returns: An EPDeveloperToolStatus indicating whether the current process has developer tool privileges.
--
-- ObjC selector: @- authorizationStatus@
authorizationStatus :: IsEPDeveloperTool epDeveloperTool => epDeveloperTool -> IO EPDeveloperToolStatus
authorizationStatus epDeveloperTool =
  sendMessage epDeveloperTool authorizationStatusSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id EPDeveloperTool)
initSelector = mkSelector "init"

-- | @Selector@ for @requestDeveloperToolAccessWithCompletionHandler:@
requestDeveloperToolAccessWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
requestDeveloperToolAccessWithCompletionHandlerSelector = mkSelector "requestDeveloperToolAccessWithCompletionHandler:"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector '[] EPDeveloperToolStatus
authorizationStatusSelector = mkSelector "authorizationStatus"

