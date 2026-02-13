{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationSingleSignOnRequest@.
module ObjC.AuthenticationServices.ASAuthorizationSingleSignOnRequest
  ( ASAuthorizationSingleSignOnRequest
  , IsASAuthorizationSingleSignOnRequest(..)
  , authorizationOptions
  , setAuthorizationOptions
  , userInterfaceEnabled
  , setUserInterfaceEnabled
  , authorizationOptionsSelector
  , setAuthorizationOptionsSelector
  , setUserInterfaceEnabledSelector
  , userInterfaceEnabledSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Parameters required by the specific Authorization Server which should be used by the selected Authorization Services extension for authorization.
--
-- ObjC selector: @- authorizationOptions@
authorizationOptions :: IsASAuthorizationSingleSignOnRequest asAuthorizationSingleSignOnRequest => asAuthorizationSingleSignOnRequest -> IO (Id NSArray)
authorizationOptions asAuthorizationSingleSignOnRequest =
  sendMessage asAuthorizationSingleSignOnRequest authorizationOptionsSelector

-- | Parameters required by the specific Authorization Server which should be used by the selected Authorization Services extension for authorization.
--
-- ObjC selector: @- setAuthorizationOptions:@
setAuthorizationOptions :: (IsASAuthorizationSingleSignOnRequest asAuthorizationSingleSignOnRequest, IsNSArray value) => asAuthorizationSingleSignOnRequest -> value -> IO ()
setAuthorizationOptions asAuthorizationSingleSignOnRequest value =
  sendMessage asAuthorizationSingleSignOnRequest setAuthorizationOptionsSelector (toNSArray value)

-- | Enables or disables the authorization user interface.
--
-- The default values is YES. If user interface is not enabled, then the authorization will fail with
--
-- See: ASAuthorizationErrorNotInteractive if it attempts to display the authorization user interface.
--
-- ObjC selector: @- userInterfaceEnabled@
userInterfaceEnabled :: IsASAuthorizationSingleSignOnRequest asAuthorizationSingleSignOnRequest => asAuthorizationSingleSignOnRequest -> IO Bool
userInterfaceEnabled asAuthorizationSingleSignOnRequest =
  sendMessage asAuthorizationSingleSignOnRequest userInterfaceEnabledSelector

-- | Enables or disables the authorization user interface.
--
-- The default values is YES. If user interface is not enabled, then the authorization will fail with
--
-- See: ASAuthorizationErrorNotInteractive if it attempts to display the authorization user interface.
--
-- ObjC selector: @- setUserInterfaceEnabled:@
setUserInterfaceEnabled :: IsASAuthorizationSingleSignOnRequest asAuthorizationSingleSignOnRequest => asAuthorizationSingleSignOnRequest -> Bool -> IO ()
setUserInterfaceEnabled asAuthorizationSingleSignOnRequest value =
  sendMessage asAuthorizationSingleSignOnRequest setUserInterfaceEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authorizationOptions@
authorizationOptionsSelector :: Selector '[] (Id NSArray)
authorizationOptionsSelector = mkSelector "authorizationOptions"

-- | @Selector@ for @setAuthorizationOptions:@
setAuthorizationOptionsSelector :: Selector '[Id NSArray] ()
setAuthorizationOptionsSelector = mkSelector "setAuthorizationOptions:"

-- | @Selector@ for @userInterfaceEnabled@
userInterfaceEnabledSelector :: Selector '[] Bool
userInterfaceEnabledSelector = mkSelector "userInterfaceEnabled"

-- | @Selector@ for @setUserInterfaceEnabled:@
setUserInterfaceEnabledSelector :: Selector '[Bool] ()
setUserInterfaceEnabledSelector = mkSelector "setUserInterfaceEnabled:"

