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
  , userInterfaceEnabledSelector
  , setUserInterfaceEnabledSelector


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

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Parameters required by the specific Authorization Server which should be used by the selected Authorization Services extension for authorization.
--
-- ObjC selector: @- authorizationOptions@
authorizationOptions :: IsASAuthorizationSingleSignOnRequest asAuthorizationSingleSignOnRequest => asAuthorizationSingleSignOnRequest -> IO (Id NSArray)
authorizationOptions asAuthorizationSingleSignOnRequest  =
  sendMsg asAuthorizationSingleSignOnRequest (mkSelector "authorizationOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Parameters required by the specific Authorization Server which should be used by the selected Authorization Services extension for authorization.
--
-- ObjC selector: @- setAuthorizationOptions:@
setAuthorizationOptions :: (IsASAuthorizationSingleSignOnRequest asAuthorizationSingleSignOnRequest, IsNSArray value) => asAuthorizationSingleSignOnRequest -> value -> IO ()
setAuthorizationOptions asAuthorizationSingleSignOnRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationSingleSignOnRequest (mkSelector "setAuthorizationOptions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Enables or disables the authorization user interface.
--
-- The default values is YES. If user interface is not enabled, then the authorization will fail with
--
-- See: ASAuthorizationErrorNotInteractive if it attempts to display the authorization user interface.
--
-- ObjC selector: @- userInterfaceEnabled@
userInterfaceEnabled :: IsASAuthorizationSingleSignOnRequest asAuthorizationSingleSignOnRequest => asAuthorizationSingleSignOnRequest -> IO Bool
userInterfaceEnabled asAuthorizationSingleSignOnRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationSingleSignOnRequest (mkSelector "userInterfaceEnabled") retCULong []

-- | Enables or disables the authorization user interface.
--
-- The default values is YES. If user interface is not enabled, then the authorization will fail with
--
-- See: ASAuthorizationErrorNotInteractive if it attempts to display the authorization user interface.
--
-- ObjC selector: @- setUserInterfaceEnabled:@
setUserInterfaceEnabled :: IsASAuthorizationSingleSignOnRequest asAuthorizationSingleSignOnRequest => asAuthorizationSingleSignOnRequest -> Bool -> IO ()
setUserInterfaceEnabled asAuthorizationSingleSignOnRequest  value =
  sendMsg asAuthorizationSingleSignOnRequest (mkSelector "setUserInterfaceEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authorizationOptions@
authorizationOptionsSelector :: Selector
authorizationOptionsSelector = mkSelector "authorizationOptions"

-- | @Selector@ for @setAuthorizationOptions:@
setAuthorizationOptionsSelector :: Selector
setAuthorizationOptionsSelector = mkSelector "setAuthorizationOptions:"

-- | @Selector@ for @userInterfaceEnabled@
userInterfaceEnabledSelector :: Selector
userInterfaceEnabledSelector = mkSelector "userInterfaceEnabled"

-- | @Selector@ for @setUserInterfaceEnabled:@
setUserInterfaceEnabledSelector :: Selector
setUserInterfaceEnabledSelector = mkSelector "setUserInterfaceEnabled:"

