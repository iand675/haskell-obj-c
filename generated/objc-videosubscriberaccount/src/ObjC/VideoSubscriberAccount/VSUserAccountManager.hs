{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @VSUserAccountManager@.
module ObjC.VideoSubscriberAccount.VSUserAccountManager
  ( VSUserAccountManager
  , IsVSUserAccountManager(..)
  , updateUserAccount_completion
  , queryAutoSignInTokenWithCompletionHandler
  , deleteAutoSignInTokenWithCompletionHandler
  , sharedUserAccountManager
  , updateUserAccount_completionSelector
  , queryAutoSignInTokenWithCompletionHandlerSelector
  , deleteAutoSignInTokenWithCompletionHandlerSelector
  , sharedUserAccountManagerSelector

  -- * Enum types
  , VSUserAccountQueryOptions(VSUserAccountQueryOptions)
  , pattern VSUserAccountQueryNone
  , pattern VSUserAccountQueryAllDevices

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

import ObjC.VideoSubscriberAccount.Internal.Classes
import ObjC.VideoSubscriberAccount.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- updateUserAccount:completion:@
updateUserAccount_completion :: (IsVSUserAccountManager vsUserAccountManager, IsVSUserAccount account) => vsUserAccountManager -> account -> Ptr () -> IO ()
updateUserAccount_completion vsUserAccountManager  account completion =
withObjCPtr account $ \raw_account ->
    sendMsg vsUserAccountManager (mkSelector "updateUserAccount:completion:") retVoid [argPtr (castPtr raw_account :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Query the auto sign in token and authorization state.
--
-- ObjC selector: @- queryAutoSignInTokenWithCompletionHandler:@
queryAutoSignInTokenWithCompletionHandler :: IsVSUserAccountManager vsUserAccountManager => vsUserAccountManager -> Ptr () -> IO ()
queryAutoSignInTokenWithCompletionHandler vsUserAccountManager  completion =
  sendMsg vsUserAccountManager (mkSelector "queryAutoSignInTokenWithCompletionHandler:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Deletes the auto sign in token.
--
-- ObjC selector: @- deleteAutoSignInTokenWithCompletionHandler:@
deleteAutoSignInTokenWithCompletionHandler :: IsVSUserAccountManager vsUserAccountManager => vsUserAccountManager -> Ptr () -> IO ()
deleteAutoSignInTokenWithCompletionHandler vsUserAccountManager  completion =
  sendMsg vsUserAccountManager (mkSelector "deleteAutoSignInTokenWithCompletionHandler:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @+ sharedUserAccountManager@
sharedUserAccountManager :: IO (Id VSUserAccountManager)
sharedUserAccountManager  =
  do
    cls' <- getRequiredClass "VSUserAccountManager"
    sendClassMsg cls' (mkSelector "sharedUserAccountManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateUserAccount:completion:@
updateUserAccount_completionSelector :: Selector
updateUserAccount_completionSelector = mkSelector "updateUserAccount:completion:"

-- | @Selector@ for @queryAutoSignInTokenWithCompletionHandler:@
queryAutoSignInTokenWithCompletionHandlerSelector :: Selector
queryAutoSignInTokenWithCompletionHandlerSelector = mkSelector "queryAutoSignInTokenWithCompletionHandler:"

-- | @Selector@ for @deleteAutoSignInTokenWithCompletionHandler:@
deleteAutoSignInTokenWithCompletionHandlerSelector :: Selector
deleteAutoSignInTokenWithCompletionHandlerSelector = mkSelector "deleteAutoSignInTokenWithCompletionHandler:"

-- | @Selector@ for @sharedUserAccountManager@
sharedUserAccountManagerSelector :: Selector
sharedUserAccountManagerSelector = mkSelector "sharedUserAccountManager"

