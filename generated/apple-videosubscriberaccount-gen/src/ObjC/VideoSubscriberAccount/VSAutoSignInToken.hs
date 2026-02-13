{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @VSAutoSignInToken@.
module ObjC.VideoSubscriberAccount.VSAutoSignInToken
  ( VSAutoSignInToken
  , IsVSAutoSignInToken(..)
  , init_
  , new
  , authorization
  , value
  , authorizationSelector
  , initSelector
  , newSelector
  , valueSelector

  -- * Enum types
  , VSAutoSignInAuthorization(VSAutoSignInAuthorization)
  , pattern VSAutoSignInAuthorizationNotDetermined
  , pattern VSAutoSignInAuthorizationGranted
  , pattern VSAutoSignInAuthorizationDenied

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.VideoSubscriberAccount.Internal.Classes
import ObjC.VideoSubscriberAccount.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVSAutoSignInToken vsAutoSignInToken => vsAutoSignInToken -> IO (Id VSAutoSignInToken)
init_ vsAutoSignInToken =
  sendOwnedMessage vsAutoSignInToken initSelector

-- | @+ new@
new :: IO (Id VSAutoSignInToken)
new  =
  do
    cls' <- getRequiredClass "VSAutoSignInToken"
    sendOwnedClassMessage cls' newSelector

-- | @- authorization@
authorization :: IsVSAutoSignInToken vsAutoSignInToken => vsAutoSignInToken -> IO VSAutoSignInAuthorization
authorization vsAutoSignInToken =
  sendMessage vsAutoSignInToken authorizationSelector

-- | @- value@
value :: IsVSAutoSignInToken vsAutoSignInToken => vsAutoSignInToken -> IO (Id NSString)
value vsAutoSignInToken =
  sendMessage vsAutoSignInToken valueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VSAutoSignInToken)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VSAutoSignInToken)
newSelector = mkSelector "new"

-- | @Selector@ for @authorization@
authorizationSelector :: Selector '[] VSAutoSignInAuthorization
authorizationSelector = mkSelector "authorization"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSString)
valueSelector = mkSelector "value"

