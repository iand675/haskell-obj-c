{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , authorizationSelector
  , valueSelector

  -- * Enum types
  , VSAutoSignInAuthorization(VSAutoSignInAuthorization)
  , pattern VSAutoSignInAuthorizationNotDetermined
  , pattern VSAutoSignInAuthorizationGranted
  , pattern VSAutoSignInAuthorizationDenied

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

-- | @- init@
init_ :: IsVSAutoSignInToken vsAutoSignInToken => vsAutoSignInToken -> IO (Id VSAutoSignInToken)
init_ vsAutoSignInToken  =
  sendMsg vsAutoSignInToken (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id VSAutoSignInToken)
new  =
  do
    cls' <- getRequiredClass "VSAutoSignInToken"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- authorization@
authorization :: IsVSAutoSignInToken vsAutoSignInToken => vsAutoSignInToken -> IO VSAutoSignInAuthorization
authorization vsAutoSignInToken  =
  fmap (coerce :: CLong -> VSAutoSignInAuthorization) $ sendMsg vsAutoSignInToken (mkSelector "authorization") retCLong []

-- | @- value@
value :: IsVSAutoSignInToken vsAutoSignInToken => vsAutoSignInToken -> IO (Id NSString)
value vsAutoSignInToken  =
  sendMsg vsAutoSignInToken (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @authorization@
authorizationSelector :: Selector
authorizationSelector = mkSelector "authorization"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

