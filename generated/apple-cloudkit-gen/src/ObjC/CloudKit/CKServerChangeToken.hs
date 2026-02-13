{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKServerChangeToken@.
module ObjC.CloudKit.CKServerChangeToken
  ( CKServerChangeToken
  , IsCKServerChangeToken(..)
  , init_
  , new
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKServerChangeToken ckServerChangeToken => ckServerChangeToken -> IO (Id CKServerChangeToken)
init_ ckServerChangeToken =
  sendOwnedMessage ckServerChangeToken initSelector

-- | @+ new@
new :: IO (Id CKServerChangeToken)
new  =
  do
    cls' <- getRequiredClass "CKServerChangeToken"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKServerChangeToken)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKServerChangeToken)
newSelector = mkSelector "new"

