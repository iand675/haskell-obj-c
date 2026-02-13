{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generic secret
--
-- Generated bindings for @LASecret@.
module ObjC.LocalAuthentication.LASecret
  ( LASecret
  , IsLASecret(..)
  , loadDataWithCompletion
  , new
  , init_
  , initSelector
  , loadDataWithCompletionSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Fetch stored data if any
--
-- @handler@ — Completion handler invoked with a generic secret stored along with the right or an error if no secret is found or the fetch operation fails.
--
-- ObjC selector: @- loadDataWithCompletion:@
loadDataWithCompletion :: IsLASecret laSecret => laSecret -> Ptr () -> IO ()
loadDataWithCompletion laSecret handler =
  sendMessage laSecret loadDataWithCompletionSelector handler

-- | Clients cannot create @LASecret@ instances directly. They typically obtain them from a @LAPersistedRight@ instance.
--
-- ObjC selector: @+ new@
new :: IO (Id LASecret)
new  =
  do
    cls' <- getRequiredClass "LASecret"
    sendOwnedClassMessage cls' newSelector

-- | Clients cannot create @LASecret@ instances directly. They typically obtain them from a @LAPersistedRight@ instance.
--
-- ObjC selector: @- init@
init_ :: IsLASecret laSecret => laSecret -> IO (Id LASecret)
init_ laSecret =
  sendOwnedMessage laSecret initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadDataWithCompletion:@
loadDataWithCompletionSelector :: Selector '[Ptr ()] ()
loadDataWithCompletionSelector = mkSelector "loadDataWithCompletion:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id LASecret)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id LASecret)
initSelector = mkSelector "init"

