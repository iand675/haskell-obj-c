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
  , loadDataWithCompletionSelector
  , newSelector
  , initSelector


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

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Fetch stored data if any
--
-- @handler@ — Completion handler invoked with a generic secret stored along with the right or an error if no secret is found or the fetch operation fails.
--
-- ObjC selector: @- loadDataWithCompletion:@
loadDataWithCompletion :: IsLASecret laSecret => laSecret -> Ptr () -> IO ()
loadDataWithCompletion laSecret  handler =
  sendMsg laSecret (mkSelector "loadDataWithCompletion:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | Clients cannot create @LASecret@ instances directly. They typically obtain them from a @LAPersistedRight@ instance.
--
-- ObjC selector: @+ new@
new :: IO (Id LASecret)
new  =
  do
    cls' <- getRequiredClass "LASecret"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Clients cannot create @LASecret@ instances directly. They typically obtain them from a @LAPersistedRight@ instance.
--
-- ObjC selector: @- init@
init_ :: IsLASecret laSecret => laSecret -> IO (Id LASecret)
init_ laSecret  =
  sendMsg laSecret (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadDataWithCompletion:@
loadDataWithCompletionSelector :: Selector
loadDataWithCompletionSelector = mkSelector "loadDataWithCompletion:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

