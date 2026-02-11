{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents an in-flight request to an account manger.
--
-- Generated bindings for @VSAccountManagerResult@.
module ObjC.VideoSubscriberAccount.VSAccountManagerResult
  ( VSAccountManagerResult
  , IsVSAccountManagerResult(..)
  , init_
  , new
  , cancel
  , initSelector
  , newSelector
  , cancelSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVSAccountManagerResult vsAccountManagerResult => vsAccountManagerResult -> IO (Id VSAccountManagerResult)
init_ vsAccountManagerResult  =
  sendMsg vsAccountManagerResult (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id VSAccountManagerResult)
new  =
  do
    cls' <- getRequiredClass "VSAccountManagerResult"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Advise the account manager that the app no longer needs the requested work to be done.
--
-- ObjC selector: @- cancel@
cancel :: IsVSAccountManagerResult vsAccountManagerResult => vsAccountManagerResult -> IO ()
cancel vsAccountManagerResult  =
  sendMsg vsAccountManagerResult (mkSelector "cancel") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

