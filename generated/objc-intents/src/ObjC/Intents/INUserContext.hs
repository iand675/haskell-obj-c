{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INUserContext@.
module ObjC.Intents.INUserContext
  ( INUserContext
  , IsINUserContext(..)
  , init_
  , becomeCurrent
  , initSelector
  , becomeCurrentSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINUserContext inUserContext => inUserContext -> IO (Id INUserContext)
init_ inUserContext  =
  sendMsg inUserContext (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Each call replaces previous context object of the same underlying type. Use this only in your iOS/watchOS app. Do not try to set the user context as current from your Intents extension or Intents UI extension.
--
-- ObjC selector: @- becomeCurrent@
becomeCurrent :: IsINUserContext inUserContext => inUserContext -> IO ()
becomeCurrent inUserContext  =
  sendMsg inUserContext (mkSelector "becomeCurrent") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @becomeCurrent@
becomeCurrentSelector :: Selector
becomeCurrentSelector = mkSelector "becomeCurrent"

