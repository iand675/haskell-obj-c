{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMIOExtensionClient@.
module ObjC.CoreMediaIO.CMIOExtensionClient
  ( CMIOExtensionClient
  , IsCMIOExtensionClient(..)
  , init_
  , new
  , clientID
  , pid
  , initSelector
  , newSelector
  , clientIDSelector
  , pidSelector


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

import ObjC.CoreMediaIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCMIOExtensionClient cmioExtensionClient => cmioExtensionClient -> IO (Id CMIOExtensionClient)
init_ cmioExtensionClient  =
  sendMsg cmioExtensionClient (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CMIOExtensionClient)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionClient"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | clientID
--
-- The client unique identifier.
--
-- ObjC selector: @- clientID@
clientID :: IsCMIOExtensionClient cmioExtensionClient => cmioExtensionClient -> IO (Id NSUUID)
clientID cmioExtensionClient  =
  sendMsg cmioExtensionClient (mkSelector "clientID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | pid
--
-- The pid of the client application.
--
-- ObjC selector: @- pid@
pid :: IsCMIOExtensionClient cmioExtensionClient => cmioExtensionClient -> IO CInt
pid cmioExtensionClient  =
  sendMsg cmioExtensionClient (mkSelector "pid") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @clientID@
clientIDSelector :: Selector
clientIDSelector = mkSelector "clientID"

-- | @Selector@ for @pid@
pidSelector :: Selector
pidSelector = mkSelector "pid"

