{-# LANGUAGE DataKinds #-}
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
  , signingID
  , pid
  , clientIDSelector
  , initSelector
  , newSelector
  , pidSelector
  , signingIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMediaIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCMIOExtensionClient cmioExtensionClient => cmioExtensionClient -> IO (Id CMIOExtensionClient)
init_ cmioExtensionClient =
  sendOwnedMessage cmioExtensionClient initSelector

-- | @+ new@
new :: IO (Id CMIOExtensionClient)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionClient"
    sendOwnedClassMessage cls' newSelector

-- | clientID
--
-- The client unique identifier.
--
-- ObjC selector: @- clientID@
clientID :: IsCMIOExtensionClient cmioExtensionClient => cmioExtensionClient -> IO (Id NSUUID)
clientID cmioExtensionClient =
  sendMessage cmioExtensionClient clientIDSelector

-- | signingID
--
-- The client's signing identifier.
--
-- ObjC selector: @- signingID@
signingID :: IsCMIOExtensionClient cmioExtensionClient => cmioExtensionClient -> IO (Id NSString)
signingID cmioExtensionClient =
  sendMessage cmioExtensionClient signingIDSelector

-- | pid
--
-- The pid of the client application.
--
-- ObjC selector: @- pid@
pid :: IsCMIOExtensionClient cmioExtensionClient => cmioExtensionClient -> IO CInt
pid cmioExtensionClient =
  sendMessage cmioExtensionClient pidSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CMIOExtensionClient)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CMIOExtensionClient)
newSelector = mkSelector "new"

-- | @Selector@ for @clientID@
clientIDSelector :: Selector '[] (Id NSUUID)
clientIDSelector = mkSelector "clientID"

-- | @Selector@ for @signingID@
signingIDSelector :: Selector '[] (Id NSString)
signingIDSelector = mkSelector "signingID"

-- | @Selector@ for @pid@
pidSelector :: Selector '[] CInt
pidSelector = mkSelector "pid"

