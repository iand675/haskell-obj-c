{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An interface for apps and daemons to interact with FSKit.
--
-- FSClient is the primary management interface for FSKit. Use this class to discover FSKit extensions installed on the system, including your own.
--
-- > Important: Don't subclass @FSClient@.
--
-- Generated bindings for @FSClient@.
module ObjC.FSKit.FSClient
  ( FSClient
  , IsFSClient(..)
  , init_
  , sharedInstance
  , initSelector
  , sharedInstanceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FSKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsFSClient fsClient => fsClient -> IO (Id FSClient)
init_ fsClient =
  sendOwnedMessage fsClient initSelector

-- | The shared instance of the FSKit client class.
--
-- ObjC selector: @+ sharedInstance@
sharedInstance :: IO (Id FSClient)
sharedInstance  =
  do
    cls' <- getRequiredClass "FSClient"
    sendClassMessage cls' sharedInstanceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id FSClient)
initSelector = mkSelector "init"

-- | @Selector@ for @sharedInstance@
sharedInstanceSelector :: Selector '[] (Id FSClient)
sharedInstanceSelector = mkSelector "sharedInstance"

