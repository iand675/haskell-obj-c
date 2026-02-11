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

import ObjC.FSKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsFSClient fsClient => fsClient -> IO (Id FSClient)
init_ fsClient  =
    sendMsg fsClient (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The shared instance of the FSKit client class.
--
-- ObjC selector: @+ sharedInstance@
sharedInstance :: IO (Id FSClient)
sharedInstance  =
  do
    cls' <- getRequiredClass "FSClient"
    sendClassMsg cls' (mkSelector "sharedInstance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @sharedInstance@
sharedInstanceSelector :: Selector
sharedInstanceSelector = mkSelector "sharedInstance"

