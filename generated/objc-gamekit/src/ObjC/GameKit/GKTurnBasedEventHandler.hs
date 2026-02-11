{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKTurnBasedEventHandler@.
module ObjC.GameKit.GKTurnBasedEventHandler
  ( GKTurnBasedEventHandler
  , IsGKTurnBasedEventHandler(..)
  , sharedTurnBasedEventHandler
  , sharedTurnBasedEventHandlerSelector


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

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sharedTurnBasedEventHandler@
sharedTurnBasedEventHandler :: IO (Id GKTurnBasedEventHandler)
sharedTurnBasedEventHandler  =
  do
    cls' <- getRequiredClass "GKTurnBasedEventHandler"
    sendClassMsg cls' (mkSelector "sharedTurnBasedEventHandler") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedTurnBasedEventHandler@
sharedTurnBasedEventHandlerSelector :: Selector
sharedTurnBasedEventHandlerSelector = mkSelector "sharedTurnBasedEventHandler"

