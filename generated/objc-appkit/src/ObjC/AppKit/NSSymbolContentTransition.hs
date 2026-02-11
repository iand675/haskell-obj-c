{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An abstract base class for transitions that can be applied to both NSImageViews and UIImageViews that have symbol-based images.
--
-- Don't use this class directly, instead use any of the concrete subclasses.
--
-- Generated bindings for @NSSymbolContentTransition@.
module ObjC.AppKit.NSSymbolContentTransition
  ( NSSymbolContentTransition
  , IsNSSymbolContentTransition(..)
  , new
  , init_
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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id NSSymbolContentTransition)
new  =
  do
    cls' <- getRequiredClass "NSSymbolContentTransition"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSSymbolContentTransition nsSymbolContentTransition => nsSymbolContentTransition -> IO (Id NSSymbolContentTransition)
init_ nsSymbolContentTransition  =
  sendMsg nsSymbolContentTransition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

