{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An abstract base class for effects that can be applied to both NSImageViews and UIImageViews that have symbol-based images.
--
-- Don't use this class directly, instead use any of the concrete subclasses.
--
-- Generated bindings for @NSSymbolEffect@.
module ObjC.AppKit.NSSymbolEffect
  ( NSSymbolEffect
  , IsNSSymbolEffect(..)
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
new :: IO (Id NSSymbolEffect)
new  =
  do
    cls' <- getRequiredClass "NSSymbolEffect"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSSymbolEffect nsSymbolEffect => nsSymbolEffect -> IO (Id NSSymbolEffect)
init_ nsSymbolEffect  =
  sendMsg nsSymbolEffect (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

