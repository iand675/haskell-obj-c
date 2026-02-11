{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXRequest@.
module ObjC.Accessibility.AXRequest
  ( AXRequest
  , IsAXRequest(..)
  , init_
  , new
  , currentRequest
  , initSelector
  , newSelector
  , currentRequestSelector


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

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAXRequest axRequest => axRequest -> IO (Id AXRequest)
init_ axRequest  =
    sendMsg axRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AXRequest)
new  =
  do
    cls' <- getRequiredClass "AXRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ currentRequest@
currentRequest :: IO (Id AXRequest)
currentRequest  =
  do
    cls' <- getRequiredClass "AXRequest"
    sendClassMsg cls' (mkSelector "currentRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @currentRequest@
currentRequestSelector :: Selector
currentRequestSelector = mkSelector "currentRequest"

