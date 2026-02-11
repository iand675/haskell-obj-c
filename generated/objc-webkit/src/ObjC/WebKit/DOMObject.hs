{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMObject@.
module ObjC.WebKit.DOMObject
  ( DOMObject
  , IsDOMObject(..)
  , init_
  , sheet
  , initSelector
  , sheetSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsDOMObject domObject => domObject -> IO (Id DOMObject)
init_ domObject  =
  sendMsg domObject (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- sheet@
sheet :: IsDOMObject domObject => domObject -> IO (Id DOMStyleSheet)
sheet domObject  =
  sendMsg domObject (mkSelector "sheet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @sheet@
sheetSelector :: Selector
sheetSelector = mkSelector "sheet"

