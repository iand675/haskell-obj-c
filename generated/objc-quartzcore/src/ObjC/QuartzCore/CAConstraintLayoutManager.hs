{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The constraint-based layout manager. *
--
-- Generated bindings for @CAConstraintLayoutManager@.
module ObjC.QuartzCore.CAConstraintLayoutManager
  ( CAConstraintLayoutManager
  , IsCAConstraintLayoutManager(..)
  , layoutManager
  , layoutManagerSelector


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

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ layoutManager@
layoutManager :: IO (Id CAConstraintLayoutManager)
layoutManager  =
  do
    cls' <- getRequiredClass "CAConstraintLayoutManager"
    sendClassMsg cls' (mkSelector "layoutManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layoutManager@
layoutManagerSelector :: Selector
layoutManagerSelector = mkSelector "layoutManager"

