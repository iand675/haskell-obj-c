{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ layoutManager@
layoutManager :: IO (Id CAConstraintLayoutManager)
layoutManager  =
  do
    cls' <- getRequiredClass "CAConstraintLayoutManager"
    sendClassMessage cls' layoutManagerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layoutManager@
layoutManagerSelector :: Selector '[] (Id CAConstraintLayoutManager)
layoutManagerSelector = mkSelector "layoutManager"

