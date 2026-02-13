{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A lasso tool for selecting parts of a drawing.
--
-- Generated bindings for @PKLassoTool@.
module ObjC.PencilKit.PKLassoTool
  ( PKLassoTool
  , IsPKLassoTool(..)
  , init_
  , initSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PencilKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPKLassoTool pkLassoTool => pkLassoTool -> IO (Id PKLassoTool)
init_ pkLassoTool =
  sendOwnedMessage pkLassoTool initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKLassoTool)
initSelector = mkSelector "init"

