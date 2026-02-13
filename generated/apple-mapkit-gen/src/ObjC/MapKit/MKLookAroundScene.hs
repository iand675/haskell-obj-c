{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKLookAroundScene@.
module ObjC.MapKit.MKLookAroundScene
  ( MKLookAroundScene
  , IsMKLookAroundScene(..)
  , new
  , init_
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MKLookAroundScene)
new  =
  do
    cls' <- getRequiredClass "MKLookAroundScene"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMKLookAroundScene mkLookAroundScene => mkLookAroundScene -> IO (Id MKLookAroundScene)
init_ mkLookAroundScene =
  sendOwnedMessage mkLookAroundScene initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MKLookAroundScene)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MKLookAroundScene)
initSelector = mkSelector "init"

