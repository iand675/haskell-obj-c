{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @WKFindResult@.
module ObjC.WebKit.WKFindResult
  ( WKFindResult
  , IsWKFindResult(..)
  , init_
  , matchFound
  , initSelector
  , matchFoundSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsWKFindResult wkFindResult => wkFindResult -> IO (Id WKFindResult)
init_ wkFindResult =
  sendOwnedMessage wkFindResult initSelector

-- | @- matchFound@
matchFound :: IsWKFindResult wkFindResult => wkFindResult -> IO Bool
matchFound wkFindResult =
  sendMessage wkFindResult matchFoundSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id WKFindResult)
initSelector = mkSelector "init"

-- | @Selector@ for @matchFound@
matchFoundSelector :: Selector '[] Bool
matchFoundSelector = mkSelector "matchFound"

