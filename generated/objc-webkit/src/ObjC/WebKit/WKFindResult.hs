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
init_ :: IsWKFindResult wkFindResult => wkFindResult -> IO (Id WKFindResult)
init_ wkFindResult  =
  sendMsg wkFindResult (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- matchFound@
matchFound :: IsWKFindResult wkFindResult => wkFindResult -> IO Bool
matchFound wkFindResult  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkFindResult (mkSelector "matchFound") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @matchFound@
matchFoundSelector :: Selector
matchFoundSelector = mkSelector "matchFound"

