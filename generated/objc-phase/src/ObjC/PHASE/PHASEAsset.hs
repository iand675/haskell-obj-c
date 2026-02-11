{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ***********************************************************************************************
--
-- PHASEAsset
--
-- An object that represents a registered asset in the asset registry.
--
-- Generated bindings for @PHASEAsset@.
module ObjC.PHASE.PHASEAsset
  ( PHASEAsset
  , IsPHASEAsset(..)
  , init_
  , new
  , identifier
  , initSelector
  , newSelector
  , identifierSelector


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

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEAsset phaseAsset => phaseAsset -> IO (Id PHASEAsset)
init_ phaseAsset  =
  sendMsg phaseAsset (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEAsset)
new  =
  do
    cls' <- getRequiredClass "PHASEAsset"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | identifier
--
-- The identifier that uniquely represents this asset.
--
-- ObjC selector: @- identifier@
identifier :: IsPHASEAsset phaseAsset => phaseAsset -> IO (Id NSString)
identifier phaseAsset  =
  sendMsg phaseAsset (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

