{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASESoundAsset
--
-- An object that represents a registered sound asset in the asset registry.
--
-- Generated bindings for @PHASESoundAsset@.
module ObjC.PHASE.PHASESoundAsset
  ( PHASESoundAsset
  , IsPHASESoundAsset(..)
  , init_
  , new
  , url
  , data_
  , type_
  , initSelector
  , newSelector
  , urlSelector
  , dataSelector
  , typeSelector

  -- * Enum types
  , PHASEAssetType(PHASEAssetType)
  , pattern PHASEAssetTypeResident
  , pattern PHASEAssetTypeStreamed

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
import ObjC.PHASE.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASESoundAsset phaseSoundAsset => phaseSoundAsset -> IO (Id PHASESoundAsset)
init_ phaseSoundAsset  =
  sendMsg phaseSoundAsset (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASESoundAsset)
new  =
  do
    cls' <- getRequiredClass "PHASESoundAsset"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | url
--
-- The URL of the sound asset, if applicable.
--
-- ObjC selector: @- url@
url :: IsPHASESoundAsset phaseSoundAsset => phaseSoundAsset -> IO (Id NSURL)
url phaseSoundAsset  =
  sendMsg phaseSoundAsset (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | data
--
-- The buffer for the sound asset, if applicable.
--
-- ObjC selector: @- data@
data_ :: IsPHASESoundAsset phaseSoundAsset => phaseSoundAsset -> IO (Id NSData)
data_ phaseSoundAsset  =
  sendMsg phaseSoundAsset (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | type
--
-- The sound asset type.
--
-- ObjC selector: @- type@
type_ :: IsPHASESoundAsset phaseSoundAsset => phaseSoundAsset -> IO PHASEAssetType
type_ phaseSoundAsset  =
  fmap (coerce :: CLong -> PHASEAssetType) $ sendMsg phaseSoundAsset (mkSelector "type") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

