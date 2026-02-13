{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , dataSelector
  , initSelector
  , newSelector
  , typeSelector
  , urlSelector

  -- * Enum types
  , PHASEAssetType(PHASEAssetType)
  , pattern PHASEAssetTypeResident
  , pattern PHASEAssetTypeStreamed

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.PHASE.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASESoundAsset phaseSoundAsset => phaseSoundAsset -> IO (Id PHASESoundAsset)
init_ phaseSoundAsset =
  sendOwnedMessage phaseSoundAsset initSelector

-- | @+ new@
new :: IO (Id PHASESoundAsset)
new  =
  do
    cls' <- getRequiredClass "PHASESoundAsset"
    sendOwnedClassMessage cls' newSelector

-- | url
--
-- The URL of the sound asset, if applicable.
--
-- ObjC selector: @- url@
url :: IsPHASESoundAsset phaseSoundAsset => phaseSoundAsset -> IO (Id NSURL)
url phaseSoundAsset =
  sendMessage phaseSoundAsset urlSelector

-- | data
--
-- The buffer for the sound asset, if applicable.
--
-- ObjC selector: @- data@
data_ :: IsPHASESoundAsset phaseSoundAsset => phaseSoundAsset -> IO (Id NSData)
data_ phaseSoundAsset =
  sendMessage phaseSoundAsset dataSelector

-- | type
--
-- The sound asset type.
--
-- ObjC selector: @- type@
type_ :: IsPHASESoundAsset phaseSoundAsset => phaseSoundAsset -> IO PHASEAssetType
type_ phaseSoundAsset =
  sendMessage phaseSoundAsset typeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASESoundAsset)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASESoundAsset)
newSelector = mkSelector "new"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @type@
typeSelector :: Selector '[] PHASEAssetType
typeSelector = mkSelector "type"

