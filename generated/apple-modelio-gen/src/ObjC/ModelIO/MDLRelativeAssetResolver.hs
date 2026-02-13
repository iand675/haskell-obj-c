{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MDLRelativeAssetResolver
--
-- The relative asset resolver searches for referenced files by checking the location of the asset for sibling files satisfying the requested name.
--
-- Generated bindings for @MDLRelativeAssetResolver@.
module ObjC.ModelIO.MDLRelativeAssetResolver
  ( MDLRelativeAssetResolver
  , IsMDLRelativeAssetResolver(..)
  , initWithAsset
  , asset
  , setAsset
  , assetSelector
  , initWithAssetSelector
  , setAssetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithAsset:@
initWithAsset :: (IsMDLRelativeAssetResolver mdlRelativeAssetResolver, IsMDLAsset asset) => mdlRelativeAssetResolver -> asset -> IO (Id MDLRelativeAssetResolver)
initWithAsset mdlRelativeAssetResolver asset =
  sendOwnedMessage mdlRelativeAssetResolver initWithAssetSelector (toMDLAsset asset)

-- | @- asset@
asset :: IsMDLRelativeAssetResolver mdlRelativeAssetResolver => mdlRelativeAssetResolver -> IO (Id MDLAsset)
asset mdlRelativeAssetResolver =
  sendMessage mdlRelativeAssetResolver assetSelector

-- | @- setAsset:@
setAsset :: (IsMDLRelativeAssetResolver mdlRelativeAssetResolver, IsMDLAsset value) => mdlRelativeAssetResolver -> value -> IO ()
setAsset mdlRelativeAssetResolver value =
  sendMessage mdlRelativeAssetResolver setAssetSelector (toMDLAsset value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAsset:@
initWithAssetSelector :: Selector '[Id MDLAsset] (Id MDLRelativeAssetResolver)
initWithAssetSelector = mkSelector "initWithAsset:"

-- | @Selector@ for @asset@
assetSelector :: Selector '[] (Id MDLAsset)
assetSelector = mkSelector "asset"

-- | @Selector@ for @setAsset:@
setAssetSelector :: Selector '[Id MDLAsset] ()
setAssetSelector = mkSelector "setAsset:"

