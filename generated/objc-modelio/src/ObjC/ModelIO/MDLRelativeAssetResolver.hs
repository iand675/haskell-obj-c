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
  , initWithAssetSelector
  , assetSelector
  , setAssetSelector


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

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithAsset:@
initWithAsset :: (IsMDLRelativeAssetResolver mdlRelativeAssetResolver, IsMDLAsset asset) => mdlRelativeAssetResolver -> asset -> IO (Id MDLRelativeAssetResolver)
initWithAsset mdlRelativeAssetResolver  asset =
withObjCPtr asset $ \raw_asset ->
    sendMsg mdlRelativeAssetResolver (mkSelector "initWithAsset:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ())] >>= ownedObject . castPtr

-- | @- asset@
asset :: IsMDLRelativeAssetResolver mdlRelativeAssetResolver => mdlRelativeAssetResolver -> IO (Id MDLAsset)
asset mdlRelativeAssetResolver  =
  sendMsg mdlRelativeAssetResolver (mkSelector "asset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAsset:@
setAsset :: (IsMDLRelativeAssetResolver mdlRelativeAssetResolver, IsMDLAsset value) => mdlRelativeAssetResolver -> value -> IO ()
setAsset mdlRelativeAssetResolver  value =
withObjCPtr value $ \raw_value ->
    sendMsg mdlRelativeAssetResolver (mkSelector "setAsset:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAsset:@
initWithAssetSelector :: Selector
initWithAssetSelector = mkSelector "initWithAsset:"

-- | @Selector@ for @asset@
assetSelector :: Selector
assetSelector = mkSelector "asset"

-- | @Selector@ for @setAsset:@
setAssetSelector :: Selector
setAssetSelector = mkSelector "setAsset:"

