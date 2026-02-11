{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that periodically checks whether additional fragments have been appended to fragmented assets.
--
-- Generated bindings for @AVFragmentedAssetMinder@.
module ObjC.AVFoundation.AVFragmentedAssetMinder
  ( AVFragmentedAssetMinder
  , IsAVFragmentedAssetMinder(..)
  , fragmentedAssetMinderWithAsset_mindingInterval
  , initWithAsset_mindingInterval
  , addFragmentedAsset
  , removeFragmentedAsset
  , mindingInterval
  , setMindingInterval
  , assets
  , fragmentedAssetMinderWithAsset_mindingIntervalSelector
  , initWithAsset_mindingIntervalSelector
  , addFragmentedAssetSelector
  , removeFragmentedAssetSelector
  , mindingIntervalSelector
  , setMindingIntervalSelector
  , assetsSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates an AVFragmentedAssetMinder, adds the specified asset to it, and sets the mindingInterval to the specified value.
--
-- - Parameter asset: An instance of AVFragmentedAsset to add to the AVFragmentedAssetMinder - Parameter mindingInterval: The initial minding interval of the AVFragmentedAssetMinder.
--
-- - Returns: A new instance of AVFragmentedAssetMinder.
--
-- ObjC selector: @+ fragmentedAssetMinderWithAsset:mindingInterval:@
fragmentedAssetMinderWithAsset_mindingInterval :: IsAVAsset asset => asset -> CDouble -> IO (Id AVFragmentedAssetMinder)
fragmentedAssetMinderWithAsset_mindingInterval asset mindingInterval =
  do
    cls' <- getRequiredClass "AVFragmentedAssetMinder"
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "fragmentedAssetMinderWithAsset:mindingInterval:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ()), argCDouble (fromIntegral mindingInterval)] >>= retainedObject . castPtr

-- | Creates an AVFragmentedAssetMinder, adds the specified asset to it, and sets the mindingInterval to the specified value.
--
-- - Parameter asset: An instance of AVFragmentedAsset to add to the AVFragmentedAssetMinder - Parameter mindingInterval: The initial minding interval of the AVFragmentedAssetMinder.
--
-- - Returns: A new instance of AVFragmentedAssetMinder.
--
-- ObjC selector: @- initWithAsset:mindingInterval:@
initWithAsset_mindingInterval :: (IsAVFragmentedAssetMinder avFragmentedAssetMinder, IsAVAsset asset) => avFragmentedAssetMinder -> asset -> CDouble -> IO (Id AVFragmentedAssetMinder)
initWithAsset_mindingInterval avFragmentedAssetMinder  asset mindingInterval =
withObjCPtr asset $ \raw_asset ->
    sendMsg avFragmentedAssetMinder (mkSelector "initWithAsset:mindingInterval:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ()), argCDouble (fromIntegral mindingInterval)] >>= ownedObject . castPtr

-- | Adds a fragmented asset to the array of assets being minded.
--
-- This method throws an exception if the asset is not a supported type (AVFragmentedAsset, AVFragmentedMovie), or if the asset is already being minded by another fragment minder.
--
-- - Parameter asset: The fragmented asset to add to the minder.
--
-- ObjC selector: @- addFragmentedAsset:@
addFragmentedAsset :: (IsAVFragmentedAssetMinder avFragmentedAssetMinder, IsAVAsset asset) => avFragmentedAssetMinder -> asset -> IO ()
addFragmentedAsset avFragmentedAssetMinder  asset =
withObjCPtr asset $ \raw_asset ->
    sendMsg avFragmentedAssetMinder (mkSelector "addFragmentedAsset:") retVoid [argPtr (castPtr raw_asset :: Ptr ())]

-- | Removes a fragmented asset from the array of assets being minded.
--
-- This method throws an exception if the asset is not a supported type (AVFragmentedAsset, AVFragmentedMovie).
--
-- - Parameter asset: The fragmented asset to remove from the minder.
--
-- ObjC selector: @- removeFragmentedAsset:@
removeFragmentedAsset :: (IsAVFragmentedAssetMinder avFragmentedAssetMinder, IsAVAsset asset) => avFragmentedAssetMinder -> asset -> IO ()
removeFragmentedAsset avFragmentedAssetMinder  asset =
withObjCPtr asset $ \raw_asset ->
    sendMsg avFragmentedAssetMinder (mkSelector "removeFragmentedAsset:") retVoid [argPtr (castPtr raw_asset :: Ptr ())]

-- | An NSTimeInterval indicating how often a check for additional fragments should be performed. The default interval is 10.0.
--
-- This property throws an excepion if a value is set less than one millisecond (0.001) in duration.
--
-- ObjC selector: @- mindingInterval@
mindingInterval :: IsAVFragmentedAssetMinder avFragmentedAssetMinder => avFragmentedAssetMinder -> IO CDouble
mindingInterval avFragmentedAssetMinder  =
  sendMsg avFragmentedAssetMinder (mkSelector "mindingInterval") retCDouble []

-- | An NSTimeInterval indicating how often a check for additional fragments should be performed. The default interval is 10.0.
--
-- This property throws an excepion if a value is set less than one millisecond (0.001) in duration.
--
-- ObjC selector: @- setMindingInterval:@
setMindingInterval :: IsAVFragmentedAssetMinder avFragmentedAssetMinder => avFragmentedAssetMinder -> CDouble -> IO ()
setMindingInterval avFragmentedAssetMinder  value =
  sendMsg avFragmentedAssetMinder (mkSelector "setMindingInterval:") retVoid [argCDouble (fromIntegral value)]

-- | An NSArray of the AVFragmentedAsset objects being minded.
--
-- ObjC selector: @- assets@
assets :: IsAVFragmentedAssetMinder avFragmentedAssetMinder => avFragmentedAssetMinder -> IO (Id NSArray)
assets avFragmentedAssetMinder  =
  sendMsg avFragmentedAssetMinder (mkSelector "assets") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fragmentedAssetMinderWithAsset:mindingInterval:@
fragmentedAssetMinderWithAsset_mindingIntervalSelector :: Selector
fragmentedAssetMinderWithAsset_mindingIntervalSelector = mkSelector "fragmentedAssetMinderWithAsset:mindingInterval:"

-- | @Selector@ for @initWithAsset:mindingInterval:@
initWithAsset_mindingIntervalSelector :: Selector
initWithAsset_mindingIntervalSelector = mkSelector "initWithAsset:mindingInterval:"

-- | @Selector@ for @addFragmentedAsset:@
addFragmentedAssetSelector :: Selector
addFragmentedAssetSelector = mkSelector "addFragmentedAsset:"

-- | @Selector@ for @removeFragmentedAsset:@
removeFragmentedAssetSelector :: Selector
removeFragmentedAssetSelector = mkSelector "removeFragmentedAsset:"

-- | @Selector@ for @mindingInterval@
mindingIntervalSelector :: Selector
mindingIntervalSelector = mkSelector "mindingInterval"

-- | @Selector@ for @setMindingInterval:@
setMindingIntervalSelector :: Selector
setMindingIntervalSelector = mkSelector "setMindingInterval:"

-- | @Selector@ for @assets@
assetsSelector :: Selector
assetsSelector = mkSelector "assets"

