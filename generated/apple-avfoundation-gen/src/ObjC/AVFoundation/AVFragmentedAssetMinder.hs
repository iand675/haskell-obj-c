{-# LANGUAGE DataKinds #-}
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
  , addFragmentedAssetSelector
  , assetsSelector
  , fragmentedAssetMinderWithAsset_mindingIntervalSelector
  , initWithAsset_mindingIntervalSelector
  , mindingIntervalSelector
  , removeFragmentedAssetSelector
  , setMindingIntervalSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' fragmentedAssetMinderWithAsset_mindingIntervalSelector (toAVAsset asset) mindingInterval

-- | Creates an AVFragmentedAssetMinder, adds the specified asset to it, and sets the mindingInterval to the specified value.
--
-- - Parameter asset: An instance of AVFragmentedAsset to add to the AVFragmentedAssetMinder - Parameter mindingInterval: The initial minding interval of the AVFragmentedAssetMinder.
--
-- - Returns: A new instance of AVFragmentedAssetMinder.
--
-- ObjC selector: @- initWithAsset:mindingInterval:@
initWithAsset_mindingInterval :: (IsAVFragmentedAssetMinder avFragmentedAssetMinder, IsAVAsset asset) => avFragmentedAssetMinder -> asset -> CDouble -> IO (Id AVFragmentedAssetMinder)
initWithAsset_mindingInterval avFragmentedAssetMinder asset mindingInterval =
  sendOwnedMessage avFragmentedAssetMinder initWithAsset_mindingIntervalSelector (toAVAsset asset) mindingInterval

-- | Adds a fragmented asset to the array of assets being minded.
--
-- This method throws an exception if the asset is not a supported type (AVFragmentedAsset, AVFragmentedMovie), or if the asset is already being minded by another fragment minder.
--
-- - Parameter asset: The fragmented asset to add to the minder.
--
-- ObjC selector: @- addFragmentedAsset:@
addFragmentedAsset :: (IsAVFragmentedAssetMinder avFragmentedAssetMinder, IsAVAsset asset) => avFragmentedAssetMinder -> asset -> IO ()
addFragmentedAsset avFragmentedAssetMinder asset =
  sendMessage avFragmentedAssetMinder addFragmentedAssetSelector (toAVAsset asset)

-- | Removes a fragmented asset from the array of assets being minded.
--
-- This method throws an exception if the asset is not a supported type (AVFragmentedAsset, AVFragmentedMovie).
--
-- - Parameter asset: The fragmented asset to remove from the minder.
--
-- ObjC selector: @- removeFragmentedAsset:@
removeFragmentedAsset :: (IsAVFragmentedAssetMinder avFragmentedAssetMinder, IsAVAsset asset) => avFragmentedAssetMinder -> asset -> IO ()
removeFragmentedAsset avFragmentedAssetMinder asset =
  sendMessage avFragmentedAssetMinder removeFragmentedAssetSelector (toAVAsset asset)

-- | An NSTimeInterval indicating how often a check for additional fragments should be performed. The default interval is 10.0.
--
-- This property throws an excepion if a value is set less than one millisecond (0.001) in duration.
--
-- ObjC selector: @- mindingInterval@
mindingInterval :: IsAVFragmentedAssetMinder avFragmentedAssetMinder => avFragmentedAssetMinder -> IO CDouble
mindingInterval avFragmentedAssetMinder =
  sendMessage avFragmentedAssetMinder mindingIntervalSelector

-- | An NSTimeInterval indicating how often a check for additional fragments should be performed. The default interval is 10.0.
--
-- This property throws an excepion if a value is set less than one millisecond (0.001) in duration.
--
-- ObjC selector: @- setMindingInterval:@
setMindingInterval :: IsAVFragmentedAssetMinder avFragmentedAssetMinder => avFragmentedAssetMinder -> CDouble -> IO ()
setMindingInterval avFragmentedAssetMinder value =
  sendMessage avFragmentedAssetMinder setMindingIntervalSelector value

-- | An NSArray of the AVFragmentedAsset objects being minded.
--
-- ObjC selector: @- assets@
assets :: IsAVFragmentedAssetMinder avFragmentedAssetMinder => avFragmentedAssetMinder -> IO (Id NSArray)
assets avFragmentedAssetMinder =
  sendMessage avFragmentedAssetMinder assetsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fragmentedAssetMinderWithAsset:mindingInterval:@
fragmentedAssetMinderWithAsset_mindingIntervalSelector :: Selector '[Id AVAsset, CDouble] (Id AVFragmentedAssetMinder)
fragmentedAssetMinderWithAsset_mindingIntervalSelector = mkSelector "fragmentedAssetMinderWithAsset:mindingInterval:"

-- | @Selector@ for @initWithAsset:mindingInterval:@
initWithAsset_mindingIntervalSelector :: Selector '[Id AVAsset, CDouble] (Id AVFragmentedAssetMinder)
initWithAsset_mindingIntervalSelector = mkSelector "initWithAsset:mindingInterval:"

-- | @Selector@ for @addFragmentedAsset:@
addFragmentedAssetSelector :: Selector '[Id AVAsset] ()
addFragmentedAssetSelector = mkSelector "addFragmentedAsset:"

-- | @Selector@ for @removeFragmentedAsset:@
removeFragmentedAssetSelector :: Selector '[Id AVAsset] ()
removeFragmentedAssetSelector = mkSelector "removeFragmentedAsset:"

-- | @Selector@ for @mindingInterval@
mindingIntervalSelector :: Selector '[] CDouble
mindingIntervalSelector = mkSelector "mindingInterval"

-- | @Selector@ for @setMindingInterval:@
setMindingIntervalSelector :: Selector '[CDouble] ()
setMindingIntervalSelector = mkSelector "setMindingInterval:"

-- | @Selector@ for @assets@
assetsSelector :: Selector '[] (Id NSArray)
assetsSelector = mkSelector "assets"

