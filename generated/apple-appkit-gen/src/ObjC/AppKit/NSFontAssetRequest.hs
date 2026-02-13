{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFontAssetRequest@.
module ObjC.AppKit.NSFontAssetRequest
  ( NSFontAssetRequest
  , IsNSFontAssetRequest(..)
  , init_
  , initWithFontDescriptors_options
  , downloadFontAssetsWithCompletionHandler
  , downloadedFontDescriptors
  , progress
  , downloadFontAssetsWithCompletionHandlerSelector
  , downloadedFontDescriptorsSelector
  , initSelector
  , initWithFontDescriptors_optionsSelector
  , progressSelector

  -- * Enum types
  , NSFontAssetRequestOptions(NSFontAssetRequestOptions)
  , pattern NSFontAssetRequestOptionUsesStandardUI

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSFontAssetRequest nsFontAssetRequest => nsFontAssetRequest -> IO (Id NSFontAssetRequest)
init_ nsFontAssetRequest =
  sendOwnedMessage nsFontAssetRequest initSelector

-- | @- initWithFontDescriptors:options:@
initWithFontDescriptors_options :: (IsNSFontAssetRequest nsFontAssetRequest, IsNSArray fontDescriptors) => nsFontAssetRequest -> fontDescriptors -> NSFontAssetRequestOptions -> IO (Id NSFontAssetRequest)
initWithFontDescriptors_options nsFontAssetRequest fontDescriptors options =
  sendOwnedMessage nsFontAssetRequest initWithFontDescriptors_optionsSelector (toNSArray fontDescriptors) options

-- | @- downloadFontAssetsWithCompletionHandler:@
downloadFontAssetsWithCompletionHandler :: IsNSFontAssetRequest nsFontAssetRequest => nsFontAssetRequest -> Ptr () -> IO ()
downloadFontAssetsWithCompletionHandler nsFontAssetRequest completionHandler =
  sendMessage nsFontAssetRequest downloadFontAssetsWithCompletionHandlerSelector completionHandler

-- | @- downloadedFontDescriptors@
downloadedFontDescriptors :: IsNSFontAssetRequest nsFontAssetRequest => nsFontAssetRequest -> IO (Id NSArray)
downloadedFontDescriptors nsFontAssetRequest =
  sendMessage nsFontAssetRequest downloadedFontDescriptorsSelector

-- | @- progress@
progress :: IsNSFontAssetRequest nsFontAssetRequest => nsFontAssetRequest -> IO (Id NSProgress)
progress nsFontAssetRequest =
  sendMessage nsFontAssetRequest progressSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSFontAssetRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithFontDescriptors:options:@
initWithFontDescriptors_optionsSelector :: Selector '[Id NSArray, NSFontAssetRequestOptions] (Id NSFontAssetRequest)
initWithFontDescriptors_optionsSelector = mkSelector "initWithFontDescriptors:options:"

-- | @Selector@ for @downloadFontAssetsWithCompletionHandler:@
downloadFontAssetsWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
downloadFontAssetsWithCompletionHandlerSelector = mkSelector "downloadFontAssetsWithCompletionHandler:"

-- | @Selector@ for @downloadedFontDescriptors@
downloadedFontDescriptorsSelector :: Selector '[] (Id NSArray)
downloadedFontDescriptorsSelector = mkSelector "downloadedFontDescriptors"

-- | @Selector@ for @progress@
progressSelector :: Selector '[] (Id NSProgress)
progressSelector = mkSelector "progress"

