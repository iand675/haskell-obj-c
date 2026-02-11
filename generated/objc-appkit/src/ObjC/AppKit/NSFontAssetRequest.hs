{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithFontDescriptors_optionsSelector
  , downloadFontAssetsWithCompletionHandlerSelector
  , downloadedFontDescriptorsSelector
  , progressSelector

  -- * Enum types
  , NSFontAssetRequestOptions(NSFontAssetRequestOptions)
  , pattern NSFontAssetRequestOptionUsesStandardUI

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSFontAssetRequest nsFontAssetRequest => nsFontAssetRequest -> IO (Id NSFontAssetRequest)
init_ nsFontAssetRequest  =
  sendMsg nsFontAssetRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithFontDescriptors:options:@
initWithFontDescriptors_options :: (IsNSFontAssetRequest nsFontAssetRequest, IsNSArray fontDescriptors) => nsFontAssetRequest -> fontDescriptors -> NSFontAssetRequestOptions -> IO (Id NSFontAssetRequest)
initWithFontDescriptors_options nsFontAssetRequest  fontDescriptors options =
withObjCPtr fontDescriptors $ \raw_fontDescriptors ->
    sendMsg nsFontAssetRequest (mkSelector "initWithFontDescriptors:options:") (retPtr retVoid) [argPtr (castPtr raw_fontDescriptors :: Ptr ()), argCULong (coerce options)] >>= ownedObject . castPtr

-- | @- downloadFontAssetsWithCompletionHandler:@
downloadFontAssetsWithCompletionHandler :: IsNSFontAssetRequest nsFontAssetRequest => nsFontAssetRequest -> Ptr () -> IO ()
downloadFontAssetsWithCompletionHandler nsFontAssetRequest  completionHandler =
  sendMsg nsFontAssetRequest (mkSelector "downloadFontAssetsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- downloadedFontDescriptors@
downloadedFontDescriptors :: IsNSFontAssetRequest nsFontAssetRequest => nsFontAssetRequest -> IO (Id NSArray)
downloadedFontDescriptors nsFontAssetRequest  =
  sendMsg nsFontAssetRequest (mkSelector "downloadedFontDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- progress@
progress :: IsNSFontAssetRequest nsFontAssetRequest => nsFontAssetRequest -> IO (Id NSProgress)
progress nsFontAssetRequest  =
  sendMsg nsFontAssetRequest (mkSelector "progress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithFontDescriptors:options:@
initWithFontDescriptors_optionsSelector :: Selector
initWithFontDescriptors_optionsSelector = mkSelector "initWithFontDescriptors:options:"

-- | @Selector@ for @downloadFontAssetsWithCompletionHandler:@
downloadFontAssetsWithCompletionHandlerSelector :: Selector
downloadFontAssetsWithCompletionHandlerSelector = mkSelector "downloadFontAssetsWithCompletionHandler:"

-- | @Selector@ for @downloadedFontDescriptors@
downloadedFontDescriptorsSelector :: Selector
downloadedFontDescriptorsSelector = mkSelector "downloadedFontDescriptors"

-- | @Selector@ for @progress@
progressSelector :: Selector
progressSelector = mkSelector "progress"

