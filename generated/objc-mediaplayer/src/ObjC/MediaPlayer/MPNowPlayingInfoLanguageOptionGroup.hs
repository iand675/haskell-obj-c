{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPNowPlayingInfoLanguageOptionGroup@.
module ObjC.MediaPlayer.MPNowPlayingInfoLanguageOptionGroup
  ( MPNowPlayingInfoLanguageOptionGroup
  , IsMPNowPlayingInfoLanguageOptionGroup(..)
  , initWithLanguageOptions_defaultLanguageOption_allowEmptySelection
  , languageOptions
  , defaultLanguageOption
  , allowEmptySelection
  , initWithLanguageOptions_defaultLanguageOption_allowEmptySelectionSelector
  , languageOptionsSelector
  , defaultLanguageOptionSelector
  , allowEmptySelectionSelector


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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithLanguageOptions:defaultLanguageOption:allowEmptySelection:@
initWithLanguageOptions_defaultLanguageOption_allowEmptySelection :: (IsMPNowPlayingInfoLanguageOptionGroup mpNowPlayingInfoLanguageOptionGroup, IsNSArray languageOptions, IsMPNowPlayingInfoLanguageOption defaultLanguageOption) => mpNowPlayingInfoLanguageOptionGroup -> languageOptions -> defaultLanguageOption -> Bool -> IO (Id MPNowPlayingInfoLanguageOptionGroup)
initWithLanguageOptions_defaultLanguageOption_allowEmptySelection mpNowPlayingInfoLanguageOptionGroup  languageOptions defaultLanguageOption allowEmptySelection =
withObjCPtr languageOptions $ \raw_languageOptions ->
  withObjCPtr defaultLanguageOption $ \raw_defaultLanguageOption ->
      sendMsg mpNowPlayingInfoLanguageOptionGroup (mkSelector "initWithLanguageOptions:defaultLanguageOption:allowEmptySelection:") (retPtr retVoid) [argPtr (castPtr raw_languageOptions :: Ptr ()), argPtr (castPtr raw_defaultLanguageOption :: Ptr ()), argCULong (if allowEmptySelection then 1 else 0)] >>= ownedObject . castPtr

-- | The available language options within this group.
--
-- ObjC selector: @- languageOptions@
languageOptions :: IsMPNowPlayingInfoLanguageOptionGroup mpNowPlayingInfoLanguageOptionGroup => mpNowPlayingInfoLanguageOptionGroup -> IO (Id NSArray)
languageOptions mpNowPlayingInfoLanguageOptionGroup  =
  sendMsg mpNowPlayingInfoLanguageOptionGroup (mkSelector "languageOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The default language option, if any, within this group.
--
-- ObjC selector: @- defaultLanguageOption@
defaultLanguageOption :: IsMPNowPlayingInfoLanguageOptionGroup mpNowPlayingInfoLanguageOptionGroup => mpNowPlayingInfoLanguageOptionGroup -> IO (Id MPNowPlayingInfoLanguageOption)
defaultLanguageOption mpNowPlayingInfoLanguageOptionGroup  =
  sendMsg mpNowPlayingInfoLanguageOptionGroup (mkSelector "defaultLanguageOption") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates whether a selection in this group is required at all times.
--
-- ObjC selector: @- allowEmptySelection@
allowEmptySelection :: IsMPNowPlayingInfoLanguageOptionGroup mpNowPlayingInfoLanguageOptionGroup => mpNowPlayingInfoLanguageOptionGroup -> IO Bool
allowEmptySelection mpNowPlayingInfoLanguageOptionGroup  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpNowPlayingInfoLanguageOptionGroup (mkSelector "allowEmptySelection") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLanguageOptions:defaultLanguageOption:allowEmptySelection:@
initWithLanguageOptions_defaultLanguageOption_allowEmptySelectionSelector :: Selector
initWithLanguageOptions_defaultLanguageOption_allowEmptySelectionSelector = mkSelector "initWithLanguageOptions:defaultLanguageOption:allowEmptySelection:"

-- | @Selector@ for @languageOptions@
languageOptionsSelector :: Selector
languageOptionsSelector = mkSelector "languageOptions"

-- | @Selector@ for @defaultLanguageOption@
defaultLanguageOptionSelector :: Selector
defaultLanguageOptionSelector = mkSelector "defaultLanguageOption"

-- | @Selector@ for @allowEmptySelection@
allowEmptySelectionSelector :: Selector
allowEmptySelectionSelector = mkSelector "allowEmptySelection"

