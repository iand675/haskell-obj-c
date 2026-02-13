{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAssetCache is a class vended by an AVAsset used for the inspection of locally available media data.
--
-- AVAssetCaches are vended by AVURLAsset's assetCache property.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVAssetCache@.
module ObjC.AVFoundation.AVAssetCache
  ( AVAssetCache
  , IsAVAssetCache(..)
  , mediaSelectionOptionsInMediaSelectionGroup
  , init_
  , new
  , mediaPresentationSettingsForMediaSelectionGroup
  , mediaPresentationLanguagesForMediaSelectionGroup
  , playableOffline
  , initSelector
  , mediaPresentationLanguagesForMediaSelectionGroupSelector
  , mediaPresentationSettingsForMediaSelectionGroupSelector
  , mediaSelectionOptionsInMediaSelectionGroupSelector
  , newSelector
  , playableOfflineSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns an array of AVMediaSelectionOptions in an AVMediaSelectionGroup that are available for offline operations, e.g. playback.
--
-- ObjC selector: @- mediaSelectionOptionsInMediaSelectionGroup:@
mediaSelectionOptionsInMediaSelectionGroup :: (IsAVAssetCache avAssetCache, IsAVMediaSelectionGroup mediaSelectionGroup) => avAssetCache -> mediaSelectionGroup -> IO (Id NSArray)
mediaSelectionOptionsInMediaSelectionGroup avAssetCache mediaSelectionGroup =
  sendMessage avAssetCache mediaSelectionOptionsInMediaSelectionGroupSelector (toAVMediaSelectionGroup mediaSelectionGroup)

-- | @- init@
init_ :: IsAVAssetCache avAssetCache => avAssetCache -> IO (Id AVAssetCache)
init_ avAssetCache =
  sendOwnedMessage avAssetCache initSelector

-- | @+ new@
new :: IO (Id AVAssetCache)
new  =
  do
    cls' <- getRequiredClass "AVAssetCache"
    sendOwnedClassMessage cls' newSelector

-- | For each AVMediaPresentationSelector defined by the AVCustomMediaSelectionScheme of an AVMediaSelectionGroup, returns the AVMediaPresentationSettings that can be satisfied for offline operations, e.g. playback.
--
-- ObjC selector: @- mediaPresentationSettingsForMediaSelectionGroup:@
mediaPresentationSettingsForMediaSelectionGroup :: (IsAVAssetCache avAssetCache, IsAVMediaSelectionGroup mediaSelectionGroup) => avAssetCache -> mediaSelectionGroup -> IO (Id NSDictionary)
mediaPresentationSettingsForMediaSelectionGroup avAssetCache mediaSelectionGroup =
  sendMessage avAssetCache mediaPresentationSettingsForMediaSelectionGroupSelector (toAVMediaSelectionGroup mediaSelectionGroup)

-- | Returns an array of extended language tags for languages that can be selected for offline operations via use of the AVMediaSelectionGroup's AVCustomMediaSelectionScheme.
--
-- ObjC selector: @- mediaPresentationLanguagesForMediaSelectionGroup:@
mediaPresentationLanguagesForMediaSelectionGroup :: (IsAVAssetCache avAssetCache, IsAVMediaSelectionGroup mediaSelectionGroup) => avAssetCache -> mediaSelectionGroup -> IO (Id NSArray)
mediaPresentationLanguagesForMediaSelectionGroup avAssetCache mediaSelectionGroup =
  sendMessage avAssetCache mediaPresentationLanguagesForMediaSelectionGroupSelector (toAVMediaSelectionGroup mediaSelectionGroup)

-- | Returns YES if a complete rendition of an AVAsset is available to be played without a network connection.
--
-- An answer of YES does not indicate that any given media selection is available for offline playback. To determine if a specific media selection is available offline, see mediaSelectionOptionsInMediaSelectionGroup:.
--
-- ObjC selector: @- playableOffline@
playableOffline :: IsAVAssetCache avAssetCache => avAssetCache -> IO Bool
playableOffline avAssetCache =
  sendMessage avAssetCache playableOfflineSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mediaSelectionOptionsInMediaSelectionGroup:@
mediaSelectionOptionsInMediaSelectionGroupSelector :: Selector '[Id AVMediaSelectionGroup] (Id NSArray)
mediaSelectionOptionsInMediaSelectionGroupSelector = mkSelector "mediaSelectionOptionsInMediaSelectionGroup:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetCache)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetCache)
newSelector = mkSelector "new"

-- | @Selector@ for @mediaPresentationSettingsForMediaSelectionGroup:@
mediaPresentationSettingsForMediaSelectionGroupSelector :: Selector '[Id AVMediaSelectionGroup] (Id NSDictionary)
mediaPresentationSettingsForMediaSelectionGroupSelector = mkSelector "mediaPresentationSettingsForMediaSelectionGroup:"

-- | @Selector@ for @mediaPresentationLanguagesForMediaSelectionGroup:@
mediaPresentationLanguagesForMediaSelectionGroupSelector :: Selector '[Id AVMediaSelectionGroup] (Id NSArray)
mediaPresentationLanguagesForMediaSelectionGroupSelector = mkSelector "mediaPresentationLanguagesForMediaSelectionGroup:"

-- | @Selector@ for @playableOffline@
playableOfflineSelector :: Selector '[] Bool
playableOfflineSelector = mkSelector "playableOffline"

