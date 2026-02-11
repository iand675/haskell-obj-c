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
  , mediaSelectionOptionsInMediaSelectionGroupSelector
  , initSelector
  , newSelector
  , mediaPresentationSettingsForMediaSelectionGroupSelector
  , mediaPresentationLanguagesForMediaSelectionGroupSelector
  , playableOfflineSelector


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

-- | Returns an array of AVMediaSelectionOptions in an AVMediaSelectionGroup that are available for offline operations, e.g. playback.
--
-- ObjC selector: @- mediaSelectionOptionsInMediaSelectionGroup:@
mediaSelectionOptionsInMediaSelectionGroup :: (IsAVAssetCache avAssetCache, IsAVMediaSelectionGroup mediaSelectionGroup) => avAssetCache -> mediaSelectionGroup -> IO (Id NSArray)
mediaSelectionOptionsInMediaSelectionGroup avAssetCache  mediaSelectionGroup =
withObjCPtr mediaSelectionGroup $ \raw_mediaSelectionGroup ->
    sendMsg avAssetCache (mkSelector "mediaSelectionOptionsInMediaSelectionGroup:") (retPtr retVoid) [argPtr (castPtr raw_mediaSelectionGroup :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsAVAssetCache avAssetCache => avAssetCache -> IO (Id AVAssetCache)
init_ avAssetCache  =
  sendMsg avAssetCache (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetCache)
new  =
  do
    cls' <- getRequiredClass "AVAssetCache"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For each AVMediaPresentationSelector defined by the AVCustomMediaSelectionScheme of an AVMediaSelectionGroup, returns the AVMediaPresentationSettings that can be satisfied for offline operations, e.g. playback.
--
-- ObjC selector: @- mediaPresentationSettingsForMediaSelectionGroup:@
mediaPresentationSettingsForMediaSelectionGroup :: (IsAVAssetCache avAssetCache, IsAVMediaSelectionGroup mediaSelectionGroup) => avAssetCache -> mediaSelectionGroup -> IO (Id NSDictionary)
mediaPresentationSettingsForMediaSelectionGroup avAssetCache  mediaSelectionGroup =
withObjCPtr mediaSelectionGroup $ \raw_mediaSelectionGroup ->
    sendMsg avAssetCache (mkSelector "mediaPresentationSettingsForMediaSelectionGroup:") (retPtr retVoid) [argPtr (castPtr raw_mediaSelectionGroup :: Ptr ())] >>= retainedObject . castPtr

-- | Returns an array of extended language tags for languages that can be selected for offline operations via use of the AVMediaSelectionGroup's AVCustomMediaSelectionScheme.
--
-- ObjC selector: @- mediaPresentationLanguagesForMediaSelectionGroup:@
mediaPresentationLanguagesForMediaSelectionGroup :: (IsAVAssetCache avAssetCache, IsAVMediaSelectionGroup mediaSelectionGroup) => avAssetCache -> mediaSelectionGroup -> IO (Id NSArray)
mediaPresentationLanguagesForMediaSelectionGroup avAssetCache  mediaSelectionGroup =
withObjCPtr mediaSelectionGroup $ \raw_mediaSelectionGroup ->
    sendMsg avAssetCache (mkSelector "mediaPresentationLanguagesForMediaSelectionGroup:") (retPtr retVoid) [argPtr (castPtr raw_mediaSelectionGroup :: Ptr ())] >>= retainedObject . castPtr

-- | Returns YES if a complete rendition of an AVAsset is available to be played without a network connection.
--
-- An answer of YES does not indicate that any given media selection is available for offline playback. To determine if a specific media selection is available offline, see mediaSelectionOptionsInMediaSelectionGroup:.
--
-- ObjC selector: @- playableOffline@
playableOffline :: IsAVAssetCache avAssetCache => avAssetCache -> IO Bool
playableOffline avAssetCache  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetCache (mkSelector "playableOffline") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mediaSelectionOptionsInMediaSelectionGroup:@
mediaSelectionOptionsInMediaSelectionGroupSelector :: Selector
mediaSelectionOptionsInMediaSelectionGroupSelector = mkSelector "mediaSelectionOptionsInMediaSelectionGroup:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @mediaPresentationSettingsForMediaSelectionGroup:@
mediaPresentationSettingsForMediaSelectionGroupSelector :: Selector
mediaPresentationSettingsForMediaSelectionGroupSelector = mkSelector "mediaPresentationSettingsForMediaSelectionGroup:"

-- | @Selector@ for @mediaPresentationLanguagesForMediaSelectionGroup:@
mediaPresentationLanguagesForMediaSelectionGroupSelector :: Selector
mediaPresentationLanguagesForMediaSelectionGroupSelector = mkSelector "mediaPresentationLanguagesForMediaSelectionGroup:"

-- | @Selector@ for @playableOffline@
playableOfflineSelector :: Selector
playableOfflineSelector = mkSelector "playableOffline"

