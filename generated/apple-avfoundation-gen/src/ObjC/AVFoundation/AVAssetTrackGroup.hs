{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAssetTrackGroup
--
-- A class whose instances describe a group of tracks in an asset.
--
-- Instances of AVAssetTrackGroup describe a single group of related tracks in an asset. For example, a track group can	describe a set of alternate tracks, which are tracks containing variations of the same content, such as content	translated into different languages, out of which only one track should be played at a time.
--
-- Clients can inspect the track groups contained in an AVAsset by loading and obtaining the value of its trackGroups property.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVAssetTrackGroup@.
module ObjC.AVFoundation.AVAssetTrackGroup
  ( AVAssetTrackGroup
  , IsAVAssetTrackGroup(..)
  , trackIDs
  , trackIDsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | trackIDs
--
-- The IDs of all of the tracks in the group.
--
-- The value of this property is an NSArray of NSNumbers interpreted as CMPersistentTrackIDs, one for each track in the	group.
--
-- ObjC selector: @- trackIDs@
trackIDs :: IsAVAssetTrackGroup avAssetTrackGroup => avAssetTrackGroup -> IO (Id NSArray)
trackIDs avAssetTrackGroup =
  sendMessage avAssetTrackGroup trackIDsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @trackIDs@
trackIDsSelector :: Selector '[] (Id NSArray)
trackIDsSelector = mkSelector "trackIDs"

