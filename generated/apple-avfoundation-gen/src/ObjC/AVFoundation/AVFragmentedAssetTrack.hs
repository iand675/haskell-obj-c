{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A subclass of AVAssetTrack for handling tracks of fragmented assets. An AVFragmentedAssetTrack is capable of changing the values of certain of its properties, if its parent asset is associated with an instance of AVFragmentedAssetMinder when one or more fragments are appended to the underlying media resource.
--
-- While its parent asset is associated with an AVFragmentedAssetMinder, AVFragmentedAssetTrack posts AVAssetTrackTimeRangeDidChangeNotification and AVAssetTrackSegmentsDidChangeNotification whenever new fragments are detected, as appropriate. Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVFragmentedAssetTrack@.
module ObjC.AVFoundation.AVFragmentedAssetTrack
  ( AVFragmentedAssetTrack
  , IsAVFragmentedAssetTrack(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

