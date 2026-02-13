{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @WKSnapshotConfiguration@.
module ObjC.WebKit.WKSnapshotConfiguration
  ( WKSnapshotConfiguration
  , IsWKSnapshotConfiguration(..)
  , snapshotWidth
  , setSnapshotWidth
  , afterScreenUpdates
  , setAfterScreenUpdates
  , afterScreenUpdatesSelector
  , setAfterScreenUpdatesSelector
  , setSnapshotWidthSelector
  , snapshotWidthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Specify a custom width to control the size of image you get back. The height will be  computed to maintain the aspect ratio established by rect.
--
-- snapshotWidth represents the width in points. If the snapshotWidth is nil, rect's width will be used.
--
-- ObjC selector: @- snapshotWidth@
snapshotWidth :: IsWKSnapshotConfiguration wkSnapshotConfiguration => wkSnapshotConfiguration -> IO (Id NSNumber)
snapshotWidth wkSnapshotConfiguration =
  sendMessage wkSnapshotConfiguration snapshotWidthSelector

-- | Specify a custom width to control the size of image you get back. The height will be  computed to maintain the aspect ratio established by rect.
--
-- snapshotWidth represents the width in points. If the snapshotWidth is nil, rect's width will be used.
--
-- ObjC selector: @- setSnapshotWidth:@
setSnapshotWidth :: (IsWKSnapshotConfiguration wkSnapshotConfiguration, IsNSNumber value) => wkSnapshotConfiguration -> value -> IO ()
setSnapshotWidth wkSnapshotConfiguration value =
  sendMessage wkSnapshotConfiguration setSnapshotWidthSelector (toNSNumber value)

-- | A Boolean value that specifies whether the snapshot should be taken after recent changes have been incorporated. The value NO will capture the screen in its current state, which might not include recent changes.
--
-- The default value is YES.
--
-- ObjC selector: @- afterScreenUpdates@
afterScreenUpdates :: IsWKSnapshotConfiguration wkSnapshotConfiguration => wkSnapshotConfiguration -> IO Bool
afterScreenUpdates wkSnapshotConfiguration =
  sendMessage wkSnapshotConfiguration afterScreenUpdatesSelector

-- | A Boolean value that specifies whether the snapshot should be taken after recent changes have been incorporated. The value NO will capture the screen in its current state, which might not include recent changes.
--
-- The default value is YES.
--
-- ObjC selector: @- setAfterScreenUpdates:@
setAfterScreenUpdates :: IsWKSnapshotConfiguration wkSnapshotConfiguration => wkSnapshotConfiguration -> Bool -> IO ()
setAfterScreenUpdates wkSnapshotConfiguration value =
  sendMessage wkSnapshotConfiguration setAfterScreenUpdatesSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @snapshotWidth@
snapshotWidthSelector :: Selector '[] (Id NSNumber)
snapshotWidthSelector = mkSelector "snapshotWidth"

-- | @Selector@ for @setSnapshotWidth:@
setSnapshotWidthSelector :: Selector '[Id NSNumber] ()
setSnapshotWidthSelector = mkSelector "setSnapshotWidth:"

-- | @Selector@ for @afterScreenUpdates@
afterScreenUpdatesSelector :: Selector '[] Bool
afterScreenUpdatesSelector = mkSelector "afterScreenUpdates"

-- | @Selector@ for @setAfterScreenUpdates:@
setAfterScreenUpdatesSelector :: Selector '[Bool] ()
setAfterScreenUpdatesSelector = mkSelector "setAfterScreenUpdates:"

