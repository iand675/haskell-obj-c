{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPPlayableContentManagerContext represents the current state of the playable content endpoint. A context is retrievable from an instance of MPPlayableContentManager.
--
-- Generated bindings for @MPPlayableContentManagerContext@.
module ObjC.MediaPlayer.MPPlayableContentManagerContext
  ( MPPlayableContentManagerContext
  , IsMPPlayableContentManagerContext(..)
  , enforcedContentItemsCount
  , enforcedContentTreeDepth
  , contentLimitsEnforced
  , contentLimitsEnabled
  , endpointAvailable
  , enforcedContentItemsCountSelector
  , enforcedContentTreeDepthSelector
  , contentLimitsEnforcedSelector
  , contentLimitsEnabledSelector
  , endpointAvailableSelector


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

-- | The number of items the content server will display when content limiting is enforced. Returns NSIntegerMax if the content server will never limit the number of items.
--
-- ObjC selector: @- enforcedContentItemsCount@
enforcedContentItemsCount :: IsMPPlayableContentManagerContext mpPlayableContentManagerContext => mpPlayableContentManagerContext -> IO CLong
enforcedContentItemsCount mpPlayableContentManagerContext  =
  sendMsg mpPlayableContentManagerContext (mkSelector "enforcedContentItemsCount") retCLong []

-- | The depth of the navigation hierarchy the content server will allow. Exceeding this limit will result in a crash.
--
-- ObjC selector: @- enforcedContentTreeDepth@
enforcedContentTreeDepth :: IsMPPlayableContentManagerContext mpPlayableContentManagerContext => mpPlayableContentManagerContext -> IO CLong
enforcedContentTreeDepth mpPlayableContentManagerContext  =
  sendMsg mpPlayableContentManagerContext (mkSelector "enforcedContentTreeDepth") retCLong []

-- | Represents whether content limits are being enforced by the content server or not.
--
-- ObjC selector: @- contentLimitsEnforced@
contentLimitsEnforced :: IsMPPlayableContentManagerContext mpPlayableContentManagerContext => mpPlayableContentManagerContext -> IO Bool
contentLimitsEnforced mpPlayableContentManagerContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpPlayableContentManagerContext (mkSelector "contentLimitsEnforced") retCULong []

-- | @- contentLimitsEnabled@
contentLimitsEnabled :: IsMPPlayableContentManagerContext mpPlayableContentManagerContext => mpPlayableContentManagerContext -> IO Bool
contentLimitsEnabled mpPlayableContentManagerContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpPlayableContentManagerContext (mkSelector "contentLimitsEnabled") retCULong []

-- | Represents whether the content server is available or not.
--
-- ObjC selector: @- endpointAvailable@
endpointAvailable :: IsMPPlayableContentManagerContext mpPlayableContentManagerContext => mpPlayableContentManagerContext -> IO Bool
endpointAvailable mpPlayableContentManagerContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpPlayableContentManagerContext (mkSelector "endpointAvailable") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enforcedContentItemsCount@
enforcedContentItemsCountSelector :: Selector
enforcedContentItemsCountSelector = mkSelector "enforcedContentItemsCount"

-- | @Selector@ for @enforcedContentTreeDepth@
enforcedContentTreeDepthSelector :: Selector
enforcedContentTreeDepthSelector = mkSelector "enforcedContentTreeDepth"

-- | @Selector@ for @contentLimitsEnforced@
contentLimitsEnforcedSelector :: Selector
contentLimitsEnforcedSelector = mkSelector "contentLimitsEnforced"

-- | @Selector@ for @contentLimitsEnabled@
contentLimitsEnabledSelector :: Selector
contentLimitsEnabledSelector = mkSelector "contentLimitsEnabled"

-- | @Selector@ for @endpointAvailable@
endpointAvailableSelector :: Selector
endpointAvailableSelector = mkSelector "endpointAvailable"

