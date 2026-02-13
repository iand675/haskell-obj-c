{-# LANGUAGE DataKinds #-}
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
  , contentLimitsEnabledSelector
  , contentLimitsEnforcedSelector
  , endpointAvailableSelector
  , enforcedContentItemsCountSelector
  , enforcedContentTreeDepthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The number of items the content server will display when content limiting is enforced. Returns NSIntegerMax if the content server will never limit the number of items.
--
-- ObjC selector: @- enforcedContentItemsCount@
enforcedContentItemsCount :: IsMPPlayableContentManagerContext mpPlayableContentManagerContext => mpPlayableContentManagerContext -> IO CLong
enforcedContentItemsCount mpPlayableContentManagerContext =
  sendMessage mpPlayableContentManagerContext enforcedContentItemsCountSelector

-- | The depth of the navigation hierarchy the content server will allow. Exceeding this limit will result in a crash.
--
-- ObjC selector: @- enforcedContentTreeDepth@
enforcedContentTreeDepth :: IsMPPlayableContentManagerContext mpPlayableContentManagerContext => mpPlayableContentManagerContext -> IO CLong
enforcedContentTreeDepth mpPlayableContentManagerContext =
  sendMessage mpPlayableContentManagerContext enforcedContentTreeDepthSelector

-- | Represents whether content limits are being enforced by the content server or not.
--
-- ObjC selector: @- contentLimitsEnforced@
contentLimitsEnforced :: IsMPPlayableContentManagerContext mpPlayableContentManagerContext => mpPlayableContentManagerContext -> IO Bool
contentLimitsEnforced mpPlayableContentManagerContext =
  sendMessage mpPlayableContentManagerContext contentLimitsEnforcedSelector

-- | @- contentLimitsEnabled@
contentLimitsEnabled :: IsMPPlayableContentManagerContext mpPlayableContentManagerContext => mpPlayableContentManagerContext -> IO Bool
contentLimitsEnabled mpPlayableContentManagerContext =
  sendMessage mpPlayableContentManagerContext contentLimitsEnabledSelector

-- | Represents whether the content server is available or not.
--
-- ObjC selector: @- endpointAvailable@
endpointAvailable :: IsMPPlayableContentManagerContext mpPlayableContentManagerContext => mpPlayableContentManagerContext -> IO Bool
endpointAvailable mpPlayableContentManagerContext =
  sendMessage mpPlayableContentManagerContext endpointAvailableSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enforcedContentItemsCount@
enforcedContentItemsCountSelector :: Selector '[] CLong
enforcedContentItemsCountSelector = mkSelector "enforcedContentItemsCount"

-- | @Selector@ for @enforcedContentTreeDepth@
enforcedContentTreeDepthSelector :: Selector '[] CLong
enforcedContentTreeDepthSelector = mkSelector "enforcedContentTreeDepth"

-- | @Selector@ for @contentLimitsEnforced@
contentLimitsEnforcedSelector :: Selector '[] Bool
contentLimitsEnforcedSelector = mkSelector "contentLimitsEnforced"

-- | @Selector@ for @contentLimitsEnabled@
contentLimitsEnabledSelector :: Selector '[] Bool
contentLimitsEnabledSelector = mkSelector "contentLimitsEnabled"

-- | @Selector@ for @endpointAvailable@
endpointAvailableSelector :: Selector '[] Bool
endpointAvailableSelector = mkSelector "endpointAvailable"

