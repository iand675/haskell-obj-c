{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMediaQuerySection@.
module ObjC.MediaPlayer.MPMediaQuerySection
  ( MPMediaQuerySection
  , IsMPMediaQuerySection(..)
  , title
  , range
  , rangeSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- title@
title :: IsMPMediaQuerySection mpMediaQuerySection => mpMediaQuerySection -> IO (Id NSString)
title mpMediaQuerySection =
  sendMessage mpMediaQuerySection titleSelector

-- | @- range@
range :: IsMPMediaQuerySection mpMediaQuerySection => mpMediaQuerySection -> IO NSRange
range mpMediaQuerySection =
  sendMessage mpMediaQuerySection rangeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @range@
rangeSelector :: Selector '[] NSRange
rangeSelector = mkSelector "range"

