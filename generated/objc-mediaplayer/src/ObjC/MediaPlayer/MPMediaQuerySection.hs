{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMediaQuerySection@.
module ObjC.MediaPlayer.MPMediaQuerySection
  ( MPMediaQuerySection
  , IsMPMediaQuerySection(..)
  , title
  , range
  , titleSelector
  , rangeSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- title@
title :: IsMPMediaQuerySection mpMediaQuerySection => mpMediaQuerySection -> IO (Id NSString)
title mpMediaQuerySection  =
  sendMsg mpMediaQuerySection (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- range@
range :: IsMPMediaQuerySection mpMediaQuerySection => mpMediaQuerySection -> IO NSRange
range mpMediaQuerySection  =
  sendMsgStret mpMediaQuerySection (mkSelector "range") retNSRange []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @range@
rangeSelector :: Selector
rangeSelector = mkSelector "range"

