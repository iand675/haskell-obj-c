{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVTextStyleRule
--
-- AVTextStyleRule represents a set of text styling attributes that can be applied to some or all of the text of legible media, such as subtitles and closed captions.
--
-- Generated bindings for @AVTextStyleRuleInternal@.
module ObjC.AVFoundation.AVTextStyleRuleInternal
  ( AVTextStyleRuleInternal
  , IsAVTextStyleRuleInternal(..)


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

