{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAppleScript@.
module ObjC.AppKit.NSAppleScript
  ( NSAppleScript
  , IsNSAppleScript(..)
  , richTextSource
  , richTextSourceSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- richTextSource@
richTextSource :: IsNSAppleScript nsAppleScript => nsAppleScript -> IO (Id NSAttributedString)
richTextSource nsAppleScript  =
  sendMsg nsAppleScript (mkSelector "richTextSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @richTextSource@
richTextSourceSelector :: Selector
richTextSourceSelector = mkSelector "richTextSource"

