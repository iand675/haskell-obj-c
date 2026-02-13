{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- richTextSource@
richTextSource :: IsNSAppleScript nsAppleScript => nsAppleScript -> IO (Id NSAttributedString)
richTextSource nsAppleScript =
  sendMessage nsAppleScript richTextSourceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @richTextSource@
richTextSourceSelector :: Selector '[] (Id NSAttributedString)
richTextSourceSelector = mkSelector "richTextSource"

