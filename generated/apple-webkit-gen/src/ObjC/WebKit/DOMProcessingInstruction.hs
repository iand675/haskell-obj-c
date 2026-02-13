{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMProcessingInstruction@.
module ObjC.WebKit.DOMProcessingInstruction
  ( DOMProcessingInstruction
  , IsDOMProcessingInstruction(..)
  , target
  , sheet
  , sheetSelector
  , targetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- target@
target :: IsDOMProcessingInstruction domProcessingInstruction => domProcessingInstruction -> IO (Id NSString)
target domProcessingInstruction =
  sendMessage domProcessingInstruction targetSelector

-- | @- sheet@
sheet :: IsDOMProcessingInstruction domProcessingInstruction => domProcessingInstruction -> IO (Id DOMStyleSheet)
sheet domProcessingInstruction =
  sendMessage domProcessingInstruction sheetSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @target@
targetSelector :: Selector '[] (Id NSString)
targetSelector = mkSelector "target"

-- | @Selector@ for @sheet@
sheetSelector :: Selector '[] (Id DOMStyleSheet)
sheetSelector = mkSelector "sheet"

