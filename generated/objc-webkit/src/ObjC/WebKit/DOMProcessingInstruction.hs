{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMProcessingInstruction@.
module ObjC.WebKit.DOMProcessingInstruction
  ( DOMProcessingInstruction
  , IsDOMProcessingInstruction(..)
  , target
  , sheet
  , targetSelector
  , sheetSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- target@
target :: IsDOMProcessingInstruction domProcessingInstruction => domProcessingInstruction -> IO (Id NSString)
target domProcessingInstruction  =
  sendMsg domProcessingInstruction (mkSelector "target") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sheet@
sheet :: IsDOMProcessingInstruction domProcessingInstruction => domProcessingInstruction -> IO (Id DOMStyleSheet)
sheet domProcessingInstruction  =
  sendMsg domProcessingInstruction (mkSelector "sheet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @sheet@
sheetSelector :: Selector
sheetSelector = mkSelector "sheet"

