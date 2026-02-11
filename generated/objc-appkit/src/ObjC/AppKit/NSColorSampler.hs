{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Manages a color sampling interface to allow the user to select a color from their screen.
--
-- Generated bindings for @NSColorSampler@.
module ObjC.AppKit.NSColorSampler
  ( NSColorSampler
  , IsNSColorSampler(..)
  , showSamplerWithSelectionHandler
  , showSamplerWithSelectionHandlerSelector


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

-- | The primary method for NSColorSampler.
--
-- Begins or attaches to an existing color sampling session which presents UI to the user for selecting a color from their screen. The handler will be called on the main thread when the user completes the session (either by selection, or cancelation). In the event of user-cancellation, @colorSelectionHandler@ will be called with @nil@.
--
-- The calling NSColorSampler instance is retained until the sampling session is completed.
--
-- ObjC selector: @- showSamplerWithSelectionHandler:@
showSamplerWithSelectionHandler :: IsNSColorSampler nsColorSampler => nsColorSampler -> Ptr () -> IO ()
showSamplerWithSelectionHandler nsColorSampler  selectionHandler =
  sendMsg nsColorSampler (mkSelector "showSamplerWithSelectionHandler:") retVoid [argPtr (castPtr selectionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @showSamplerWithSelectionHandler:@
showSamplerWithSelectionHandlerSelector :: Selector
showSamplerWithSelectionHandlerSelector = mkSelector "showSamplerWithSelectionHandler:"

