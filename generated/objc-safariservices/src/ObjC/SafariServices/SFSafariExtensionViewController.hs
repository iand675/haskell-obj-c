{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFSafariExtensionViewController@.
module ObjC.SafariServices.SFSafariExtensionViewController
  ( SFSafariExtensionViewController
  , IsSFSafariExtensionViewController(..)
  , dismissPopover
  , dismissPopoverSelector


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

import ObjC.SafariServices.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- dismissPopover@
dismissPopover :: IsSFSafariExtensionViewController sfSafariExtensionViewController => sfSafariExtensionViewController -> IO ()
dismissPopover sfSafariExtensionViewController  =
  sendMsg sfSafariExtensionViewController (mkSelector "dismissPopover") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dismissPopover@
dismissPopoverSelector :: Selector
dismissPopoverSelector = mkSelector "dismissPopover"

