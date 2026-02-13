{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SafariServices.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- dismissPopover@
dismissPopover :: IsSFSafariExtensionViewController sfSafariExtensionViewController => sfSafariExtensionViewController -> IO ()
dismissPopover sfSafariExtensionViewController =
  sendMessage sfSafariExtensionViewController dismissPopoverSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dismissPopover@
dismissPopoverSelector :: Selector '[] ()
dismissPopoverSelector = mkSelector "dismissPopover"

