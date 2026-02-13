{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFChooseIdentityTableCellView@.
module ObjC.SecurityInterface.SFChooseIdentityTableCellView
  ( SFChooseIdentityTableCellView
  , IsSFChooseIdentityTableCellView(..)
  , issuerTextField
  , setIssuerTextField
  , issuerTextFieldSelector
  , setIssuerTextFieldSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SecurityInterface.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- issuerTextField@
issuerTextField :: IsSFChooseIdentityTableCellView sfChooseIdentityTableCellView => sfChooseIdentityTableCellView -> IO (Id NSTextField)
issuerTextField sfChooseIdentityTableCellView =
  sendMessage sfChooseIdentityTableCellView issuerTextFieldSelector

-- | @- setIssuerTextField:@
setIssuerTextField :: (IsSFChooseIdentityTableCellView sfChooseIdentityTableCellView, IsNSTextField value) => sfChooseIdentityTableCellView -> value -> IO ()
setIssuerTextField sfChooseIdentityTableCellView value =
  sendMessage sfChooseIdentityTableCellView setIssuerTextFieldSelector (toNSTextField value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @issuerTextField@
issuerTextFieldSelector :: Selector '[] (Id NSTextField)
issuerTextFieldSelector = mkSelector "issuerTextField"

-- | @Selector@ for @setIssuerTextField:@
setIssuerTextFieldSelector :: Selector '[Id NSTextField] ()
setIssuerTextFieldSelector = mkSelector "setIssuerTextField:"

