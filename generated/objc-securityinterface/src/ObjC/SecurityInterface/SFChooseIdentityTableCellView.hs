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

import ObjC.SecurityInterface.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- issuerTextField@
issuerTextField :: IsSFChooseIdentityTableCellView sfChooseIdentityTableCellView => sfChooseIdentityTableCellView -> IO (Id NSTextField)
issuerTextField sfChooseIdentityTableCellView  =
  sendMsg sfChooseIdentityTableCellView (mkSelector "issuerTextField") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIssuerTextField:@
setIssuerTextField :: (IsSFChooseIdentityTableCellView sfChooseIdentityTableCellView, IsNSTextField value) => sfChooseIdentityTableCellView -> value -> IO ()
setIssuerTextField sfChooseIdentityTableCellView  value =
withObjCPtr value $ \raw_value ->
    sendMsg sfChooseIdentityTableCellView (mkSelector "setIssuerTextField:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @issuerTextField@
issuerTextFieldSelector :: Selector
issuerTextFieldSelector = mkSelector "issuerTextField"

-- | @Selector@ for @setIssuerTextField:@
setIssuerTextFieldSelector :: Selector
setIssuerTextFieldSelector = mkSelector "setIssuerTextField:"

