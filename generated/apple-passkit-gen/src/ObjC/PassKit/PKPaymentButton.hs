{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPaymentButton@.
module ObjC.PassKit.PKPaymentButton
  ( PKPaymentButton
  , IsPKPaymentButton(..)
  , buttonWithType_style
  , initWithPaymentButtonType_paymentButtonStyle
  , buttonWithType_style_disableCardArt
  , initWithPaymentButtonType_paymentButtonStyle_disableCardArt
  , cornerRadius
  , setCornerRadius
  , buttonWithType_styleSelector
  , buttonWithType_style_disableCardArtSelector
  , cornerRadiusSelector
  , initWithPaymentButtonType_paymentButtonStyleSelector
  , initWithPaymentButtonType_paymentButtonStyle_disableCardArtSelector
  , setCornerRadiusSelector

  -- * Enum types
  , PKPaymentButtonStyle(PKPaymentButtonStyle)
  , pattern PKPaymentButtonStyleWhite
  , pattern PKPaymentButtonStyleWhiteOutline
  , pattern PKPaymentButtonStyleBlack
  , pattern PKPaymentButtonStyleAutomatic
  , PKPaymentButtonType(PKPaymentButtonType)
  , pattern PKPaymentButtonTypePlain
  , pattern PKPaymentButtonTypeBuy
  , pattern PKPaymentButtonTypeSetUp
  , pattern PKPaymentButtonTypeInStore
  , pattern PKPaymentButtonTypeDonate
  , pattern PKPaymentButtonTypeCheckout
  , pattern PKPaymentButtonTypeBook
  , pattern PKPaymentButtonTypeSubscribe
  , pattern PKPaymentButtonTypeReload
  , pattern PKPaymentButtonTypeAddMoney
  , pattern PKPaymentButtonTypeTopUp
  , pattern PKPaymentButtonTypeOrder
  , pattern PKPaymentButtonTypeRent
  , pattern PKPaymentButtonTypeSupport
  , pattern PKPaymentButtonTypeContribute
  , pattern PKPaymentButtonTypeTip
  , pattern PKPaymentButtonTypeContinue

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.PassKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ buttonWithType:style:@
buttonWithType_style :: PKPaymentButtonType -> PKPaymentButtonStyle -> IO (Id PKPaymentButton)
buttonWithType_style buttonType buttonStyle =
  do
    cls' <- getRequiredClass "PKPaymentButton"
    sendClassMessage cls' buttonWithType_styleSelector buttonType buttonStyle

-- | @- initWithPaymentButtonType:paymentButtonStyle:@
initWithPaymentButtonType_paymentButtonStyle :: IsPKPaymentButton pkPaymentButton => pkPaymentButton -> PKPaymentButtonType -> PKPaymentButtonStyle -> IO (Id PKPaymentButton)
initWithPaymentButtonType_paymentButtonStyle pkPaymentButton type_ style =
  sendOwnedMessage pkPaymentButton initWithPaymentButtonType_paymentButtonStyleSelector type_ style

-- | @+ buttonWithType:style:disableCardArt:@
buttonWithType_style_disableCardArt :: PKPaymentButtonType -> PKPaymentButtonStyle -> Bool -> IO (Id PKPaymentButton)
buttonWithType_style_disableCardArt buttonType buttonStyle disableCardArt =
  do
    cls' <- getRequiredClass "PKPaymentButton"
    sendClassMessage cls' buttonWithType_style_disableCardArtSelector buttonType buttonStyle disableCardArt

-- | @- initWithPaymentButtonType:paymentButtonStyle:disableCardArt:@
initWithPaymentButtonType_paymentButtonStyle_disableCardArt :: IsPKPaymentButton pkPaymentButton => pkPaymentButton -> PKPaymentButtonType -> PKPaymentButtonStyle -> Bool -> IO (Id PKPaymentButton)
initWithPaymentButtonType_paymentButtonStyle_disableCardArt pkPaymentButton type_ style disableCardArt =
  sendOwnedMessage pkPaymentButton initWithPaymentButtonType_paymentButtonStyle_disableCardArtSelector type_ style disableCardArt

-- | @- cornerRadius@
cornerRadius :: IsPKPaymentButton pkPaymentButton => pkPaymentButton -> IO CDouble
cornerRadius pkPaymentButton =
  sendMessage pkPaymentButton cornerRadiusSelector

-- | @- setCornerRadius:@
setCornerRadius :: IsPKPaymentButton pkPaymentButton => pkPaymentButton -> CDouble -> IO ()
setCornerRadius pkPaymentButton value =
  sendMessage pkPaymentButton setCornerRadiusSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @buttonWithType:style:@
buttonWithType_styleSelector :: Selector '[PKPaymentButtonType, PKPaymentButtonStyle] (Id PKPaymentButton)
buttonWithType_styleSelector = mkSelector "buttonWithType:style:"

-- | @Selector@ for @initWithPaymentButtonType:paymentButtonStyle:@
initWithPaymentButtonType_paymentButtonStyleSelector :: Selector '[PKPaymentButtonType, PKPaymentButtonStyle] (Id PKPaymentButton)
initWithPaymentButtonType_paymentButtonStyleSelector = mkSelector "initWithPaymentButtonType:paymentButtonStyle:"

-- | @Selector@ for @buttonWithType:style:disableCardArt:@
buttonWithType_style_disableCardArtSelector :: Selector '[PKPaymentButtonType, PKPaymentButtonStyle, Bool] (Id PKPaymentButton)
buttonWithType_style_disableCardArtSelector = mkSelector "buttonWithType:style:disableCardArt:"

-- | @Selector@ for @initWithPaymentButtonType:paymentButtonStyle:disableCardArt:@
initWithPaymentButtonType_paymentButtonStyle_disableCardArtSelector :: Selector '[PKPaymentButtonType, PKPaymentButtonStyle, Bool] (Id PKPaymentButton)
initWithPaymentButtonType_paymentButtonStyle_disableCardArtSelector = mkSelector "initWithPaymentButtonType:paymentButtonStyle:disableCardArt:"

-- | @Selector@ for @cornerRadius@
cornerRadiusSelector :: Selector '[] CDouble
cornerRadiusSelector = mkSelector "cornerRadius"

-- | @Selector@ for @setCornerRadius:@
setCornerRadiusSelector :: Selector '[CDouble] ()
setCornerRadiusSelector = mkSelector "setCornerRadius:"

