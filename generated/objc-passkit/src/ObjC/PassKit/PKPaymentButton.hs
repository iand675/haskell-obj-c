{-# LANGUAGE PatternSynonyms #-}
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
  , initWithPaymentButtonType_paymentButtonStyleSelector
  , buttonWithType_style_disableCardArtSelector
  , initWithPaymentButtonType_paymentButtonStyle_disableCardArtSelector
  , cornerRadiusSelector
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

import ObjC.PassKit.Internal.Classes
import ObjC.PassKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ buttonWithType:style:@
buttonWithType_style :: PKPaymentButtonType -> PKPaymentButtonStyle -> IO (Id PKPaymentButton)
buttonWithType_style buttonType buttonStyle =
  do
    cls' <- getRequiredClass "PKPaymentButton"
    sendClassMsg cls' (mkSelector "buttonWithType:style:") (retPtr retVoid) [argCLong (coerce buttonType), argCLong (coerce buttonStyle)] >>= retainedObject . castPtr

-- | @- initWithPaymentButtonType:paymentButtonStyle:@
initWithPaymentButtonType_paymentButtonStyle :: IsPKPaymentButton pkPaymentButton => pkPaymentButton -> PKPaymentButtonType -> PKPaymentButtonStyle -> IO (Id PKPaymentButton)
initWithPaymentButtonType_paymentButtonStyle pkPaymentButton  type_ style =
  sendMsg pkPaymentButton (mkSelector "initWithPaymentButtonType:paymentButtonStyle:") (retPtr retVoid) [argCLong (coerce type_), argCLong (coerce style)] >>= ownedObject . castPtr

-- | @+ buttonWithType:style:disableCardArt:@
buttonWithType_style_disableCardArt :: PKPaymentButtonType -> PKPaymentButtonStyle -> Bool -> IO (Id PKPaymentButton)
buttonWithType_style_disableCardArt buttonType buttonStyle disableCardArt =
  do
    cls' <- getRequiredClass "PKPaymentButton"
    sendClassMsg cls' (mkSelector "buttonWithType:style:disableCardArt:") (retPtr retVoid) [argCLong (coerce buttonType), argCLong (coerce buttonStyle), argCULong (if disableCardArt then 1 else 0)] >>= retainedObject . castPtr

-- | @- initWithPaymentButtonType:paymentButtonStyle:disableCardArt:@
initWithPaymentButtonType_paymentButtonStyle_disableCardArt :: IsPKPaymentButton pkPaymentButton => pkPaymentButton -> PKPaymentButtonType -> PKPaymentButtonStyle -> Bool -> IO (Id PKPaymentButton)
initWithPaymentButtonType_paymentButtonStyle_disableCardArt pkPaymentButton  type_ style disableCardArt =
  sendMsg pkPaymentButton (mkSelector "initWithPaymentButtonType:paymentButtonStyle:disableCardArt:") (retPtr retVoid) [argCLong (coerce type_), argCLong (coerce style), argCULong (if disableCardArt then 1 else 0)] >>= ownedObject . castPtr

-- | @- cornerRadius@
cornerRadius :: IsPKPaymentButton pkPaymentButton => pkPaymentButton -> IO CDouble
cornerRadius pkPaymentButton  =
  sendMsg pkPaymentButton (mkSelector "cornerRadius") retCDouble []

-- | @- setCornerRadius:@
setCornerRadius :: IsPKPaymentButton pkPaymentButton => pkPaymentButton -> CDouble -> IO ()
setCornerRadius pkPaymentButton  value =
  sendMsg pkPaymentButton (mkSelector "setCornerRadius:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @buttonWithType:style:@
buttonWithType_styleSelector :: Selector
buttonWithType_styleSelector = mkSelector "buttonWithType:style:"

-- | @Selector@ for @initWithPaymentButtonType:paymentButtonStyle:@
initWithPaymentButtonType_paymentButtonStyleSelector :: Selector
initWithPaymentButtonType_paymentButtonStyleSelector = mkSelector "initWithPaymentButtonType:paymentButtonStyle:"

-- | @Selector@ for @buttonWithType:style:disableCardArt:@
buttonWithType_style_disableCardArtSelector :: Selector
buttonWithType_style_disableCardArtSelector = mkSelector "buttonWithType:style:disableCardArt:"

-- | @Selector@ for @initWithPaymentButtonType:paymentButtonStyle:disableCardArt:@
initWithPaymentButtonType_paymentButtonStyle_disableCardArtSelector :: Selector
initWithPaymentButtonType_paymentButtonStyle_disableCardArtSelector = mkSelector "initWithPaymentButtonType:paymentButtonStyle:disableCardArt:"

-- | @Selector@ for @cornerRadius@
cornerRadiusSelector :: Selector
cornerRadiusSelector = mkSelector "cornerRadius"

-- | @Selector@ for @setCornerRadius:@
setCornerRadiusSelector :: Selector
setCornerRadiusSelector = mkSelector "setCornerRadius:"

