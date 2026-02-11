{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationAppleIDButton@.
module ObjC.AuthenticationServices.ASAuthorizationAppleIDButton
  ( ASAuthorizationAppleIDButton
  , IsASAuthorizationAppleIDButton(..)
  , buttonWithType_style
  , initWithAuthorizationButtonType_authorizationButtonStyle
  , cornerRadius
  , setCornerRadius
  , buttonWithType_styleSelector
  , initWithAuthorizationButtonType_authorizationButtonStyleSelector
  , cornerRadiusSelector
  , setCornerRadiusSelector

  -- * Enum types
  , ASAuthorizationAppleIDButtonStyle(ASAuthorizationAppleIDButtonStyle)
  , pattern ASAuthorizationAppleIDButtonStyleWhite
  , pattern ASAuthorizationAppleIDButtonStyleWhiteOutline
  , pattern ASAuthorizationAppleIDButtonStyleBlack
  , ASAuthorizationAppleIDButtonType(ASAuthorizationAppleIDButtonType)
  , pattern ASAuthorizationAppleIDButtonTypeSignIn
  , pattern ASAuthorizationAppleIDButtonTypeContinue
  , pattern ASAuthorizationAppleIDButtonTypeSignUp
  , pattern ASAuthorizationAppleIDButtonTypeDefault

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

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.AuthenticationServices.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ buttonWithType:style:@
buttonWithType_style :: ASAuthorizationAppleIDButtonType -> ASAuthorizationAppleIDButtonStyle -> IO (Id ASAuthorizationAppleIDButton)
buttonWithType_style type_ style =
  do
    cls' <- getRequiredClass "ASAuthorizationAppleIDButton"
    sendClassMsg cls' (mkSelector "buttonWithType:style:") (retPtr retVoid) [argCLong (coerce type_), argCLong (coerce style)] >>= retainedObject . castPtr

-- | @- initWithAuthorizationButtonType:authorizationButtonStyle:@
initWithAuthorizationButtonType_authorizationButtonStyle :: IsASAuthorizationAppleIDButton asAuthorizationAppleIDButton => asAuthorizationAppleIDButton -> ASAuthorizationAppleIDButtonType -> ASAuthorizationAppleIDButtonStyle -> IO (Id ASAuthorizationAppleIDButton)
initWithAuthorizationButtonType_authorizationButtonStyle asAuthorizationAppleIDButton  type_ style =
  sendMsg asAuthorizationAppleIDButton (mkSelector "initWithAuthorizationButtonType:authorizationButtonStyle:") (retPtr retVoid) [argCLong (coerce type_), argCLong (coerce style)] >>= ownedObject . castPtr

-- | Set a custom corner radius to be used by this button.
--
-- ObjC selector: @- cornerRadius@
cornerRadius :: IsASAuthorizationAppleIDButton asAuthorizationAppleIDButton => asAuthorizationAppleIDButton -> IO CDouble
cornerRadius asAuthorizationAppleIDButton  =
  sendMsg asAuthorizationAppleIDButton (mkSelector "cornerRadius") retCDouble []

-- | Set a custom corner radius to be used by this button.
--
-- ObjC selector: @- setCornerRadius:@
setCornerRadius :: IsASAuthorizationAppleIDButton asAuthorizationAppleIDButton => asAuthorizationAppleIDButton -> CDouble -> IO ()
setCornerRadius asAuthorizationAppleIDButton  value =
  sendMsg asAuthorizationAppleIDButton (mkSelector "setCornerRadius:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @buttonWithType:style:@
buttonWithType_styleSelector :: Selector
buttonWithType_styleSelector = mkSelector "buttonWithType:style:"

-- | @Selector@ for @initWithAuthorizationButtonType:authorizationButtonStyle:@
initWithAuthorizationButtonType_authorizationButtonStyleSelector :: Selector
initWithAuthorizationButtonType_authorizationButtonStyleSelector = mkSelector "initWithAuthorizationButtonType:authorizationButtonStyle:"

-- | @Selector@ for @cornerRadius@
cornerRadiusSelector :: Selector
cornerRadiusSelector = mkSelector "cornerRadius"

-- | @Selector@ for @setCornerRadius:@
setCornerRadiusSelector :: Selector
setCornerRadiusSelector = mkSelector "setCornerRadius:"

