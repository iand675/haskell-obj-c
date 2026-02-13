{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , cornerRadiusSelector
  , initWithAuthorizationButtonType_authorizationButtonStyleSelector
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' buttonWithType_styleSelector type_ style

-- | @- initWithAuthorizationButtonType:authorizationButtonStyle:@
initWithAuthorizationButtonType_authorizationButtonStyle :: IsASAuthorizationAppleIDButton asAuthorizationAppleIDButton => asAuthorizationAppleIDButton -> ASAuthorizationAppleIDButtonType -> ASAuthorizationAppleIDButtonStyle -> IO (Id ASAuthorizationAppleIDButton)
initWithAuthorizationButtonType_authorizationButtonStyle asAuthorizationAppleIDButton type_ style =
  sendOwnedMessage asAuthorizationAppleIDButton initWithAuthorizationButtonType_authorizationButtonStyleSelector type_ style

-- | Set a custom corner radius to be used by this button.
--
-- ObjC selector: @- cornerRadius@
cornerRadius :: IsASAuthorizationAppleIDButton asAuthorizationAppleIDButton => asAuthorizationAppleIDButton -> IO CDouble
cornerRadius asAuthorizationAppleIDButton =
  sendMessage asAuthorizationAppleIDButton cornerRadiusSelector

-- | Set a custom corner radius to be used by this button.
--
-- ObjC selector: @- setCornerRadius:@
setCornerRadius :: IsASAuthorizationAppleIDButton asAuthorizationAppleIDButton => asAuthorizationAppleIDButton -> CDouble -> IO ()
setCornerRadius asAuthorizationAppleIDButton value =
  sendMessage asAuthorizationAppleIDButton setCornerRadiusSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @buttonWithType:style:@
buttonWithType_styleSelector :: Selector '[ASAuthorizationAppleIDButtonType, ASAuthorizationAppleIDButtonStyle] (Id ASAuthorizationAppleIDButton)
buttonWithType_styleSelector = mkSelector "buttonWithType:style:"

-- | @Selector@ for @initWithAuthorizationButtonType:authorizationButtonStyle:@
initWithAuthorizationButtonType_authorizationButtonStyleSelector :: Selector '[ASAuthorizationAppleIDButtonType, ASAuthorizationAppleIDButtonStyle] (Id ASAuthorizationAppleIDButton)
initWithAuthorizationButtonType_authorizationButtonStyleSelector = mkSelector "initWithAuthorizationButtonType:authorizationButtonStyle:"

-- | @Selector@ for @cornerRadius@
cornerRadiusSelector :: Selector '[] CDouble
cornerRadiusSelector = mkSelector "cornerRadius"

-- | @Selector@ for @setCornerRadius:@
setCornerRadiusSelector :: Selector '[CDouble] ()
setCornerRadiusSelector = mkSelector "setCornerRadius:"

