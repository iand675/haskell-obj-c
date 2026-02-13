{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contains information about a decoded message
--
-- Generated bindings for @MEDecodedMessage@.
module ObjC.MailKit.MEDecodedMessage
  ( MEDecodedMessage
  , IsMEDecodedMessage(..)
  , new
  , init_
  , initWithData_securityInformation_context
  , initWithData_securityInformation_context_banner
  , rawData
  , securityInformation
  , context
  , banner
  , bannerSelector
  , contextSelector
  , initSelector
  , initWithData_securityInformation_contextSelector
  , initWithData_securityInformation_context_bannerSelector
  , newSelector
  , rawDataSelector
  , securityInformationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MailKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MEDecodedMessage)
new  =
  do
    cls' <- getRequiredClass "MEDecodedMessage"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMEDecodedMessage meDecodedMessage => meDecodedMessage -> IO (Id MEDecodedMessage)
init_ meDecodedMessage =
  sendOwnedMessage meDecodedMessage initSelector

-- | @- initWithData:securityInformation:context:@
initWithData_securityInformation_context :: (IsMEDecodedMessage meDecodedMessage, IsNSData rawData, IsMEMessageSecurityInformation securityInformation, IsNSData context) => meDecodedMessage -> rawData -> securityInformation -> context -> IO (Id MEDecodedMessage)
initWithData_securityInformation_context meDecodedMessage rawData securityInformation context =
  sendOwnedMessage meDecodedMessage initWithData_securityInformation_contextSelector (toNSData rawData) (toMEMessageSecurityInformation securityInformation) (toNSData context)

-- | @- initWithData:securityInformation:context:banner:@
initWithData_securityInformation_context_banner :: (IsMEDecodedMessage meDecodedMessage, IsNSData rawData, IsMEMessageSecurityInformation securityInformation, IsNSData context, IsMEDecodedMessageBanner banner) => meDecodedMessage -> rawData -> securityInformation -> context -> banner -> IO (Id MEDecodedMessage)
initWithData_securityInformation_context_banner meDecodedMessage rawData securityInformation context banner =
  sendOwnedMessage meDecodedMessage initWithData_securityInformation_context_bannerSelector (toNSData rawData) (toMEMessageSecurityInformation securityInformation) (toNSData context) (toMEDecodedMessageBanner banner)

-- | The decoded MIME data for the message The decoded data should not be encrypted or contain any signatures that were decoded. The @rawData@ here should only contain MIME parts that a standard email parser can decode without needing to decrypt. All information on the encryption and signature status should be defined in @securityInformation.@ If the message is unable to be decrypted this should be left nil and an error message will be displayed to the user.
--
-- ObjC selector: @- rawData@
rawData :: IsMEDecodedMessage meDecodedMessage => meDecodedMessage -> IO (Id NSData)
rawData meDecodedMessage =
  sendMessage meDecodedMessage rawDataSelector

-- | The security information for whether or not the message was signed, encrypted, or had an errors in decoding.
--
-- ObjC selector: @- securityInformation@
securityInformation :: IsMEDecodedMessage meDecodedMessage => meDecodedMessage -> IO (Id MEMessageSecurityInformation)
securityInformation meDecodedMessage =
  sendMessage meDecodedMessage securityInformationSelector

-- | The context for the decoded message. This will be passed back to the extension when Mail loads the extension's custom view controller for the message.
--
-- ObjC selector: @- context@
context :: IsMEDecodedMessage meDecodedMessage => meDecodedMessage -> IO (Id NSData)
context meDecodedMessage =
  sendMessage meDecodedMessage contextSelector

-- | Suggestion information used to populate a suggestion banner at the top of the message view. Clicking on the action associated with the suggestion banner will present the extension's view controller for the provided message context.
--
-- ObjC selector: @- banner@
banner :: IsMEDecodedMessage meDecodedMessage => meDecodedMessage -> IO (Id MEDecodedMessageBanner)
banner meDecodedMessage =
  sendMessage meDecodedMessage bannerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MEDecodedMessage)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MEDecodedMessage)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithData:securityInformation:context:@
initWithData_securityInformation_contextSelector :: Selector '[Id NSData, Id MEMessageSecurityInformation, Id NSData] (Id MEDecodedMessage)
initWithData_securityInformation_contextSelector = mkSelector "initWithData:securityInformation:context:"

-- | @Selector@ for @initWithData:securityInformation:context:banner:@
initWithData_securityInformation_context_bannerSelector :: Selector '[Id NSData, Id MEMessageSecurityInformation, Id NSData, Id MEDecodedMessageBanner] (Id MEDecodedMessage)
initWithData_securityInformation_context_bannerSelector = mkSelector "initWithData:securityInformation:context:banner:"

-- | @Selector@ for @rawData@
rawDataSelector :: Selector '[] (Id NSData)
rawDataSelector = mkSelector "rawData"

-- | @Selector@ for @securityInformation@
securityInformationSelector :: Selector '[] (Id MEMessageSecurityInformation)
securityInformationSelector = mkSelector "securityInformation"

-- | @Selector@ for @context@
contextSelector :: Selector '[] (Id NSData)
contextSelector = mkSelector "context"

-- | @Selector@ for @banner@
bannerSelector :: Selector '[] (Id MEDecodedMessageBanner)
bannerSelector = mkSelector "banner"

