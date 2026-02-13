{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contains information about a message signer
--
-- Generated bindings for @MEMessageSigner@.
module ObjC.MailKit.MEMessageSigner
  ( MEMessageSigner
  , IsMEMessageSigner(..)
  , new
  , init_
  , initWithEmailAddresses_signatureLabel_context
  , emailAddresses
  , label
  , context
  , contextSelector
  , emailAddressesSelector
  , initSelector
  , initWithEmailAddresses_signatureLabel_contextSelector
  , labelSelector
  , newSelector


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
new :: IO (Id MEMessageSigner)
new  =
  do
    cls' <- getRequiredClass "MEMessageSigner"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMEMessageSigner meMessageSigner => meMessageSigner -> IO (Id MEMessageSigner)
init_ meMessageSigner =
  sendOwnedMessage meMessageSigner initSelector

-- | @- initWithEmailAddresses:signatureLabel:context:@
initWithEmailAddresses_signatureLabel_context :: (IsMEMessageSigner meMessageSigner, IsNSArray emailAddresses, IsNSString label, IsNSData context) => meMessageSigner -> emailAddresses -> label -> context -> IO (Id MEMessageSigner)
initWithEmailAddresses_signatureLabel_context meMessageSigner emailAddresses label context =
  sendOwnedMessage meMessageSigner initWithEmailAddresses_signatureLabel_contextSelector (toNSArray emailAddresses) (toNSString label) (toNSData context)

-- | Email addresses associated with the signature.
--
-- ObjC selector: @- emailAddresses@
emailAddresses :: IsMEMessageSigner meMessageSigner => meMessageSigner -> IO (Id NSArray)
emailAddresses meMessageSigner =
  sendMessage meMessageSigner emailAddressesSelector

-- | The message signers label. Shown in the message header view. For instance, "John Smith".
--
-- ObjC selector: @- label@
label :: IsMEMessageSigner meMessageSigner => meMessageSigner -> IO (Id NSString)
label meMessageSigner =
  sendMessage meMessageSigner labelSelector

-- | The context for the message signature. This might include the signing certificate. This will be passed back to the extension for either verifying the signature or if the user wishes to view signature information.
--
-- ObjC selector: @- context@
context :: IsMEMessageSigner meMessageSigner => meMessageSigner -> IO (Id NSData)
context meMessageSigner =
  sendMessage meMessageSigner contextSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MEMessageSigner)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MEMessageSigner)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithEmailAddresses:signatureLabel:context:@
initWithEmailAddresses_signatureLabel_contextSelector :: Selector '[Id NSArray, Id NSString, Id NSData] (Id MEMessageSigner)
initWithEmailAddresses_signatureLabel_contextSelector = mkSelector "initWithEmailAddresses:signatureLabel:context:"

-- | @Selector@ for @emailAddresses@
emailAddressesSelector :: Selector '[] (Id NSArray)
emailAddressesSelector = mkSelector "emailAddresses"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @context@
contextSelector :: Selector '[] (Id NSData)
contextSelector = mkSelector "context"

