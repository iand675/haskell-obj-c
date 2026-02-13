{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKSecureElementPass@.
module ObjC.PassKit.PKSecureElementPass
  ( PKSecureElementPass
  , IsPKSecureElementPass(..)
  , primaryAccountIdentifier
  , primaryAccountNumberSuffix
  , deviceAccountIdentifier
  , deviceAccountNumberSuffix
  , passActivationState
  , devicePassIdentifier
  , pairedTerminalIdentifier
  , deviceAccountIdentifierSelector
  , deviceAccountNumberSuffixSelector
  , devicePassIdentifierSelector
  , pairedTerminalIdentifierSelector
  , passActivationStateSelector
  , primaryAccountIdentifierSelector
  , primaryAccountNumberSuffixSelector

  -- * Enum types
  , PKSecureElementPassActivationState(PKSecureElementPassActivationState)
  , pattern PKSecureElementPassActivationStateActivated
  , pattern PKSecureElementPassActivationStateRequiresActivation
  , pattern PKSecureElementPassActivationStateActivating
  , pattern PKSecureElementPassActivationStateSuspended
  , pattern PKSecureElementPassActivationStateDeactivated

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.PassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- primaryAccountIdentifier@
primaryAccountIdentifier :: IsPKSecureElementPass pkSecureElementPass => pkSecureElementPass -> IO (Id NSString)
primaryAccountIdentifier pkSecureElementPass =
  sendMessage pkSecureElementPass primaryAccountIdentifierSelector

-- | @- primaryAccountNumberSuffix@
primaryAccountNumberSuffix :: IsPKSecureElementPass pkSecureElementPass => pkSecureElementPass -> IO (Id NSString)
primaryAccountNumberSuffix pkSecureElementPass =
  sendMessage pkSecureElementPass primaryAccountNumberSuffixSelector

-- | @- deviceAccountIdentifier@
deviceAccountIdentifier :: IsPKSecureElementPass pkSecureElementPass => pkSecureElementPass -> IO (Id NSString)
deviceAccountIdentifier pkSecureElementPass =
  sendMessage pkSecureElementPass deviceAccountIdentifierSelector

-- | @- deviceAccountNumberSuffix@
deviceAccountNumberSuffix :: IsPKSecureElementPass pkSecureElementPass => pkSecureElementPass -> IO (Id NSString)
deviceAccountNumberSuffix pkSecureElementPass =
  sendMessage pkSecureElementPass deviceAccountNumberSuffixSelector

-- | @- passActivationState@
passActivationState :: IsPKSecureElementPass pkSecureElementPass => pkSecureElementPass -> IO PKSecureElementPassActivationState
passActivationState pkSecureElementPass =
  sendMessage pkSecureElementPass passActivationStateSelector

-- | @- devicePassIdentifier@
devicePassIdentifier :: IsPKSecureElementPass pkSecureElementPass => pkSecureElementPass -> IO (Id NSString)
devicePassIdentifier pkSecureElementPass =
  sendMessage pkSecureElementPass devicePassIdentifierSelector

-- | @- pairedTerminalIdentifier@
pairedTerminalIdentifier :: IsPKSecureElementPass pkSecureElementPass => pkSecureElementPass -> IO (Id NSString)
pairedTerminalIdentifier pkSecureElementPass =
  sendMessage pkSecureElementPass pairedTerminalIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @primaryAccountIdentifier@
primaryAccountIdentifierSelector :: Selector '[] (Id NSString)
primaryAccountIdentifierSelector = mkSelector "primaryAccountIdentifier"

-- | @Selector@ for @primaryAccountNumberSuffix@
primaryAccountNumberSuffixSelector :: Selector '[] (Id NSString)
primaryAccountNumberSuffixSelector = mkSelector "primaryAccountNumberSuffix"

-- | @Selector@ for @deviceAccountIdentifier@
deviceAccountIdentifierSelector :: Selector '[] (Id NSString)
deviceAccountIdentifierSelector = mkSelector "deviceAccountIdentifier"

-- | @Selector@ for @deviceAccountNumberSuffix@
deviceAccountNumberSuffixSelector :: Selector '[] (Id NSString)
deviceAccountNumberSuffixSelector = mkSelector "deviceAccountNumberSuffix"

-- | @Selector@ for @passActivationState@
passActivationStateSelector :: Selector '[] PKSecureElementPassActivationState
passActivationStateSelector = mkSelector "passActivationState"

-- | @Selector@ for @devicePassIdentifier@
devicePassIdentifierSelector :: Selector '[] (Id NSString)
devicePassIdentifierSelector = mkSelector "devicePassIdentifier"

-- | @Selector@ for @pairedTerminalIdentifier@
pairedTerminalIdentifierSelector :: Selector '[] (Id NSString)
pairedTerminalIdentifierSelector = mkSelector "pairedTerminalIdentifier"

