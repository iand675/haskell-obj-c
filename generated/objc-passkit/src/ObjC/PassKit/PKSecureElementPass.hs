{-# LANGUAGE PatternSynonyms #-}
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
  , primaryAccountIdentifierSelector
  , primaryAccountNumberSuffixSelector
  , deviceAccountIdentifierSelector
  , deviceAccountNumberSuffixSelector
  , passActivationStateSelector
  , devicePassIdentifierSelector
  , pairedTerminalIdentifierSelector

  -- * Enum types
  , PKSecureElementPassActivationState(PKSecureElementPassActivationState)
  , pattern PKSecureElementPassActivationStateActivated
  , pattern PKSecureElementPassActivationStateRequiresActivation
  , pattern PKSecureElementPassActivationStateActivating
  , pattern PKSecureElementPassActivationStateSuspended
  , pattern PKSecureElementPassActivationStateDeactivated

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
import ObjC.Foundation.Internal.Classes

-- | @- primaryAccountIdentifier@
primaryAccountIdentifier :: IsPKSecureElementPass pkSecureElementPass => pkSecureElementPass -> IO (Id NSString)
primaryAccountIdentifier pkSecureElementPass  =
  sendMsg pkSecureElementPass (mkSelector "primaryAccountIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- primaryAccountNumberSuffix@
primaryAccountNumberSuffix :: IsPKSecureElementPass pkSecureElementPass => pkSecureElementPass -> IO (Id NSString)
primaryAccountNumberSuffix pkSecureElementPass  =
  sendMsg pkSecureElementPass (mkSelector "primaryAccountNumberSuffix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- deviceAccountIdentifier@
deviceAccountIdentifier :: IsPKSecureElementPass pkSecureElementPass => pkSecureElementPass -> IO (Id NSString)
deviceAccountIdentifier pkSecureElementPass  =
  sendMsg pkSecureElementPass (mkSelector "deviceAccountIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- deviceAccountNumberSuffix@
deviceAccountNumberSuffix :: IsPKSecureElementPass pkSecureElementPass => pkSecureElementPass -> IO (Id NSString)
deviceAccountNumberSuffix pkSecureElementPass  =
  sendMsg pkSecureElementPass (mkSelector "deviceAccountNumberSuffix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- passActivationState@
passActivationState :: IsPKSecureElementPass pkSecureElementPass => pkSecureElementPass -> IO PKSecureElementPassActivationState
passActivationState pkSecureElementPass  =
  fmap (coerce :: CLong -> PKSecureElementPassActivationState) $ sendMsg pkSecureElementPass (mkSelector "passActivationState") retCLong []

-- | @- devicePassIdentifier@
devicePassIdentifier :: IsPKSecureElementPass pkSecureElementPass => pkSecureElementPass -> IO (Id NSString)
devicePassIdentifier pkSecureElementPass  =
  sendMsg pkSecureElementPass (mkSelector "devicePassIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pairedTerminalIdentifier@
pairedTerminalIdentifier :: IsPKSecureElementPass pkSecureElementPass => pkSecureElementPass -> IO (Id NSString)
pairedTerminalIdentifier pkSecureElementPass  =
  sendMsg pkSecureElementPass (mkSelector "pairedTerminalIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @primaryAccountIdentifier@
primaryAccountIdentifierSelector :: Selector
primaryAccountIdentifierSelector = mkSelector "primaryAccountIdentifier"

-- | @Selector@ for @primaryAccountNumberSuffix@
primaryAccountNumberSuffixSelector :: Selector
primaryAccountNumberSuffixSelector = mkSelector "primaryAccountNumberSuffix"

-- | @Selector@ for @deviceAccountIdentifier@
deviceAccountIdentifierSelector :: Selector
deviceAccountIdentifierSelector = mkSelector "deviceAccountIdentifier"

-- | @Selector@ for @deviceAccountNumberSuffix@
deviceAccountNumberSuffixSelector :: Selector
deviceAccountNumberSuffixSelector = mkSelector "deviceAccountNumberSuffix"

-- | @Selector@ for @passActivationState@
passActivationStateSelector :: Selector
passActivationStateSelector = mkSelector "passActivationState"

-- | @Selector@ for @devicePassIdentifier@
devicePassIdentifierSelector :: Selector
devicePassIdentifierSelector = mkSelector "devicePassIdentifier"

-- | @Selector@ for @pairedTerminalIdentifier@
pairedTerminalIdentifierSelector :: Selector
pairedTerminalIdentifierSelector = mkSelector "pairedTerminalIdentifier"

