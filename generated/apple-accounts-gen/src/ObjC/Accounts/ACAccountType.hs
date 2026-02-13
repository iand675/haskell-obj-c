{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ACAccountType@.
module ObjC.Accounts.ACAccountType
  ( ACAccountType
  , IsACAccountType(..)
  , accountTypeDescription
  , identifier
  , accessGranted
  , accessGrantedSelector
  , accountTypeDescriptionSelector
  , identifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accounts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- accountTypeDescription@
accountTypeDescription :: IsACAccountType acAccountType => acAccountType -> IO (Id NSString)
accountTypeDescription acAccountType =
  sendMessage acAccountType accountTypeDescriptionSelector

-- | @- identifier@
identifier :: IsACAccountType acAccountType => acAccountType -> IO (Id NSString)
identifier acAccountType =
  sendMessage acAccountType identifierSelector

-- | @- accessGranted@
accessGranted :: IsACAccountType acAccountType => acAccountType -> IO Bool
accessGranted acAccountType =
  sendMessage acAccountType accessGrantedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @accountTypeDescription@
accountTypeDescriptionSelector :: Selector '[] (Id NSString)
accountTypeDescriptionSelector = mkSelector "accountTypeDescription"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @accessGranted@
accessGrantedSelector :: Selector '[] Bool
accessGrantedSelector = mkSelector "accessGranted"

