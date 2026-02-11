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
  , accountTypeDescriptionSelector
  , identifierSelector
  , accessGrantedSelector


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

import ObjC.Accounts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- accountTypeDescription@
accountTypeDescription :: IsACAccountType acAccountType => acAccountType -> IO (Id NSString)
accountTypeDescription acAccountType  =
  sendMsg acAccountType (mkSelector "accountTypeDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- identifier@
identifier :: IsACAccountType acAccountType => acAccountType -> IO (Id NSString)
identifier acAccountType  =
  sendMsg acAccountType (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- accessGranted@
accessGranted :: IsACAccountType acAccountType => acAccountType -> IO Bool
accessGranted acAccountType  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg acAccountType (mkSelector "accessGranted") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @accountTypeDescription@
accountTypeDescriptionSelector :: Selector
accountTypeDescriptionSelector = mkSelector "accountTypeDescription"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @accessGranted@
accessGrantedSelector :: Selector
accessGrantedSelector = mkSelector "accessGranted"

