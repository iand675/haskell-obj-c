{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSError@.
module ObjC.FileProvider.NSError
  ( NSError
  , IsNSError(..)
  , fileProviderErrorForNonExistentItemWithIdentifier
  , fileProviderErrorForNonExistentItemWithIdentifierSelector


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

import ObjC.FileProvider.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ fileProviderErrorForNonExistentItemWithIdentifier:@
fileProviderErrorForNonExistentItemWithIdentifier :: IsNSString itemIdentifier => itemIdentifier -> IO (Id NSError)
fileProviderErrorForNonExistentItemWithIdentifier itemIdentifier =
  do
    cls' <- getRequiredClass "NSError"
    withObjCPtr itemIdentifier $ \raw_itemIdentifier ->
      sendClassMsg cls' (mkSelector "fileProviderErrorForNonExistentItemWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_itemIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fileProviderErrorForNonExistentItemWithIdentifier:@
fileProviderErrorForNonExistentItemWithIdentifierSelector :: Selector
fileProviderErrorForNonExistentItemWithIdentifierSelector = mkSelector "fileProviderErrorForNonExistentItemWithIdentifier:"

