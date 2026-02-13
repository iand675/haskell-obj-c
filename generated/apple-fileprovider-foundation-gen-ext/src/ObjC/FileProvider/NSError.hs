{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FileProvider.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ fileProviderErrorForNonExistentItemWithIdentifier:@
fileProviderErrorForNonExistentItemWithIdentifier :: IsNSString itemIdentifier => itemIdentifier -> IO (Id NSError)
fileProviderErrorForNonExistentItemWithIdentifier itemIdentifier =
  do
    cls' <- getRequiredClass "NSError"
    sendClassMessage cls' fileProviderErrorForNonExistentItemWithIdentifierSelector (toNSString itemIdentifier)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fileProviderErrorForNonExistentItemWithIdentifier:@
fileProviderErrorForNonExistentItemWithIdentifierSelector :: Selector '[Id NSString] (Id NSError)
fileProviderErrorForNonExistentItemWithIdentifierSelector = mkSelector "fileProviderErrorForNonExistentItemWithIdentifier:"

