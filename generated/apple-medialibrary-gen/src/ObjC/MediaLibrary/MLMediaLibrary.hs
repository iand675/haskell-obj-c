{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MLMediaLibrary@.
module ObjC.MediaLibrary.MLMediaLibrary
  ( MLMediaLibrary
  , IsMLMediaLibrary(..)
  , initWithOptions
  , mediaSources
  , initWithOptionsSelector
  , mediaSourcesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaLibrary.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithOptions:@
initWithOptions :: (IsMLMediaLibrary mlMediaLibrary, IsNSDictionary options) => mlMediaLibrary -> options -> IO (Id MLMediaLibrary)
initWithOptions mlMediaLibrary options =
  sendOwnedMessage mlMediaLibrary initWithOptionsSelector (toNSDictionary options)

-- | @- mediaSources@
mediaSources :: IsMLMediaLibrary mlMediaLibrary => mlMediaLibrary -> IO (Id NSDictionary)
mediaSources mlMediaLibrary =
  sendMessage mlMediaLibrary mediaSourcesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithOptions:@
initWithOptionsSelector :: Selector '[Id NSDictionary] (Id MLMediaLibrary)
initWithOptionsSelector = mkSelector "initWithOptions:"

-- | @Selector@ for @mediaSources@
mediaSourcesSelector :: Selector '[] (Id NSDictionary)
mediaSourcesSelector = mkSelector "mediaSources"

