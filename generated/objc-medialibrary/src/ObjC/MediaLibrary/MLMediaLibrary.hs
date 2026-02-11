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

import ObjC.MediaLibrary.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithOptions:@
initWithOptions :: (IsMLMediaLibrary mlMediaLibrary, IsNSDictionary options) => mlMediaLibrary -> options -> IO (Id MLMediaLibrary)
initWithOptions mlMediaLibrary  options =
withObjCPtr options $ \raw_options ->
    sendMsg mlMediaLibrary (mkSelector "initWithOptions:") (retPtr retVoid) [argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- mediaSources@
mediaSources :: IsMLMediaLibrary mlMediaLibrary => mlMediaLibrary -> IO (Id NSDictionary)
mediaSources mlMediaLibrary  =
  sendMsg mlMediaLibrary (mkSelector "mediaSources") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithOptions:@
initWithOptionsSelector :: Selector
initWithOptionsSelector = mkSelector "initWithOptions:"

-- | @Selector@ for @mediaSources@
mediaSourcesSelector :: Selector
mediaSourcesSelector = mkSelector "mediaSources"

