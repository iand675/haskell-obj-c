{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFSafariExtension@.
module ObjC.SafariServices.SFSafariExtension
  ( SFSafariExtension
  , IsSFSafariExtension(..)
  , new
  , init_
  , getBaseURIWithCompletionHandler
  , newSelector
  , initSelector
  , getBaseURIWithCompletionHandlerSelector


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

import ObjC.SafariServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id SFSafariExtension)
new  =
  do
    cls' <- getRequiredClass "SFSafariExtension"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSFSafariExtension sfSafariExtension => sfSafariExtension -> IO (Id SFSafariExtension)
init_ sfSafariExtension  =
  sendMsg sfSafariExtension (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Calls the completion handler with the base URI of the extension.
--
-- ObjC selector: @+ getBaseURIWithCompletionHandler:@
getBaseURIWithCompletionHandler :: Ptr () -> IO ()
getBaseURIWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "SFSafariExtension"
    sendClassMsg cls' (mkSelector "getBaseURIWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @getBaseURIWithCompletionHandler:@
getBaseURIWithCompletionHandlerSelector :: Selector
getBaseURIWithCompletionHandlerSelector = mkSelector "getBaseURIWithCompletionHandler:"

